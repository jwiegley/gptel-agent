;;; gptel-agent-safety.el --- Doom loop detection for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: January 2025
;; Version: 0.1.0
;; Keywords: tools, convenience, safety
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5") (ring "1.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module implements doom loop detection and prevention for gptel-agent,
;; protecting against repetitive tool call patterns that waste tokens and time.
;;
;; Features:
;; - Ring-buffer based tracking of recent tool calls
;; - Multiple detection algorithms for identical and similar patterns
;; - Configurable thresholds and similarity scoring
;; - Interactive intervention UI with multiple resolution options
;; - Token waste estimation
;; - Detailed logging of incidents
;; - Global and per-session control
;;
;; Detection Patterns:
;;
;; 1. Identical Calls: Exact repetition of tool name and arguments
;; 2. Similar Calls: Near-identical calls (using string-distance)
;; 3. Alternating Pattern: A->B->A->B cycling
;; 4. Oscillating Results: Same tool returning different results repeatedly
;;
;; Configuration:
;;
;; Customize `gptel-agent-doom-loop-threshold' to set how many repetitions
;; trigger detection (default 3).
;;
;; Customize `gptel-agent-doom-loop-action' to control what happens when
;; a loop is detected:
;;   - 'warn: Show warning buffer and prompt for action
;;   - 'auto-adjust: Automatically adjust temperature and continue
;;   - 'abort: Immediately stop execution
;;
;; Usage:
;;
;;   (gptel-agent-safety-mode 1)  ; Enable globally
;;   (gptel-agent-doom-loop-status)  ; View current state
;;
;; The system automatically integrates with gptel-agent-tools.el via
;; `gptel-post-response-functions' hook.

;;; Code:

(require 'cl-lib)
(require 'ring)

(declare-function gptel--update-status "gptel" (msg &optional face))

(defgroup gptel-agent-safety nil
  "Doom loop detection and prevention for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-safety-")

;;;; Customization Options

(defcustom gptel-agent-doom-loop-threshold 3
  "Number of repetitions before triggering doom loop detection.

When a tool call pattern repeats this many times, the doom loop
detector will activate and potentially intervene."
  :type 'integer
  :group 'gptel-agent-safety)

(defcustom gptel-agent-doom-loop-similarity 0.8
  "Similarity threshold for detecting near-identical calls.

A value between 0.0 (completely different) and 1.0 (identical).
Calls with similarity above this threshold are considered part of
a potential doom loop.  Default 0.8 catches minor variations while
allowing intentional parameter changes."
  :type 'float
  :group 'gptel-agent-safety)

(defcustom gptel-agent-doom-loop-action 'warn
  "Action to take when a doom loop is detected.

- `warn': Display warning buffer and prompt user for action
- `auto-adjust': Automatically adjust temperature and continue
- `abort': Immediately stop execution"
  :type '(choice (const :tag "Warn user" warn)
                 (const :tag "Auto-adjust parameters" auto-adjust)
                 (const :tag "Abort execution" abort))
  :group 'gptel-agent-safety)

(defcustom gptel-agent-doom-loop-buffer-size 20
  "Size of the ring buffer for tracking recent tool calls.

Larger values allow detection of longer-period patterns but use
more memory.  Must be at least twice `gptel-agent-doom-loop-threshold'."
  :type 'integer
  :group 'gptel-agent-safety)

(defcustom gptel-agent-doom-loop-enabled t
  "Whether doom loop detection is enabled.

When nil, tool calls are not tracked and detection does not occur."
  :type 'boolean
  :group 'gptel-agent-safety)

;;;; Internal State

(defvar-local gptel-agent--recent-tool-calls nil
  "Ring buffer of recent tool calls.

Each entry is a plist with keys:
  :tool      - Tool name (string)
  :args      - Tool arguments (plist or list)
  :result    - Tool execution result (string)
  :timestamp - Time of execution (time value)")

(defvar-local gptel-agent--doom-loop-log nil
  "List of doom loop incidents in this buffer.

Each entry is a plist with keys:
  :timestamp - When the loop was detected
  :pattern   - Description of the detected pattern
  :count     - Number of repetitions
  :action    - Action taken (continue/abort/adjust)
  :score     - Confidence score (0.0-1.0)")

;;;; Ring Buffer Management

(defun gptel-agent--ensure-ring ()
  "Ensure the tool call ring buffer exists and is properly sized."
  (unless gptel-agent--recent-tool-calls
    (setq gptel-agent--recent-tool-calls
          (make-ring gptel-agent-doom-loop-buffer-size)))
  ;; Resize if configuration changed
  (when (/= (ring-size gptel-agent--recent-tool-calls)
            gptel-agent-doom-loop-buffer-size)
    (let ((old-ring gptel-agent--recent-tool-calls)
          (new-ring (make-ring gptel-agent-doom-loop-buffer-size)))
      (dotimes (i (min (ring-length old-ring)
                       gptel-agent-doom-loop-buffer-size))
        (ring-insert new-ring (ring-ref old-ring i)))
      (setq gptel-agent--recent-tool-calls new-ring))))

(defun gptel-agent--track-tool-call (tool args result)
  "Add a tool call entry to the tracking ring buffer.

TOOL is the tool name (string or symbol).
ARGS is the argument list or plist.
RESULT is the execution result (string)."
  (when gptel-agent-doom-loop-enabled
    (gptel-agent--ensure-ring)
    (ring-insert gptel-agent--recent-tool-calls
                 (list :tool (if (symbolp tool) (symbol-name tool) tool)
                       :args args
                       :result result
                       :timestamp (current-time)))))

(defun gptel-agent--get-recent-calls (&optional n)
  "Retrieve the last N tool call entries from the ring buffer.

Returns a list of plists, most recent first.  If N is nil or larger
than the ring length, returns all entries."
  (when gptel-agent--recent-tool-calls
    (let* ((ring-len (ring-length gptel-agent--recent-tool-calls))
           (n (or n ring-len))
           (count (min n ring-len))
           (result nil))
      (dotimes (i count)
        (push (ring-ref gptel-agent--recent-tool-calls i) result))
      ;; ring-ref 0 is most recent, so reversing gives most recent first
      (nreverse result))))

(defun gptel-agent--clear-tool-history ()
  "Reset the tool call tracking ring buffer."
  (interactive)
  (when gptel-agent--recent-tool-calls
    (setq gptel-agent--recent-tool-calls
          (make-ring gptel-agent-doom-loop-buffer-size)))
  (message "Tool call history cleared"))

;;;; Pattern Detection Utilities

(defun gptel-agent--normalize-tool-args (tool args)
  "Normalize ARGS for TOOL to enable consistent comparison.

Handles path normalization, whitespace trimming, and argument ordering."
  (let ((normalized
         (cond
          ;; Plist args
          ((and (consp args) (keywordp (car args)))
           (let ((result nil))
             (cl-loop for (key val) on args by #'cddr
                      do (push key result)
                      (push (if (stringp val)
                                (string-trim val)
                              val)
                            result))
             (nreverse result)))
          ;; List args
          ((consp args)
           (mapcar (lambda (arg)
                     (if (stringp arg)
                         (string-trim arg)
                       arg))
                   args))
          ;; Single value
          ((stringp args)
           (string-trim args))
          ;; Other
          (t args))))
    ;; Normalize file paths for bash, read, write, edit tools
    (when (member tool '("Bash" "bash" "Read" "read" "Write" "write"
                         "Edit" "edit" "Grep" "grep" "Glob" "glob"))
      (setq normalized
            (if (consp normalized)
                (mapcar (lambda (arg)
                          (if (and (stringp arg)
                                   (string-match-p "/" arg))
                              (expand-file-name arg)
                            arg))
                        normalized)
              (when (and (stringp normalized)
                         (string-match-p "/" normalized))
                (expand-file-name normalized)))))
    normalized))

(defun gptel-agent--calls-identical-p (call1 call2)
  "Check if CALL1 and CALL2 are identical tool calls.

Compares tool names and normalized arguments for exact match."
  (and (equal (plist-get call1 :tool)
              (plist-get call2 :tool))
       (equal (gptel-agent--normalize-tool-args
               (plist-get call1 :tool)
               (plist-get call1 :args))
              (gptel-agent--normalize-tool-args
               (plist-get call2 :tool)
               (plist-get call2 :args)))))

(defun gptel-agent--calls-similar-p (call1 call2 threshold)
  "Check if CALL1 and CALL2 are similar within THRESHOLD.

Uses string-distance (Levenshtein) to compare serialized forms.
Returns non-nil if similarity is >= THRESHOLD (0.0-1.0)."
  (let* ((tool1 (plist-get call1 :tool))
         (tool2 (plist-get call2 :tool)))
    ;; Tools must be the same
    (when (equal tool1 tool2)
      (let* ((args1-str (format "%S" (gptel-agent--normalize-tool-args
                                      tool1 (plist-get call1 :args))))
             (args2-str (format "%S" (gptel-agent--normalize-tool-args
                                      tool2 (plist-get call2 :args))))
             (max-len (max (length args1-str) (length args2-str))))
        (when (> max-len 0)
          (let ((distance (string-distance args1-str args2-str)))
            (>= (- 1.0 (/ (float distance) max-len)) threshold)))))))

;;;; Pattern Detection Algorithms

(defun gptel-agent--detect-identical-sequence ()
  "Detect sequences of identical tool calls.

Returns plist with :type 'identical, :pattern, :count, or nil.
Counts all consecutive identical calls from the most recent one."
  ;; First check if threshold is met using minimum calls
  (let ((threshold-calls (gptel-agent--get-recent-calls gptel-agent-doom-loop-threshold)))
    (when (>= (length threshold-calls) gptel-agent-doom-loop-threshold)
      (let ((first-call (car threshold-calls))
            (all-identical t))
        ;; Check if threshold calls are all identical
        (dolist (call (cdr threshold-calls))
          (unless (gptel-agent--calls-identical-p first-call call)
            (setq all-identical nil)))
        (when all-identical
          ;; Now count ALL consecutive identical calls from the buffer
          (let* ((all-calls (gptel-agent--get-recent-calls))
                 (count 0))
            (cl-loop for call in all-calls
                     while (gptel-agent--calls-identical-p first-call call)
                     do (cl-incf count))
            (list :type 'identical
                  :pattern (format "Identical call to '%s' with args: %S"
                                   (plist-get first-call :tool)
                                   (plist-get first-call :args))
                  :count count)))))))

(defun gptel-agent--detect-similar-sequence ()
  "Detect sequences of similar (but not identical) tool calls.

Returns plist with :type 'similar, :pattern, :count, or nil.
Counts all consecutive similar calls from the most recent one."
  (let ((threshold-calls (gptel-agent--get-recent-calls gptel-agent-doom-loop-threshold)))
    (when (>= (length threshold-calls) gptel-agent-doom-loop-threshold)
      (let ((first-call (car threshold-calls))
            (all-similar t))
        (dolist (call (cdr threshold-calls))
          (unless (or (gptel-agent--calls-identical-p first-call call)
                      (gptel-agent--calls-similar-p
                       first-call call gptel-agent-doom-loop-similarity))
            (setq all-similar nil)))
        (when all-similar
          ;; Now count ALL consecutive similar calls from the buffer
          (let* ((all-calls (gptel-agent--get-recent-calls))
                 (count 0))
            (cl-loop for call in all-calls
                     while (or (gptel-agent--calls-identical-p first-call call)
                               (gptel-agent--calls-similar-p
                                first-call call gptel-agent-doom-loop-similarity))
                     do (cl-incf count))
            (list :type 'similar
                  :pattern (format "Similar calls to '%s' (threshold: %.2f)"
                                   (plist-get first-call :tool)
                                   gptel-agent-doom-loop-similarity)
                  :count count)))))))

(defun gptel-agent--detect-alternating-pattern ()
  "Detect A->B->A->B alternating pattern.

Returns plist with :type 'alternating, :pattern, :count, or nil."
  (let ((calls (gptel-agent--get-recent-calls (* 2 gptel-agent-doom-loop-threshold))))
    (when (>= (length calls) (* 2 gptel-agent-doom-loop-threshold))
      (let ((pairs nil))
        ;; Group into consecutive pairs
        (cl-loop for (a b) on calls by #'cddr
                 while b
                 do (push (cons a b) pairs))
        (setq pairs (nreverse pairs))
        ;; Check if all pairs match the first pair
        (when (>= (length pairs) gptel-agent-doom-loop-threshold)
          (let ((first-pair (car pairs))
                (all-match t))
            (dolist (pair (cdr pairs))
              (unless (and (gptel-agent--calls-identical-p
                            (car first-pair) (car pair))
                           (gptel-agent--calls-identical-p
                            (cdr first-pair) (cdr pair)))
                (setq all-match nil)))
            (when all-match
              (list :type 'alternating
                    :pattern (format "Alternating between '%s' and '%s'"
                                     (plist-get (car first-pair) :tool)
                                     (plist-get (cdr first-pair) :tool))
                    :count (* 2 (length pairs))))))))))

(defun gptel-agent--detect-oscillating-results ()
  "Detect same tool with different results repeatedly.

Returns plist with :type 'oscillating, :pattern, :count, or nil."
  (let ((calls (gptel-agent--get-recent-calls gptel-agent-doom-loop-threshold)))
    (when (>= (length calls) gptel-agent-doom-loop-threshold)
      (let ((tool (plist-get (car calls) :tool))
            (args (plist-get (car calls) :args))
            (results (mapcar (lambda (c) (plist-get c :result)) calls))
            (same-tool t)
            (same-args t))
        ;; Check if all calls are to the same tool with same args
        (dolist (call calls)
          (unless (equal (plist-get call :tool) tool)
            (setq same-tool nil))
          (unless (equal (plist-get call :args) args)
            (setq same-args nil)))
        ;; Check if results differ
        (when (and same-tool same-args
                   (> (length (delete-dups (copy-sequence results))) 1))
          (list :type 'oscillating
                :pattern (format "Tool '%s' returning varying results" tool)
                :count (length calls)))))))

(defun gptel-agent--detect-doom-loop ()
  "Main doom loop detection function.

Runs all detection algorithms and returns the first match found,
or nil if no loop detected."
  (or (gptel-agent--detect-identical-sequence)
      (gptel-agent--detect-similar-sequence)
      (gptel-agent--detect-alternating-pattern)
      (gptel-agent--detect-oscillating-results)))

(defun gptel-agent--doom-loop-score ()
  "Calculate confidence score for current potential doom loop.

Returns a float between 0.0 (no confidence) and 1.0 (certain)."
  (let ((loop-info (gptel-agent--detect-doom-loop)))
    (if (not loop-info)
        0.0
      (let ((type-base (pcase (plist-get loop-info :type)
                         ('identical 0.8)
                         ('similar 0.7)
                         ('alternating 0.65)
                         ('oscillating 0.6)
                         (_ 0.5)))
            (count (plist-get loop-info :count))
            (threshold gptel-agent-doom-loop-threshold))
        ;; Score increases with count beyond threshold, asymptotically approaching 1.0
        ;; count-factor is >= 1 when threshold is met
        (let* ((count-factor (/ (float count) threshold))
               ;; Linear bonus for exceeding threshold, capped
               ;; Each extra repetition beyond threshold adds ~0.02 to the score
               (extra-reps (max 0 (- count threshold)))
               (bonus (* 0.02 extra-reps))
               (score (+ type-base bonus)))
          (min 1.0 score))))))

;;;; Token Estimation

(defun gptel-agent--estimate-wasted-tokens (calls)
  "Estimate number of wasted tokens from CALLS.

This is a rough approximation: 1 token ≈ 4 characters for English.
Includes tool call overhead and response tokens."
  (let ((total-chars 0))
    (dolist (call calls)
      ;; Tool call: name + args (serialized)
      (let* ((tool (plist-get call :tool))
             (args (format "%S" (plist-get call :args)))
             (result (or (plist-get call :result) ""))
             ;; Overhead: JSON structure, field names
             (overhead 100))
        (setq total-chars (+ total-chars
                             (length tool)
                             (length args)
                             (length result)
                             overhead))))
    ;; Convert chars to tokens (rough)
    (/ total-chars 4)))

;;;; Intervention UI

(defvar gptel-agent-doom-loop-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'gptel-agent-doom-loop-continue)
    (define-key map (kbd "r") #'gptel-agent-doom-loop-retry)
    (define-key map (kbd "a") #'gptel-agent-doom-loop-abort)
    (define-key map (kbd "q") #'gptel-agent-doom-loop-dismiss)
    map)
  "Keymap for `gptel-agent-doom-loop-mode'.")

(define-derived-mode gptel-agent-doom-loop-mode special-mode "DoomLoop"
  "Major mode for doom loop warning buffer.

Key bindings:
\\{gptel-agent-doom-loop-mode-map}"
  (setq-local cursor-type nil)
  (setq buffer-read-only t))

(defvar gptel-agent--doom-loop-decision nil
  "Stores user decision from doom loop intervention buffer.")

(defun gptel-agent-doom-loop-continue ()
  "Continue execution, optionally adjusting temperature."
  (interactive)
  (setq gptel-agent--doom-loop-decision 'continue)
  (quit-window t))

(defun gptel-agent-doom-loop-retry ()
  "Retry with guidance (increase temperature, add context)."
  (interactive)
  (setq gptel-agent--doom-loop-decision 'retry)
  (quit-window t))

(defun gptel-agent-doom-loop-abort ()
  "Abort current task."
  (interactive)
  (setq gptel-agent--doom-loop-decision 'abort)
  (quit-window t))

(defun gptel-agent-doom-loop-dismiss ()
  "Dismiss warning without action (same as continue)."
  (interactive)
  (setq gptel-agent--doom-loop-decision 'continue)
  (quit-window t))

(defun gptel-agent--display-doom-loop-warning (loop-info)
  "Display warning buffer for detected doom loop LOOP-INFO.

Shows pattern details, affected calls, token waste estimate,
and prompts user for action."
  (let ((buffer (get-buffer-create "*GPTel Doom Loop Warning*"))
        (calls (gptel-agent--get-recent-calls (plist-get loop-info :count)))
        (score (gptel-agent--doom-loop-score))
        (wasted-tokens (gptel-agent--estimate-wasted-tokens calls)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (gptel-agent-doom-loop-mode)
        (insert (propertize "⚠ DOOM LOOP DETECTED\n\n" 'face 'error))
        (insert (propertize "Pattern: " 'face 'bold))
        (insert (plist-get loop-info :pattern) "\n")
        (insert (propertize "Type: " 'face 'bold))
        (insert (format "%s\n" (plist-get loop-info :type)))
        (insert (propertize "Repetitions: " 'face 'bold))
        (insert (format "%d\n" (plist-get loop-info :count)))
        (insert (propertize "Confidence: " 'face 'bold))
        (insert (format "%.0f%%\n" (* 100 score)))
        (insert (propertize "Estimated wasted tokens: " 'face 'bold))
        (insert (format "~%d\n\n" wasted-tokens))

        (insert (propertize "Recent Tool Calls:\n" 'face 'bold))
        (insert (propertize (make-string 60 ?-) 'face 'shadow) "\n")
        (dolist (call calls)
          (insert (format "• %s" (plist-get call :tool)))
          (let ((args (plist-get call :args)))
            (when args
              (insert (format "\n  Args: %S" args))))
          (let ((result (plist-get call :result)))
            (when (and result (< (length result) 200))
              (insert (format "\n  Result: %s"
                              (truncate-string-to-width result 80 nil nil t)))))
          (insert "\n\n"))

        (insert (propertize (make-string 60 ?=) 'face 'shadow) "\n\n")
        (insert (propertize "Actions:\n" 'face 'bold))
        (insert "  [c] Continue - Proceed with current parameters\n")
        (insert "  [r] Retry - Increase temperature and add guidance\n")
        (insert "  [a] Abort - Stop current task\n")
        (insert "  [q] Dismiss - Hide this warning\n\n")
        (insert (propertize "Choose an action: " 'face 'warning)))
      (goto-char (point-min))
      (pop-to-buffer buffer)
      (setq gptel-agent--doom-loop-decision nil)
      ;; Wait for user input
      (while (not gptel-agent--doom-loop-decision)
        (sit-for 0.1))
      gptel-agent--doom-loop-decision)))

;;;; Integration and Control

(defun gptel-agent--handle-doom-loop (loop-info)
  "Handle detected doom loop according to configuration.

LOOP-INFO is the plist returned by `gptel-agent--detect-doom-loop'.
Returns the action taken: continue, abort, or adjust."
  (let ((action gptel-agent-doom-loop-action)
        (decision nil))
    (pcase action
      ('warn
       (setq decision (gptel-agent--display-doom-loop-warning loop-info)))
      ('auto-adjust
       (message "Doom loop detected - auto-adjusting parameters...")
       (setq decision 'retry))
      ('abort
       (message "Doom loop detected - aborting!")
       (setq decision 'abort)))

    ;; Log the incident
    (push (list :timestamp (current-time)
                :pattern (plist-get loop-info :pattern)
                :count (plist-get loop-info :count)
                :action decision
                :score (gptel-agent--doom-loop-score))
          gptel-agent--doom-loop-log)

    decision))

(defun gptel-agent--safety-post-response-hook (status)
  "Hook function for `gptel-post-response-functions'.

STATUS is the response status from gptel.
Performs doom loop detection after each LLM response."
  (when (and gptel-agent-doom-loop-enabled
             (eq status 'finished))
    (when-let ((loop-info (gptel-agent--detect-doom-loop)))
      (let ((decision (gptel-agent--handle-doom-loop loop-info)))
        (pcase decision
          ('abort
           (gptel--update-status
            " [Doom Loop - Aborted]" 'error)
           (keyboard-quit))
          ('retry
           (gptel--update-status
            " [Doom Loop - Adjusting]" 'warning)))))))

;;;###autoload
(defun gptel-agent-doom-loop-status ()
  "Display current doom loop detection status and history."
  (interactive)
  (let ((recent-calls (gptel-agent--get-recent-calls 10))
        (log gptel-agent--doom-loop-log)
        (loop-info (gptel-agent--detect-doom-loop))
        (score (gptel-agent--doom-loop-score)))
    (with-current-buffer (get-buffer-create "*GPTel Safety Status*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize "GPTel Agent Safety Status\n" 'face 'bold))
        (insert (propertize (make-string 40 ?=) 'face 'shadow) "\n\n")

        (insert (propertize "Configuration:\n" 'face 'bold))
        (insert (format "  Enabled: %s\n" gptel-agent-doom-loop-enabled))
        (insert (format "  Threshold: %d repetitions\n"
                        gptel-agent-doom-loop-threshold))
        (insert (format "  Similarity: %.2f\n"
                        gptel-agent-doom-loop-similarity))
        (insert (format "  Action: %s\n" gptel-agent-doom-loop-action))
        (insert (format "  Buffer size: %d\n\n"
                        gptel-agent-doom-loop-buffer-size))

        (insert (propertize "Current State:\n" 'face 'bold))
        (insert (format "  Tracked calls: %d\n"
                        (if gptel-agent--recent-tool-calls
                            (ring-length gptel-agent--recent-tool-calls)
                          0)))
        (insert (format "  Loop detected: %s\n"
                        (if loop-info "YES" "NO")))
        (insert (format "  Confidence: %.0f%%\n\n" (* 100 score)))

        (when loop-info
          (insert (propertize "Detected Loop:\n" 'face 'warning))
          (insert (format "  Type: %s\n" (plist-get loop-info :type)))
          (insert (format "  Pattern: %s\n" (plist-get loop-info :pattern)))
          (insert (format "  Count: %d\n\n" (plist-get loop-info :count))))

        (insert (propertize "Recent Tool Calls:\n" 'face 'bold))
        (if recent-calls
            (dolist (call recent-calls)
              (insert (format "  • %s @ %s\n"
                              (plist-get call :tool)
                              (format-time-string "%H:%M:%S"
                                                  (plist-get call :timestamp)))))
          (insert "  (none)\n"))
        (insert "\n")

        (insert (propertize "Incident Log:\n" 'face 'bold))
        (if log
            (dolist (incident log)
              (insert (format "  [%s] %s (count=%d, action=%s, score=%.0f%%)\n"
                              (format-time-string "%Y-%m-%d %H:%M:%S"
                                                  (plist-get incident :timestamp))
                              (plist-get incident :pattern)
                              (plist-get incident :count)
                              (plist-get incident :action)
                              (* 100 (plist-get incident :score)))))
          (insert "  (none)\n")))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(define-minor-mode gptel-agent-safety-mode
  "Global minor mode for gptel-agent doom loop detection.

When enabled, tracks tool calls and detects repetitive patterns
that may indicate the LLM is stuck in a loop.  Intervention occurs
based on `gptel-agent-doom-loop-action'."
  :global t
  :group 'gptel-agent-safety
  (if gptel-agent-safety-mode
      (progn
        (add-hook 'gptel-post-response-functions
                  #'gptel-agent--safety-post-response-hook)
        (message "GPTel Agent safety mode enabled"))
    (remove-hook 'gptel-post-response-functions
                 #'gptel-agent--safety-post-response-hook)
    (message "GPTel Agent safety mode disabled")))

;;;; =====================================================================
;;;; External Directory Access Control (FR-014)
;;;; =====================================================================

;; This section implements protection against accidental file operations
;; outside project boundaries.

;;;; Directory Access Customization

(defgroup gptel-agent-boundary nil
  "External directory access control for gptel-agent."
  :group 'gptel-agent-safety
  :prefix "gptel-agent-boundary-")

(defcustom gptel-agent-external-path-whitelist nil
  "Global whitelist of external paths that are always allowed.

List of paths that should be accessible outside the project boundary.
Paths should be absolute. Directory paths should end with '/'.
Glob patterns are supported (* for any characters, ** for recursive).

Examples:
  (\"/tmp/\" \"~/.config/\" \"/var/log/*.log\")"
  :type '(repeat string)
  :group 'gptel-agent-boundary)

(defcustom gptel-agent-external-read-policy 'ask
  "Policy for reading files outside project boundary.

- `allow': Always allow reading external files (with warning)
- `ask': Ask for permission via the approval system
- `deny': Deny all external file reads"
  :type '(choice (const :tag "Allow with warning" allow)
                 (const :tag "Ask for permission" ask)
                 (const :tag "Deny access" deny))
  :group 'gptel-agent-boundary)

(defcustom gptel-agent-external-write-policy 'deny
  "Policy for writing files outside project boundary.

- `allow': Allow writing external files (with warning)
- `ask': Ask for permission via the approval system
- `deny': Deny all external file writes"
  :type '(choice (const :tag "Allow with warning" allow)
                 (const :tag "Ask for permission" ask)
                 (const :tag "Deny access" deny))
  :group 'gptel-agent-boundary)

(defcustom gptel-agent-resolve-symlinks t
  "Whether to resolve symlinks when checking path boundaries.

If non-nil (default), symlinks are resolved before boundary checks
to prevent symlink-based boundary escapes."
  :type 'boolean
  :group 'gptel-agent-boundary)

(defcustom gptel-agent-show-external-warnings t
  "Whether to show visual warnings for external path access.

When non-nil, accessing paths outside the boundary triggers a visual
warning even when access is allowed."
  :type 'boolean
  :group 'gptel-agent-boundary)

;;;; Directory Access Variables

(defvar-local gptel-agent--project-boundary-cache nil
  "Cached project boundary for current buffer.

Plist containing :boundary PATH :timestamp TIME :whitelist LIST")

;;;; Project Boundary Detection

(defun gptel-agent--get-project-boundary ()
  "Get the project boundary for access control.

Returns the project root using project.el, or falls back to
`default-directory'. Custom boundaries from .gptel-agent.el override.

The result is cached per-buffer for performance."
  ;; Check cache validity
  (let ((cached-boundary (plist-get gptel-agent--project-boundary-cache :boundary))
        (cached-time (plist-get gptel-agent--project-boundary-cache :timestamp)))
    (if (and cached-boundary
             cached-time
             (< (float-time (time-subtract (current-time) cached-time)) 60))
        cached-boundary
      ;; Compute fresh boundary
      (let* ((boundary
              (or
               ;; Check for custom boundary in project config
               (gptel-agent--get-config-boundary)
               ;; Use project.el
               (when-let* ((proj (and (fboundp 'project-current)
                                      (project-current nil)))
                           (root (and (fboundp 'project-root)
                                      (project-root proj))))
                 (expand-file-name root))
               ;; Fallback to default-directory
               (expand-file-name default-directory)))
             (whitelist (gptel-agent--get-config-whitelist)))
        ;; Cache result
        (setq gptel-agent--project-boundary-cache
              (list :boundary boundary
                    :timestamp (current-time)
                    :whitelist whitelist))
        boundary))))

(defun gptel-agent--get-config-boundary ()
  "Get custom boundary from .gptel-agent.el configuration."
  (when-let* ((root (and (fboundp 'gptel-agent--project-root)
                         (gptel-agent--project-root)))
              (file (expand-file-name ".gptel-agent.el" root))
              ((file-exists-p file)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((config (read (current-buffer))))
            (when (and (listp config)
                       (eq (car config) 'gptel-agent-project-config))
              (when-let ((custom (plist-get (cdr config) :project-boundary)))
                (expand-file-name custom root)))))
      (error nil))))

(defun gptel-agent--get-config-whitelist ()
  "Get external paths whitelist from .gptel-agent.el configuration."
  (when-let* ((root (and (fboundp 'gptel-agent--project-root)
                         (gptel-agent--project-root)))
              (file (expand-file-name ".gptel-agent.el" root))
              ((file-exists-p file)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((config (read (current-buffer))))
            (when (and (listp config)
                       (eq (car config) 'gptel-agent-project-config))
              (plist-get (cdr config) :external-paths))))
      (error nil))))

(defun gptel-agent--invalidate-boundary-cache ()
  "Invalidate the project boundary cache."
  (setq gptel-agent--project-boundary-cache nil))

;;;; Path Normalization

(defun gptel-agent--normalize-path (path)
  "Normalize PATH for boundary checking.

Expands ~ and relative paths, optionally resolves symlinks.
Returns an absolute, canonical path."
  (when (and path (stringp path))
    (let ((expanded (expand-file-name path)))
      (if (and gptel-agent-resolve-symlinks
               (file-exists-p expanded))
          (file-truename expanded)
        expanded))))

;;;; Path Checking Functions

(defun gptel-agent--path-within-boundary-p (path)
  "Check if PATH is within the project boundary.

Returns non-nil if PATH is inside the project boundary."
  (when-let* ((normalized (gptel-agent--normalize-path path))
              (boundary (gptel-agent--get-project-boundary)))
    (file-in-directory-p normalized boundary)))

(defun gptel-agent--path-matches-glob-p (path pattern)
  "Check if PATH matches glob PATTERN.

Supports * for any characters, ? for single character, and ** for recursive directories."
  (let* ((pattern-expanded (expand-file-name pattern))
         ;; First escape the pattern
         (quoted (regexp-quote pattern-expanded))
         ;; Replace escaped **/ (which is now \\*\\*/) with placeholder
         ;; This handles the common case of **/ meaning "any directories"
         (with-double-star-slash (replace-regexp-in-string
                                  (regexp-quote "\\*\\*/")
                                  "\000DOUBLE_STAR_SLASH\000"
                                  quoted))
         ;; Replace remaining escaped ** with placeholder
         (with-double-star (replace-regexp-in-string
                            (regexp-quote "\\*\\*")
                            "\000DOUBLE_STAR\000"
                            with-double-star-slash))
         ;; Replace escaped * (which is now \\*) with [^/]* (matches anything except /)
         (with-single-star (replace-regexp-in-string
                            (regexp-quote "\\*")
                            "[^/]*"
                            with-double-star))
         ;; Replace escaped ? (which is now \\?) with [^/] (matches any single non-slash char)
         (with-question (replace-regexp-in-string
                         (regexp-quote "\\?")
                         "[^/]"
                         with-single-star))
         ;; Replace double-star-slash placeholder: matches any path or no path
         (with-dss-replaced (replace-regexp-in-string
                             "\000DOUBLE_STAR_SLASH\000"
                             "\\(.+/\\)?"
                             with-question
                             nil t))
         ;; Replace double-star placeholder: matches any characters including /
         (regex (replace-regexp-in-string
                 "\000DOUBLE_STAR\000"
                 ".*"
                 with-dss-replaced
                 nil t))
         ;; Anchor the pattern
         (anchored-regex (concat "^" regex "$")))
    (string-match-p anchored-regex (gptel-agent--normalize-path path))))

(defun gptel-agent--path-in-whitelist-p (path)
  "Check if PATH is in the whitelist.

Checks both global `gptel-agent-external-path-whitelist' and
project-level :external-paths configuration."
  (let* ((normalized (gptel-agent--normalize-path path))
         (project-whitelist (plist-get gptel-agent--project-boundary-cache :whitelist))
         (all-whitelist (append gptel-agent-external-path-whitelist project-whitelist)))
    (cl-some
     (lambda (entry)
       (let ((entry-expanded (expand-file-name entry)))
         (or
          ;; Exact match
          (string= normalized entry-expanded)
          ;; Directory prefix (entry ends with /)
          (and (string-suffix-p "/" entry-expanded)
               (string-prefix-p entry-expanded normalized))
          ;; Glob pattern match
          (gptel-agent--path-matches-glob-p normalized entry))))
     all-whitelist)))

(defun gptel-agent--check-path-access (path operation)
  "Check if PATH access for OPERATION is allowed.

OPERATION is one of: `read', `write', `edit', `execute'.
Returns:
  - `allow': Access is allowed (proceed)
  - `allow-with-warning': Access allowed but show warning
  - `ask': Need to ask for permission
  - `deny': Access denied"
  (let* ((normalized (gptel-agent--normalize-path path))
         (within-boundary (gptel-agent--path-within-boundary-p normalized))
         (in-whitelist (gptel-agent--path-in-whitelist-p normalized))
         (policy (cond
                  ((eq operation 'read) gptel-agent-external-read-policy)
                  ((memq operation '(write edit execute)) gptel-agent-external-write-policy)
                  (t gptel-agent-external-write-policy))))

    (cond
     ;; Within project boundary - always allow
     (within-boundary 'allow)

     ;; In whitelist - allow with warning
     (in-whitelist 'allow-with-warning)

     ;; Apply policy
     (t (cond
         ((eq policy 'allow) 'allow-with-warning)
         ((eq policy 'ask) 'ask)
         ((eq policy 'deny) 'deny))))))

;;;; Visual Warning System

(defface gptel-agent-external-path-warning
  '((t :background "dark orange" :foreground "black" :weight bold))
  "Face for external path access warnings."
  :group 'gptel-agent-boundary)

(defun gptel-agent--show-external-path-warning (path operation)
  "Display warning for external PATH access with OPERATION."
  (when gptel-agent-show-external-warnings
    (let* ((boundary (gptel-agent--get-project-boundary))
           (in-whitelist (gptel-agent--path-in-whitelist-p path))
           (msg (if in-whitelist
                    (format "Whitelisted external path: %s" path)
                  (format "External %s: %s (outside %s)"
                          operation path
                          (abbreviate-file-name boundary)))))
      (message "%s" (propertize msg 'face 'gptel-agent-external-path-warning)))))

;;;; Path Extraction from Commands

(defun gptel-agent--extract-paths-from-bash-command (cmd)
  "Extract file paths from bash command CMD.

Returns a list of potential file paths found in the command."
  (let ((paths nil))
    ;; Common patterns for file operations
    (dolist (pattern '(
                       ;; cat, head, tail, less, more - handle options like -n 10, -f
                       ;; Options may have arguments (-n 10) so use general option pattern
                       "\\<\\(cat\\|head\\|tail\\|less\\|more\\)\\s-+\\(?:-[a-zA-Z]+\\s-*[0-9]*\\s-+\\)*\\([^ |;&<>]+\\)"
                       ;; echo with redirection
                       ">+\\s-*\\([^ |;&]+\\)"
                       ;; cp, mv - extract both source and dest
                       "\\<\\(cp\\|mv\\)\\s-+\\(?:-[a-zA-Z]+\\s-+\\)?\\([^ ]+\\)\\s-+\\([^ ]+\\)"
                       ;; rm
                       "\\<rm\\s-+\\(?:-[rf]+\\s-+\\)?\\([^ |;&]+\\)"
                       ;; touch, mkdir
                       "\\<\\(touch\\|mkdir\\)\\s-+\\(?:-[p]+\\s-+\\)?\\([^ |;&]+\\)"
                       ;; chmod, chown
                       "\\<\\(chmod\\|chown\\)\\s-+[^ ]+\\s-+\\([^ |;&]+\\)"
                       ))
      (let ((pos 0))
        (while (string-match pattern cmd pos)
          (dotimes (n (/ (length (match-data)) 2))
            (when-let ((match (match-string n cmd)))
              (when (and (string-match-p "^[/~.]" match)
                         (not (string-match-p "^-" match)))
                (push match paths))))
          (setq pos (match-end 0)))))
    (delete-dups paths)))

;;;; Integration Functions

(defun gptel-agent--check-file-access (path operation)
  "Check file access for PATH with OPERATION, handling results.

Returns non-nil if access should proceed, nil if denied.
Shows warnings and integrates with permission system as needed."
  (let ((access (gptel-agent--check-path-access path operation)))
    (pcase access
      ('allow t)
      ('allow-with-warning
       (gptel-agent--show-external-path-warning path operation)
       t)
      ('ask
       ;; Integrate with transient permission system if available
       (if (fboundp 'gptel-agent-request-approval)
           (let ((result nil)
                 (done nil))
             (gptel-agent-request-approval
              'external-path
              (list :path path :operation operation)
              (lambda (decision)
                (setq result (memq decision
                                   '(approve-once approve-session approve-project)))
                (setq done t)))
             ;; Wait for decision (simple blocking approach)
             (while (not done) (sit-for 0.1))
             result)
         ;; No permission system - fall back to y-or-n-p
         (y-or-n-p (format "Allow %s access to external path %s? "
                           operation path))))
      ('deny
       (message "Access denied: %s is outside project boundary" path)
       nil))))

(defun gptel-agent--check-bash-command-paths (cmd)
  "Check all paths in bash CMD for access permission.

Returns non-nil if all paths are allowed, nil if any denied."
  (let ((paths (gptel-agent--extract-paths-from-bash-command cmd)))
    (cl-every
     (lambda (path)
       (gptel-agent--check-file-access path 'execute))
     paths)))

(provide 'gptel-agent-safety)
;;; gptel-agent-safety.el ends here
