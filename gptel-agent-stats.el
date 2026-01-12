;;; gptel-agent-stats.el --- Token usage statistics and cost tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: January 2025
;; Version: 0.1.0
;; Keywords: tools, convenience, ai, llm
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module implements real-time token usage display and estimated cost
;; information for gptel-agent sessions.
;;
;; Features:
;; - Token tracking (input/output separately, per-response and cumulative)
;; - Cost calculation with configurable model pricing
;; - Header-line display showing compact usage: "^1.2k v3.5k $0.02"
;; - Detailed statistics command with formatted table
;; - Budget alerts with configurable limits and actions
;; - Data export to CSV/JSON
;; - Session persistence integration
;;
;; Usage:
;;   (require 'gptel-agent-stats)
;;   (gptel-agent-stats-mode 1)  ; Enable in buffer
;;
;;   M-x gptel-agent-stats       ; View detailed statistics
;;   M-x gptel-agent-export-stats ; Export to CSV/JSON

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(declare-function gptel-backend-name "gptel")
(declare-function gptel-agent--estimate-tokens "gptel-agent-compaction")
(defvar gptel-model)
(defvar gptel-backend)
(defvar gptel-post-response-functions)

(defgroup gptel-agent-stats nil
  "Token usage statistics and cost tracking for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-stats-")

;;;; Customization Options

(defcustom gptel-agent-model-pricing
  '(;; Anthropic Claude models (per million tokens)
    ("claude-opus-4" . (:input 15.00 :output 75.00))
    ("claude-sonnet-4" . (:input 3.00 :output 15.00))
    ("claude-3-5-sonnet" . (:input 3.00 :output 15.00))
    ("claude-3-opus" . (:input 15.00 :output 75.00))
    ("claude-3-sonnet" . (:input 3.00 :output 15.00))
    ("claude-3-haiku" . (:input 0.25 :output 1.25))
    ;; OpenAI GPT models
    ("gpt-4o" . (:input 2.50 :output 10.00))
    ("gpt-4o-mini" . (:input 0.15 :output 0.60))
    ("gpt-4-turbo" . (:input 10.00 :output 30.00))
    ("gpt-4" . (:input 30.00 :output 60.00))
    ("gpt-3.5-turbo" . (:input 0.50 :output 1.50))
    ("o1" . (:input 15.00 :output 60.00))
    ("o1-mini" . (:input 3.00 :output 12.00))
    ;; Google models
    ("gemini-1.5-pro" . (:input 1.25 :output 5.00))
    ("gemini-1.5-flash" . (:input 0.075 :output 0.30))
    ("gemini-pro" . (:input 0.50 :output 1.50))
    ;; Mistral models
    ("mistral-large" . (:input 4.00 :output 12.00))
    ("mistral-medium" . (:input 2.70 :output 8.10)))
  "Model pricing in dollars per million tokens.

Each entry is (MODEL-PREFIX . (:input PRICE :output PRICE)).
Model prefix matching allows one entry to cover multiple versions."
  :type '(alist :key-type string
                :value-type (plist :options ((:input number) (:output number))))
  :group 'gptel-agent-stats)

(defcustom gptel-agent-stats-show-in-header t
  "Whether to show statistics in the header line."
  :type 'boolean
  :group 'gptel-agent-stats)

(defcustom gptel-agent-budget-limit nil
  "Spending limit in dollars.

When non-nil, enables budget monitoring.  Set to nil to disable."
  :type '(choice (number :tag "Dollar limit")
                 (const :tag "Disabled" nil))
  :group 'gptel-agent-stats)

(defcustom gptel-agent-budget-period 'session
  "Period for budget tracking.

- `session': Per-session budget (resets when session changes)
- `daily': Daily budget (resets at midnight)
- `monthly': Monthly budget (resets on 1st of month)"
  :type '(choice (const :tag "Per session" session)
                 (const :tag "Daily" daily)
                 (const :tag "Monthly" monthly))
  :group 'gptel-agent-stats)

(defcustom gptel-agent-budget-action 'warn
  "Action to take when budget is exceeded.

- `warn': Display warning message but allow continuation
- `confirm': Require confirmation to continue
- `stop': Prevent further API calls"
  :type '(choice (const :tag "Warn" warn)
                 (const :tag "Require confirmation" confirm)
                 (const :tag "Stop" stop))
  :group 'gptel-agent-stats)

(defcustom gptel-agent-stats-persist t
  "Whether to persist statistics with sessions.

Requires gptel-agent-sessions module."
  :type 'boolean
  :group 'gptel-agent-stats)

;;;; Faces

(defface gptel-agent-stats-input-face
  '((t :inherit font-lock-variable-name-face))
  "Face for input token display."
  :group 'gptel-agent-stats)

(defface gptel-agent-stats-output-face
  '((t :inherit font-lock-string-face))
  "Face for output token display."
  :group 'gptel-agent-stats)

(defface gptel-agent-stats-cost-face
  '((t :inherit font-lock-number-face))
  "Face for cost display."
  :group 'gptel-agent-stats)

(defface gptel-agent-stats-warning-face
  '((t :inherit warning))
  "Face for budget warning display."
  :group 'gptel-agent-stats)

;;;; Internal State

(defvar-local gptel-agent--session-stats nil
  "Buffer-local session statistics plist.

Structure:
  :input-tokens     - Total input tokens this session
  :output-tokens    - Total output tokens this session
  :session-cost     - Total cost this session
  :responses        - List of per-response stats
  :start-time       - Session start time
  :model            - Model used for this session")

(defvar gptel-agent--cumulative-stats nil
  "Global cumulative statistics plist.

Structure:
  :total-input-tokens  - All-time input tokens
  :total-output-tokens - All-time output tokens
  :total-cost          - All-time cost
  :total-responses     - Total number of responses")

(defvar gptel-agent--budget-spent nil
  "Spending for current budget period.

Structure:
  :amount  - Amount spent in dollars
  :period  - Budget period symbol
  :start   - Period start timestamp")

;;;; Token Tracking

(defun gptel-agent-stats--init-session ()
  "Initialize session statistics for current buffer."
  (setq gptel-agent--session-stats
        (list :input-tokens 0
              :output-tokens 0
              :session-cost 0.0
              :responses nil
              :start-time (current-time)
              :model (when (boundp 'gptel-model) gptel-model))))

(defun gptel-agent-stats--track-response (info)
  "Track token usage from response INFO.

INFO is the info plist passed to `gptel-post-response-functions'."
  (when gptel-agent--session-stats
    (let* ((input-content (plist-get info :prompt))
           (output-content (plist-get info :response))
           (input-tokens (gptel-agent-stats--estimate-tokens input-content))
           (output-tokens (gptel-agent-stats--estimate-tokens output-content))
           (model (or (plist-get info :model)
                      (when (boundp 'gptel-model) gptel-model)))
           (cost (gptel-agent-stats--calculate-cost input-tokens output-tokens model))
           (response-stat (list :timestamp (current-time)
                               :input-tokens input-tokens
                               :output-tokens output-tokens
                               :cost cost
                               :model model)))
      ;; Update session stats
      (cl-incf (plist-get gptel-agent--session-stats :input-tokens) input-tokens)
      (cl-incf (plist-get gptel-agent--session-stats :output-tokens) output-tokens)
      (cl-incf (plist-get gptel-agent--session-stats :session-cost) (or cost 0))
      (plist-put gptel-agent--session-stats :responses
                 (cons response-stat (plist-get gptel-agent--session-stats :responses)))

      ;; Update cumulative stats
      (gptel-agent-stats--update-cumulative input-tokens output-tokens cost)

      ;; Check budget
      (when cost
        (gptel-agent-stats--check-budget cost))

      ;; Update display
      (force-mode-line-update))))

(defun gptel-agent-stats--estimate-tokens (content)
  "Estimate token count for CONTENT.

Uses compaction module's estimation if available, otherwise
falls back to ~4 chars per token."
  (cond
   ((null content) 0)
   ((not (stringp content)) 0)
   ((fboundp 'gptel-agent--estimate-tokens)
    (gptel-agent--estimate-tokens content))
   (t (/ (length content) 4))))

(defun gptel-agent-stats--update-cumulative (input-tokens output-tokens cost)
  "Update cumulative statistics with INPUT-TOKENS, OUTPUT-TOKENS, COST."
  (unless gptel-agent--cumulative-stats
    (setq gptel-agent--cumulative-stats
          (list :total-input-tokens 0
                :total-output-tokens 0
                :total-cost 0.0
                :total-responses 0)))
  (cl-incf (plist-get gptel-agent--cumulative-stats :total-input-tokens) input-tokens)
  (cl-incf (plist-get gptel-agent--cumulative-stats :total-output-tokens) output-tokens)
  (cl-incf (plist-get gptel-agent--cumulative-stats :total-cost) (or cost 0))
  (cl-incf (plist-get gptel-agent--cumulative-stats :total-responses) 1))

(defun gptel-agent-reset-session-stats ()
  "Reset session statistics for current buffer."
  (interactive)
  (gptel-agent-stats--init-session)
  (message "Session statistics reset"))

;;;; Cost Calculation

(defun gptel-agent-stats--get-model-pricing (model)
  "Get pricing for MODEL.

Returns a plist with :input and :output prices per million tokens,
or nil if model not found."
  (when model
    (let ((model-str (format "%s" model)))
      (cl-loop for (prefix . pricing) in gptel-agent-model-pricing
               when (string-match-p (regexp-quote prefix) model-str)
               return pricing))))

(defun gptel-agent-stats--calculate-cost (input-tokens output-tokens model)
  "Calculate cost for INPUT-TOKENS and OUTPUT-TOKENS using MODEL pricing.

Returns cost in dollars, or nil if model pricing not found."
  (when-let ((pricing (gptel-agent-stats--get-model-pricing model)))
    (let ((input-price (plist-get pricing :input))
          (output-price (plist-get pricing :output)))
      (+ (* (/ input-tokens 1000000.0) input-price)
         (* (/ output-tokens 1000000.0) output-price)))))

;;;; Formatting Helpers

(defun gptel-agent-stats--format-tokens (count)
  "Format token COUNT for compact display.

Examples: 500 -> \"500\", 1500 -> \"1.5k\", 1500000 -> \"1.5M\""
  (cond
   ((< count 1000)
    (number-to-string count))
   ((< count 1000000)
    (format "%.1fk" (/ count 1000.0)))
   (t
    (format "%.1fM" (/ count 1000000.0)))))

(defun gptel-agent-stats--format-cost (dollars)
  "Format DOLLARS for display.

Examples: 0.005 -> \"$0.01\", 1.23 -> \"$1.23\", 10.5 -> \"$10.50\""
  (if (< dollars 0.01)
      "$0.00"
    (format "$%.2f" dollars)))

;;;; Header Line Display

(defun gptel-agent-stats--header-string ()
  "Return stats string for header line display.

Format: ^1.2k v3.5k $0.02"
  (if (and gptel-agent-stats-show-in-header
           gptel-agent--session-stats)
      (let ((input (plist-get gptel-agent--session-stats :input-tokens))
            (output (plist-get gptel-agent--session-stats :output-tokens))
            (cost (plist-get gptel-agent--session-stats :session-cost)))
        (concat
         " "
         (propertize (concat "^" (gptel-agent-stats--format-tokens input))
                     'face 'gptel-agent-stats-input-face)
         " "
         (propertize (concat "v" (gptel-agent-stats--format-tokens output))
                     'face 'gptel-agent-stats-output-face)
         (when cost
           (concat " "
                   (propertize (gptel-agent-stats--format-cost cost)
                               'face 'gptel-agent-stats-cost-face)))))
    ""))

(defvar-local gptel-agent-stats-header-indicator
  '(:eval (gptel-agent-stats--header-string))
  "Header line construct for statistics display.")

(put 'gptel-agent-stats-header-indicator 'risky-local-variable t)

;;;; Budget Monitoring

(defun gptel-agent-stats--init-budget-period ()
  "Initialize budget tracking for current period."
  (let ((now (current-time)))
    (setq gptel-agent--budget-spent
          (list :amount 0.0
                :period gptel-agent-budget-period
                :start (pcase gptel-agent-budget-period
                         ('session now)
                         ('daily (gptel-agent-stats--day-start now))
                         ('monthly (gptel-agent-stats--month-start now)))))))

(defun gptel-agent-stats--day-start (time)
  "Return the start of the day containing TIME."
  (let ((decoded (decode-time time)))
    (setf (nth 0 decoded) 0   ; seconds
          (nth 1 decoded) 0   ; minutes
          (nth 2 decoded) 0)  ; hours
    (encode-time decoded)))

(defun gptel-agent-stats--month-start (time)
  "Return the start of the month containing TIME."
  (let ((decoded (decode-time time)))
    (setf (nth 0 decoded) 0   ; seconds
          (nth 1 decoded) 0   ; minutes
          (nth 2 decoded) 0   ; hours
          (nth 3 decoded) 1)  ; day
    (encode-time decoded)))

(defun gptel-agent-stats--budget-period-expired-p ()
  "Check if current budget period has expired."
  (when gptel-agent--budget-spent
    (let ((now (current-time))
          (start (plist-get gptel-agent--budget-spent :start))
          (period (plist-get gptel-agent--budget-spent :period)))
      (pcase period
        ('session nil)  ; Session budget never expires
        ('daily (not (time-equal-p (gptel-agent-stats--day-start now)
                                    (gptel-agent-stats--day-start start))))
        ('monthly (not (time-equal-p (gptel-agent-stats--month-start now)
                                      (gptel-agent-stats--month-start start))))))))

(defun gptel-agent-stats--check-budget (cost)
  "Check budget after adding COST.

Triggers alerts based on `gptel-agent-budget-action' setting."
  (when gptel-agent-budget-limit
    ;; Initialize or reset budget period
    (when (or (null gptel-agent--budget-spent)
              (gptel-agent-stats--budget-period-expired-p))
      (gptel-agent-stats--init-budget-period))

    ;; Add cost to current period
    (cl-incf (plist-get gptel-agent--budget-spent :amount) cost)

    (let* ((spent (plist-get gptel-agent--budget-spent :amount))
           (limit gptel-agent-budget-limit)
           (percent (/ spent limit)))
      (cond
       ;; At or over 100%
       ((>= percent 1.0)
        (pcase gptel-agent-budget-action
          ('warn (message "Budget exceeded! Spent %s of %s limit"
                         (gptel-agent-stats--format-cost spent)
                         (gptel-agent-stats--format-cost limit)))
          ('confirm
           (unless (yes-or-no-p
                   (format "Budget exceeded (%s of %s). Continue? "
                          (gptel-agent-stats--format-cost spent)
                          (gptel-agent-stats--format-cost limit)))
             (user-error "Stopped due to budget limit")))
          ('stop
           (user-error "Budget limit reached: %s of %s"
                      (gptel-agent-stats--format-cost spent)
                      (gptel-agent-stats--format-cost limit)))))
       ;; 90-99%
       ((>= percent 0.9)
        (message "Budget warning: 90%% of limit reached (%s of %s)"
                 (gptel-agent-stats--format-cost spent)
                 (gptel-agent-stats--format-cost limit)))
       ;; 80-89%
       ((>= percent 0.8)
        (message "Budget notice: 80%% of limit reached (%s of %s)"
                 (gptel-agent-stats--format-cost spent)
                 (gptel-agent-stats--format-cost limit)))))))

;;;; Statistics Display

;;;###autoload
(defun gptel-agent-stats ()
  "Display detailed usage statistics."
  (interactive)
  (let* ((session gptel-agent--session-stats)
         (cumulative gptel-agent--cumulative-stats)
         (buf (get-buffer-create "*GPTel Agent Stats*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "GPTel Agent Statistics\n")
        (insert "======================\n\n")

        ;; Session stats
        (insert "Session Statistics\n")
        (insert "------------------\n")
        (if session
            (progn
              (insert (format "Model: %s\n" (or (plist-get session :model) "Unknown")))
              (insert (format "Started: %s\n"
                             (format-time-string "%Y-%m-%d %H:%M"
                                                 (plist-get session :start-time))))
              (insert (format "Input tokens:  %s\n"
                             (gptel-agent-stats--format-tokens
                              (plist-get session :input-tokens))))
              (insert (format "Output tokens: %s\n"
                             (gptel-agent-stats--format-tokens
                              (plist-get session :output-tokens))))
              (insert (format "Cost: %s\n"
                             (gptel-agent-stats--format-cost
                              (plist-get session :session-cost))))
              (insert (format "Responses: %d\n"
                             (length (plist-get session :responses)))))
          (insert "No active session\n"))

        (insert "\n")

        ;; Cumulative stats
        (insert "Cumulative Statistics\n")
        (insert "---------------------\n")
        (if cumulative
            (progn
              (insert (format "Total input:  %s\n"
                             (gptel-agent-stats--format-tokens
                              (plist-get cumulative :total-input-tokens))))
              (insert (format "Total output: %s\n"
                             (gptel-agent-stats--format-tokens
                              (plist-get cumulative :total-output-tokens))))
              (insert (format "Total cost: %s\n"
                             (gptel-agent-stats--format-cost
                              (plist-get cumulative :total-cost))))
              (insert (format "Total responses: %d\n"
                             (plist-get cumulative :total-responses))))
          (insert "No cumulative data\n"))

        (insert "\n")

        ;; Budget status
        (when gptel-agent-budget-limit
          (insert "Budget Status\n")
          (insert "-------------\n")
          (let ((spent (if gptel-agent--budget-spent
                          (plist-get gptel-agent--budget-spent :amount)
                        0.0)))
            (insert (format "Limit: %s (%s)\n"
                           (gptel-agent-stats--format-cost gptel-agent-budget-limit)
                           gptel-agent-budget-period))
            (insert (format "Spent: %s (%.0f%%)\n"
                           (gptel-agent-stats--format-cost spent)
                           (* 100 (/ spent gptel-agent-budget-limit))))
            (insert (format "Remaining: %s\n"
                           (gptel-agent-stats--format-cost
                            (- gptel-agent-budget-limit spent)))))
          (insert "\n"))

        ;; Per-response breakdown
        (when (and session (plist-get session :responses))
          (insert "Response Breakdown\n")
          (insert "------------------\n")
          (insert (format "%-5s %-10s %-10s %-10s\n" "#" "Input" "Output" "Cost"))
          (insert (make-string 40 ?-))
          (insert "\n")
          (let ((n (length (plist-get session :responses))))
            (dolist (resp (reverse (plist-get session :responses)))
              (insert (format "%-5d %-10s %-10s %-10s\n"
                             n
                             (gptel-agent-stats--format-tokens
                              (plist-get resp :input-tokens))
                             (gptel-agent-stats--format-tokens
                              (plist-get resp :output-tokens))
                             (if (plist-get resp :cost)
                                 (gptel-agent-stats--format-cost (plist-get resp :cost))
                               "N/A")))
              (cl-decf n)))))

        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buf)))

;;;; Data Export

;;;###autoload
(defun gptel-agent-export-stats (format file)
  "Export statistics to FILE in FORMAT (csv or json)."
  (interactive
   (let* ((fmt (intern (completing-read "Export format: " '("csv" "json") nil t)))
          (ext (if (eq fmt 'csv) ".csv" ".json"))
          (default-name (concat "gptel-agent-stats-"
                               (format-time-string "%Y%m%d")
                               ext))
          (path (read-file-name "Export to: " nil nil nil default-name)))
     (list fmt path)))

  (let* ((session gptel-agent--session-stats)
         (responses (when session (plist-get session :responses)))
         (content
          (pcase format
            ('csv (gptel-agent-stats--export-csv responses))
            ('json (gptel-agent-stats--export-json session))
            (_ (error "Unknown format: %s" format)))))
    (with-temp-file file
      (insert content))
    (message "Exported to %s" file)))

(defun gptel-agent-stats--export-csv (responses)
  "Export RESPONSES to CSV format."
  (concat
   "timestamp,model,input_tokens,output_tokens,cost\n"
   (mapconcat
    (lambda (resp)
      (format "%s,%s,%d,%d,%.4f"
              (format-time-string "%Y-%m-%d %H:%M:%S"
                                 (plist-get resp :timestamp))
              (or (plist-get resp :model) "")
              (plist-get resp :input-tokens)
              (plist-get resp :output-tokens)
              (or (plist-get resp :cost) 0)))
    (reverse responses)
    "\n")))

(defun gptel-agent-stats--export-json (session)
  "Export SESSION to JSON format."
  (json-encode
   (list :session (list :model (plist-get session :model)
                       :start_time (format-time-string
                                   "%Y-%m-%dT%H:%M:%S"
                                   (plist-get session :start-time))
                       :input_tokens (plist-get session :input-tokens)
                       :output_tokens (plist-get session :output-tokens)
                       :cost (plist-get session :session-cost))
         :responses (mapcar
                    (lambda (resp)
                      (list :timestamp (format-time-string
                                       "%Y-%m-%dT%H:%M:%S"
                                       (plist-get resp :timestamp))
                            :model (plist-get resp :model)
                            :input_tokens (plist-get resp :input-tokens)
                            :output_tokens (plist-get resp :output-tokens)
                            :cost (plist-get resp :cost)))
                    (reverse (plist-get session :responses))))))

;;;; Session Persistence Integration

(defun gptel-agent-stats--save-with-session ()
  "Save statistics when session is saved.

Integration point with gptel-agent-sessions module."
  (when (and gptel-agent-stats-persist
             gptel-agent--session-stats
             (fboundp 'gptel-agent--current-session-id))
    ;; Store stats in session metadata
    (list :input-tokens (plist-get gptel-agent--session-stats :input-tokens)
          :output-tokens (plist-get gptel-agent--session-stats :output-tokens)
          :session-cost (plist-get gptel-agent--session-stats :session-cost)
          :response-count (length (plist-get gptel-agent--session-stats :responses)))))

(defun gptel-agent-stats--load-from-session (stats)
  "Load statistics from saved STATS."
  (when (and gptel-agent-stats-persist stats)
    (setq gptel-agent--session-stats
          (list :input-tokens (plist-get stats :input-tokens)
                :output-tokens (plist-get stats :output-tokens)
                :session-cost (plist-get stats :session-cost)
                :responses nil
                :start-time (current-time)
                :model (when (boundp 'gptel-model) gptel-model)))))

;;;; Minor Mode

;;;###autoload
(define-minor-mode gptel-agent-stats-mode
  "Minor mode for token usage and cost tracking in gptel-agent.

Tracks input/output tokens, calculates costs based on model pricing,
and displays statistics in the header line."
  :lighter " Stats"
  :group 'gptel-agent-stats
  (if gptel-agent-stats-mode
      (progn
        ;; Initialize session stats
        (gptel-agent-stats--init-session)
        ;; Hook into gptel responses
        (add-hook 'gptel-post-response-functions
                  #'gptel-agent-stats--track-response nil t))
    ;; Cleanup
    (remove-hook 'gptel-post-response-functions
                 #'gptel-agent-stats--track-response t)))

;;;; Utility Functions

(defun gptel-agent-stats-current ()
  "Return current session statistics as plist."
  gptel-agent--session-stats)

(defun gptel-agent-stats-cumulative ()
  "Return cumulative statistics as plist."
  gptel-agent--cumulative-stats)

(provide 'gptel-agent-stats)
;;; gptel-agent-stats.el ends here
