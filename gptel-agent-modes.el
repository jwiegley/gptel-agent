;;; gptel-agent-modes.el --- Enhanced mode switching for gptel-agent -*- lexical-binding: t; -*-

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

;; This module enhances gptel-agent's mode switching capabilities with:
;; - Configurable keyboard shortcuts
;; - Mode-specific visual indicators with custom faces
;; - Custom mode definitions beyond agent/plan
;; - Mode cycling with repeated shortcut presses
;; - Automatic tool availability adjustment per mode
;;
;; Default modes:
;; - Agent: Full tool access for autonomous task execution
;; - Plan: Read-only tools for planning without modifications
;;
;; Usage:
;;   (require 'gptel-agent-modes)
;;   ;; Mode switching is enabled automatically in gptel-agent buffers
;;   ;; Use C-c C-a m (default) or customize gptel-agent-mode-switch-key
;;
;; Custom Modes:
;;   Add custom modes to `gptel-agent-custom-modes':
;;   (add-to-list 'gptel-agent-custom-modes
;;                '(review . (:preset gptel-review
;;                            :display "Review"
;;                            :face gptel-agent-mode-review-face
;;                            :tools (Read Grep Glob))))

;;; Code:

(require 'cl-lib)

(declare-function gptel--apply-preset "gptel")
(declare-function gptel-backend-name "gptel")
(declare-function gptel-get-tool "gptel")
(defvar gptel-backend)
(defvar gptel-tools)
(defvar gptel-use-header-line)
(defvar header-line-format)

(defgroup gptel-agent-modes nil
  "Mode switching enhancements for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-mode-")

;;;; Customization Options

(defcustom gptel-agent-mode-switch-key "C-c C-a m"
  "Keyboard shortcut for mode switching in gptel-agent buffers.

Set to nil to disable the keyboard shortcut."
  :type '(choice (string :tag "Key binding")
                 (const :tag "Disabled" nil))
  :group 'gptel-agent-modes)

(defcustom gptel-agent-mode-indicator 'header-line
  "Where to display the mode indicator.

- `header-line': Show in header line (default, integrates with existing display)
- `mode-line': Show in mode line
- `both': Show in both header and mode line"
  :type '(choice (const :tag "Header line" header-line)
                 (const :tag "Mode line" mode-line)
                 (const :tag "Both" both))
  :group 'gptel-agent-modes)

(defcustom gptel-agent-mode-show-message t
  "Whether to show message when switching modes."
  :type 'boolean
  :group 'gptel-agent-modes)

(defcustom gptel-agent-custom-modes
  '((agent . (:preset gptel-agent
              :display "Agent"
              :description "Full tool access for autonomous task execution"
              :tools all))
    (plan . (:preset gptel-plan
             :display "Plan"
             :description "Read-only tools for planning without modifications"
             :tools read-only)))
  "Alist defining available modes for gptel-agent.

Each entry is (MODE-SYMBOL . PLIST) where PLIST contains:
  :preset     - Symbol name of the gptel preset to apply
  :display    - String to show in mode indicator
  :description - Optional description for mode
  :tools      - Tool availability:
                - `all': All tools available
                - `read-only': Only read-only tools (Read, Grep, Glob, etc.)
                - List of tool symbols to enable
  :face       - Optional face for mode indicator (overrides default)"
  :type '(alist :key-type symbol
                :value-type (plist :options ((:preset symbol)
                                            (:display string)
                                            (:description string)
                                            (:tools (choice (const all)
                                                           (const read-only)
                                                           (repeat symbol)))
                                            (:face face))))
  :group 'gptel-agent-modes)

;;;; Faces

(defface gptel-agent-mode-agent-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for Agent mode indicator."
  :group 'gptel-agent-modes)

(defface gptel-agent-mode-plan-face
  '((t :inherit font-lock-doc-face :weight bold))
  "Face for Plan mode indicator."
  :group 'gptel-agent-modes)

(defface gptel-agent-mode-custom-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Default face for custom mode indicators."
  :group 'gptel-agent-modes)

;;;; Internal State

(defvar-local gptel-agent--current-mode nil
  "Current mode symbol in this buffer.

One of the keys from `gptel-agent-custom-modes'.")

(defvar-local gptel-agent--mode-index 0
  "Index of current mode in the mode list for cycling.")

(defvar-local gptel-agent--original-tools nil
  "Original tools list before mode-specific adjustments.

Used to restore full tool set when switching modes.")

;;;; Read-Only Tools Definition

(defconst gptel-agent--read-only-tools
  '("Read" "Grep" "Glob" "List" "ListBuffers" "WebFetch" "WebSearch"
    "Describe" "Apropos" "Info" "Agent")
  "List of tools considered read-only (non-modifying).

These tools can read and search but don't modify files or state.")

;;;; Tool Management

(defun gptel-agent--get-all-tools ()
  "Return list of all available tool names."
  ;; Note: We use `bound-and-true-p' to check if gptel-tools is set.
  ;; This handles both globally bound and dynamically let-bound cases.
  (when (bound-and-true-p gptel-tools)
    (mapcar (lambda (tool)
              (if (symbolp tool)
                  (symbol-name tool)
                (format "%s" tool)))
            gptel-tools)))

(defun gptel-agent--filter-tools (tool-spec)
  "Filter tools according to TOOL-SPEC.

TOOL-SPEC can be:
- `all': Return all available tools
- `read-only': Return only read-only tools
- A list of tool names to filter to"
  (let ((all-tools (or gptel-agent--original-tools
                       (bound-and-true-p gptel-tools))))
    (pcase tool-spec
      ('all all-tools)
      ('read-only
       (cl-remove-if-not
        (lambda (tool)
          (let ((name (if (symbolp tool)
                         (symbol-name tool)
                       (when (functionp 'gptel-tool-name)
                         (funcall 'gptel-tool-name tool)))))
            (member name gptel-agent--read-only-tools)))
        all-tools))
      ((pred listp)
       (cl-remove-if-not
        (lambda (tool)
          (let ((name (if (symbolp tool)
                         (symbol-name tool)
                       (when (functionp 'gptel-tool-name)
                         (funcall 'gptel-tool-name tool)))))
            (member name (mapcar #'symbol-name tool-spec))))
        all-tools))
      (_ all-tools))))

(defun gptel-agent--apply-tool-restrictions (tool-spec)
  "Apply tool restrictions according to TOOL-SPEC."
  ;; Save original tools on first call
  (unless gptel-agent--original-tools
    (setq gptel-agent--original-tools (bound-and-true-p gptel-tools)))
  ;; Apply filter
  (setq gptel-tools (gptel-agent--filter-tools tool-spec)))

;;;; Mode Management

(defun gptel-agent--mode-list ()
  "Return ordered list of mode symbols from `gptel-agent-custom-modes'."
  (mapcar #'car gptel-agent-custom-modes))

(defun gptel-agent--get-mode-plist (mode)
  "Get the property list for MODE from `gptel-agent-custom-modes'."
  (cdr (assq mode gptel-agent-custom-modes)))

(defun gptel-agent--mode-face (mode)
  "Get the face for MODE indicator."
  (let ((plist (gptel-agent--get-mode-plist mode)))
    (or (plist-get plist :face)
        (pcase mode
          ('agent 'gptel-agent-mode-agent-face)
          ('plan 'gptel-agent-mode-plan-face)
          (_ 'gptel-agent-mode-custom-face)))))

(defun gptel-agent--mode-display (mode)
  "Get the display string for MODE."
  (let ((plist (gptel-agent--get-mode-plist mode)))
    (or (plist-get plist :display)
        (capitalize (symbol-name mode)))))

(defun gptel-agent--mode-preset (mode)
  "Get the preset symbol for MODE."
  (let ((plist (gptel-agent--get-mode-plist mode)))
    (plist-get plist :preset)))

(defun gptel-agent--mode-tools (mode)
  "Get the tool specification for MODE."
  (let ((plist (gptel-agent--get-mode-plist mode)))
    (or (plist-get plist :tools) 'all)))

;;;; Mode Switching

(defun gptel-agent--switch-to-mode (mode)
  "Switch to MODE, applying its preset and tool restrictions."
  (let ((preset (gptel-agent--mode-preset mode))
        (tools (gptel-agent--mode-tools mode)))

    ;; Apply the preset
    (when preset
      (gptel--apply-preset
       preset
       (lambda (sym val) (set (make-local-variable sym) val))))

    ;; Apply tool restrictions
    (gptel-agent--apply-tool-restrictions tools)

    ;; Update state
    (setq gptel-agent--current-mode mode)

    ;; Update visual indicator
    (force-mode-line-update)

    ;; Show message
    (when gptel-agent-mode-show-message
      (let ((display (gptel-agent--mode-display mode))
            (desc (plist-get (gptel-agent--get-mode-plist mode) :description)))
        (message "Switched to %s mode%s"
                 display
                 (if desc (format " - %s" desc) ""))))))

;;;###autoload
(defun gptel-agent-toggle-mode (&optional arg)
  "Toggle between agent modes or cycle through custom modes.

With prefix ARG, show mode selection menu instead of cycling.

When called repeatedly, cycles through modes in `gptel-agent-custom-modes'.
Mode order is determined by the alist order."
  (interactive "P")
  (unless gptel-agent--current-mode
    (setq gptel-agent--current-mode 'agent))

  (if arg
      ;; Show selection menu
      (let* ((modes (gptel-agent--mode-list))
             (choices (mapcar
                      (lambda (mode)
                        (cons (format "%s - %s"
                                     (gptel-agent--mode-display mode)
                                     (or (plist-get (gptel-agent--get-mode-plist mode)
                                                   :description)
                                        ""))
                              mode))
                      modes))
             (choice (completing-read "Select mode: " choices nil t)))
        (gptel-agent--switch-to-mode (cdr (assoc choice choices))))

    ;; Cycle to next mode
    (let* ((modes (gptel-agent--mode-list))
           (current-idx (or (cl-position gptel-agent--current-mode modes) 0))
           (next-idx (mod (1+ current-idx) (length modes)))
           (next-mode (nth next-idx modes)))
      (setq gptel-agent--mode-index next-idx)
      (gptel-agent--switch-to-mode next-mode))))

;;;###autoload
(defun gptel-agent-set-mode (mode)
  "Set the current mode to MODE.

MODE should be a symbol from `gptel-agent-custom-modes'."
  (interactive
   (list (intern
          (completing-read "Mode: "
                          (mapcar #'symbol-name (gptel-agent--mode-list))
                          nil t))))
  (gptel-agent--switch-to-mode mode))

;;;; Mode Indicator Display

(defun gptel-agent--mode-indicator-string ()
  "Return the mode indicator string for display."
  (let* ((mode (or gptel-agent--current-mode 'agent))
         (display (gptel-agent--mode-display mode))
         (face (gptel-agent--mode-face mode)))
    (propertize (format "[%s]" display) 'face face)))

(defvar-local gptel-agent-mode-line-indicator
  '(:eval (when gptel-agent--current-mode
            (gptel-agent--mode-indicator-string)))
  "Mode line construct for displaying current agent mode.")

(put 'gptel-agent-mode-line-indicator 'risky-local-variable t)

(defun gptel-agent--update-mode-line ()
  "Add mode indicator to mode-line if configured."
  (when (memq gptel-agent-mode-indicator '(mode-line both))
    (unless (memq 'gptel-agent-mode-line-indicator mode-line-format)
      (setq mode-line-format
            (append mode-line-format
                    '(gptel-agent-mode-line-indicator))))))

;;;; Header Line Integration

(defun gptel-agent--make-header-line-mode-button ()
  "Create clickable mode button for header line."
  (let* ((mode (or gptel-agent--current-mode 'agent))
         (display (gptel-agent--mode-display mode))
         (face (gptel-agent--mode-face mode))
         (next-mode (let* ((modes (gptel-agent--mode-list))
                          (idx (or (cl-position mode modes) 0)))
                     (nth (mod (1+ idx) (length modes)) modes)))
         (next-display (gptel-agent--mode-display next-mode)))
    (propertize
     (buttonize (format "[%s]" display)
               (lambda (&rest _)
                 (gptel-agent-toggle-mode))
               nil
               (format "Switch to %s mode" next-display))
     'face face)))

;;;; Keymap

(defvar gptel-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (when gptel-agent-mode-switch-key
      (define-key map (kbd gptel-agent-mode-switch-key)
                  #'gptel-agent-toggle-mode))
    map)
  "Keymap for gptel-agent mode switching.

Active in gptel-agent buffers.")

;;;; Minor Mode for Integration

(defvar gptel-agent-enhanced-mode-functions nil
  "Functions to call when enhanced mode is enabled in a buffer.")

;;;###autoload
(define-minor-mode gptel-agent-enhanced-mode
  "Minor mode providing enhanced mode switching for gptel-agent.

Enables:
- Keyboard shortcut for mode switching (\\[gptel-agent-toggle-mode])
- Mode-specific visual indicators
- Mode cycling through custom modes
- Automatic tool availability adjustment"
  :lighter nil
  :keymap gptel-agent-mode-map
  :group 'gptel-agent-modes
  (if gptel-agent-enhanced-mode
      (progn
        ;; Initialize state
        (unless gptel-agent--current-mode
          (setq gptel-agent--current-mode 'agent))
        ;; Save original tools
        (setq gptel-agent--original-tools (bound-and-true-p gptel-tools))
        ;; Update mode line if configured
        (gptel-agent--update-mode-line)
        ;; Run hook functions
        (run-hooks 'gptel-agent-enhanced-mode-functions))
    ;; Cleanup
    (setq gptel-agent--current-mode nil)
    (setq gptel-agent--mode-index 0)
    ;; Restore original tools
    (when gptel-agent--original-tools
      (setq gptel-tools gptel-agent--original-tools)
      (setq gptel-agent--original-tools nil))))

;;;; Status Display

;;;###autoload
(defun gptel-agent-mode-status ()
  "Display current mode status."
  (interactive)
  (let* ((mode (or gptel-agent--current-mode 'agent))
         (plist (gptel-agent--get-mode-plist mode))
         (tools (gptel-agent--mode-tools mode)))
    (message "Mode: %s | Tools: %s | Preset: %s"
             (gptel-agent--mode-display mode)
             (pcase tools
               ('all "all")
               ('read-only "read-only")
               ((pred listp) (format "%d selected" (length tools))))
             (gptel-agent--mode-preset mode))))

;;;; Integration Hook

(defun gptel-agent-modes-setup ()
  "Set up enhanced mode switching in the current buffer.

Call this from gptel-agent buffer initialization."
  (gptel-agent-enhanced-mode 1))

(provide 'gptel-agent-modes)
;;; gptel-agent-modes.el ends here
