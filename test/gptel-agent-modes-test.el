;;; gptel-agent-modes-test.el --- Tests for gptel-agent-modes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Keywords: tests
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tests for gptel-agent-modes.el enhanced mode switching.
;;
;; Test coverage:
;; - Keyboard shortcut binding
;; - Mode cycling through multiple modes
;; - Context preservation on mode switch
;; - Tool availability adjustment per mode
;; - Visual indicator updates
;; - Custom mode definitions

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'gptel-agent-modes)

;;;; Test Helpers

;; Ensure gptel-tools is defined as a special variable for testing
(defvar gptel-tools nil
  "Tools available for gptel agent operations (test definition).")

(defvar gptel-agent-modes-test--saved-gptel-tools nil
  "Saved value of gptel-tools before test.")

(defun gptel-agent-modes-test--setup ()
  "Set up test environment with clean state."
  (setq gptel-agent--current-mode nil)
  (setq gptel-agent--mode-index 0)
  (setq gptel-agent--original-tools nil)
  ;; Save and clear gptel-tools
  (setq gptel-agent-modes-test--saved-gptel-tools gptel-tools)
  (setq gptel-tools nil))

(defun gptel-agent-modes-test--teardown ()
  "Restore test environment after test."
  (setq gptel-tools gptel-agent-modes-test--saved-gptel-tools))

(defmacro gptel-agent-modes-test--with-clean-state (&rest body)
  "Execute BODY with clean test state."
  `(with-temp-buffer
     (gptel-agent-modes-test--setup)
     (unwind-protect
         (progn ,@body)
       (gptel-agent-modes-test--teardown))))

;;;; Customization Tests

(ert-deftest gptel-agent-modes-test-switch-key-default ()
  "Test default value for mode switch key."
  (should (string= (default-value 'gptel-agent-mode-switch-key)
                   "C-c C-a m")))

(ert-deftest gptel-agent-modes-test-indicator-default ()
  "Test default value for mode indicator."
  (should (eq (default-value 'gptel-agent-mode-indicator)
              'header-line)))

(ert-deftest gptel-agent-modes-test-show-message-default ()
  "Test default value for show message."
  (should (default-value 'gptel-agent-mode-show-message)))

(ert-deftest gptel-agent-modes-test-custom-modes-default ()
  "Test default custom modes includes agent and plan."
  (let ((modes gptel-agent-custom-modes))
    (should (assq 'agent modes))
    (should (assq 'plan modes))))

;;;; Face Tests

(ert-deftest gptel-agent-modes-test-agent-face-defined ()
  "Test agent face is defined."
  (should (facep 'gptel-agent-mode-agent-face)))

(ert-deftest gptel-agent-modes-test-plan-face-defined ()
  "Test plan face is defined."
  (should (facep 'gptel-agent-mode-plan-face)))

(ert-deftest gptel-agent-modes-test-custom-face-defined ()
  "Test custom face is defined."
  (should (facep 'gptel-agent-mode-custom-face)))

;;;; Mode List Tests

(ert-deftest gptel-agent-modes-test-mode-list ()
  "Test mode list returns mode symbols."
  (let ((modes (gptel-agent--mode-list)))
    (should (listp modes))
    (should (member 'agent modes))
    (should (member 'plan modes))))

(ert-deftest gptel-agent-modes-test-get-mode-plist ()
  "Test getting mode plist."
  (let ((plist (gptel-agent--get-mode-plist 'agent)))
    (should plist)
    (should (plist-get plist :preset))
    (should (plist-get plist :display))))

(ert-deftest gptel-agent-modes-test-mode-display-agent ()
  "Test mode display for agent."
  (should (string= (gptel-agent--mode-display 'agent) "Agent")))

(ert-deftest gptel-agent-modes-test-mode-display-plan ()
  "Test mode display for plan."
  (should (string= (gptel-agent--mode-display 'plan) "Plan")))

(ert-deftest gptel-agent-modes-test-mode-face-agent ()
  "Test mode face for agent."
  (should (eq (gptel-agent--mode-face 'agent)
              'gptel-agent-mode-agent-face)))

(ert-deftest gptel-agent-modes-test-mode-face-plan ()
  "Test mode face for plan."
  (should (eq (gptel-agent--mode-face 'plan)
              'gptel-agent-mode-plan-face)))

(ert-deftest gptel-agent-modes-test-mode-preset-agent ()
  "Test mode preset for agent."
  (should (eq (gptel-agent--mode-preset 'agent) 'gptel-agent)))

(ert-deftest gptel-agent-modes-test-mode-preset-plan ()
  "Test mode preset for plan."
  (should (eq (gptel-agent--mode-preset 'plan) 'gptel-plan)))

(ert-deftest gptel-agent-modes-test-mode-tools-agent ()
  "Test mode tools for agent."
  (should (eq (gptel-agent--mode-tools 'agent) 'all)))

(ert-deftest gptel-agent-modes-test-mode-tools-plan ()
  "Test mode tools for plan."
  (should (eq (gptel-agent--mode-tools 'plan) 'read-only)))

;;;; Read-Only Tools Tests

(ert-deftest gptel-agent-modes-test-read-only-tools-defined ()
  "Test read-only tools constant is defined."
  (should (listp gptel-agent--read-only-tools))
  (should (member "Read" gptel-agent--read-only-tools))
  (should (member "Grep" gptel-agent--read-only-tools))
  (should (member "Glob" gptel-agent--read-only-tools)))

(ert-deftest gptel-agent-modes-test-read-only-excludes-write ()
  "Test read-only tools excludes Write."
  (should-not (member "Write" gptel-agent--read-only-tools))
  (should-not (member "Edit" gptel-agent--read-only-tools))
  (should-not (member "Bash" gptel-agent--read-only-tools)))

;;;; Tool Filtering Tests

(ert-deftest gptel-agent-modes-test-filter-tools-all ()
  "Test tool filtering with all spec."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools '(Read Write Bash)))
      (setq gptel-agent--original-tools gptel-tools)
      (let ((filtered (gptel-agent--filter-tools 'all)))
        (should (= (length filtered) 3))))))

(ert-deftest gptel-agent-modes-test-filter-tools-list ()
  "Test tool filtering with explicit list."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools '(Read Write Bash Grep)))
      (setq gptel-agent--original-tools gptel-tools)
      (let ((filtered (gptel-agent--filter-tools '(Read Grep))))
        ;; Should only contain Read and Grep
        (should (<= (length filtered) 2))))))

;;;; Mode Indicator Tests

(ert-deftest gptel-agent-modes-test-indicator-string ()
  "Test mode indicator string generation."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode 'agent)
    (let ((str (gptel-agent--mode-indicator-string)))
      (should (stringp str))
      (should (string-match-p "Agent" str)))))

(ert-deftest gptel-agent-modes-test-indicator-string-plan ()
  "Test mode indicator string for plan mode."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode 'plan)
    (let ((str (gptel-agent--mode-indicator-string)))
      (should (string-match-p "Plan" str)))))

;;;; Mode Cycling Tests

(ert-deftest gptel-agent-modes-test-cycling-order ()
  "Test mode cycling goes in order."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode 'agent)
    ;; Simulate toggle
    (let* ((modes (gptel-agent--mode-list))
           (idx (cl-position 'agent modes))
           (next-idx (mod (1+ idx) (length modes)))
           (next-mode (nth next-idx modes)))
      (should (memq next-mode modes)))))

(ert-deftest gptel-agent-modes-test-cycling-wraps ()
  "Test mode cycling wraps around."
  (gptel-agent-modes-test--with-clean-state
    (let* ((modes (gptel-agent--mode-list))
           (last-idx (1- (length modes)))
           (next-idx (mod (1+ last-idx) (length modes))))
      (should (= next-idx 0)))))

;;;; Keymap Tests

(ert-deftest gptel-agent-modes-test-keymap-defined ()
  "Test keymap is defined."
  (should (keymapp gptel-agent-mode-map)))

(ert-deftest gptel-agent-modes-test-keymap-has-binding ()
  "Test keymap has mode toggle binding."
  (when gptel-agent-mode-switch-key
    (should (eq (lookup-key gptel-agent-mode-map
                           (kbd gptel-agent-mode-switch-key))
               #'gptel-agent-toggle-mode))))

;;;; Minor Mode Tests

(ert-deftest gptel-agent-modes-test-minor-mode-enables ()
  "Test minor mode can be enabled."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (gptel-agent-enhanced-mode 1)
    (should gptel-agent-enhanced-mode)
    (should gptel-agent--current-mode)
    (gptel-agent-enhanced-mode -1)))

(ert-deftest gptel-agent-modes-test-minor-mode-disables ()
  "Test minor mode can be disabled."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (gptel-agent-enhanced-mode 1)
    (gptel-agent-enhanced-mode -1)
    (should-not gptel-agent-enhanced-mode)
    (should-not gptel-agent--current-mode)))

(ert-deftest gptel-agent-modes-test-minor-mode-default-agent ()
  "Test minor mode defaults to agent mode."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (gptel-agent-enhanced-mode 1)
    (should (eq gptel-agent--current-mode 'agent))
    (gptel-agent-enhanced-mode -1)))

;;;; Custom Mode Tests

(ert-deftest gptel-agent-modes-test-custom-mode-structure ()
  "Test custom mode alist structure."
  (dolist (entry gptel-agent-custom-modes)
    (should (symbolp (car entry)))
    (should (listp (cdr entry)))
    (should (plist-get (cdr entry) :preset))
    (should (plist-get (cdr entry) :display))))

(ert-deftest gptel-agent-modes-test-adding-custom-mode ()
  "Test adding a custom mode."
  (let ((gptel-agent-custom-modes
         (append gptel-agent-custom-modes
                '((review . (:preset gptel-review
                             :display "Review"
                             :tools read-only))))))
    (should (assq 'review gptel-agent-custom-modes))
    (let ((modes (gptel-agent--mode-list)))
      (should (member 'review modes)))))

;;;; State Preservation Tests

(ert-deftest gptel-agent-modes-test-original-tools-saved ()
  "Test original tools are saved on first mode enable."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (let ((gptel-tools '(Read Write Bash)))
      (gptel-agent-enhanced-mode 1)
      (should gptel-agent--original-tools)
      (should (= (length gptel-agent--original-tools) 3))
      (gptel-agent-enhanced-mode -1))))

(ert-deftest gptel-agent-modes-test-original-tools-restored ()
  "Test original tools are restored on mode disable."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (let ((gptel-tools '(Read Write Bash)))
      (gptel-agent-enhanced-mode 1)
      (setq gptel-tools '(Read)) ; Simulate restriction
      (gptel-agent-enhanced-mode -1)
      (should (= (length gptel-tools) 3)))))

;;;; Additional Customization Tests

(ert-deftest gptel-agent-modes-test-customization-group ()
  "Test customization group is defined."
  (should (get 'gptel-agent-modes 'custom-group)))

(ert-deftest gptel-agent-modes-test-switch-key-type ()
  "Test mode switch key type."
  (let ((type (get 'gptel-agent-mode-switch-key 'custom-type)))
    (should (consp type))
    (should (eq (car type) 'choice))))

(ert-deftest gptel-agent-modes-test-indicator-type ()
  "Test mode indicator type."
  (let ((type (get 'gptel-agent-mode-indicator 'custom-type)))
    (should (consp type))
    (should (eq (car type) 'choice))))

;;;; Get All Tools Tests

(ert-deftest gptel-agent-modes-test-get-all-tools ()
  "Test getting all tools."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools '(Read Write Bash)))
      (let ((tools (gptel-agent--get-all-tools)))
        (should (= (length tools) 3))
        (should (member "Read" tools))
        (should (member "Write" tools))
        (should (member "Bash" tools))))))

(ert-deftest gptel-agent-modes-test-get-all-tools-empty ()
  "Test getting all tools when none defined."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools nil))
      (should-not (gptel-agent--get-all-tools)))))

(ert-deftest gptel-agent-modes-test-get-all-tools-unbound ()
  "Test getting all tools when gptel-tools unbound."
  (gptel-agent-modes-test--with-clean-state
    ;; Make gptel-tools void
    (let ((gptel-tools-backup (when (boundp 'gptel-tools) gptel-tools)))
      (makunbound 'gptel-tools)
      (should-not (gptel-agent--get-all-tools))
      (when gptel-tools-backup
        (setq gptel-tools gptel-tools-backup)))))

;;;; Tool Filtering Extended Tests

(ert-deftest gptel-agent-modes-test-filter-tools-read-only ()
  "Test tool filtering with read-only spec."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools '(Read Write Bash Grep Glob)))
      (setq gptel-agent--original-tools gptel-tools)
      (let ((filtered (gptel-agent--filter-tools 'read-only)))
        ;; Should contain Read, Grep, Glob but not Write, Bash
        (should (cl-every (lambda (tool)
                            (member (symbol-name tool)
                                    gptel-agent--read-only-tools))
                          filtered))))))

(ert-deftest gptel-agent-modes-test-filter-tools-unknown-spec ()
  "Test tool filtering with unknown spec returns all."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools '(Read Write Bash)))
      (setq gptel-agent--original-tools gptel-tools)
      (let ((filtered (gptel-agent--filter-tools 'unknown-spec)))
        (should (= (length filtered) 3))))))

(ert-deftest gptel-agent-modes-test-filter-tools-empty-list ()
  "Test tool filtering with empty list spec."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools '(Read Write Bash)))
      (setq gptel-agent--original-tools gptel-tools)
      (let ((filtered (gptel-agent--filter-tools '())))
        (should (= (length filtered) 0))))))

(ert-deftest gptel-agent-modes-test-filter-tools-no-original ()
  "Test tool filtering uses gptel-tools when no original."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools '(Read Write)))
      (setq gptel-agent--original-tools nil)
      (let ((filtered (gptel-agent--filter-tools 'all)))
        (should (= (length filtered) 2))))))

;;;; Apply Tool Restrictions Tests

(ert-deftest gptel-agent-modes-test-apply-tool-restrictions ()
  "Test applying tool restrictions."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools '(Read Write Bash Grep Glob)))
      (gptel-agent--apply-tool-restrictions 'read-only)
      ;; Original tools should be saved
      (should gptel-agent--original-tools)
      (should (= (length gptel-agent--original-tools) 5))
      ;; Current tools should be filtered
      (should (< (length gptel-tools) 5)))))

(ert-deftest gptel-agent-modes-test-apply-tool-restrictions-preserves-original ()
  "Test applying restrictions preserves original tools."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-tools '(Read Write Bash)))
      (setq gptel-agent--original-tools '(Read Write Bash Grep))
      (gptel-agent--apply-tool-restrictions '(Read))
      ;; Original should remain unchanged
      (should (= (length gptel-agent--original-tools) 4)))))

;;;; Mode Display Fallback Tests

(ert-deftest gptel-agent-modes-test-mode-display-fallback ()
  "Test mode display fallback for undefined mode."
  (gptel-agent-modes-test--with-clean-state
    (let ((display (gptel-agent--mode-display 'unknown-mode)))
      (should (stringp display))
      ;; `capitalize' capitalizes each word, so "unknown-mode" -> "Unknown-Mode"
      (should (string= display "Unknown-Mode")))))

(ert-deftest gptel-agent-modes-test-mode-face-custom-fallback ()
  "Test mode face fallback for custom mode."
  (gptel-agent-modes-test--with-clean-state
    (should (eq (gptel-agent--mode-face 'custom-mode)
                'gptel-agent-mode-custom-face))))

(ert-deftest gptel-agent-modes-test-mode-tools-default ()
  "Test mode tools default when not specified."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-custom-modes '((test . (:preset test)))))
      (should (eq (gptel-agent--mode-tools 'test) 'all)))))

(ert-deftest gptel-agent-modes-test-mode-preset-nil ()
  "Test mode preset when not in custom modes."
  (gptel-agent-modes-test--with-clean-state
    (should-not (gptel-agent--mode-preset 'nonexistent))))

;;;; Switch To Mode Tests

(ert-deftest gptel-agent-modes-test-switch-to-mode-updates-state ()
  "Test switch-to-mode updates current mode state."
  (gptel-agent-modes-test--with-clean-state
    (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _))))
      (gptel-agent--switch-to-mode 'plan)
      (should (eq gptel-agent--current-mode 'plan)))))

(ert-deftest gptel-agent-modes-test-switch-to-mode-shows-message ()
  "Test switch-to-mode shows message when enabled."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-mode-show-message t)
          (message-shown nil))
      (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _)))
                ((symbol-function 'message)
                 (lambda (&rest _) (setq message-shown t))))
        (gptel-agent--switch-to-mode 'agent)
        (should message-shown)))))

(ert-deftest gptel-agent-modes-test-switch-to-mode-no-message ()
  "Test switch-to-mode doesn't show message when disabled."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-mode-show-message nil)
          (message-shown nil))
      (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _)))
                ((symbol-function 'message)
                 (lambda (&rest _) (setq message-shown t))))
        (gptel-agent--switch-to-mode 'agent)
        (should-not message-shown)))))

(ert-deftest gptel-agent-modes-test-switch-to-mode-applies-preset ()
  "Test switch-to-mode applies preset."
  (gptel-agent-modes-test--with-clean-state
    (let ((preset-applied nil))
      (cl-letf (((symbol-function 'gptel--apply-preset)
                 (lambda (preset _) (setq preset-applied preset))))
        (gptel-agent--switch-to-mode 'agent)
        (should (eq preset-applied 'gptel-agent))))))

(ert-deftest gptel-agent-modes-test-switch-to-mode-nil-preset ()
  "Test switch-to-mode handles nil preset."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-custom-modes '((nopreset . (:display "No Preset"
                                                   :tools all)))))
      (cl-letf (((symbol-function 'gptel--apply-preset)
                 (lambda (&rest _) (error "Should not be called"))))
        ;; Should not error
        (gptel-agent--switch-to-mode 'nopreset)
        (should (eq gptel-agent--current-mode 'nopreset))))))

;;;; Toggle Mode Tests

(ert-deftest gptel-agent-modes-test-toggle-mode-initializes ()
  "Test toggle-mode initializes current mode if nil."
  (gptel-agent-modes-test--with-clean-state
    (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _))))
      (gptel-agent-toggle-mode)
      (should gptel-agent--current-mode))))

(ert-deftest gptel-agent-modes-test-toggle-mode-cycles ()
  "Test toggle-mode cycles through modes."
  (gptel-agent-modes-test--with-clean-state
    (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _))))
      (setq gptel-agent--current-mode 'agent)
      (gptel-agent-toggle-mode)
      ;; Should switch to next mode (plan)
      (should (eq gptel-agent--current-mode 'plan)))))

(ert-deftest gptel-agent-modes-test-toggle-mode-wraps ()
  "Test toggle-mode wraps around to first mode."
  (gptel-agent-modes-test--with-clean-state
    (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _))))
      (setq gptel-agent--current-mode 'plan)
      (gptel-agent-toggle-mode)
      ;; Should wrap back to agent
      (should (eq gptel-agent--current-mode 'agent)))))

(ert-deftest gptel-agent-modes-test-toggle-mode-with-prefix ()
  "Test toggle-mode with prefix arg shows menu."
  (gptel-agent-modes-test--with-clean-state
    (let ((menu-shown nil))
      (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (setq menu-shown t)
                   "Agent - Full tool access for autonomous task execution")))
        (gptel-agent-toggle-mode t)
        (should menu-shown)))))

;;;; Set Mode Tests

(ert-deftest gptel-agent-modes-test-set-mode ()
  "Test set-mode sets the mode directly."
  (gptel-agent-modes-test--with-clean-state
    (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _))))
      (gptel-agent-set-mode 'plan)
      (should (eq gptel-agent--current-mode 'plan)))))

;;;; Mode Status Tests

(ert-deftest gptel-agent-modes-test-mode-status ()
  "Test mode-status displays status message."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode 'agent)
    (let ((status-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq status-message (apply #'format fmt args)))))
        (gptel-agent-mode-status)
        (should (stringp status-message))
        (should (string-match-p "Agent" status-message))
        (should (string-match-p "all" status-message))))))

(ert-deftest gptel-agent-modes-test-mode-status-plan ()
  "Test mode-status for plan mode."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode 'plan)
    (let ((status-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq status-message (apply #'format fmt args)))))
        (gptel-agent-mode-status)
        (should (string-match-p "Plan" status-message))
        (should (string-match-p "read-only" status-message))))))

(ert-deftest gptel-agent-modes-test-mode-status-custom-tools ()
  "Test mode-status for mode with explicit tool list."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-custom-modes '((test . (:preset test
                                               :display "Test"
                                               :tools (Read Write))))))
      (setq gptel-agent--current-mode 'test)
      (let ((status-message nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq status-message (apply #'format fmt args)))))
          (gptel-agent-mode-status)
          (should (string-match-p "2 selected" status-message)))))))

(ert-deftest gptel-agent-modes-test-mode-status-nil-defaults-to-agent ()
  "Test mode-status defaults to agent when nil."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode nil)
    (let ((status-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq status-message (apply #'format fmt args)))))
        (gptel-agent-mode-status)
        (should (string-match-p "Agent" status-message))))))

;;;; Mode Line Update Tests

(ert-deftest gptel-agent-modes-test-update-mode-line-when-configured ()
  "Test update-mode-line adds indicator when configured for mode-line."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-mode-indicator 'mode-line)
          (mode-line-format '("%b")))
      (gptel-agent--update-mode-line)
      (should (memq 'gptel-agent-mode-line-indicator mode-line-format)))))

(ert-deftest gptel-agent-modes-test-update-mode-line-both ()
  "Test update-mode-line adds indicator when configured for both."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-mode-indicator 'both)
          (mode-line-format '("%b")))
      (gptel-agent--update-mode-line)
      (should (memq 'gptel-agent-mode-line-indicator mode-line-format)))))

(ert-deftest gptel-agent-modes-test-update-mode-line-not-for-header ()
  "Test update-mode-line doesn't add indicator when configured for header-line."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-mode-indicator 'header-line)
          (mode-line-format '("%b")))
      (gptel-agent--update-mode-line)
      (should-not (memq 'gptel-agent-mode-line-indicator mode-line-format)))))

(ert-deftest gptel-agent-modes-test-update-mode-line-no-duplicates ()
  "Test update-mode-line doesn't add duplicate indicators."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-mode-indicator 'mode-line)
          (mode-line-format '("%b" gptel-agent-mode-line-indicator)))
      (gptel-agent--update-mode-line)
      ;; Should still only have one indicator
      (should (= 1 (cl-count 'gptel-agent-mode-line-indicator mode-line-format))))))

;;;; Header Line Button Tests

(ert-deftest gptel-agent-modes-test-make-header-line-button ()
  "Test header line button creation."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode 'agent)
    (let ((button (gptel-agent--make-header-line-mode-button)))
      (should (stringp button))
      (should (string-match-p "Agent" button)))))

(ert-deftest gptel-agent-modes-test-header-line-button-has-face ()
  "Test header line button has face property."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode 'agent)
    (let ((button (gptel-agent--make-header-line-mode-button)))
      (should (get-text-property 0 'face button)))))

(ert-deftest gptel-agent-modes-test-header-line-button-nil-mode ()
  "Test header line button defaults to agent when mode is nil."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode nil)
    (let ((button (gptel-agent--make-header-line-mode-button)))
      (should (string-match-p "Agent" button)))))

;;;; Indicator String Tests

(ert-deftest gptel-agent-modes-test-indicator-string-nil-mode ()
  "Test indicator string defaults to agent when mode is nil."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode nil)
    (let ((str (gptel-agent--mode-indicator-string)))
      (should (string-match-p "Agent" str)))))

(ert-deftest gptel-agent-modes-test-indicator-string-has-brackets ()
  "Test indicator string has brackets."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode 'agent)
    (let ((str (gptel-agent--mode-indicator-string)))
      (should (string-match-p "\\[" str))
      (should (string-match-p "\\]" str)))))

(ert-deftest gptel-agent-modes-test-indicator-string-has-face ()
  "Test indicator string has face property."
  (gptel-agent-modes-test--with-clean-state
    (setq gptel-agent--current-mode 'agent)
    (let ((str (gptel-agent--mode-indicator-string)))
      (should (get-text-property 0 'face str)))))

;;;; Mode Line Indicator Variable Tests

(ert-deftest gptel-agent-modes-test-mode-line-indicator-risky ()
  "Test mode line indicator is marked as risky local variable."
  (should (get 'gptel-agent-mode-line-indicator 'risky-local-variable)))

;;;; Modes Setup Tests

(ert-deftest gptel-agent-modes-test-modes-setup ()
  "Test modes-setup enables enhanced mode."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (gptel-agent-modes-setup)
    (should gptel-agent-enhanced-mode)
    (gptel-agent-enhanced-mode -1)))

;;;; Buffer Local Variable Tests

(ert-deftest gptel-agent-modes-test-current-mode-buffer-local ()
  "Test current mode is buffer local."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (setq gptel-agent--current-mode 'agent)
    (with-temp-buffer
      (gptel-agent-modes-test--setup)
      (setq gptel-agent--current-mode 'plan)
      (should (eq gptel-agent--current-mode 'plan)))
    (should (eq gptel-agent--current-mode 'agent))))

(ert-deftest gptel-agent-modes-test-mode-index-buffer-local ()
  "Test mode index is buffer local."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (setq gptel-agent--mode-index 1)
    (with-temp-buffer
      (gptel-agent-modes-test--setup)
      (setq gptel-agent--mode-index 2)
      (should (= gptel-agent--mode-index 2)))
    (should (= gptel-agent--mode-index 1))))

(ert-deftest gptel-agent-modes-test-original-tools-buffer-local ()
  "Test original tools is buffer local."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (setq gptel-agent--original-tools '(a b))
    (with-temp-buffer
      (gptel-agent-modes-test--setup)
      (setq gptel-agent--original-tools '(c d))
      (should (equal gptel-agent--original-tools '(c d))))
    (should (equal gptel-agent--original-tools '(a b)))))

;;;; Minor Mode Hook Tests

(ert-deftest gptel-agent-modes-test-enhanced-mode-runs-hook ()
  "Test enhanced mode runs hook functions."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (let ((hook-called nil))
      (add-hook 'gptel-agent-enhanced-mode-functions
                (lambda () (setq hook-called t)))
      (gptel-agent-enhanced-mode 1)
      (should hook-called)
      (gptel-agent-enhanced-mode -1)
      (remove-hook 'gptel-agent-enhanced-mode-functions
                   (lambda () (setq hook-called t))))))

;;;; Minor Mode State Reset Tests

(ert-deftest gptel-agent-modes-test-minor-mode-resets-index ()
  "Test minor mode resets mode index on disable."
  (with-temp-buffer
    (gptel-agent-modes-test--setup)
    (gptel-agent-enhanced-mode 1)
    (setq gptel-agent--mode-index 5)
    (gptel-agent-enhanced-mode -1)
    (should (= gptel-agent--mode-index 0))))

;;;; Edge Case Tests

(ert-deftest gptel-agent-modes-test-empty-custom-modes ()
  "Test behavior with empty custom modes."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-custom-modes nil))
      (should-not (gptel-agent--mode-list))
      (should-not (gptel-agent--get-mode-plist 'agent)))))

(ert-deftest gptel-agent-modes-test-single-mode ()
  "Test cycling with single mode wraps to itself."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-custom-modes '((only . (:preset only
                                               :display "Only"
                                               :tools all)))))
      (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _))))
        (setq gptel-agent--current-mode 'only)
        (gptel-agent-toggle-mode)
        (should (eq gptel-agent--current-mode 'only))))))

(ert-deftest gptel-agent-modes-test-mode-not-in-list ()
  "Test cycling when current mode not in list."
  (gptel-agent-modes-test--with-clean-state
    (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _))))
      (setq gptel-agent--current-mode 'nonexistent)
      (gptel-agent-toggle-mode)
      ;; Should cycle from index 0 (first mode after nonexistent)
      (should gptel-agent--current-mode))))

(ert-deftest gptel-agent-modes-test-mode-plist-nil-display ()
  "Test mode with nil display uses capitalized symbol name."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-custom-modes '((test . (:preset test)))))
      (should (string= (gptel-agent--mode-display 'test) "Test")))))

(ert-deftest gptel-agent-modes-test-custom-face-override ()
  "Test custom face override in mode definition."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-custom-modes
           '((custom . (:preset custom
                        :display "Custom"
                        :face font-lock-warning-face)))))
      (should (eq (gptel-agent--mode-face 'custom)
                  'font-lock-warning-face)))))

;;;; Read-Only Tools Content Tests

(ert-deftest gptel-agent-modes-test-read-only-includes-webfetch ()
  "Test read-only tools includes WebFetch."
  (should (member "WebFetch" gptel-agent--read-only-tools)))

(ert-deftest gptel-agent-modes-test-read-only-includes-websearch ()
  "Test read-only tools includes WebSearch."
  (should (member "WebSearch" gptel-agent--read-only-tools)))

(ert-deftest gptel-agent-modes-test-read-only-includes-agent ()
  "Test read-only tools includes Agent."
  (should (member "Agent" gptel-agent--read-only-tools)))

(ert-deftest gptel-agent-modes-test-read-only-excludes-delete ()
  "Test read-only tools excludes destructive tools."
  (should-not (member "Delete" gptel-agent--read-only-tools))
  (should-not (member "Move" gptel-agent--read-only-tools)))

;;;; Mode Description Tests

(ert-deftest gptel-agent-modes-test-agent-has-description ()
  "Test agent mode has description."
  (let ((plist (gptel-agent--get-mode-plist 'agent)))
    (should (plist-get plist :description))))

(ert-deftest gptel-agent-modes-test-plan-has-description ()
  "Test plan mode has description."
  (let ((plist (gptel-agent--get-mode-plist 'plan)))
    (should (plist-get plist :description))))

(ert-deftest gptel-agent-modes-test-switch-mode-shows-description ()
  "Test switch-to-mode includes description in message."
  (gptel-agent-modes-test--with-clean-state
    (let ((gptel-agent-mode-show-message t)
          (shown-message nil))
      (cl-letf (((symbol-function 'gptel--apply-preset) (lambda (&rest _)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (gptel-agent--switch-to-mode 'agent)
        (should (string-match-p "Full tool access" shown-message))))))

(provide 'gptel-agent-modes-test)
;;; gptel-agent-modes-test.el ends here
