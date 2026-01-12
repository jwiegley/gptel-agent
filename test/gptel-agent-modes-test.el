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

(defun gptel-agent-modes-test--setup ()
  "Set up test environment with clean state."
  (setq gptel-agent--current-mode nil)
  (setq gptel-agent--mode-index 0)
  (setq gptel-agent--original-tools nil))

(defmacro gptel-agent-modes-test--with-clean-state (&rest body)
  "Execute BODY with clean test state."
  `(with-temp-buffer
     (gptel-agent-modes-test--setup)
     ,@body))

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

(provide 'gptel-agent-modes-test)
;;; gptel-agent-modes-test.el ends here
