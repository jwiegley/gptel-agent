;;; gptel-agent-transient-test.el --- Tests for gptel-agent-transient -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tests for gptel-agent-transient.el covering:
;; - Session permission caching
;; - Project permission persistence
;; - Approval decision handling
;; - Overlay integration
;; - Fallback UI behavior

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'gptel-agent-transient)

;;;; Test Helpers

(defvar gptel-agent-transient-test--temp-dirs nil
  "List of temporary directories to clean up after tests.")

(defun gptel-agent-transient-test--make-temp-project ()
  "Create a temporary project directory for testing.
Returns the path to the created directory."
  (let ((dir (make-temp-file "gptel-agent-transient-test-" t)))
    (push dir gptel-agent-transient-test--temp-dirs)
    dir))

(defun gptel-agent-transient-test--cleanup ()
  "Clean up test resources."
  (dolist (dir gptel-agent-transient-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq gptel-agent-transient-test--temp-dirs nil))

(defmacro gptel-agent-transient-test--with-cleanup (&rest body)
  "Execute BODY with automatic cleanup."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (gptel-agent-transient-test--cleanup)))

(defun gptel-agent-transient-test--write-config (dir content)
  "Write CONTENT to .gptel-agent.el in DIR."
  (let ((file (expand-file-name ".gptel-agent.el" dir)))
    (with-temp-file file
      (insert content))
    file))

;;;; Session Permission Tests

(ert-deftest gptel-agent-transient-test-init-session-permissions ()
  "Test session permissions initialization."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--init-session-permissions)
    (should (hash-table-p gptel-agent--session-permissions))))

(ert-deftest gptel-agent-transient-test-store-session-permission ()
  "Test storing session permissions."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve)
    (let ((entry (gethash 'Bash gptel-agent--session-permissions)))
      (should entry)
      (should (eq (plist-get entry :decision) 'approve)))))

(ert-deftest gptel-agent-transient-test-store-session-permission-with-pattern ()
  "Test storing session permissions with argument patterns."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve "git *")
    (let ((entry (gethash 'Bash gptel-agent--session-permissions)))
      (should entry)
      (should (eq (plist-get entry :decision) 'approve))
      (should (string= (plist-get entry :args-pattern) "git *")))))

(ert-deftest gptel-agent-transient-test-check-session-permission-approve ()
  "Test checking session permission returns approve."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve)
    (should (eq (gptel-agent--check-session-permission 'Bash '("git status"))
                'approve))))

(ert-deftest gptel-agent-transient-test-check-session-permission-deny ()
  "Test checking session permission returns deny."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Eval 'deny)
    (should (eq (gptel-agent--check-session-permission 'Eval '("(delete-file)"))
                'deny))))

(ert-deftest gptel-agent-transient-test-check-session-permission-no-match ()
  "Test checking session permission returns nil when no match."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (should (null (gptel-agent--check-session-permission 'Unknown '("args"))))))

(ert-deftest gptel-agent-transient-test-clear-session-permissions ()
  "Test clearing session permissions."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve)
    (gptel-agent-clear-session-permissions)
    (should (null gptel-agent--session-permissions))))

(ert-deftest gptel-agent-transient-test-session-permissions-buffer-local ()
  "Test that session permissions are buffer-local."
  (let ((buf1 (generate-new-buffer "*test-buf-1*"))
        (buf2 (generate-new-buffer "*test-buf-2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (setq gptel-agent--session-permissions nil)
            (gptel-agent--store-session-permission 'Bash 'approve))
          (with-current-buffer buf2
            (setq gptel-agent--session-permissions nil)
            (should (null (gptel-agent--check-session-permission 'Bash '())))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;;; Decision Conversion Tests

(ert-deftest gptel-agent-transient-test-decision-to-symbol-y ()
  "Test decision conversion for 'y' (approve once)."
  (should (eq (gptel-agent--decision-to-symbol ?y) 'approve-once)))

(ert-deftest gptel-agent-transient-test-decision-to-symbol-a ()
  "Test decision conversion for 'a' (approve session)."
  (should (eq (gptel-agent--decision-to-symbol ?a) 'approve-session)))

(ert-deftest gptel-agent-transient-test-decision-to-symbol-p ()
  "Test decision conversion for 'p' (approve project)."
  (should (eq (gptel-agent--decision-to-symbol ?p) 'approve-project)))

(ert-deftest gptel-agent-transient-test-decision-to-symbol-n ()
  "Test decision conversion for 'n' (deny once)."
  (should (eq (gptel-agent--decision-to-symbol ?n) 'deny-once)))

(ert-deftest gptel-agent-transient-test-decision-to-symbol-d ()
  "Test decision conversion for 'd' (deny always)."
  (should (eq (gptel-agent--decision-to-symbol ?d) 'deny-always)))

(ert-deftest gptel-agent-transient-test-decision-to-symbol-invalid ()
  "Test decision conversion for invalid input."
  (should (null (gptel-agent--decision-to-symbol ?x))))

;;;; Approval Decisions List Tests

(ert-deftest gptel-agent-transient-test-approval-decisions-defined ()
  "Test that approval decisions constant is defined."
  (should (listp gptel-agent--approval-decisions))
  (should (= (length gptel-agent--approval-decisions) 5)))

(ert-deftest gptel-agent-transient-test-approval-decisions-contents ()
  "Test approval decisions constant contents."
  (should (memq 'approve-once gptel-agent--approval-decisions))
  (should (memq 'approve-session gptel-agent--approval-decisions))
  (should (memq 'approve-project gptel-agent--approval-decisions))
  (should (memq 'deny-once gptel-agent--approval-decisions))
  (should (memq 'deny-always gptel-agent--approval-decisions)))

;;;; Transient Availability Tests

(ert-deftest gptel-agent-transient-test-transient-available-custom ()
  "Test transient availability check respects customization."
  (let ((gptel-agent-approval-use-transient nil))
    (should-not (gptel-agent--transient-available-p))))

;;;; Complete Approval Tests

(ert-deftest gptel-agent-transient-test-complete-approval-callback ()
  "Test that complete approval calls callback."
  (let ((called nil)
        (decision-received nil))
    (setq gptel-agent--pending-approval
          (list :tool-name 'Bash
                :args '("test")
                :callback (lambda (d)
                            (setq called t decision-received d))))
    (with-temp-buffer
      (setq gptel-agent--session-permissions nil)
      (gptel-agent--complete-approval 'approve-once)
      (should called)
      (should (eq decision-received 'approve-once)))))

(ert-deftest gptel-agent-transient-test-complete-approval-stores-session ()
  "Test that approve-session stores permission."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (setq gptel-agent--pending-approval
          (list :tool-name 'Bash
                :args '("test")
                :callback #'ignore))
    (gptel-agent--complete-approval 'approve-session)
    (should (eq (gptel-agent--check-session-permission 'Bash '("test"))
                'approve))))

(ert-deftest gptel-agent-transient-test-complete-approval-stores-deny ()
  "Test that deny-always stores permission."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (setq gptel-agent--pending-approval
          (list :tool-name 'Eval
                :args '("test")
                :callback #'ignore))
    (gptel-agent--complete-approval 'deny-always)
    (should (eq (gptel-agent--check-session-permission 'Eval '("test"))
                'deny))))

;;;; Approval Menu Description Tests

(ert-deftest gptel-agent-transient-test-menu-description ()
  "Test approval menu description generation."
  (setq gptel-agent--pending-approval
        (list :tool-name 'Bash :args '("git status")))
  (let ((desc (gptel-agent--approval-menu-description)))
    (should (stringp desc))
    (should (string-match-p "Bash" desc))
    (should (string-match-p "git status" desc))))

(ert-deftest gptel-agent-transient-test-menu-description-truncates ()
  "Test that long args are truncated in menu description."
  (setq gptel-agent--pending-approval
        (list :tool-name 'Bash
              :args (list (make-string 100 ?x))))
  (let ((desc (gptel-agent--approval-menu-description)))
    (should (< (length desc) 150))))

;;;; Request Approval Tests

(ert-deftest gptel-agent-transient-test-request-approval-uses-cache ()
  "Test that request approval uses cached decisions."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve)
    (let ((decision-received nil))
      (gptel-agent-request-approval
       'Bash '("test")
       (lambda (d) (setq decision-received d)))
      ;; Should immediately return cached result
      (should (eq decision-received 'approve-once)))))

(ert-deftest gptel-agent-transient-test-request-approval-deny-cache ()
  "Test that request approval uses cached deny decisions."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Eval 'deny)
    (let ((decision-received nil))
      (gptel-agent-request-approval
       'Eval '("test")
       (lambda (d) (setq decision-received d)))
      (should (eq decision-received 'deny-once)))))

;;;; Overlay Integration Tests

(ert-deftest gptel-agent-transient-test-approval-keymap-defined ()
  "Test that approval keymap is defined."
  (should (keymapp gptel-agent--approval-keymap)))

(ert-deftest gptel-agent-transient-test-approval-keymap-bindings ()
  "Test approval keymap has expected bindings."
  (should (eq (lookup-key gptel-agent--approval-keymap "y")
              #'gptel-agent--overlay-approve-once))
  (should (eq (lookup-key gptel-agent--approval-keymap "a")
              #'gptel-agent--overlay-approve-session))
  (should (eq (lookup-key gptel-agent--approval-keymap "p")
              #'gptel-agent--overlay-approve-project))
  (should (eq (lookup-key gptel-agent--approval-keymap "n")
              #'gptel-agent--overlay-deny-once))
  (should (eq (lookup-key gptel-agent--approval-keymap "d")
              #'gptel-agent--overlay-deny-always)))

(ert-deftest gptel-agent-transient-test-add-approval-to-overlay ()
  "Test adding approval UI to an overlay."
  (with-temp-buffer
    (insert "test content")
    (let ((ov (make-overlay 1 12)))
      (gptel-agent--add-approval-to-overlay ov 'Bash '("test") #'ignore)
      (should (overlay-get ov 'gptel-agent-permission))
      (should (eq (overlay-get ov 'gptel-agent-tool-name) 'Bash))
      (should (overlay-get ov 'before-string))
      (should (string-match-p "APPROVAL"
                              (overlay-get ov 'before-string)))
      (delete-overlay ov))))

;;;; Tool Denied Tests

(ert-deftest gptel-agent-transient-test-tool-denied-returns-error ()
  "Test that tool denied returns error structure."
  (let ((result (gptel-agent--tool-denied 'Bash '("rm -rf /"))))
    (should (listp result))
    (should (eq (car result) :error))
    (should (stringp (cadr result)))
    (should (string-match-p "denied" (cadr result)))))

;;;; Project Permission Tests

(ert-deftest gptel-agent-transient-test-load-project-permissions-missing ()
  "Test loading permissions when file doesn't exist."
  (gptel-agent-transient-test--with-cleanup
    ;; Without a project root, should return nil
    (let ((result (gptel-agent--load-project-permission-config)))
      (should (null result)))))

;;;; Customization Tests

(ert-deftest gptel-agent-transient-test-custom-use-transient-default ()
  "Test default value for use-transient custom."
  (should gptel-agent-approval-use-transient))

(ert-deftest gptel-agent-transient-test-custom-show-preview-default ()
  "Test default value for show-preview custom."
  (should gptel-agent-approval-show-preview))

;;;; String/Symbol Normalization Tests

(ert-deftest gptel-agent-transient-test-store-permission-string-tool ()
  "Test storing permission with string tool name."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission "Bash" 'approve)
    ;; Should be stored as symbol
    (should (gethash 'Bash gptel-agent--session-permissions))))

(ert-deftest gptel-agent-transient-test-check-permission-string-tool ()
  "Test checking permission with string tool name."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve)
    (should (eq (gptel-agent--check-session-permission "Bash" '())
                'approve))))

(provide 'gptel-agent-transient-test)
;;; gptel-agent-transient-test.el ends here
