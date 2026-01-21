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

;;;; Additional Customization Tests

(ert-deftest gptel-agent-transient-test-customization-group ()
  "Test customization group is defined."
  (should (get 'gptel-agent-approval 'custom-group)))

(ert-deftest gptel-agent-transient-test-use-transient-type ()
  "Test use-transient custom type."
  (let ((type (get 'gptel-agent-approval-use-transient 'custom-type)))
    (should (eq type 'boolean))))

(ert-deftest gptel-agent-transient-test-show-preview-type ()
  "Test show-preview custom type."
  (let ((type (get 'gptel-agent-approval-show-preview 'custom-type)))
    (should (eq type 'boolean))))

;;;; Session Permission Pattern Matching Tests

(ert-deftest gptel-agent-transient-test-check-permission-pattern-match ()
  "Test pattern matching in session permission check."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve "git")
    ;; Pattern matches
    (should (eq (gptel-agent--check-session-permission 'Bash '("git status"))
                'approve))))

(ert-deftest gptel-agent-transient-test-check-permission-pattern-no-match ()
  "Test pattern not matching returns nil."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve "git")
    ;; Pattern doesn't match
    (should (null (gptel-agent--check-session-permission 'Bash '("rm -rf /"))))))

(ert-deftest gptel-agent-transient-test-check-permission-nil-pattern ()
  "Test nil pattern matches everything."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve nil)
    ;; Nil pattern matches any args
    (should (eq (gptel-agent--check-session-permission 'Bash '("anything"))
                'approve))))

(ert-deftest gptel-agent-transient-test-session-permission-timestamp ()
  "Test session permission stores timestamp."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--store-session-permission 'Bash 'approve)
    (let ((entry (gethash 'Bash gptel-agent--session-permissions)))
      (should (plist-get entry :timestamp))
      (should (listp (plist-get entry :timestamp))))))

;;;; Project Permission Tests

(ert-deftest gptel-agent-transient-test-load-project-permissions-valid ()
  "Test loading valid project permissions."
  (gptel-agent-transient-test--with-cleanup
    (let ((dir (gptel-agent-transient-test--make-temp-project)))
      (gptel-agent-transient-test--write-config
       dir
       "(gptel-agent-project-config :permissions ((Bash . allow)))")
      (cl-letf (((symbol-function 'gptel-agent--project-root) (lambda () dir)))
        (let ((perms (gptel-agent--load-project-permission-config)))
          (should perms)
          (should (eq (alist-get 'Bash perms) 'allow)))))))

(ert-deftest gptel-agent-transient-test-load-project-permissions-invalid ()
  "Test loading invalid project permissions."
  (gptel-agent-transient-test--with-cleanup
    (let ((dir (gptel-agent-transient-test--make-temp-project)))
      (gptel-agent-transient-test--write-config dir "invalid syntax {{")
      (cl-letf (((symbol-function 'gptel-agent--project-root) (lambda () dir)))
        ;; Should return nil on parse error
        (should (null (gptel-agent--load-project-permission-config)))))))

(ert-deftest gptel-agent-transient-test-load-project-permissions-wrong-header ()
  "Test loading config with wrong header."
  (gptel-agent-transient-test--with-cleanup
    (let ((dir (gptel-agent-transient-test--make-temp-project)))
      (gptel-agent-transient-test--write-config
       dir
       "(wrong-header :permissions ((Bash . allow)))")
      (cl-letf (((symbol-function 'gptel-agent--project-root) (lambda () dir)))
        ;; Should return nil for wrong header
        (should (null (gptel-agent--load-project-permission-config)))))))

(ert-deftest gptel-agent-transient-test-save-project-permission-approve ()
  "Test saving project permission for approve."
  (gptel-agent-transient-test--with-cleanup
    (let ((dir (gptel-agent-transient-test--make-temp-project)))
      (cl-letf (((symbol-function 'gptel-agent--project-root) (lambda () dir)))
        (gptel-agent--save-project-permission 'Bash 'approve)
        ;; Verify file was created
        (should (file-exists-p (expand-file-name ".gptel-agent.el" dir)))
        ;; Verify contents
        (let ((perms (gptel-agent--load-project-permission-config)))
          (should (eq (alist-get 'Bash perms) 'allow)))))))

(ert-deftest gptel-agent-transient-test-save-project-permission-deny ()
  "Test saving project permission for deny."
  (gptel-agent-transient-test--with-cleanup
    (let ((dir (gptel-agent-transient-test--make-temp-project)))
      (cl-letf (((symbol-function 'gptel-agent--project-root) (lambda () dir)))
        (gptel-agent--save-project-permission 'Eval 'deny)
        (let ((perms (gptel-agent--load-project-permission-config)))
          (should (eq (alist-get 'Eval perms) 'deny)))))))

(ert-deftest gptel-agent-transient-test-save-project-permission-with-pattern ()
  "Test saving project permission with pattern."
  (gptel-agent-transient-test--with-cleanup
    (let ((dir (gptel-agent-transient-test--make-temp-project)))
      (cl-letf (((symbol-function 'gptel-agent--project-root) (lambda () dir)))
        (gptel-agent--save-project-permission 'Bash 'approve "git.*")
        (let ((perms (gptel-agent--load-project-permission-config)))
          (should (alist-get 'Bash perms)))))))

(ert-deftest gptel-agent-transient-test-save-project-permission-no-root ()
  "Test saving permission fails without project root."
  (cl-letf (((symbol-function 'gptel-agent--project-root) (lambda () nil)))
    (should-error (gptel-agent--save-project-permission 'Bash 'approve)
                  :type 'user-error)))

(ert-deftest gptel-agent-transient-test-save-project-permission-update ()
  "Test updating existing project permission."
  (gptel-agent-transient-test--with-cleanup
    (let ((dir (gptel-agent-transient-test--make-temp-project)))
      (gptel-agent-transient-test--write-config
       dir
       "(gptel-agent-project-config :permissions ((Bash . deny)))")
      (cl-letf (((symbol-function 'gptel-agent--project-root) (lambda () dir)))
        ;; Update to approve
        (gptel-agent--save-project-permission 'Bash 'approve)
        (let ((perms (gptel-agent--load-project-permission-config)))
          (should (eq (alist-get 'Bash perms) 'allow)))))))

(ert-deftest gptel-agent-transient-test-save-project-permission-string-tool ()
  "Test saving with string tool name."
  (gptel-agent-transient-test--with-cleanup
    (let ((dir (gptel-agent-transient-test--make-temp-project)))
      (cl-letf (((symbol-function 'gptel-agent--project-root) (lambda () dir)))
        (gptel-agent--save-project-permission "Write" 'approve)
        (let ((perms (gptel-agent--load-project-permission-config)))
          (should (eq (alist-get 'Write perms) 'allow)))))))

;;;; Fallback UI Tests

(ert-deftest gptel-agent-transient-test-approval-fallback-approve ()
  "Test fallback approval with y key."
  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y)))
    (should (eq (gptel-agent--approval-fallback 'Bash '("test"))
                'approve-once))))

(ert-deftest gptel-agent-transient-test-approval-fallback-approve-session ()
  "Test fallback approval with a key."
  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?a)))
    (should (eq (gptel-agent--approval-fallback 'Bash '("test"))
                'approve-session))))

(ert-deftest gptel-agent-transient-test-approval-fallback-deny ()
  "Test fallback denial with n key."
  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n)))
    (should (eq (gptel-agent--approval-fallback 'Bash '("test"))
                'deny-once))))

(ert-deftest gptel-agent-transient-test-approval-fallback-prompt-format ()
  "Test fallback prompt includes tool name."
  (let ((captured-prompt nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (prompt &rest _)
                 (setq captured-prompt prompt)
                 ?y)))
      (gptel-agent--approval-fallback 'Bash '("git status"))
      (should (string-match-p "Bash" captured-prompt))
      (should (string-match-p "git status" captured-prompt)))))

(ert-deftest gptel-agent-transient-test-approval-fallback-truncates-long-args ()
  "Test fallback truncates long arguments."
  (let ((captured-prompt nil)
        (long-arg (make-string 100 ?x)))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (prompt &rest _)
                 (setq captured-prompt prompt)
                 ?y)))
      (gptel-agent--approval-fallback 'Bash (list long-arg))
      ;; Should be truncated
      (should (< (length captured-prompt) 200)))))

;;;; Interactive Approval Command Tests

(ert-deftest gptel-agent-transient-test-approve-once-command ()
  "Test approve-once interactive command."
  (let ((decision-received nil))
    (setq gptel-agent--pending-approval
          (list :tool-name 'Bash
                :args '("test")
                :callback (lambda (d) (setq decision-received d))))
    (with-temp-buffer
      (setq gptel-agent--session-permissions nil)
      (gptel-agent--approve-once)
      (should (eq decision-received 'approve-once)))))

(ert-deftest gptel-agent-transient-test-approve-session-command ()
  "Test approve-session interactive command."
  (let ((decision-received nil))
    (setq gptel-agent--pending-approval
          (list :tool-name 'Bash
                :args '("test")
                :callback (lambda (d) (setq decision-received d))))
    (with-temp-buffer
      (setq gptel-agent--session-permissions nil)
      (gptel-agent--approve-session)
      (should (eq decision-received 'approve-session))
      ;; Should also store in session
      (should (eq (gptel-agent--check-session-permission 'Bash '())
                  'approve)))))

(ert-deftest gptel-agent-transient-test-deny-once-command ()
  "Test deny-once interactive command."
  (let ((decision-received nil))
    (setq gptel-agent--pending-approval
          (list :tool-name 'Bash
                :args '("test")
                :callback (lambda (d) (setq decision-received d))))
    (with-temp-buffer
      (gptel-agent--deny-once)
      (should (eq decision-received 'deny-once)))))

(ert-deftest gptel-agent-transient-test-deny-always-command ()
  "Test deny-always interactive command."
  (let ((decision-received nil))
    (setq gptel-agent--pending-approval
          (list :tool-name 'Bash
                :args '("test")
                :callback (lambda (d) (setq decision-received d))))
    (with-temp-buffer
      (setq gptel-agent--session-permissions nil)
      (gptel-agent--deny-always)
      (should (eq decision-received 'deny-always))
      ;; Should also store in session
      (should (eq (gptel-agent--check-session-permission 'Bash '())
                  'deny)))))

;;;; Complete Approval Extended Tests

(ert-deftest gptel-agent-transient-test-complete-approval-clears-pending ()
  "Test complete approval clears pending state."
  (setq gptel-agent--pending-approval
        (list :tool-name 'Bash
              :args '("test")
              :callback #'ignore))
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--complete-approval 'approve-once)
    (should (null gptel-agent--pending-approval))))

(ert-deftest gptel-agent-transient-test-complete-approval-nil-pending ()
  "Test complete approval handles nil pending state."
  (setq gptel-agent--pending-approval nil)
  ;; Should not error
  (gptel-agent--complete-approval 'approve-once))

(ert-deftest gptel-agent-transient-test-complete-approval-nil-callback ()
  "Test complete approval handles nil callback."
  (setq gptel-agent--pending-approval
        (list :tool-name 'Bash
              :args '("test")
              :callback nil))
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    ;; Should not error
    (gptel-agent--complete-approval 'approve-once)))

;;;; Menu Description Extended Tests

(ert-deftest gptel-agent-transient-test-menu-description-nil-pending ()
  "Test menu description with nil pending."
  (setq gptel-agent--pending-approval nil)
  (let ((desc (gptel-agent--approval-menu-description)))
    (should (stringp desc))))

(ert-deftest gptel-agent-transient-test-menu-description-nil-args ()
  "Test menu description with nil args."
  (setq gptel-agent--pending-approval
        (list :tool-name 'Bash :args nil))
  (let ((desc (gptel-agent--approval-menu-description)))
    (should (stringp desc))
    (should (string-match-p "Bash" desc))))

;;;; Overlay Integration Extended Tests

(ert-deftest gptel-agent-transient-test-overlay-approve-once ()
  "Test overlay approve once calls callback."
  (with-temp-buffer
    (let ((decision-received nil))
      (setq gptel-agent--overlay-approval-callback
            (lambda (d) (setq decision-received d)))
      (gptel-agent--overlay-approve-once)
      (should (eq decision-received 'approve-once)))))

(ert-deftest gptel-agent-transient-test-overlay-approve-once-no-callback ()
  "Test overlay approve once with no callback."
  (with-temp-buffer
    (setq gptel-agent--overlay-approval-callback nil)
    ;; Should not error
    (gptel-agent--overlay-approve-once)))

(ert-deftest gptel-agent-transient-test-overlay-deny-once ()
  "Test overlay deny once calls callback."
  (with-temp-buffer
    (let ((decision-received nil))
      (setq gptel-agent--overlay-approval-callback
            (lambda (d) (setq decision-received d)))
      (gptel-agent--overlay-deny-once)
      (should (eq decision-received 'deny-once)))))

(ert-deftest gptel-agent-transient-test-add-approval-preserves-keymap ()
  "Test adding approval to overlay with existing keymap."
  (with-temp-buffer
    (insert "test content")
    (let ((ov (make-overlay 1 12))
          (existing-map (make-sparse-keymap)))
      (define-key existing-map "x" #'ignore)
      (overlay-put ov 'keymap existing-map)
      (gptel-agent--add-approval-to-overlay ov 'Bash '("test") #'ignore)
      ;; Should have composed keymap
      (let ((final-map (overlay-get ov 'keymap)))
        (should (keymapp final-map))
        ;; Should still have old binding
        (should (eq (lookup-key final-map "y")
                    #'gptel-agent--overlay-approve-once)))
      (delete-overlay ov))))

(ert-deftest gptel-agent-transient-test-add-approval-preserves-before-string ()
  "Test adding approval preserves existing before-string."
  (with-temp-buffer
    (insert "test content")
    (let ((ov (make-overlay 1 12)))
      (overlay-put ov 'before-string "[EXISTING] ")
      (gptel-agent--add-approval-to-overlay ov 'Bash '("test") #'ignore)
      (let ((before-str (overlay-get ov 'before-string)))
        (should (string-match-p "APPROVAL" before-str))
        (should (string-match-p "EXISTING" before-str)))
      (delete-overlay ov))))

(ert-deftest gptel-agent-transient-test-overlay-stores-tool-args ()
  "Test overlay stores tool args."
  (with-temp-buffer
    (insert "test content")
    (let ((ov (make-overlay 1 12)))
      (gptel-agent--add-approval-to-overlay ov 'Bash '("git" "status") #'ignore)
      (should (equal (overlay-get ov 'gptel-agent-tool-args)
                     '("git" "status")))
      (delete-overlay ov))))

;;;; Tool Denied Extended Tests

(ert-deftest gptel-agent-transient-test-tool-denied-message ()
  "Test tool denied shows message."
  (let ((message-shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) (setq message-shown t))))
      (gptel-agent--tool-denied 'Bash '("rm -rf /"))
      (should message-shown))))

(ert-deftest gptel-agent-transient-test-tool-denied-includes-tool-name ()
  "Test tool denied error includes tool name."
  (let ((result (gptel-agent--tool-denied 'SomeTool '("args"))))
    (should (string-match-p "SomeTool" (cadr result)))))

(ert-deftest gptel-agent-transient-test-tool-denied-includes-args ()
  "Test tool denied error includes args."
  (let ((result (gptel-agent--tool-denied 'Tool '("my-arg"))))
    (should (string-match-p "my-arg" (cadr result)))))

;;;; Resolve With Approval Tests

(ert-deftest gptel-agent-transient-test-resolve-with-approval-allow ()
  "Test resolve with approval when permission is allow."
  (let ((executed nil))
    (cl-letf (((symbol-function 'gptel-agent--check-permission)
               (lambda (&rest _) 'allow)))
      (gptel-agent--resolve-with-approval
       'Bash '("test") (lambda () (setq executed t)))
      (should executed))))

(ert-deftest gptel-agent-transient-test-resolve-with-approval-deny ()
  "Test resolve with approval when permission is deny."
  (let ((denied nil))
    (cl-letf (((symbol-function 'gptel-agent--check-permission)
               (lambda (&rest _) 'deny))
              ((symbol-function 'gptel-agent--tool-denied)
               (lambda (&rest _) (setq denied t) nil)))
      (gptel-agent--resolve-with-approval
       'Bash '("test") (lambda () (error "Should not execute")))
      (should denied))))

(ert-deftest gptel-agent-transient-test-resolve-with-approval-ask ()
  "Test resolve with approval when permission is ask."
  (let ((approval-requested nil))
    (cl-letf (((symbol-function 'gptel-agent--check-permission)
               (lambda (&rest _) 'ask))
              ((symbol-function 'gptel-agent-request-approval)
               (lambda (tool args cb)
                 (setq approval-requested t)
                 (funcall cb 'approve-once))))
      (gptel-agent--resolve-with-approval
       'Bash '("test") #'ignore)
      (should approval-requested))))

(ert-deftest gptel-agent-transient-test-resolve-without-permission-func ()
  "Test resolve without gptel-agent--check-permission available."
  (let ((approval-requested nil))
    (cl-letf (((symbol-function 'gptel-agent--check-permission) nil)
              ((symbol-function 'gptel-agent-request-approval)
               (lambda (tool args cb)
                 (setq approval-requested t)
                 (funcall cb 'approve-once))))
      (fmakunbound 'gptel-agent--check-permission)
      (gptel-agent--resolve-with-approval
       'Bash '("test") #'ignore)
      (should approval-requested))))

;;;; Request Approval Extended Tests

(ert-deftest gptel-agent-transient-test-request-approval-fallback ()
  "Test request approval uses fallback when transient unavailable."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (let ((gptel-agent-approval-use-transient nil)
          (decision-received nil))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y)))
        (gptel-agent-request-approval
         'Bash '("test")
         (lambda (d) (setq decision-received d)))
        (should (eq decision-received 'approve-once))))))

(ert-deftest gptel-agent-transient-test-request-approval-stores-session ()
  "Test request approval stores session permission via fallback."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (let ((gptel-agent-approval-use-transient nil))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?a)))
        (gptel-agent-request-approval 'Bash '("test") #'ignore)
        ;; Should have stored session permission
        (should (eq (gptel-agent--check-session-permission 'Bash '())
                    'approve))))))

(ert-deftest gptel-agent-transient-test-request-approval-stores-deny ()
  "Test request approval stores deny via fallback."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (let ((gptel-agent-approval-use-transient nil))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?d)))
        (gptel-agent-request-approval 'Bash '("test") #'ignore)
        (should (eq (gptel-agent--check-session-permission 'Bash '())
                    'deny))))))

;;;; Buffer Local Variable Tests

(ert-deftest gptel-agent-transient-test-overlay-callback-buffer-local ()
  "Test overlay approval callback is buffer local."
  (let ((buf1 (generate-new-buffer "*test-1*"))
        (buf2 (generate-new-buffer "*test-2*")))
    (unwind-protect
        (progn
          (with-current-buffer buf1
            (setq gptel-agent--overlay-approval-callback 'callback1))
          (with-current-buffer buf2
            (setq gptel-agent--overlay-approval-callback 'callback2)
            (should (eq gptel-agent--overlay-approval-callback 'callback2)))
          (with-current-buffer buf1
            (should (eq gptel-agent--overlay-approval-callback 'callback1))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;;; Edge Case Tests

(ert-deftest gptel-agent-transient-test-init-permissions-idempotent ()
  "Test init session permissions is idempotent."
  (with-temp-buffer
    (setq gptel-agent--session-permissions nil)
    (gptel-agent--init-session-permissions)
    (gptel-agent--store-session-permission 'Bash 'approve)
    ;; Init again should not clear
    (gptel-agent--init-session-permissions)
    (should (eq (gptel-agent--check-session-permission 'Bash '())
                'approve))))

(ert-deftest gptel-agent-transient-test-decision-conversion-all-chars ()
  "Test all decision character conversions."
  (should (eq (gptel-agent--decision-to-symbol ?y) 'approve-once))
  (should (eq (gptel-agent--decision-to-symbol ?a) 'approve-session))
  (should (eq (gptel-agent--decision-to-symbol ?p) 'approve-project))
  (should (eq (gptel-agent--decision-to-symbol ?n) 'deny-once))
  (should (eq (gptel-agent--decision-to-symbol ?d) 'deny-always))
  (should (null (gptel-agent--decision-to-symbol ?z)))
  (should (null (gptel-agent--decision-to-symbol ?1))))

(ert-deftest gptel-agent-transient-test-fallback-non-string-args ()
  "Test fallback handles non-string args."
  (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y)))
    ;; Should not error with list args
    (should (eq (gptel-agent--approval-fallback 'Tool '(1 2 3))
                'approve-once))))

(provide 'gptel-agent-transient-test)
;;; gptel-agent-transient-test.el ends here
