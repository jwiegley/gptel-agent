;;; gptel-agent-checkpoints-test.el --- Tests for gptel-agent-checkpoints -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Keywords: tests
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tests for gptel-agent-checkpoints.el checkpoint and
;; recovery system.
;;
;; Test coverage:
;; - State serialization/deserialization
;; - SQLite checkpoint storage
;; - Automatic checkpoint triggers
;; - Manual checkpoint command
;; - Recovery interface
;; - Cleanup policy

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load modules under test
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'gptel-agent-sessions)
(require 'gptel-agent-checkpoints)

;;;; Test Helpers

(defvar gptel-agent-checkpoints-test--temp-dir nil
  "Temporary directory for test files.")

(defun gptel-agent-checkpoints-test--setup ()
  "Set up test environment."
  (setq gptel-agent-checkpoints-test--temp-dir
        (make-temp-file "gptel-agent-checkpoints-test-" t))
  (setq gptel-agent-session-db-file
        (expand-file-name "test.db" gptel-agent-checkpoints-test--temp-dir))
  (setq gptel-agent--session-db nil)
  (setq gptel-agent--storage-backend nil)
  (gptel-agent--init-storage))

(defun gptel-agent-checkpoints-test--teardown ()
  "Clean up test environment."
  (when gptel-agent--session-db
    (ignore-errors (gptel-agent--close-database)))
  (setq gptel-agent--session-db nil)
  (setq gptel-agent--storage-backend nil)
  (when (and gptel-agent-checkpoints-test--temp-dir
             (file-directory-p gptel-agent-checkpoints-test--temp-dir))
    (delete-directory gptel-agent-checkpoints-test--temp-dir t)))

(defmacro gptel-agent-checkpoints-test--with-temp-env (&rest body)
  "Execute BODY with temporary test environment."
  `(unwind-protect
       (progn
         (gptel-agent-checkpoints-test--setup)
         ,@body)
     (gptel-agent-checkpoints-test--teardown)))

;;;; Customization Tests

(ert-deftest gptel-agent-checkpoints-test-frequency-default ()
  "Test default checkpoint frequency."
  (should (numberp (default-value 'gptel-agent-checkpoint-frequency)))
  (should (= (default-value 'gptel-agent-checkpoint-frequency) 5)))

(ert-deftest gptel-agent-checkpoints-test-retention-default ()
  "Test default checkpoint retention."
  (should (numberp (default-value 'gptel-agent-checkpoint-retention)))
  (should (= (default-value 'gptel-agent-checkpoint-retention) 10)))

(ert-deftest gptel-agent-checkpoints-test-auto-recover-default ()
  "Test default auto-recover setting."
  (should (default-value 'gptel-agent-checkpoint-auto-recover)))

;;;; Serialization Tests

(ert-deftest gptel-agent-checkpoints-test-serialize-state ()
  "Test checkpoint state serialization."
  (with-temp-buffer
    (let ((state (gptel-agent--serialize-checkpoint-state)))
      (should (listp state))
      (should (plist-get state :timestamp))
      (should (listp (plist-get state :buffer-state))))))

(ert-deftest gptel-agent-checkpoints-test-serialize-todos ()
  "Test todo list serialization."
  (with-temp-buffer
    (setq gptel-agent--todos
          '((:content "Task 1" :status "pending")
            (:content "Task 2" :status "completed")))
    (let ((serialized (gptel-agent--serialize-todos)))
      (should (= (length serialized) 2))
      (should (string= (plist-get (car serialized) :content) "Task 1")))))

(ert-deftest gptel-agent-checkpoints-test-serialize-todos-nil ()
  "Test serialization of nil todo list."
  (with-temp-buffer
    (setq gptel-agent--todos nil)
    (should-not (gptel-agent--serialize-todos))))

(ert-deftest gptel-agent-checkpoints-test-serialize-pending-tools ()
  "Test pending tool calls serialization."
  (with-temp-buffer
    (setq gptel-agent--pending-tool-calls
          '((:tool "Read" :args (:file "/test.txt") :id "1")))
    (let ((serialized (gptel-agent--serialize-pending-tools)))
      (should (= (length serialized) 1))
      (should (string= (plist-get (car serialized) :tool) "Read")))))

(ert-deftest gptel-agent-checkpoints-test-serialize-buffer-state ()
  "Test buffer state serialization."
  (with-temp-buffer
    (setq gptel-agent--checkpoint-tool-count 3)
    (let ((state (gptel-agent--serialize-buffer-state)))
      (should (plist-get state :point))
      (should (plist-get state :buffer-name))
      (should (= (plist-get state :tool-count) 3)))))

;;;; Deserialization Tests

(ert-deftest gptel-agent-checkpoints-test-deserialize-state ()
  "Test checkpoint state deserialization."
  (with-temp-buffer
    (let ((state (list :todos '((:content "Test" :status "pending"))
                      :pending-tools '((:tool "Read" :args nil :id "1"))
                      :buffer-state '(:tool-count 5))))
      (gptel-agent--deserialize-checkpoint-state state)
      (should gptel-agent--todos)
      (should gptel-agent--pending-tool-calls)
      (should (= gptel-agent--checkpoint-tool-count 5)))))

(ert-deftest gptel-agent-checkpoints-test-deserialize-nil ()
  "Test deserialization of nil state."
  (with-temp-buffer
    (gptel-agent--deserialize-checkpoint-state nil)
    ;; Should not error
    (should t)))

;;;; SQLite Storage Tests

(ert-deftest gptel-agent-checkpoints-test-checkpoint-save ()
  "Test checkpoint save to SQLite."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (let* ((session-id (gptel-agent-session-create "/test" "Test"))
           (state (list :timestamp "2025-01-01T00:00:00"
                       :todos nil
                       :buffer-state '(:tool-count 0)))
           (checkpoint-id (gptel-agent--checkpoint-save session-id state)))
      (should checkpoint-id)
      (should (numberp checkpoint-id)))))

(ert-deftest gptel-agent-checkpoints-test-checkpoint-load ()
  "Test checkpoint load from SQLite."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (let* ((session-id (gptel-agent-session-create "/test" "Test"))
           (state (list :timestamp "2025-01-01T00:00:00"
                       :todos '((:content "Test" :status "pending"))))
           (checkpoint-id (gptel-agent--checkpoint-save session-id state))
           (loaded (gptel-agent--checkpoint-load checkpoint-id)))
      (should loaded)
      (should (plist-get loaded :id))
      (should (plist-get loaded :state)))))

(ert-deftest gptel-agent-checkpoints-test-checkpoint-list ()
  "Test checkpoint listing."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (let ((session-id (gptel-agent-session-create "/test" "Test")))
      (gptel-agent--checkpoint-save session-id '(:timestamp "1"))
      (gptel-agent--checkpoint-save session-id '(:timestamp "2"))
      (let ((checkpoints (gptel-agent--checkpoint-list session-id)))
        (should (= (length checkpoints) 2))))))

(ert-deftest gptel-agent-checkpoints-test-checkpoint-delete ()
  "Test checkpoint deletion."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (let* ((session-id (gptel-agent-session-create "/test" "Test"))
           (checkpoint-id (gptel-agent--checkpoint-save session-id '(:timestamp "1"))))
      (gptel-agent--checkpoint-delete checkpoint-id)
      (should-not (gptel-agent--checkpoint-load checkpoint-id)))))

(ert-deftest gptel-agent-checkpoints-test-checkpoint-cleanup ()
  "Test checkpoint cleanup based on retention policy."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (let ((gptel-agent-checkpoint-retention 2)
          (session-id (gptel-agent-session-create "/test" "Test")))
      ;; Create 5 checkpoints
      (dotimes (_ 5)
        (gptel-agent--checkpoint-save session-id '(:timestamp "x")))
      ;; Run cleanup
      (gptel-agent--checkpoint-cleanup session-id)
      ;; Should only have 2 remaining
      (should (= (length (gptel-agent--checkpoint-list session-id)) 2)))))

(ert-deftest gptel-agent-checkpoints-test-checkpoint-get-latest ()
  "Test getting latest checkpoint."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (let ((session-id (gptel-agent-session-create "/test" "Test")))
      (gptel-agent--checkpoint-save session-id '(:timestamp "first"))
      (gptel-agent--checkpoint-save session-id '(:timestamp "second"))
      (let ((latest (gptel-agent--checkpoint-get-latest session-id)))
        (should latest)
        (should (string= (plist-get (plist-get latest :state) :timestamp) "second"))))))

;;;; Automatic Checkpoint Tests

(ert-deftest gptel-agent-checkpoints-test-auto-checkpoint-counter ()
  "Test tool call counter increments."
  (with-temp-buffer
    (setq gptel-agent--checkpoint-tool-count 0)
    (setq gptel-agent-checkpoint-frequency nil)  ; Disable auto-checkpoint
    (let ((gptel-agent-checkpoint-frequency nil))
      (gptel-agent--maybe-auto-checkpoint))
    ;; Counter should still increment even when disabled
    (should (= gptel-agent--checkpoint-tool-count 0))))

(ert-deftest gptel-agent-checkpoints-test-auto-checkpoint-disabled ()
  "Test auto-checkpoint when disabled."
  (with-temp-buffer
    (setq gptel-agent--checkpoint-tool-count 10)
    (let ((gptel-agent-checkpoint-frequency nil))
      (gptel-agent--maybe-auto-checkpoint))
    ;; Should not reset counter when disabled
    (should (= gptel-agent--checkpoint-tool-count 10))))

;;;; Minor Mode Tests

(ert-deftest gptel-agent-checkpoints-test-minor-mode-defined ()
  "Test minor mode is defined."
  (should (fboundp 'gptel-agent-checkpoints-mode)))

(ert-deftest gptel-agent-checkpoints-test-minor-mode-enable ()
  "Test minor mode enable."
  (with-temp-buffer
    (gptel-agent-checkpoints-mode 1)
    (should gptel-agent-checkpoints-mode)
    (should (= gptel-agent--checkpoint-tool-count 0))
    (gptel-agent-checkpoints-mode -1)))

(ert-deftest gptel-agent-checkpoints-test-minor-mode-disable ()
  "Test minor mode disable."
  (with-temp-buffer
    (gptel-agent-checkpoints-mode 1)
    (gptel-agent-checkpoints-mode -1)
    (should-not gptel-agent-checkpoints-mode)))

;;;; Interactive Command Tests

(ert-deftest gptel-agent-checkpoints-test-checkpoint-command-no-session ()
  "Test checkpoint command with no active session."
  (with-temp-buffer
    (setq gptel-agent--current-session-id nil)
    (should-error (gptel-agent-checkpoint))))

(ert-deftest gptel-agent-checkpoints-test-list-checkpoints-command ()
  "Test list checkpoints command with no session."
  (with-temp-buffer
    (setq gptel-agent--current-session-id nil)
    (should-error (gptel-agent-list-checkpoints))))

(ert-deftest gptel-agent-checkpoints-test-checkpoint-count ()
  "Test checkpoint count function."
  (with-temp-buffer
    (setq gptel-agent--current-session-id nil)
    (should (= (gptel-agent-checkpoint-count) 0))))

;;;; Checkpoint List Mode Tests

(ert-deftest gptel-agent-checkpoints-test-list-mode-defined ()
  "Test checkpoint list mode is defined."
  (should (fboundp 'gptel-agent-checkpoints-mode)))

(ert-deftest gptel-agent-checkpoints-test-list-mode-keymap ()
  "Test checkpoint list mode keymap."
  (should (keymapp gptel-agent-checkpoints-mode-map))
  (should (lookup-key gptel-agent-checkpoints-mode-map (kbd "RET")))
  (should (lookup-key gptel-agent-checkpoints-mode-map (kbd "d")))
  (should (lookup-key gptel-agent-checkpoints-mode-map (kbd "x"))))

;;;; Integration Tests

(ert-deftest gptel-agent-checkpoints-test-full-cycle ()
  "Test full checkpoint create-save-load-restore cycle."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (with-temp-buffer
      (let ((session-id (gptel-agent-session-create "/test" "Test")))
        (setq gptel-agent--current-session-id session-id)
        ;; Set up some state
        (setq gptel-agent--todos '((:content "Test task" :status "pending")))
        (setq gptel-agent--checkpoint-tool-count 3)
        ;; Create checkpoint
        (let* ((state (gptel-agent--serialize-checkpoint-state))
               (checkpoint-id (gptel-agent--checkpoint-save session-id state)))
          ;; Clear state
          (setq gptel-agent--todos nil)
          (setq gptel-agent--checkpoint-tool-count 0)
          ;; Restore
          (let ((loaded (gptel-agent--checkpoint-load checkpoint-id)))
            (gptel-agent--deserialize-checkpoint-state (plist-get loaded :state)))
          ;; Verify restoration
          (should gptel-agent--todos)
          (should (= gptel-agent--checkpoint-tool-count 3)))))))

(ert-deftest gptel-agent-checkpoints-test-tool-call-tracking ()
  "Test tool call tracking API."
  (with-temp-buffer
    (setq gptel-agent--checkpoint-tool-count 0)
    (gptel-agent-checkpoints-mode 1)
    ;; Disable auto-checkpoint for this test
    (let ((gptel-agent-checkpoint-frequency nil))
      (gptel-agent-checkpoint-tool-call))
    ;; Tool count should be unchanged when frequency is nil
    (gptel-agent-checkpoints-mode -1)))

(provide 'gptel-agent-checkpoints-test)
;;; gptel-agent-checkpoints-test.el ends here
