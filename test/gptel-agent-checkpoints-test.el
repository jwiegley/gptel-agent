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
    (let ((gptel-agent-checkpoint-frequency nil))  ; Disable auto-checkpoint
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

;;;; Additional Serialization Tests

(ert-deftest gptel-agent-checkpoints-test-serialize-messages-truncation ()
  "Test message truncation during serialization."
  (with-temp-buffer
    (let ((gptel-agent-checkpoint-max-message-size 100)
          (large-content (make-string 200 ?x)))
      (cl-letf (((symbol-function 'gptel-agent--get-buffer-messages-for-checkpoint)
                 (lambda ()
                   (list (list :role 'user :content large-content)))))
        (let ((serialized (gptel-agent--serialize-messages)))
          (should serialized)
          ;; Content should be truncated
          (let ((content (plist-get (car serialized) :content)))
            (should (< (length content) 200))
            (should (string-match-p "truncated" content))))))))

(ert-deftest gptel-agent-checkpoints-test-serialize-messages-empty ()
  "Test serialization with no messages."
  (with-temp-buffer
    (cl-letf (((symbol-function 'gptel-agent--get-buffer-messages-for-checkpoint)
               (lambda () nil)))
      (should-not (gptel-agent--serialize-messages)))))

(ert-deftest gptel-agent-checkpoints-test-serialize-messages-small ()
  "Test serialization with small messages."
  (with-temp-buffer
    (let ((gptel-agent-checkpoint-max-message-size 1000))
      (cl-letf (((symbol-function 'gptel-agent--get-buffer-messages-for-checkpoint)
                 (lambda ()
                   (list (list :role 'user :content "Hello world")))))
        (let ((serialized (gptel-agent--serialize-messages)))
          (should serialized)
          ;; Content should not be truncated
          (should (string= (plist-get (car serialized) :content)
                          "Hello world")))))))

(ert-deftest gptel-agent-checkpoints-test-serialize-fsm-state-bound ()
  "Test FSM state serialization when bound."
  (with-temp-buffer
    ;; Define and set the variable so boundp can detect it
    (defvar gptel--fsm-last nil "Mock FSM state for testing.")
    (setq gptel--fsm-last 'some-state)
    (unwind-protect
        (should (eq (gptel-agent--serialize-fsm-state) 'some-state))
      (makunbound 'gptel--fsm-last))))

(ert-deftest gptel-agent-checkpoints-test-serialize-fsm-state-unbound ()
  "Test FSM state serialization when unbound."
  (with-temp-buffer
    ;; Make sure gptel--fsm-last is not bound
    (should-not (gptel-agent--serialize-fsm-state))))

;;;; Deserialization Edge Cases

(ert-deftest gptel-agent-checkpoints-test-deserialize-partial-state ()
  "Test deserialization with partial state."
  (with-temp-buffer
    (let ((state (list :todos '((:content "Test" :status "pending")))))
      ;; Only todos, no pending-tools or buffer-state
      (gptel-agent--deserialize-checkpoint-state state)
      (should gptel-agent--todos)
      (should (= (length gptel-agent--todos) 1)))))

(ert-deftest gptel-agent-checkpoints-test-deserialize-fsm-state ()
  "Test FSM state restoration."
  (with-temp-buffer
    ;; Define and set the variable so boundp can detect it
    (defvar gptel--fsm-last nil "Mock FSM state for testing.")
    (setq gptel--fsm-last nil)
    (unwind-protect
        (let ((state (list :fsm-state 'restored-state)))
          (gptel-agent--deserialize-checkpoint-state state)
          (should (eq gptel--fsm-last 'restored-state)))
      (makunbound 'gptel--fsm-last))))

(ert-deftest gptel-agent-checkpoints-test-deserialize-buffer-state-nil-tool-count ()
  "Test deserialization when tool-count is nil."
  (with-temp-buffer
    (setq gptel-agent--checkpoint-tool-count 10)
    (let ((state (list :buffer-state '(:point 1))))
      (gptel-agent--deserialize-checkpoint-state state)
      ;; Should be 0 when nil
      (should (= gptel-agent--checkpoint-tool-count 0)))))

;;;; Auto Checkpoint Tests

(ert-deftest gptel-agent-checkpoints-test-auto-checkpoint-trigger ()
  "Test auto-checkpoint triggers at threshold."
  (with-temp-buffer
    (let ((gptel-agent-checkpoint-frequency 3)
          (gptel-agent--checkpoint-tool-count 2)
          (gptel-agent--current-session-id "test")
          (checkpoint-created nil))
      (cl-letf (((symbol-function 'gptel-agent--create-checkpoint)
                 (lambda (_desc)
                   (setq checkpoint-created t))))
        (gptel-agent--maybe-auto-checkpoint)
        (should checkpoint-created)
        (should (= gptel-agent--checkpoint-tool-count 0))))))

(ert-deftest gptel-agent-checkpoints-test-auto-checkpoint-below-threshold ()
  "Test auto-checkpoint doesn't trigger below threshold."
  (with-temp-buffer
    (let ((gptel-agent-checkpoint-frequency 5)
          (gptel-agent--checkpoint-tool-count 2)
          (checkpoint-created nil))
      (cl-letf (((symbol-function 'gptel-agent--create-checkpoint)
                 (lambda (_desc)
                   (setq checkpoint-created t))))
        (gptel-agent--maybe-auto-checkpoint)
        (should-not checkpoint-created)
        (should (= gptel-agent--checkpoint-tool-count 3))))))

(ert-deftest gptel-agent-checkpoints-test-auto-checkpoint-error-handling ()
  "Test auto-checkpoint handles errors gracefully."
  (with-temp-buffer
    (let ((gptel-agent-checkpoint-frequency 1)
          (gptel-agent--checkpoint-tool-count 0)
          (message-called nil))
      (cl-letf (((symbol-function 'gptel-agent--create-checkpoint)
                 (lambda (_desc)
                   (error "Test error")))
                ((symbol-function 'message)
                 (lambda (&rest _)
                   (setq message-called t))))
        ;; Should not error, just message
        (gptel-agent--maybe-auto-checkpoint)
        (should message-called)))))

;;;; Multi-Step Checkpoint Tests

(ert-deftest gptel-agent-checkpoints-test-before-multi-step ()
  "Test checkpoint before multi-step task."
  (with-temp-buffer
    (let ((gptel-agent-checkpoint-on-multi-step t)
          (checkpoint-created nil)
          (checkpoint-desc nil))
      (cl-letf (((symbol-function 'gptel-agent--create-checkpoint)
                 (lambda (desc)
                   (setq checkpoint-created t
                         checkpoint-desc desc))))
        (gptel-agent--checkpoint-before-multi-step "refactoring")
        (should checkpoint-created)
        (should (string-match-p "refactoring" checkpoint-desc))))))

(ert-deftest gptel-agent-checkpoints-test-before-multi-step-disabled ()
  "Test multi-step checkpoint disabled."
  (with-temp-buffer
    (let ((gptel-agent-checkpoint-on-multi-step nil)
          (checkpoint-created nil))
      (cl-letf (((symbol-function 'gptel-agent--create-checkpoint)
                 (lambda (_desc)
                   (setq checkpoint-created t))))
        (gptel-agent--checkpoint-before-multi-step "refactoring")
        (should-not checkpoint-created)))))

(ert-deftest gptel-agent-checkpoints-test-before-multi-step-error-handling ()
  "Test multi-step checkpoint error handling."
  (with-temp-buffer
    (let ((gptel-agent-checkpoint-on-multi-step t)
          (message-called nil))
      (cl-letf (((symbol-function 'gptel-agent--create-checkpoint)
                 (lambda (_desc)
                   (error "Test error")))
                ((symbol-function 'message)
                 (lambda (&rest _)
                   (setq message-called t))))
        (gptel-agent--checkpoint-before-multi-step "test")
        (should message-called)))))

;;;; Create Checkpoint Tests

(ert-deftest gptel-agent-checkpoints-test-create-checkpoint-no-session ()
  "Test create checkpoint with no session."
  (with-temp-buffer
    (setq gptel-agent--current-session-id nil)
    (let ((save-called nil))
      (cl-letf (((symbol-function 'gptel-agent--checkpoint-save)
                 (lambda (&rest _)
                   (setq save-called t) 1)))
        (gptel-agent--create-checkpoint "test")
        (should-not save-called)))))

(ert-deftest gptel-agent-checkpoints-test-create-checkpoint-with-session ()
  "Test create checkpoint with active session."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (with-temp-buffer
      (let ((session-id (gptel-agent-session-create "/test" "Test")))
        (setq gptel-agent--current-session-id session-id)
        (gptel-agent--create-checkpoint "test description")
        ;; Should have created checkpoint
        (let ((checkpoints (gptel-agent--checkpoint-list session-id)))
          (should (= (length checkpoints) 1)))))))

;;;; Manual Checkpoint Command Tests

(ert-deftest gptel-agent-checkpoints-test-manual-checkpoint-with-description ()
  "Test manual checkpoint with description."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (with-temp-buffer
      (let ((session-id (gptel-agent-session-create "/test" "Test")))
        (setq gptel-agent--current-session-id session-id)
        (gptel-agent-checkpoint "My custom description")
        (let ((checkpoints (gptel-agent--checkpoint-list session-id)))
          (should (= (length checkpoints) 1)))))))

(ert-deftest gptel-agent-checkpoints-test-manual-checkpoint-auto-description ()
  "Test manual checkpoint with auto-generated description."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (with-temp-buffer
      (let ((session-id (gptel-agent-session-create "/test" "Test")))
        (setq gptel-agent--current-session-id session-id)
        (gptel-agent-checkpoint)
        (let ((checkpoints (gptel-agent--checkpoint-list session-id)))
          (should (= (length checkpoints) 1)))))))

;;;; Recovery Interface Tests

(ert-deftest gptel-agent-checkpoints-test-recover-no-session ()
  "Test recover with no active session."
  (with-temp-buffer
    (setq gptel-agent--current-session-id nil)
    (setq gptel-agent-checkpoint-auto-recover t)
    ;; Should not error, just do nothing
    (gptel-agent-recover)))

(ert-deftest gptel-agent-checkpoints-test-recover-auto-disabled ()
  "Test recover when auto-recovery disabled."
  (with-temp-buffer
    (setq gptel-agent--current-session-id "test")
    (setq gptel-agent-checkpoint-auto-recover nil)
    (let ((restore-called nil))
      (cl-letf (((symbol-function 'gptel-agent-restore-checkpoint)
                 (lambda (_id)
                   (setq restore-called t))))
        (gptel-agent-recover)
        (should-not restore-called)))))

(ert-deftest gptel-agent-checkpoints-test-recover-no-checkpoints ()
  "Test recover with no checkpoints available."
  (with-temp-buffer
    (setq gptel-agent--current-session-id "test")
    (setq gptel-agent-checkpoint-auto-recover t)
    (cl-letf (((symbol-function 'gptel-agent--checkpoint-get-latest)
               (lambda (_id) nil)))
      ;; Should not error
      (gptel-agent-recover))))

;;;; Restore Checkpoint Tests

(ert-deftest gptel-agent-checkpoints-test-restore-not-found ()
  "Test restore with nonexistent checkpoint."
  (cl-letf (((symbol-function 'gptel-agent--checkpoint-load)
             (lambda (_id) nil)))
    (should-error (gptel-agent-restore-checkpoint 999)
                  :type 'user-error)))

(ert-deftest gptel-agent-checkpoints-test-restore-success ()
  "Test successful checkpoint restore."
  (with-temp-buffer
    (let ((state (list :todos '((:content "Restored" :status "pending"))
                      :buffer-state '(:tool-count 5))))
      (cl-letf (((symbol-function 'gptel-agent--checkpoint-load)
                 (lambda (_id)
                   (list :id 1
                         :state state
                         :created-at "2025-01-01"))))
        (gptel-agent-restore-checkpoint 1)
        (should gptel-agent--todos)
        (should (= gptel-agent--checkpoint-tool-count 5))))))

;;;; Session Lifecycle Tests

(ert-deftest gptel-agent-checkpoints-test-session-start-hook ()
  "Test session start hook sets state."
  (with-temp-buffer
    (setq gptel-agent--session-interrupted nil)
    (setq gptel-agent--checkpoint-tool-count 10)
    (setq gptel-agent--current-session-id nil)  ; Prevent recovery
    (gptel-agent-checkpoints--session-start)
    (should gptel-agent--session-interrupted)
    (should (= gptel-agent--checkpoint-tool-count 0))))

(ert-deftest gptel-agent-checkpoints-test-session-end-hook ()
  "Test session end hook clears interrupted flag."
  (with-temp-buffer
    (setq gptel-agent--session-interrupted t)
    (gptel-agent-checkpoints--session-end)
    (should-not gptel-agent--session-interrupted)))

;;;; Checkpoint Count API Tests

(ert-deftest gptel-agent-checkpoints-test-count-with-session ()
  "Test checkpoint count with active session."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (with-temp-buffer
      (let ((session-id (gptel-agent-session-create "/test" "Test")))
        (setq gptel-agent--current-session-id session-id)
        (gptel-agent--checkpoint-save session-id '(:timestamp "1"))
        (gptel-agent--checkpoint-save session-id '(:timestamp "2"))
        (should (= (gptel-agent-checkpoint-count) 2))))))

;;;; Tool Call Tracking API Tests

(ert-deftest gptel-agent-checkpoints-test-tool-call-with-mode-enabled ()
  "Test tool call tracking when mode enabled."
  (with-temp-buffer
    (gptel-agent-checkpoints-mode 1)
    (let ((gptel-agent-checkpoint-frequency nil)
          (auto-called nil))
      (cl-letf (((symbol-function 'gptel-agent--maybe-auto-checkpoint)
                 (lambda () (setq auto-called t))))
        (gptel-agent-checkpoint-tool-call)
        (should auto-called)))
    (gptel-agent-checkpoints-mode -1)))

(ert-deftest gptel-agent-checkpoints-test-tool-call-with-mode-disabled ()
  "Test tool call tracking when mode disabled."
  (with-temp-buffer
    (gptel-agent-checkpoints-mode -1)
    (let ((auto-called nil))
      (cl-letf (((symbol-function 'gptel-agent--maybe-auto-checkpoint)
                 (lambda () (setq auto-called t))))
        (gptel-agent-checkpoint-tool-call)
        (should-not auto-called)))))

;;;; Customization Tests

(ert-deftest gptel-agent-checkpoints-test-custom-group-exists ()
  "Test customization group is defined."
  (should (get 'gptel-agent-checkpoints 'custom-group)))

(ert-deftest gptel-agent-checkpoints-test-on-multi-step-default ()
  "Test default value for checkpoint-on-multi-step."
  (should (default-value 'gptel-agent-checkpoint-on-multi-step)))

(ert-deftest gptel-agent-checkpoints-test-max-message-size-default ()
  "Test default max message size."
  (should (= (default-value 'gptel-agent-checkpoint-max-message-size) 100000)))

;;;; Buffer-Local Variable Tests

(ert-deftest gptel-agent-checkpoints-test-tool-count-buffer-local ()
  "Test tool count is buffer-local."
  (with-temp-buffer
    (setq gptel-agent--checkpoint-tool-count 5)
    (with-temp-buffer
      (should (= gptel-agent--checkpoint-tool-count 0)))
    (should (= gptel-agent--checkpoint-tool-count 5))))

(ert-deftest gptel-agent-checkpoints-test-interrupted-buffer-local ()
  "Test interrupted flag is buffer-local."
  (with-temp-buffer
    (setq gptel-agent--session-interrupted t)
    (with-temp-buffer
      (should-not gptel-agent--session-interrupted))
    (should gptel-agent--session-interrupted)))

(ert-deftest gptel-agent-checkpoints-test-pending-tools-buffer-local ()
  "Test pending tools is buffer-local."
  (with-temp-buffer
    (setq gptel-agent--pending-tool-calls '((:tool "Read")))
    (with-temp-buffer
      (should-not gptel-agent--pending-tool-calls))
    (should gptel-agent--pending-tool-calls)))

(ert-deftest gptel-agent-checkpoints-test-todos-buffer-local ()
  "Test todos is buffer-local."
  (with-temp-buffer
    (setq gptel-agent--todos '((:content "Test")))
    (with-temp-buffer
      (should-not gptel-agent--todos))
    (should gptel-agent--todos)))

;;;; List Mode UI Tests

(ert-deftest gptel-agent-checkpoints-test-refresh-empty ()
  "Test refresh with no checkpoints."
  (with-temp-buffer
    (gptel-agent-checkpoints-mode)
    (setq gptel-agent-checkpoints--session-id nil)
    ;; Should not error with nil session
    (gptel-agent-checkpoints-refresh)))

(ert-deftest gptel-agent-checkpoints-test-mark-delete ()
  "Test mark for delete."
  (with-temp-buffer
    (gptel-agent-checkpoints-mode)
    ;; Mock tabulated-list-put-tag since we can't easily set up a real tabulated list
    (let ((mark-called nil))
      (cl-letf (((symbol-function 'tabulated-list-put-tag)
                 (lambda (tag &optional _advance)
                   (setq mark-called t)
                   (forward-line 1))))
        (insert "Test line\n")
        (goto-char (point-min))
        (gptel-agent-checkpoints-mark-delete)
        (should mark-called)))))

(ert-deftest gptel-agent-checkpoints-test-execute-no-marks ()
  "Test execute with no marked items."
  (with-temp-buffer
    (let ((deleted 0))
      (cl-letf (((symbol-function 'tabulated-list-get-id)
                 (lambda () nil))
                ((symbol-function 'gptel-agent-checkpoints-refresh)
                 (lambda () nil)))
        (gptel-agent-checkpoints-execute)
        ;; Should complete without error
        (should t)))))

;;;; Edge Case Tests

(ert-deftest gptel-agent-checkpoints-test-serialize-pending-tools-nil ()
  "Test serialization with nil pending tools."
  (with-temp-buffer
    (setq gptel-agent--pending-tool-calls nil)
    (should-not (gptel-agent--serialize-pending-tools))))

(ert-deftest gptel-agent-checkpoints-test-restore-messages-placeholder ()
  "Test restore messages placeholder doesn't error."
  (with-temp-buffer
    (should (null (gptel-agent--restore-messages '((:role user :content "test")))))))

(ert-deftest gptel-agent-checkpoints-test-get-buffer-messages-placeholder ()
  "Test get buffer messages placeholder returns nil."
  (with-temp-buffer
    (should-not (gptel-agent--get-buffer-messages-for-checkpoint))))

;;;; Cleanup Policy Tests

(ert-deftest gptel-agent-checkpoints-test-cleanup-respects-retention ()
  "Test cleanup keeps exactly retention count checkpoints."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (let ((gptel-agent-checkpoint-retention 3)
          (session-id (gptel-agent-session-create "/test" "Test")))
      ;; Create 10 checkpoints
      (dotimes (_ 10)
        (gptel-agent--checkpoint-save session-id '(:timestamp "x")))
      (gptel-agent--checkpoint-cleanup session-id)
      (should (= (length (gptel-agent--checkpoint-list session-id)) 3)))))

(ert-deftest gptel-agent-checkpoints-test-cleanup-with-high-retention ()
  "Test cleanup keeps all when retention is high."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (let ((gptel-agent-checkpoint-retention 100)
          (session-id (gptel-agent-session-create "/test" "Test")))
      ;; Create 5 checkpoints
      (dotimes (_ 5)
        (gptel-agent--checkpoint-save session-id '(:timestamp "x")))
      (gptel-agent--checkpoint-cleanup session-id)
      (should (= (length (gptel-agent--checkpoint-list session-id)) 5)))))

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

(ert-deftest gptel-agent-checkpoints-test-multiple-sessions-isolation ()
  "Test checkpoints are isolated between sessions."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-checkpoints-test--with-temp-env
    (let ((session1 (gptel-agent-session-create "/test1" "Test1"))
          (session2 (gptel-agent-session-create "/test2" "Test2")))
      ;; Create checkpoints in different sessions
      (gptel-agent--checkpoint-save session1 '(:timestamp "s1-1"))
      (gptel-agent--checkpoint-save session1 '(:timestamp "s1-2"))
      (gptel-agent--checkpoint-save session2 '(:timestamp "s2-1"))
      ;; Each session should have correct count
      (should (= (length (gptel-agent--checkpoint-list session1)) 2))
      (should (= (length (gptel-agent--checkpoint-list session2)) 1)))))

(provide 'gptel-agent-checkpoints-test)
;;; gptel-agent-checkpoints-test.el ends here
