;;; gptel-agent-sessions-test.el --- Tests for gptel-agent-sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Keywords: tests
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tests for gptel-agent-sessions.el SQLite session persistence.
;;
;; Test coverage:
;; - UUID generation
;; - Storage backend initialization
;; - Session CRUD operations (create, save, load, list, delete)
;; - JSON fallback storage
;; - Auto-save mechanism
;; - Retention policy
;; - Markdown export
;; - Session browser mode

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'gptel-agent-sessions)

;;;; Test Helpers

(defvar gptel-agent-sessions-test--temp-dir nil
  "Temporary directory for test files.")

(defun gptel-agent-sessions-test--setup ()
  "Set up test environment with temp directories."
  (setq gptel-agent-sessions-test--temp-dir
        (make-temp-file "gptel-agent-sessions-test-" t))
  ;; Override paths to use temp dir
  (setq gptel-agent-session-db-file
        (expand-file-name "test-sessions.db" gptel-agent-sessions-test--temp-dir))
  (setq gptel-agent-session-json-dir
        (expand-file-name "json-sessions/" gptel-agent-sessions-test--temp-dir))
  ;; Reset state
  (setq gptel-agent--session-db nil)
  (setq gptel-agent--storage-backend nil))

(defun gptel-agent-sessions-test--teardown ()
  "Clean up test environment."
  ;; Close database
  (when gptel-agent--session-db
    (ignore-errors (gptel-agent--close-database)))
  ;; Reset state
  (setq gptel-agent--session-db nil)
  (setq gptel-agent--storage-backend nil)
  ;; Remove temp dir
  (when (and gptel-agent-sessions-test--temp-dir
             (file-directory-p gptel-agent-sessions-test--temp-dir))
    (delete-directory gptel-agent-sessions-test--temp-dir t)))

(defmacro gptel-agent-sessions-test--with-temp-env (&rest body)
  "Execute BODY with temporary test environment."
  `(unwind-protect
       (progn
         (gptel-agent-sessions-test--setup)
         ,@body)
     (gptel-agent-sessions-test--teardown)))

;;;; UUID Generation Tests

(ert-deftest gptel-agent-sessions-test-uuid-generation ()
  "Test UUID generation produces valid format."
  (let ((uuid (gptel-agent--generate-uuid)))
    (should (stringp uuid))
    (should (= (length uuid) 36))
    (should (string-match-p
             "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$"
             uuid))))

(ert-deftest gptel-agent-sessions-test-uuid-uniqueness ()
  "Test UUID generation produces unique values."
  (let ((uuids (cl-loop repeat 100 collect (gptel-agent--generate-uuid))))
    (should (= (length uuids) (length (delete-dups (copy-sequence uuids)))))))

;;;; Customization Tests

(ert-deftest gptel-agent-sessions-test-default-db-file ()
  "Test default database file location."
  (should (stringp (default-value 'gptel-agent-session-db-file)))
  (should (string-match-p "gptel-agent" (default-value 'gptel-agent-session-db-file))))

(ert-deftest gptel-agent-sessions-test-default-auto-save ()
  "Test default auto-save interval."
  (should (numberp (default-value 'gptel-agent-session-auto-save-interval)))
  (should (> (default-value 'gptel-agent-session-auto-save-interval) 0)))

(ert-deftest gptel-agent-sessions-test-default-retention-days ()
  "Test default retention days."
  (should (numberp (default-value 'gptel-agent-session-retention-days)))
  (should (> (default-value 'gptel-agent-session-retention-days) 0)))

(ert-deftest gptel-agent-sessions-test-default-retention-count ()
  "Test default retention count."
  (should (numberp (default-value 'gptel-agent-session-retention-count)))
  (should (> (default-value 'gptel-agent-session-retention-count) 0)))

;;;; Storage Backend Tests

(ert-deftest gptel-agent-sessions-test-sqlite-available-check ()
  "Test SQLite availability check."
  (let ((result (gptel-agent--sqlite-available-p)))
    (should (memq result '(t nil)))))

(ert-deftest gptel-agent-sessions-test-init-storage-backend ()
  "Test storage backend initialization."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-storage)
    (should gptel-agent--storage-backend)
    (should (memq gptel-agent--storage-backend '(sqlite json)))))

(ert-deftest gptel-agent-sessions-test-ensure-db-directory ()
  "Test database directory creation."
  (gptel-agent-sessions-test--with-temp-env
    (let ((dir (file-name-directory gptel-agent-session-db-file)))
      (gptel-agent--ensure-db-directory)
      (should (file-directory-p dir)))))

;;;; JSON Backend Tests

(ert-deftest gptel-agent-sessions-test-json-ensure-dir ()
  "Test JSON directory creation."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--ensure-json-dir)
    (should (file-directory-p gptel-agent-session-json-dir))))

(ert-deftest gptel-agent-sessions-test-json-session-file-path ()
  "Test JSON session file path generation."
  (gptel-agent-sessions-test--with-temp-env
    (let* ((id "test-uuid-1234")
           (path (gptel-agent--json-session-file id)))
      (should (string-match-p "test-uuid-1234\\.json$" path))
      (should (string-prefix-p gptel-agent-session-json-dir path)))))

(ert-deftest gptel-agent-sessions-test-json-create ()
  "Test JSON session creation."
  (gptel-agent-sessions-test--with-temp-env
    (setq gptel-agent--storage-backend 'json)
    (let* ((id (gptel-agent--json-session-create "/test/project" "Test Session"))
           (file (gptel-agent--json-session-file id)))
      (should (stringp id))
      (should (file-exists-p file)))))

(ert-deftest gptel-agent-sessions-test-json-save-load ()
  "Test JSON session save and load."
  (gptel-agent-sessions-test--with-temp-env
    (setq gptel-agent--storage-backend 'json)
    (let* ((id (gptel-agent--json-session-create "/test/project" "Test"))
           (messages '((:role user :content "Hello" :tokens 5)
                       (:role assistant :content "Hi there" :tokens 8))))
      (gptel-agent--json-session-save id messages "gpt-4")
      (let ((loaded (gptel-agent--json-session-load id)))
        (should loaded)
        (should (string= (plist-get loaded :id) id))
        (should (string= (plist-get loaded :project-root) "/test/project"))))))

(ert-deftest gptel-agent-sessions-test-json-list ()
  "Test JSON session listing."
  (gptel-agent-sessions-test--with-temp-env
    (setq gptel-agent--storage-backend 'json)
    (gptel-agent--json-session-create "/project/a" "Session A")
    (gptel-agent--json-session-create "/project/b" "Session B")
    (let ((all-sessions (gptel-agent--json-session-list))
          (project-a (gptel-agent--json-session-list "/project/a")))
      (should (= (length all-sessions) 2))
      (should (= (length project-a) 1)))))

(ert-deftest gptel-agent-sessions-test-json-delete ()
  "Test JSON session deletion."
  (gptel-agent-sessions-test--with-temp-env
    (setq gptel-agent--storage-backend 'json)
    (let* ((id (gptel-agent--json-session-create "/test" "Test"))
           (file (gptel-agent--json-session-file id)))
      (should (file-exists-p file))
      (gptel-agent--json-session-delete id)
      (should-not (file-exists-p file)))))

;;;; SQLite Backend Tests (when available)

(ert-deftest gptel-agent-sessions-test-sqlite-init-database ()
  "Test SQLite database initialization."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-database)
    (should gptel-agent--session-db)
    (should (file-exists-p gptel-agent-session-db-file))))

(ert-deftest gptel-agent-sessions-test-sqlite-create ()
  "Test SQLite session creation."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-database)
    (setq gptel-agent--storage-backend 'sqlite)
    (let ((id (gptel-agent--sqlite-session-create "/test/project" "Test Session")))
      (should (stringp id))
      (should (> (length id) 0)))))

(ert-deftest gptel-agent-sessions-test-sqlite-save-load ()
  "Test SQLite session save and load."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-database)
    (setq gptel-agent--storage-backend 'sqlite)
    (let* ((id (gptel-agent--sqlite-session-create "/test/project" "Test"))
           (messages '((:role user :content "Hello" :tokens 5)
                       (:role assistant :content "Hi there" :tokens 8))))
      (gptel-agent--sqlite-session-save id messages "gpt-4")
      (let ((loaded (gptel-agent--sqlite-session-load id)))
        (should loaded)
        (should (string= (plist-get loaded :id) id))
        (should (= (length (plist-get loaded :messages)) 2))
        (should (= (plist-get loaded :token-count) 13))))))

(ert-deftest gptel-agent-sessions-test-sqlite-list ()
  "Test SQLite session listing."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-database)
    (setq gptel-agent--storage-backend 'sqlite)
    (gptel-agent--sqlite-session-create "/project/a" "Session A")
    (gptel-agent--sqlite-session-create "/project/b" "Session B")
    (let ((all-sessions (gptel-agent--sqlite-session-list))
          (project-a (gptel-agent--sqlite-session-list "/project/a")))
      (should (= (length all-sessions) 2))
      (should (= (length project-a) 1)))))

(ert-deftest gptel-agent-sessions-test-sqlite-delete ()
  "Test SQLite session deletion."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-database)
    (setq gptel-agent--storage-backend 'sqlite)
    (let ((id (gptel-agent--sqlite-session-create "/test" "Test")))
      (should (gptel-agent--sqlite-session-load id))
      (gptel-agent--sqlite-session-delete id)
      (should-not (gptel-agent--sqlite-session-load id)))))

(ert-deftest gptel-agent-sessions-test-sqlite-archive ()
  "Test SQLite session archiving."
  (skip-unless (gptel-agent--sqlite-available-p))
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-database)
    (setq gptel-agent--storage-backend 'sqlite)
    (let ((id (gptel-agent--sqlite-session-create "/test" "Test")))
      (gptel-agent--sqlite-session-set-archived id t)
      (let ((session (gptel-agent--sqlite-session-load id)))
        (should (plist-get session :is-archived)))
      (gptel-agent--sqlite-session-set-archived id nil)
      (let ((session (gptel-agent--sqlite-session-load id)))
        (should-not (plist-get session :is-archived))))))

;;;; Backend Dispatch Tests

(ert-deftest gptel-agent-sessions-test-dispatch-create ()
  "Test session create dispatch."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-storage)
    (let ((id (gptel-agent-session-create "/test" "Test")))
      (should (stringp id)))))

(ert-deftest gptel-agent-sessions-test-dispatch-save-load ()
  "Test session save/load dispatch."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-storage)
    (let* ((id (gptel-agent-session-create "/test" "Test"))
           (messages '((:role user :content "Test message"))))
      (gptel-agent-session-save id messages)
      (let ((loaded (gptel-agent-session-load id)))
        (should loaded)
        (should (string= (plist-get loaded :id) id))))))

(ert-deftest gptel-agent-sessions-test-dispatch-list ()
  "Test session list dispatch."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-storage)
    (gptel-agent-session-create "/test" "Test")
    (let ((sessions (gptel-agent-session-list)))
      (should (listp sessions))
      (should (>= (length sessions) 1)))))

(ert-deftest gptel-agent-sessions-test-dispatch-delete ()
  "Test session delete dispatch."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-storage)
    (let ((id (gptel-agent-session-create "/test" "Test")))
      (should (gptel-agent-session-load id))
      (gptel-agent-session-delete id)
      (should-not (gptel-agent-session-load id)))))

(ert-deftest gptel-agent-sessions-test-dispatch-uninitialized-error ()
  "Test error when storage not initialized."
  (gptel-agent-sessions-test--with-temp-env
    ;; Don't initialize storage
    (should-error (gptel-agent-session-create "/test" "Test"))))

;;;; Auto-Save Tests

(ert-deftest gptel-agent-sessions-test-dirty-flag ()
  "Test dirty flag operations."
  (with-temp-buffer
    (should-not gptel-agent--session-dirty)
    (gptel-agent--mark-dirty)
    (should gptel-agent--session-dirty)
    (gptel-agent--clear-dirty)
    (should-not gptel-agent--session-dirty)))

(ert-deftest gptel-agent-sessions-test-auto-save-timer-start ()
  "Test auto-save timer start."
  (with-temp-buffer
    (should-not gptel-agent--auto-save-timer)
    (gptel-agent--start-auto-save)
    (should gptel-agent--auto-save-timer)
    (gptel-agent--stop-auto-save)
    (should-not gptel-agent--auto-save-timer)))

(ert-deftest gptel-agent-sessions-test-auto-save-timer-stop ()
  "Test auto-save timer stop."
  (with-temp-buffer
    (gptel-agent--start-auto-save)
    (should gptel-agent--auto-save-timer)
    (gptel-agent--stop-auto-save)
    (should-not gptel-agent--auto-save-timer)))

(ert-deftest gptel-agent-sessions-test-auto-save-disabled ()
  "Test auto-save when disabled."
  (with-temp-buffer
    (let ((gptel-agent-session-auto-save-interval nil))
      (gptel-agent--start-auto-save)
      (should-not gptel-agent--auto-save-timer))))

;;;; Markdown Export Tests

(ert-deftest gptel-agent-sessions-test-export-markdown-format ()
  "Test markdown export format."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-storage)
    (let* ((id (gptel-agent-session-create "/test" "Test Export"))
           (messages '((:role user :content "Hello")
                       (:role assistant :content "Hi there"))))
      (gptel-agent-session-save id messages "gpt-4")
      (let ((markdown (gptel-agent-session-export-markdown id)))
        (should (stringp markdown))
        (should (string-match-p "# GPTel Agent Session" markdown))
        (should (string-match-p "Test Export" markdown))
        (should (string-match-p "## User" markdown))
        (should (string-match-p "## Assistant" markdown))
        (should (string-match-p "Hello" markdown))
        (should (string-match-p "Hi there" markdown))))))

(ert-deftest gptel-agent-sessions-test-export-markdown-to-file ()
  "Test markdown export to file."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-storage)
    (let* ((id (gptel-agent-session-create "/test" "Test"))
           (output-file (expand-file-name "export.md" gptel-agent-sessions-test--temp-dir))
           (messages '((:role user :content "Test"))))
      (gptel-agent-session-save id messages)
      (gptel-agent-session-export-markdown id output-file)
      (should (file-exists-p output-file))
      (with-temp-buffer
        (insert-file-contents output-file)
        (should (string-match-p "GPTel Agent Session" (buffer-string)))))))

(ert-deftest gptel-agent-sessions-test-export-nonexistent-error ()
  "Test export of nonexistent session."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent--init-storage)
    (should-error (gptel-agent-session-export-markdown "nonexistent-id"))))

;;;; Session Browser Mode Tests

(ert-deftest gptel-agent-sessions-test-mode-defined ()
  "Test sessions mode is defined."
  (should (fboundp 'gptel-agent-sessions-mode)))

(ert-deftest gptel-agent-sessions-test-mode-keymap ()
  "Test sessions mode keymap."
  (should (keymapp gptel-agent-sessions-mode-map))
  (should (lookup-key gptel-agent-sessions-mode-map (kbd "RET")))
  (should (lookup-key gptel-agent-sessions-mode-map (kbd "d")))
  (should (lookup-key gptel-agent-sessions-mode-map (kbd "x")))
  (should (lookup-key gptel-agent-sessions-mode-map (kbd "g"))))

(ert-deftest gptel-agent-sessions-test-sessions-command ()
  "Test gptel-agent-sessions command."
  (gptel-agent-sessions-test--with-temp-env
    (gptel-agent-sessions)
    (should (get-buffer "*GPTel Agent Sessions*"))
    (with-current-buffer "*GPTel Agent Sessions*"
      (should (derived-mode-p 'gptel-agent-sessions-mode)))
    (kill-buffer "*GPTel Agent Sessions*")))

;;;; Minor Mode Tests

(ert-deftest gptel-agent-sessions-test-minor-mode-defined ()
  "Test session minor mode is defined."
  (should (fboundp 'gptel-agent-session-mode)))

(ert-deftest gptel-agent-sessions-test-minor-mode-enable ()
  "Test session minor mode enable."
  (gptel-agent-sessions-test--with-temp-env
    (with-temp-buffer
      (gptel-agent-session-mode 1)
      (should gptel-agent-session-mode)
      (should gptel-agent--storage-backend)
      (gptel-agent-session-mode -1))))

(ert-deftest gptel-agent-sessions-test-minor-mode-disable ()
  "Test session minor mode disable."
  (gptel-agent-sessions-test--with-temp-env
    (with-temp-buffer
      (gptel-agent-session-mode 1)
      (gptel-agent-session-mode -1)
      (should-not gptel-agent-session-mode))))

(ert-deftest gptel-agent-sessions-test-minor-mode-lighter ()
  "Test session minor mode lighter."
  (gptel-agent-sessions-test--with-temp-env
    (with-temp-buffer
      (gptel-agent-session-mode 1)
      (should (string-match-p "Sess" (format-mode-line minor-mode-alist)))
      (gptel-agent-session-mode -1))))

;;;; Interactive Command Tests

(ert-deftest gptel-agent-sessions-test-cleanup-command ()
  "Test cleanup command exists."
  (should (fboundp 'gptel-agent-cleanup-sessions)))

(ert-deftest gptel-agent-sessions-test-resume-command ()
  "Test resume command exists."
  (should (fboundp 'gptel-agent-resume)))

(ert-deftest gptel-agent-sessions-test-status-command ()
  "Test session mode status command."
  (should (fboundp 'gptel-agent-mode-status)))

(provide 'gptel-agent-sessions-test)
;;; gptel-agent-sessions-test.el ends here
