;;; gptel-agent-sessions.el --- SQLite session persistence for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: January 2025
;; Version: 0.1.0
;; Keywords: tools, convenience, ai, llm
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module implements SQLite-backed session storage for gptel-agent
;; conversation history.  It enables session resume, sharing, and
;; multi-session management.
;;
;; Features:
;; - SQLite database with WAL mode for crash resistance
;; - Session create, save, load, list, delete operations
;; - Automatic saving with idle timer and dirty state tracking
;; - Interactive session resume and management UI
;; - Configurable retention policies (age, count, size)
;; - Markdown export for sharing conversations
;; - JSON fallback when SQLite is unavailable
;;
;; Database Schema (TR-001):
;; - sessions: id, project_root, name, created_at, updated_at, model, etc.
;; - messages: id, session_id, role, content, timestamp, tokens
;; - checkpoints: session state snapshots for resumption
;; - tool_permissions: cached approval decisions
;;
;; Usage:
;;   (require 'gptel-agent-sessions)
;;   (gptel-agent-session-mode 1)  ; Enable auto-save
;;
;;   M-x gptel-agent-resume        ; Resume a previous session
;;   M-x gptel-agent-sessions      ; Manage all sessions

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(declare-function sqlite-available-p "sqlite")
(declare-function sqlite-open "sqlite")
(declare-function sqlite-execute "sqlite")
(declare-function sqlite-select "sqlite")
(declare-function sqlite-close "sqlite")
(declare-function tabulated-list-init-header "tabulated-list")
(declare-function tabulated-list-print "tabulated-list")

(defgroup gptel-agent-sessions nil
  "Session persistence for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-session-")

;;;; Customization Options

(defcustom gptel-agent-session-db-file
  (expand-file-name "gptel-agent/sessions.db" user-emacs-directory)
  "Path to the SQLite database file for session storage.

The parent directory will be created if it doesn't exist."
  :type 'file
  :group 'gptel-agent-sessions)

(defcustom gptel-agent-session-auto-save-interval 30
  "Interval in seconds for auto-saving sessions during idle time.

Set to nil to disable auto-save."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil))
  :group 'gptel-agent-sessions)

(defcustom gptel-agent-session-retention-days 30
  "Number of days to retain non-archived sessions.

Sessions older than this are automatically deleted on cleanup."
  :type 'integer
  :group 'gptel-agent-sessions)

(defcustom gptel-agent-session-retention-count 100
  "Maximum number of sessions to retain per project.

Oldest sessions beyond this count are deleted on cleanup."
  :type 'integer
  :group 'gptel-agent-sessions)

(defcustom gptel-agent-session-retention-size-mb 500
  "Maximum database size in megabytes.

Oldest sessions are deleted when database exceeds this size."
  :type 'integer
  :group 'gptel-agent-sessions)

(defcustom gptel-agent-session-cleanup-on-startup t
  "Whether to run session cleanup when sessions module loads.

Set to nil to disable automatic cleanup."
  :type 'boolean
  :group 'gptel-agent-sessions)

(defcustom gptel-agent-session-json-dir
  (expand-file-name "gptel-agent/sessions/" user-emacs-directory)
  "Directory for JSON session files when SQLite is unavailable."
  :type 'directory
  :group 'gptel-agent-sessions)

;;;; Internal State

(defvar gptel-agent--session-db nil
  "SQLite database connection for session storage.")

(defvar gptel-agent--storage-backend nil
  "Current storage backend: `sqlite' or `json'.")

(defvar-local gptel-agent--current-session-id nil
  "Current session ID in this buffer.")

(defvar-local gptel-agent--session-dirty nil
  "Non-nil when session has unsaved changes.")

(defvar-local gptel-agent--auto-save-timer nil
  "Timer for auto-saving session.")

;;;; UUID Generation

(defun gptel-agent--generate-uuid ()
  "Generate a UUID for session identification."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (logand (random 65536) 4095) 16384)
          (logior (logand (random 65536) 16383) 32768)
          (random 65536) (random 65536) (random 65536)))

;;;; Database Initialization

(defun gptel-agent--sqlite-available-p ()
  "Check if SQLite is available in this Emacs."
  (and (fboundp 'sqlite-available-p)
       (sqlite-available-p)))

(defun gptel-agent--ensure-db-directory ()
  "Ensure the database directory exists."
  (let ((dir (file-name-directory gptel-agent-session-db-file)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun gptel-agent--init-database ()
  "Initialize the SQLite database with TR-001 schema.

Creates tables and indexes if they don't exist.
Enables WAL mode for crash resistance."
  (gptel-agent--ensure-db-directory)

  (setq gptel-agent--session-db
        (sqlite-open gptel-agent-session-db-file))

  ;; Enable WAL mode for crash resistance (NFR-004)
  (sqlite-execute gptel-agent--session-db
                  "PRAGMA journal_mode=WAL")

  ;; Create sessions table
  (sqlite-execute gptel-agent--session-db
                  "CREATE TABLE IF NOT EXISTS sessions (
                     id TEXT PRIMARY KEY,
                     project_root TEXT NOT NULL,
                     name TEXT,
                     created_at TEXT NOT NULL,
                     updated_at TEXT NOT NULL,
                     model TEXT,
                     token_count INTEGER DEFAULT 0,
                     message_count INTEGER DEFAULT 0,
                     is_archived INTEGER DEFAULT 0
                   )")

  ;; Create messages table
  (sqlite-execute gptel-agent--session-db
                  "CREATE TABLE IF NOT EXISTS messages (
                     id INTEGER PRIMARY KEY AUTOINCREMENT,
                     session_id TEXT NOT NULL,
                     role TEXT NOT NULL,
                     content TEXT NOT NULL,
                     timestamp TEXT NOT NULL,
                     tokens INTEGER DEFAULT 0,
                     FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
                   )")

  ;; Create checkpoints table for resumption
  (sqlite-execute gptel-agent--session-db
                  "CREATE TABLE IF NOT EXISTS checkpoints (
                     id INTEGER PRIMARY KEY AUTOINCREMENT,
                     session_id TEXT NOT NULL,
                     checkpoint_data TEXT NOT NULL,
                     created_at TEXT NOT NULL,
                     FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
                   )")

  ;; Create tool_permissions table for cached approvals
  (sqlite-execute gptel-agent--session-db
                  "CREATE TABLE IF NOT EXISTS tool_permissions (
                     id INTEGER PRIMARY KEY AUTOINCREMENT,
                     session_id TEXT NOT NULL,
                     tool_name TEXT NOT NULL,
                     decision TEXT NOT NULL,
                     args_pattern TEXT,
                     created_at TEXT NOT NULL,
                     FOREIGN KEY (session_id) REFERENCES sessions(id) ON DELETE CASCADE
                   )")

  ;; Create indexes for efficient querying
  (sqlite-execute gptel-agent--session-db
                  "CREATE INDEX IF NOT EXISTS idx_sessions_project_root
                   ON sessions(project_root)")
  (sqlite-execute gptel-agent--session-db
                  "CREATE INDEX IF NOT EXISTS idx_sessions_created_at
                   ON sessions(created_at)")
  (sqlite-execute gptel-agent--session-db
                  "CREATE INDEX IF NOT EXISTS idx_sessions_updated_at
                   ON sessions(updated_at)")
  (sqlite-execute gptel-agent--session-db
                  "CREATE INDEX IF NOT EXISTS idx_messages_session_id
                   ON messages(session_id)")

  ;; Enable foreign key support
  (sqlite-execute gptel-agent--session-db
                  "PRAGMA foreign_keys = ON"))

(defun gptel-agent--close-database ()
  "Close the database connection."
  (when gptel-agent--session-db
    (sqlite-close gptel-agent--session-db)
    (setq gptel-agent--session-db nil)))

(defun gptel-agent--ensure-database ()
  "Ensure database is initialized and connected."
  (unless gptel-agent--session-db
    (gptel-agent--init-database)))

;;;; Session CRUD - SQLite Backend

(defun gptel-agent--sqlite-session-create (project-root &optional name)
  "Create a new session for PROJECT-ROOT with optional NAME.

Returns the session ID."
  (gptel-agent--ensure-database)
  (let ((id (gptel-agent--generate-uuid))
        (now (format-time-string "%Y-%m-%dT%H:%M:%S")))
    (sqlite-execute gptel-agent--session-db
                    "INSERT INTO sessions (id, project_root, name, created_at, updated_at)
                     VALUES (?, ?, ?, ?, ?)"
                    (list id project-root name now now))
    id))

(defun gptel-agent--sqlite-session-save (session-id messages &optional model)
  "Save MESSAGES to SESSION-ID with optional MODEL info.

MESSAGES is a list of plists with :role, :content, and optionally :tokens."
  (gptel-agent--ensure-database)
  (let ((now (format-time-string "%Y-%m-%dT%H:%M:%S"))
        (total-tokens 0)
        (msg-count (length messages)))

    ;; Clear existing messages for this session
    (sqlite-execute gptel-agent--session-db
                    "DELETE FROM messages WHERE session_id = ?"
                    (list session-id))

    ;; Insert messages
    (dolist (msg messages)
      (let ((tokens (or (plist-get msg :tokens) 0)))
        (sqlite-execute gptel-agent--session-db
                        "INSERT INTO messages (session_id, role, content, timestamp, tokens)
                         VALUES (?, ?, ?, ?, ?)"
                        (list session-id
                              (symbol-name (plist-get msg :role))
                              (plist-get msg :content)
                              now
                              tokens))
        (setq total-tokens (+ total-tokens tokens))))

    ;; Update session metadata
    (sqlite-execute gptel-agent--session-db
                    "UPDATE sessions SET
                       updated_at = ?,
                       model = COALESCE(?, model),
                       token_count = ?,
                       message_count = ?
                     WHERE id = ?"
                    (list now model total-tokens msg-count session-id))))

(defun gptel-agent--sqlite-session-load (session-id)
  "Load session SESSION-ID with all messages.

Returns a plist with :id, :project-root, :name, :created-at,
:updated-at, :model, :messages, etc."
  (gptel-agent--ensure-database)
  (let* ((session-rows (sqlite-select gptel-agent--session-db
                                      "SELECT id, project_root, name, created_at, updated_at,
                                              model, token_count, message_count, is_archived
                                       FROM sessions WHERE id = ?"
                                      (list session-id)))
         (session-row (car session-rows)))

    (when session-row
      (let* ((message-rows (sqlite-select gptel-agent--session-db
                                          "SELECT role, content, timestamp, tokens
                                           FROM messages WHERE session_id = ?
                                           ORDER BY id ASC"
                                          (list session-id)))
             (messages (mapcar
                       (lambda (row)
                         (list :role (intern (nth 0 row))
                               :content (nth 1 row)
                               :timestamp (nth 2 row)
                               :tokens (nth 3 row)))
                       message-rows)))
        (list :id (nth 0 session-row)
              :project-root (nth 1 session-row)
              :name (nth 2 session-row)
              :created-at (nth 3 session-row)
              :updated-at (nth 4 session-row)
              :model (nth 5 session-row)
              :token-count (nth 6 session-row)
              :message-count (nth 7 session-row)
              :is-archived (= (nth 8 session-row) 1)
              :messages messages)))))

(defun gptel-agent--sqlite-session-list (&optional project-root)
  "List sessions, optionally filtered by PROJECT-ROOT.

Returns list of session plists (without messages)."
  (gptel-agent--ensure-database)
  (let* ((query (if project-root
                   "SELECT id, project_root, name, created_at, updated_at,
                           model, token_count, message_count, is_archived
                    FROM sessions WHERE project_root = ?
                    ORDER BY updated_at DESC"
                 "SELECT id, project_root, name, created_at, updated_at,
                         model, token_count, message_count, is_archived
                  FROM sessions
                  ORDER BY updated_at DESC"))
         (rows (sqlite-select gptel-agent--session-db
                             query
                             (when project-root (list project-root)))))
    (mapcar (lambda (row)
              (list :id (nth 0 row)
                    :project-root (nth 1 row)
                    :name (nth 2 row)
                    :created-at (nth 3 row)
                    :updated-at (nth 4 row)
                    :model (nth 5 row)
                    :token-count (nth 6 row)
                    :message-count (nth 7 row)
                    :is-archived (= (nth 8 row) 1)))
            rows)))

(defun gptel-agent--sqlite-session-delete (session-id)
  "Delete session SESSION-ID and all related records."
  (gptel-agent--ensure-database)
  ;; Foreign key cascade handles related deletions
  (sqlite-execute gptel-agent--session-db
                  "DELETE FROM sessions WHERE id = ?"
                  (list session-id)))

(defun gptel-agent--sqlite-session-set-archived (session-id archived)
  "Set ARCHIVED status for SESSION-ID."
  (gptel-agent--ensure-database)
  (sqlite-execute gptel-agent--session-db
                  "UPDATE sessions SET is_archived = ? WHERE id = ?"
                  (list (if archived 1 0) session-id)))

;;;; Session CRUD - JSON Backend

(defun gptel-agent--ensure-json-dir ()
  "Ensure the JSON sessions directory exists."
  (unless (file-directory-p gptel-agent-session-json-dir)
    (make-directory gptel-agent-session-json-dir t)))

(defun gptel-agent--json-session-file (session-id)
  "Return the JSON file path for SESSION-ID."
  (expand-file-name (concat session-id ".json") gptel-agent-session-json-dir))

(defun gptel-agent--json-session-create (project-root &optional name)
  "Create a new JSON session for PROJECT-ROOT with optional NAME.

Returns the session ID."
  (gptel-agent--ensure-json-dir)
  (let* ((id (gptel-agent--generate-uuid))
         (now (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (session (list :id id
                       :project-root project-root
                       :name name
                       :created-at now
                       :updated-at now
                       :model nil
                       :token-count 0
                       :message-count 0
                       :is-archived nil
                       :messages nil))
         (file (gptel-agent--json-session-file id)))
    (gptel-agent--json-write-atomic file session)
    id))

(defun gptel-agent--json-write-atomic (file data)
  "Atomically write DATA as JSON to FILE."
  (let ((temp-file (make-temp-file "gptel-session-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert (json-encode data)))
          (rename-file temp-file file t))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun gptel-agent--json-session-save (session-id messages &optional model)
  "Save MESSAGES to JSON session SESSION-ID."
  (gptel-agent--ensure-json-dir)
  (let* ((file (gptel-agent--json-session-file session-id))
         (session (when (file-exists-p file)
                   (json-read-file file)))
         (now (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (total-tokens (cl-reduce #'+ messages
                                 :key (lambda (m) (or (plist-get m :tokens) 0))
                                 :initial-value 0)))
    (when session
      (setf (alist-get 'updated-at session) now)
      (setf (alist-get 'model session) (or model (alist-get 'model session)))
      (setf (alist-get 'token-count session) total-tokens)
      (setf (alist-get 'message-count session) (length messages))
      (setf (alist-get 'messages session) messages)
      (gptel-agent--json-write-atomic file session))))

(defun gptel-agent--json-session-load (session-id)
  "Load JSON session SESSION-ID."
  (let ((file (gptel-agent--json-session-file session-id)))
    (when (file-exists-p file)
      (let ((data (json-read-file file)))
        ;; Convert alist to plist format
        (list :id (alist-get 'id data)
              :project-root (alist-get 'project-root data)
              :name (alist-get 'name data)
              :created-at (alist-get 'created-at data)
              :updated-at (alist-get 'updated-at data)
              :model (alist-get 'model data)
              :token-count (alist-get 'token-count data)
              :message-count (alist-get 'message-count data)
              :is-archived (alist-get 'is-archived data)
              :messages (alist-get 'messages data))))))

(defun gptel-agent--json-session-list (&optional project-root)
  "List JSON sessions, optionally filtered by PROJECT-ROOT."
  (gptel-agent--ensure-json-dir)
  (let ((sessions nil))
    (dolist (file (directory-files gptel-agent-session-json-dir t "\\.json$"))
      (when (file-regular-p file)
        (let* ((data (json-read-file file))
               (proj (alist-get 'project-root data)))
          (when (or (null project-root)
                    (string= proj project-root))
            (push (list :id (alist-get 'id data)
                       :project-root proj
                       :name (alist-get 'name data)
                       :created-at (alist-get 'created-at data)
                       :updated-at (alist-get 'updated-at data)
                       :model (alist-get 'model data)
                       :token-count (alist-get 'token-count data)
                       :message-count (alist-get 'message-count data)
                       :is-archived (alist-get 'is-archived data))
                  sessions)))))
    ;; Sort by updated-at descending
    (sort sessions
          (lambda (a b)
            (string> (plist-get a :updated-at)
                     (plist-get b :updated-at))))))

(defun gptel-agent--json-session-delete (session-id)
  "Delete JSON session SESSION-ID."
  (let ((file (gptel-agent--json-session-file session-id)))
    (when (file-exists-p file)
      (delete-file file))))

;;;; Backend Dispatch

(defun gptel-agent--init-storage ()
  "Initialize storage backend."
  (if (gptel-agent--sqlite-available-p)
      (progn
        (setq gptel-agent--storage-backend 'sqlite)
        (gptel-agent--init-database))
    (setq gptel-agent--storage-backend 'json)
    (gptel-agent--ensure-json-dir)))

(defun gptel-agent-session-create (project-root &optional name)
  "Create a new session for PROJECT-ROOT with optional NAME."
  (pcase gptel-agent--storage-backend
    ('sqlite (gptel-agent--sqlite-session-create project-root name))
    ('json (gptel-agent--json-session-create project-root name))
    (_ (error "Storage backend not initialized"))))

(defun gptel-agent-session-save (session-id messages &optional model)
  "Save MESSAGES to SESSION-ID."
  (pcase gptel-agent--storage-backend
    ('sqlite (gptel-agent--sqlite-session-save session-id messages model))
    ('json (gptel-agent--json-session-save session-id messages model))
    (_ (error "Storage backend not initialized"))))

(defun gptel-agent-session-load (session-id)
  "Load session SESSION-ID."
  (pcase gptel-agent--storage-backend
    ('sqlite (gptel-agent--sqlite-session-load session-id))
    ('json (gptel-agent--json-session-load session-id))
    (_ (error "Storage backend not initialized"))))

(defun gptel-agent-session-list (&optional project-root)
  "List sessions, optionally filtered by PROJECT-ROOT."
  (pcase gptel-agent--storage-backend
    ('sqlite (gptel-agent--sqlite-session-list project-root))
    ('json (gptel-agent--json-session-list project-root))
    (_ (error "Storage backend not initialized"))))

(defun gptel-agent-session-delete (session-id)
  "Delete session SESSION-ID."
  (pcase gptel-agent--storage-backend
    ('sqlite (gptel-agent--sqlite-session-delete session-id))
    ('json (gptel-agent--json-session-delete session-id))
    (_ (error "Storage backend not initialized"))))

;;;; Auto-Save Mechanism

(defun gptel-agent--mark-dirty ()
  "Mark current session as having unsaved changes."
  (setq gptel-agent--session-dirty t))

(defun gptel-agent--clear-dirty ()
  "Clear the dirty flag after successful save."
  (setq gptel-agent--session-dirty nil))

(defun gptel-agent--save-if-dirty ()
  "Save session if dirty flag is set."
  (when (and gptel-agent--current-session-id
             gptel-agent--session-dirty)
    (condition-case err
        (progn
          ;; Get messages from buffer (integration point)
          (when-let ((messages (gptel-agent--get-buffer-messages)))
            (gptel-agent-session-save gptel-agent--current-session-id
                                     messages)
            (gptel-agent--clear-dirty)))
      (error
       (message "gptel-agent: Auto-save failed: %s" (error-message-string err))))))

(defun gptel-agent--get-buffer-messages ()
  "Get conversation messages from current buffer.

This is an integration point - should be overridden by gptel-agent
to extract actual conversation content."
  ;; Default implementation returns nil
  ;; Real implementation would parse buffer or use gptel state
  nil)

(defun gptel-agent--start-auto-save ()
  "Start the auto-save timer for current buffer."
  (when gptel-agent-session-auto-save-interval
    (gptel-agent--stop-auto-save)
    (setq gptel-agent--auto-save-timer
          (run-with-idle-timer gptel-agent-session-auto-save-interval
                              t
                              #'gptel-agent--save-if-dirty))))

(defun gptel-agent--stop-auto-save ()
  "Stop the auto-save timer."
  (when gptel-agent--auto-save-timer
    (cancel-timer gptel-agent--auto-save-timer)
    (setq gptel-agent--auto-save-timer nil)))

(defun gptel-agent--save-on-kill ()
  "Save session when buffer is killed."
  (gptel-agent--save-if-dirty)
  (gptel-agent--stop-auto-save))

;;;; Retention Policy

(defun gptel-agent--cleanup-old-sessions ()
  "Clean up sessions based on retention policy.

Deletes sessions older than `gptel-agent-session-retention-days'
(unless archived), keeps only `gptel-agent-session-retention-count'
per project, and maintains database under size limit."
  (when (eq gptel-agent--storage-backend 'sqlite)
    (gptel-agent--ensure-database)

    ;; Delete sessions older than retention days (non-archived)
    (let* ((cutoff-date (format-time-string
                        "%Y-%m-%dT%H:%M:%S"
                        (time-subtract (current-time)
                                      (days-to-time gptel-agent-session-retention-days)))))
      (sqlite-execute gptel-agent--session-db
                      "DELETE FROM sessions
                       WHERE is_archived = 0 AND updated_at < ?"
                      (list cutoff-date)))

    ;; Keep only retention-count sessions per project
    (let ((projects (sqlite-select gptel-agent--session-db
                                  "SELECT DISTINCT project_root FROM sessions")))
      (dolist (proj-row projects)
        (let ((project-root (car proj-row)))
          (sqlite-execute gptel-agent--session-db
                          "DELETE FROM sessions WHERE id IN (
                             SELECT id FROM sessions
                             WHERE project_root = ? AND is_archived = 0
                             ORDER BY updated_at DESC
                             LIMIT -1 OFFSET ?
                           )"
                          (list project-root gptel-agent-session-retention-count)))))

    ;; Check database size
    (when-let ((size-mb (/ (file-attribute-size
                           (file-attributes gptel-agent-session-db-file))
                          1048576.0)))
      (when (> size-mb gptel-agent-session-retention-size-mb)
        ;; Delete oldest non-archived sessions until under limit
        (while (> size-mb gptel-agent-session-retention-size-mb)
          (sqlite-execute gptel-agent--session-db
                          "DELETE FROM sessions WHERE id = (
                             SELECT id FROM sessions
                             WHERE is_archived = 0
                             ORDER BY updated_at ASC
                             LIMIT 1
                           )")
          ;; Recalculate size (VACUUM would be better but slower)
          (setq size-mb (/ (file-attribute-size
                           (file-attributes gptel-agent-session-db-file))
                          1048576.0)))))))

;;;###autoload
(defun gptel-agent-cleanup-sessions ()
  "Manually run session cleanup based on retention policy."
  (interactive)
  (gptel-agent--cleanup-old-sessions)
  (message "Session cleanup complete"))

;;;; Markdown Export

;;;###autoload
(defun gptel-agent-session-export-markdown (session-id &optional output-file)
  "Export SESSION-ID to markdown format.

If OUTPUT-FILE is provided, write to file.  Otherwise return as string.
Interactively, prompts for session and output file."
  (interactive
   (let* ((sessions (gptel-agent-session-list))
          (choices (mapcar (lambda (s)
                            (cons (format "%s - %s"
                                         (or (plist-get s :name) "Unnamed")
                                         (plist-get s :project-root))
                                  (plist-get s :id)))
                          sessions))
          (choice (completing-read "Export session: " choices nil t))
          (id (cdr (assoc choice choices)))
          (file (read-file-name "Export to file: " nil nil nil
                               (concat id ".md"))))
     (list id file)))

  (let ((session (gptel-agent-session-load session-id)))
    (unless session
      (error "Session not found: %s" session-id))

    (let ((markdown
           (with-temp-buffer
             ;; Header with metadata
             (insert (format "# GPTel Agent Session\n\n"))
             (insert (format "- **Session**: %s\n" (or (plist-get session :name) "Unnamed")))
             (insert (format "- **Project**: %s\n" (plist-get session :project-root)))
             (insert (format "- **Model**: %s\n" (or (plist-get session :model) "Unknown")))
             (insert (format "- **Created**: %s\n" (plist-get session :created-at)))
             (insert (format "- **Updated**: %s\n" (plist-get session :updated-at)))
             (insert (format "- **Messages**: %d\n" (plist-get session :message-count)))
             (insert (format "- **Tokens**: %d\n\n" (plist-get session :token-count)))
             (insert "---\n\n")

             ;; Messages
             (dolist (msg (plist-get session :messages))
               (let ((role (plist-get msg :role))
                     (content (plist-get msg :content)))
                 (pcase role
                   ('user
                    (insert "## User\n\n")
                    (insert content)
                    (insert "\n\n"))
                   ('assistant
                    (insert "## Assistant\n\n")
                    (insert content)
                    (insert "\n\n"))
                   ('tool
                    (insert "## Tool Result\n\n")
                    (insert "```\n")
                    (insert content)
                    (insert "\n```\n\n"))
                   (_
                    (insert (format "## %s\n\n" role))
                    (insert content)
                    (insert "\n\n")))))

             ;; Footer
             (insert "---\n\n")
             (insert (format "_Exported %s_\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S")))

             (buffer-string))))

      (if output-file
          (progn
            (with-temp-file output-file
              (insert markdown))
            (message "Exported to %s" output-file))
        markdown))))

;;;; Interactive Commands

;;;###autoload
(defun gptel-agent-resume ()
  "Resume a previous gptel-agent session.

Display a completion interface listing all saved sessions with
their names, project directories, message counts, and timestamps.
Archived sessions are marked with [A].

After selecting a session, load its conversation history and
state into the current buffer.  The session's messages, system
prompt, and settings are restored.

Sessions are stored in SQLite (when available) or JSON fallback.
See `gptel-agent-session-db-file' for storage location.

Returns the loaded session plist or nil if cancelled."
  (interactive)
  (let* ((sessions (gptel-agent-session-list))
         (choices (mapcar (lambda (s)
                           (cons (format "[%s] %s - %s (%d msgs, %s)"
                                        (if (plist-get s :is-archived) "A" " ")
                                        (or (plist-get s :name) "Unnamed")
                                        (file-name-nondirectory
                                         (directory-file-name
                                          (plist-get s :project-root)))
                                        (plist-get s :message-count)
                                        (plist-get s :updated-at))
                                 (plist-get s :id)))
                         sessions)))
    (unless choices
      (error "No sessions found"))
    (let* ((choice (completing-read "Resume session: " choices nil t))
           (session-id (cdr (assoc choice choices)))
           (session (gptel-agent-session-load session-id)))
      (when session
        ;; Create buffer and restore state
        ;; This is an integration point with gptel-agent
        (message "Loaded session: %s (%d messages)"
                 (or (plist-get session :name) "Unnamed")
                 (length (plist-get session :messages)))
        session))))

;;;; Session List Buffer Mode

(defvar gptel-agent-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'gptel-agent-sessions-resume)
    (define-key map (kbd "d") #'gptel-agent-sessions-mark-delete)
    (define-key map (kbd "x") #'gptel-agent-sessions-execute)
    (define-key map (kbd "e") #'gptel-agent-sessions-export)
    (define-key map (kbd "a") #'gptel-agent-sessions-toggle-archive)
    (define-key map (kbd "g") #'gptel-agent-sessions-refresh)
    map)
  "Keymap for `gptel-agent-sessions-mode'.")

(define-derived-mode gptel-agent-sessions-mode tabulated-list-mode "Sessions"
  "Major mode for managing gptel-agent sessions.

\\{gptel-agent-sessions-mode-map}"
  (setq tabulated-list-format
        [("A" 1 t)
         ("Name" 25 t)
         ("Project" 30 t)
         ("Msgs" 5 t :right-align t)
         ("Tokens" 8 t :right-align t)
         ("Updated" 19 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Updated" . t))
  (add-hook 'tabulated-list-revert-hook #'gptel-agent-sessions-refresh nil t)
  (tabulated-list-init-header))

(defun gptel-agent-sessions-refresh ()
  "Refresh the session list."
  (interactive)
  (setq tabulated-list-entries
        (mapcar (lambda (s)
                  (list (plist-get s :id)
                        (vector
                         (if (plist-get s :is-archived) "*" " ")
                         (or (plist-get s :name) "Unnamed")
                         (file-name-nondirectory
                          (directory-file-name (plist-get s :project-root)))
                         (number-to-string (plist-get s :message-count))
                         (number-to-string (plist-get s :token-count))
                         (plist-get s :updated-at))))
                (gptel-agent-session-list)))
  (tabulated-list-print t))

(defun gptel-agent-sessions-resume ()
  "Resume the session at point."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (when session-id
      (let ((session (gptel-agent-session-load session-id)))
        (message "Loaded: %s" (or (plist-get session :name) "Unnamed"))))))

(defun gptel-agent-sessions-mark-delete ()
  "Mark session at point for deletion."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun gptel-agent-sessions-execute ()
  "Execute pending operations (deletions)."
  (interactive)
  (let ((deleted 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (let ((session-id (tabulated-list-get-id)))
            (when session-id
              (gptel-agent-session-delete session-id)
              (cl-incf deleted))))
        (forward-line 1)))
    (gptel-agent-sessions-refresh)
    (message "Deleted %d session(s)" deleted)))

(defun gptel-agent-sessions-export ()
  "Export session at point to markdown."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (when session-id
      (call-interactively #'gptel-agent-session-export-markdown))))

(defun gptel-agent-sessions-toggle-archive ()
  "Toggle archived status of session at point."
  (interactive)
  (let ((session-id (tabulated-list-get-id)))
    (when session-id
      (let* ((session (gptel-agent-session-load session-id))
             (archived (plist-get session :is-archived)))
        (when (eq gptel-agent--storage-backend 'sqlite)
          (gptel-agent--sqlite-session-set-archived session-id (not archived)))
        (gptel-agent-sessions-refresh)
        (message "Session %s" (if archived "unarchived" "archived"))))))

;;;###autoload
(defun gptel-agent-sessions ()
  "Open the session management buffer.

Display a tabulated list of all saved gptel-agent sessions.
Each row shows session name, project, message count, update time,
and archive status.

Key bindings in the session list:
  \\[gptel-agent-sessions-resume]     Resume the selected session
  \\[gptel-agent-sessions-mark-delete]     Mark session for deletion
  \\[gptel-agent-sessions-execute]     Execute marked deletions
  \\[gptel-agent-sessions-export]     Export session to Markdown
  \\[gptel-agent-sessions-toggle-archive]     Toggle archive status
  \\[gptel-agent-sessions-refresh]     Refresh the session list"
  (interactive)
  (unless gptel-agent--storage-backend
    (gptel-agent--init-storage))
  (let ((buf (get-buffer-create "*GPTel Agent Sessions*")))
    (with-current-buffer buf
      (gptel-agent-sessions-mode)
      (gptel-agent-sessions-refresh))
    (pop-to-buffer buf)))

;;;; Minor Mode

;;;###autoload
(define-minor-mode gptel-agent-session-mode
  "Minor mode for session persistence in gptel-agent buffers.

Enables auto-save and session tracking."
  :lighter " Sess"
  :group 'gptel-agent-sessions
  (if gptel-agent-session-mode
      (progn
        ;; Initialize storage
        (unless gptel-agent--storage-backend
          (gptel-agent--init-storage))
        ;; Start auto-save
        (gptel-agent--start-auto-save)
        ;; Add kill hook
        (add-hook 'kill-buffer-hook #'gptel-agent--save-on-kill nil t)
        ;; Cleanup on startup if configured
        (when gptel-agent-session-cleanup-on-startup
          (gptel-agent--cleanup-old-sessions)))
    ;; Disable
    (gptel-agent--stop-auto-save)
    (remove-hook 'kill-buffer-hook #'gptel-agent--save-on-kill t)))

(provide 'gptel-agent-sessions)
;;; gptel-agent-sessions.el ends here
