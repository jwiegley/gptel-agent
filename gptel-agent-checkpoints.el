;;; gptel-agent-checkpoints.el --- Checkpoint and recovery for gptel-agent -*- lexical-binding: t; -*-

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

;; This module implements checkpoint and recovery functionality for gptel-agent.
;; Checkpoints capture the full conversation state enabling recovery from
;; interruptions during long-running tasks.
;;
;; Features:
;; - Automatic checkpoints after configurable number of tool calls
;; - Manual checkpoint command with optional description
;; - Full state serialization (messages, todos, pending tools, FSM state)
;; - Recovery interface with checkpoint browsing
;; - Auto-prompt for recovery on interrupted session detection
;; - Configurable retention policy
;;
;; Checkpoint contents:
;; - Conversation messages
;; - Todo list (gptel-agent--todos)
;; - Pending tool calls
;; - FSM state (gptel--fsm-last)
;; - Buffer-local variables
;;
;; Usage:
;;   (require 'gptel-agent-checkpoints)
;;   (gptel-agent-checkpoints-mode 1)  ; Enable auto-checkpoints
;;
;;   M-x gptel-agent-checkpoint        ; Create manual checkpoint
;;   M-x gptel-agent-recover           ; Recover from checkpoint
;;   M-x gptel-agent-list-checkpoints  ; Browse checkpoints

;;; Code:

(require 'cl-lib)
(require 'json)
(eval-when-compile (require 'subr-x))

(declare-function gptel-agent-session-load "gptel-agent-sessions")
(declare-function gptel-agent--ensure-database "gptel-agent-sessions")
(declare-function sqlite-execute "sqlite")
(declare-function sqlite-select "sqlite")
(declare-function tabulated-list-init-header "tabulated-list")
(declare-function tabulated-list-print "tabulated-list")

(defvar gptel-agent--session-db)
(defvar gptel-agent--current-session-id)
(defvar gptel--fsm-last)

(defgroup gptel-agent-checkpoints nil
  "Checkpoint and recovery for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-checkpoint-")

;;;; Customization Options

(defcustom gptel-agent-checkpoint-frequency 5
  "Number of tool calls between automatic checkpoints.

Set to nil to disable automatic checkpoints."
  :type '(choice (integer :tag "Tool calls")
                 (const :tag "Disabled" nil))
  :group 'gptel-agent-checkpoints)

(defcustom gptel-agent-checkpoint-on-multi-step t
  "Whether to create checkpoint before multi-step tasks."
  :type 'boolean
  :group 'gptel-agent-checkpoints)

(defcustom gptel-agent-checkpoint-retention 10
  "Maximum number of checkpoints to retain per session.

Oldest checkpoints beyond this count are automatically deleted."
  :type 'integer
  :group 'gptel-agent-checkpoints)

(defcustom gptel-agent-checkpoint-max-message-size 100000
  "Maximum size in characters for serialized messages.

Messages exceeding this are truncated in checkpoint."
  :type 'integer
  :group 'gptel-agent-checkpoints)

(defcustom gptel-agent-checkpoint-auto-recover t
  "Whether to prompt for recovery when interrupted session detected."
  :type 'boolean
  :group 'gptel-agent-checkpoints)

;;;; Internal State

(defvar-local gptel-agent--checkpoint-tool-count 0
  "Count of tool calls since last checkpoint.")

(defvar-local gptel-agent--session-interrupted nil
  "Non-nil when session is in-progress (not cleanly ended).")

(defvar-local gptel-agent--pending-tool-calls nil
  "List of pending tool calls.")

(defvar-local gptel-agent--todos nil
  "Buffer-local todo list.")

;;;; State Serialization

(defun gptel-agent--serialize-checkpoint-state ()
  "Serialize current session state for checkpoint.

Returns a plist suitable for JSON encoding."
  (list :timestamp (format-time-string "%Y-%m-%dT%H:%M:%S")
        :messages (gptel-agent--serialize-messages)
        :todos (gptel-agent--serialize-todos)
        :pending-tools (gptel-agent--serialize-pending-tools)
        :fsm-state (gptel-agent--serialize-fsm-state)
        :buffer-state (gptel-agent--serialize-buffer-state)))

(defun gptel-agent--serialize-messages ()
  "Serialize conversation messages with size limit."
  (let ((messages (gptel-agent--get-buffer-messages-for-checkpoint)))
    (when messages
      (let ((serialized (mapcar
                        (lambda (msg)
                          (let ((content (plist-get msg :content)))
                            ;; Truncate large messages
                            (when (and (stringp content)
                                      (> (length content)
                                         gptel-agent-checkpoint-max-message-size))
                              (setq content
                                    (concat
                                     (substring content 0
                                               (- gptel-agent-checkpoint-max-message-size 50))
                                     "\n[...truncated for checkpoint...]")))
                            (list :role (plist-get msg :role)
                                  :content content)))
                        messages)))
        serialized))))

(defun gptel-agent--get-buffer-messages-for-checkpoint ()
  "Get messages from current buffer for checkpoint.

Integration point with gptel conversation state."
  ;; This should be overridden/advised by gptel-agent to get actual messages
  ;; Default implementation returns nil
  nil)

(defun gptel-agent--serialize-todos ()
  "Serialize todo list."
  (when gptel-agent--todos
    (mapcar (lambda (todo)
              (list :content (plist-get todo :content)
                    :status (plist-get todo :status)))
            gptel-agent--todos)))

(defun gptel-agent--serialize-pending-tools ()
  "Serialize pending tool calls."
  (when gptel-agent--pending-tool-calls
    (mapcar (lambda (call)
              (list :tool (plist-get call :tool)
                    :args (plist-get call :args)
                    :id (plist-get call :id)))
            gptel-agent--pending-tool-calls)))

(defun gptel-agent--serialize-fsm-state ()
  "Serialize FSM state."
  (when (boundp 'gptel--fsm-last)
    gptel--fsm-last))

(defun gptel-agent--serialize-buffer-state ()
  "Serialize relevant buffer-local variables."
  (list :point (point)
        :buffer-name (buffer-name)
        :default-directory default-directory
        :tool-count gptel-agent--checkpoint-tool-count))

;;;; State Deserialization

(defun gptel-agent--deserialize-checkpoint-state (state)
  "Deserialize STATE plist and restore to current buffer."
  (when state
    ;; Restore messages (integration point)
    (when-let ((messages (plist-get state :messages)))
      (gptel-agent--restore-messages messages))

    ;; Restore todos
    (when-let ((todos (plist-get state :todos)))
      (setq gptel-agent--todos
            (mapcar (lambda (todo)
                      (list :content (plist-get todo :content)
                            :status (plist-get todo :status)))
                    todos)))

    ;; Restore pending tools
    (when-let ((tools (plist-get state :pending-tools)))
      (setq gptel-agent--pending-tool-calls tools))

    ;; Restore FSM state
    (when-let ((fsm (plist-get state :fsm-state)))
      (when (boundp 'gptel--fsm-last)
        (setq gptel--fsm-last fsm)))

    ;; Restore buffer state
    (when-let ((buf-state (plist-get state :buffer-state)))
      (setq gptel-agent--checkpoint-tool-count
            (or (plist-get buf-state :tool-count) 0)))))

(defun gptel-agent--restore-messages (_messages)
  "Restore MESSAGES to current buffer.

Integration point with gptel conversation state.
This should be overridden/advised by gptel-agent."
  ;; Default implementation does nothing
  nil)

;;;; SQLite Operations

(defun gptel-agent--checkpoint-save (session-id state &optional description)
  "Save checkpoint STATE for SESSION-ID with optional DESCRIPTION.

Returns the checkpoint ID."
  (gptel-agent--ensure-database)
  (let ((now (format-time-string "%Y-%m-%dT%H:%M:%S"))
        (state-json (json-encode state)))
    (sqlite-execute gptel-agent--session-db
                    "INSERT INTO checkpoints (session_id, checkpoint_data, created_at)
                     VALUES (?, ?, ?)"
                    (list session-id state-json now))
    ;; Get the inserted ID
    (let ((result (sqlite-select gptel-agent--session-db
                                "SELECT last_insert_rowid()")))
      (when result
        (caar result)))))

(defun gptel-agent--checkpoint-load (checkpoint-id)
  "Load checkpoint by CHECKPOINT-ID.

Returns a plist with :id, :session-id, :created-at, :state."
  (gptel-agent--ensure-database)
  (let ((rows (sqlite-select gptel-agent--session-db
                            "SELECT id, session_id, checkpoint_data, created_at
                             FROM checkpoints WHERE id = ?"
                            (list checkpoint-id))))
    (when (car rows)
      (let ((row (car rows)))
        (list :id (nth 0 row)
              :session-id (nth 1 row)
              :state (json-read-from-string (nth 2 row))
              :created-at (nth 3 row))))))

(defun gptel-agent--checkpoint-list (session-id)
  "List all checkpoints for SESSION-ID.

Returns list of plists with :id, :created-at."
  (gptel-agent--ensure-database)
  (let ((rows (sqlite-select gptel-agent--session-db
                            "SELECT id, created_at
                             FROM checkpoints
                             WHERE session_id = ?
                             ORDER BY created_at DESC"
                            (list session-id))))
    (mapcar (lambda (row)
              (list :id (nth 0 row)
                    :created-at (nth 1 row)))
            rows)))

(defun gptel-agent--checkpoint-delete (checkpoint-id)
  "Delete checkpoint by CHECKPOINT-ID."
  (gptel-agent--ensure-database)
  (sqlite-execute gptel-agent--session-db
                  "DELETE FROM checkpoints WHERE id = ?"
                  (list checkpoint-id)))

(defun gptel-agent--checkpoint-cleanup (session-id)
  "Clean up old checkpoints for SESSION-ID based on retention policy.

Keeps the `gptel-agent-checkpoint-retention' most recent checkpoints."
  (gptel-agent--ensure-database)
  (sqlite-execute gptel-agent--session-db
                  "DELETE FROM checkpoints WHERE id IN (
                     SELECT id FROM checkpoints
                     WHERE session_id = ?
                     ORDER BY created_at DESC
                     LIMIT -1 OFFSET ?
                   )"
                  (list session-id gptel-agent-checkpoint-retention)))

(defun gptel-agent--checkpoint-get-latest (session-id)
  "Get the most recent checkpoint for SESSION-ID."
  (gptel-agent--ensure-database)
  (let ((rows (sqlite-select gptel-agent--session-db
                            "SELECT id, checkpoint_data, created_at
                             FROM checkpoints
                             WHERE session_id = ?
                             ORDER BY created_at DESC
                             LIMIT 1"
                            (list session-id))))
    (when (car rows)
      (let ((row (car rows)))
        (list :id (nth 0 row)
              :state (json-read-from-string (nth 1 row))
              :created-at (nth 2 row))))))

;;;; Automatic Checkpoints

(defun gptel-agent--maybe-auto-checkpoint ()
  "Create automatic checkpoint if tool count threshold reached."
  (when gptel-agent-checkpoint-frequency
    (cl-incf gptel-agent--checkpoint-tool-count)
    (when (>= gptel-agent--checkpoint-tool-count
              gptel-agent-checkpoint-frequency)
      (condition-case err
          (progn
            (gptel-agent--create-checkpoint
             (format "Auto-checkpoint after %d tool calls"
                     gptel-agent--checkpoint-tool-count))
            (setq gptel-agent--checkpoint-tool-count 0))
        (error
         (message "gptel-agent: Auto-checkpoint failed: %s"
                  (error-message-string err)))))))

(defun gptel-agent--checkpoint-before-multi-step (description)
  "Create checkpoint before multi-step task with DESCRIPTION."
  (when gptel-agent-checkpoint-on-multi-step
    (condition-case err
        (gptel-agent--create-checkpoint
         (format "Before multi-step task: %s" description))
      (error
       (message "gptel-agent: Pre-task checkpoint failed: %s"
                (error-message-string err))))))

(defun gptel-agent--create-checkpoint (description)
  "Create a checkpoint with DESCRIPTION."
  (when gptel-agent--current-session-id
    (let ((state (gptel-agent--serialize-checkpoint-state)))
      (gptel-agent--checkpoint-save gptel-agent--current-session-id
                                   state
                                   description)
      (gptel-agent--checkpoint-cleanup gptel-agent--current-session-id))))

;;;; Manual Checkpoint Command

;;;###autoload
(defun gptel-agent-checkpoint (&optional description)
  "Create a checkpoint of current session state.

With prefix arg, prompt for DESCRIPTION.
Without prefix arg, create checkpoint with auto-generated description."
  (interactive
   (list (when current-prefix-arg
           (read-string "Checkpoint description: "))))
  (unless gptel-agent--current-session-id
    (user-error "No active gptel-agent session"))

  (let* ((desc (or description
                   (format "Manual checkpoint at %s"
                          (format-time-string "%H:%M:%S"))))
         (state (gptel-agent--serialize-checkpoint-state))
         (checkpoint-id (gptel-agent--checkpoint-save
                        gptel-agent--current-session-id
                        state
                        desc)))
    (gptel-agent--checkpoint-cleanup gptel-agent--current-session-id)

    (let ((count (length (gptel-agent--checkpoint-list
                         gptel-agent--current-session-id))))
      (message "Checkpoint created: #%d - %s (%d of %d max retained)"
               checkpoint-id desc count gptel-agent-checkpoint-retention))))

;;;; Recovery Interface

;;;###autoload
(defun gptel-agent-recover ()
  "Check for interrupted sessions and offer recovery.

Prompts user to restore from the most recent checkpoint
if an interrupted session is detected."
  (interactive)
  (when (and gptel-agent--current-session-id
             gptel-agent-checkpoint-auto-recover)
    (when-let ((checkpoint (gptel-agent--checkpoint-get-latest
                           gptel-agent--current-session-id)))
      (when (yes-or-no-p
             (format "Found checkpoint from %s. Restore? "
                    (plist-get checkpoint :created-at)))
        (gptel-agent-restore-checkpoint (plist-get checkpoint :id))))))

;;;###autoload
(defun gptel-agent-restore-checkpoint (checkpoint-id)
  "Restore session state from CHECKPOINT-ID."
  (interactive
   (let* ((checkpoints (if gptel-agent--current-session-id
                          (gptel-agent--checkpoint-list
                           gptel-agent--current-session-id)
                        nil))
          (choices (mapcar (lambda (cp)
                            (cons (format "#%d - %s"
                                         (plist-get cp :id)
                                         (plist-get cp :created-at))
                                  (plist-get cp :id)))
                          checkpoints)))
     (unless choices
       (user-error "No checkpoints found for current session"))
     (let ((choice (completing-read "Restore checkpoint: " choices nil t)))
       (list (cdr (assoc choice choices))))))

  (let ((checkpoint (gptel-agent--checkpoint-load checkpoint-id)))
    (unless checkpoint
      (user-error "Checkpoint not found: %s" checkpoint-id))

    (gptel-agent--deserialize-checkpoint-state (plist-get checkpoint :state))

    (message "Restored checkpoint #%d from %s"
             (plist-get checkpoint :id)
             (plist-get checkpoint :created-at))))

;;;; Checkpoint List Buffer Mode

(defvar gptel-agent-checkpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'gptel-agent-checkpoints-restore)
    (define-key map (kbd "r") #'gptel-agent-checkpoints-restore)
    (define-key map (kbd "d") #'gptel-agent-checkpoints-mark-delete)
    (define-key map (kbd "x") #'gptel-agent-checkpoints-execute)
    (define-key map (kbd "g") #'gptel-agent-checkpoints-refresh)
    map)
  "Keymap for `gptel-agent-checkpoints-mode'.")

(define-derived-mode gptel-agent-checkpoints-mode tabulated-list-mode
  "Checkpoints"
  "Major mode for browsing gptel-agent checkpoints.

\\{gptel-agent-checkpoints-mode-map}"
  (setq tabulated-list-format
        [("ID" 6 t :right-align t)
         ("Created" 20 t)
         ("Tool Count" 10 t :right-align t)
         ("Messages" 10 t :right-align t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Created" . t))
  (add-hook 'tabulated-list-revert-hook #'gptel-agent-checkpoints-refresh nil t)
  (tabulated-list-init-header))

(defvar-local gptel-agent-checkpoints--session-id nil
  "Session ID for checkpoint list buffer.")

(defun gptel-agent-checkpoints-refresh ()
  "Refresh the checkpoint list."
  (interactive)
  (when gptel-agent-checkpoints--session-id
    (let ((checkpoints (gptel-agent--checkpoint-list
                       gptel-agent-checkpoints--session-id)))
      (setq tabulated-list-entries
            (mapcar (lambda (cp)
                      (let* ((full (gptel-agent--checkpoint-load
                                   (plist-get cp :id)))
                             (state (plist-get full :state))
                             (buf-state (when state (plist-get state :buffer-state)))
                             (messages (when state (plist-get state :messages))))
                        (list (plist-get cp :id)
                              (vector
                               (number-to-string (plist-get cp :id))
                               (plist-get cp :created-at)
                               (number-to-string
                                (or (when buf-state (plist-get buf-state :tool-count)) 0))
                               (number-to-string
                                (if messages (length messages) 0))))))
                    checkpoints)))
    (tabulated-list-print t)))

(defun gptel-agent-checkpoints-restore ()
  "Restore the checkpoint at point."
  (interactive)
  (let ((checkpoint-id (tabulated-list-get-id)))
    (when checkpoint-id
      (gptel-agent-restore-checkpoint checkpoint-id)
      (quit-window))))

(defun gptel-agent-checkpoints-mark-delete ()
  "Mark checkpoint at point for deletion."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun gptel-agent-checkpoints-execute ()
  "Execute pending operations (deletions)."
  (interactive)
  (let ((deleted 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (let ((checkpoint-id (tabulated-list-get-id)))
            (when checkpoint-id
              (gptel-agent--checkpoint-delete checkpoint-id)
              (cl-incf deleted))))
        (forward-line 1)))
    (gptel-agent-checkpoints-refresh)
    (message "Deleted %d checkpoint(s)" deleted)))

;;;###autoload
(defun gptel-agent-list-checkpoints (&optional session-id)
  "List all checkpoints for SESSION-ID or current session.

Display in a buffer with timestamp, description, and restore option."
  (interactive)
  (let ((sid (or session-id gptel-agent--current-session-id)))
    (unless sid
      (user-error "No active session"))
    (let ((buf (get-buffer-create "*GPTel Agent Checkpoints*")))
      (with-current-buffer buf
        (gptel-agent-checkpoints-mode)
        (setq gptel-agent-checkpoints--session-id sid)
        (gptel-agent-checkpoints-refresh))
      (pop-to-buffer buf))))

;;;; Session Lifecycle Hooks

(defun gptel-agent-checkpoints--session-start ()
  "Hook called when session starts."
  (setq gptel-agent--session-interrupted t)
  (setq gptel-agent--checkpoint-tool-count 0)
  ;; Check for recovery
  (when gptel-agent-checkpoint-auto-recover
    (gptel-agent-recover)))

(defun gptel-agent-checkpoints--session-end ()
  "Hook called when session ends cleanly."
  (setq gptel-agent--session-interrupted nil))

;;;; Minor Mode

;;;###autoload
(define-minor-mode gptel-agent-checkpoints-mode
  "Minor mode for automatic checkpoints in gptel-agent buffers.

Enables automatic checkpoint creation based on tool call frequency."
  :lighter " Chk"
  :group 'gptel-agent-checkpoints
  (if gptel-agent-checkpoints-mode
      (progn
        (setq gptel-agent--checkpoint-tool-count 0))
    (setq gptel-agent--checkpoint-tool-count 0)))

;;;; Public API

(defun gptel-agent-checkpoint-count ()
  "Return number of checkpoints for current session."
  (if gptel-agent--current-session-id
      (length (gptel-agent--checkpoint-list gptel-agent--current-session-id))
    0))

(defun gptel-agent-checkpoint-tool-call ()
  "Record a tool call for checkpoint tracking.

Call this from tool execution to trigger auto-checkpoints."
  (when gptel-agent-checkpoints-mode
    (gptel-agent--maybe-auto-checkpoint)))

(provide 'gptel-agent-checkpoints)
;;; gptel-agent-checkpoints.el ends here
