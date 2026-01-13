;;; gptel-agent-multi.el --- Multi-session support for gptel-agent -*- lexical-binding: t; -*-

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

;; This module implements multi-session management for gptel-agent.
;; It enables switching between multiple active agent sessions, tracks
;; session activity, manages session resources, and prevents resource
;; exhaustion.
;;
;; Features:
;; - Buffer-local session metadata tracking
;; - Session switching with completion interface
;; - Session activity monitoring and idle detection
;; - Configurable session limits with warnings
;; - Automatic idle session cleanup
;; - Session renaming with buffer name updates
;;
;; Usage:
;;   (require 'gptel-agent-multi)
;;
;;   M-x gptel-agent-switch          ; Switch to another active session
;;   M-x gptel-agent-rename-session  ; Rename current session
;;   M-x gptel-agent-close-idle-sessions  ; Close inactive sessions
;;
;; Integration with gptel-agent.el:
;; - The `gptel-agent' function should call `gptel-agent--register-session'
;;   after creating a new buffer to enable multi-session tracking.
;; - The `gptel-send' function should call `gptel-agent--update-activity'
;;   to track when sessions are actively used.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(defgroup gptel-agent-multi nil
  "Multi-session management for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-")

;;;; Customization Options

(defcustom gptel-agent-max-sessions 5
  "Maximum number of concurrent agent sessions.

When this limit is reached, a warning is displayed when creating
new sessions.  Use `gptel-agent-close-idle-sessions' to clean up
inactive sessions.

Set to nil to disable session limit checking."
  :type '(choice (integer :tag "Maximum sessions")
                 (const :tag "Unlimited" nil))
  :group 'gptel-agent-multi)

(defcustom gptel-agent-idle-timeout 300
  "Seconds of inactivity before a session is considered idle.

Sessions are considered idle if they haven't had any activity
(gptel-send calls) within this time period.  Idle sessions can
be closed with `gptel-agent-close-idle-sessions'.

Set to nil to disable idle detection."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Never idle" nil))
  :group 'gptel-agent-multi)

;;;; Buffer-local Session Metadata

(defvar-local gptel-agent--session-name nil
  "Name of the current session in this buffer.
This is a human-readable name that can be set with
`gptel-agent-rename-session'.")

(defvar-local gptel-agent--last-activity nil
  "Timestamp of last activity in this session.
Updated by `gptel-agent--update-activity', typically called
from `gptel-send' or similar interactive commands.")

;;;; Session Registry

(defvar gptel-agent--active-sessions nil
  "List of active agent session buffers.
Each buffer in this list is an agent buffer with session metadata.")

(defun gptel-agent--register-session (buffer)
  "Register BUFFER as an active agent session.
Initializes session metadata and adds to session registry.
Should be called after creating a new agent buffer."
  (with-current-buffer buffer
    (setq gptel-agent--last-activity (current-time))
    (unless gptel-agent--session-name
      (setq gptel-agent--session-name
            (buffer-name))))
  (cl-pushnew buffer gptel-agent--active-sessions)
  (gptel-agent--check-session-limit))

(defun gptel-agent--unregister-session (buffer)
  "Unregister BUFFER from active session registry.
Typically called when buffer is killed."
  (setq gptel-agent--active-sessions
        (delq buffer gptel-agent--active-sessions)))

(defun gptel-agent--update-activity (&optional buffer)
  "Update last activity timestamp for BUFFER (default: current buffer).
Call this from `gptel-send' or similar commands to track session usage."
  (with-current-buffer (or buffer (current-buffer))
    (when (memq (current-buffer) gptel-agent--active-sessions)
      (setq gptel-agent--last-activity (current-time)))))

;;;; Session Limit Management

(defun gptel-agent--check-session-limit ()
  "Check if session count is approaching or at limit.
Display warnings when appropriate."
  (when gptel-agent-max-sessions
    (let ((count (length gptel-agent--active-sessions)))
      (cond
       ((>= count gptel-agent-max-sessions)
        (display-warning
         'gptel-agent
         (format "Maximum session limit reached (%d/%d). Consider closing idle sessions with M-x gptel-agent-close-idle-sessions."
                 count gptel-agent-max-sessions)
         :warning))
       ((>= count (- gptel-agent-max-sessions 1))
        (display-warning
         'gptel-agent
         (format "Approaching session limit (%d/%d)."
                 count gptel-agent-max-sessions)
         :warning))))))

;;;; Session Query

(defun gptel-agent--get-active-sessions ()
  "Return list of active session buffers with metadata.

Each element is a plist with:
  :buffer - The buffer object
  :name - Session name
  :project - Project directory
  :model - Current model (if available)
  :idle - Seconds since last activity (or nil if no timeout)
  :idle-p - Non-nil if session is considered idle

Results are sorted by most recently active first."
  (let ((sessions nil))
    (dolist (buf gptel-agent--active-sessions)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let* ((last-activity gptel-agent--last-activity)
                 (idle-seconds
                  (when last-activity
                    (float-time (time-since last-activity))))
                 (is-idle
                  (and gptel-agent-idle-timeout
                       idle-seconds
                       (> idle-seconds gptel-agent-idle-timeout))))
            (push (list :buffer buf
                       :name (or gptel-agent--session-name (buffer-name))
                       :project (abbreviate-file-name default-directory)
                       :model (and (boundp 'gptel-model) gptel-model)
                       :idle (when idle-seconds (round idle-seconds))
                       :idle-p is-idle)
                  sessions)))))
    ;; Sort by most recent activity first
    (sort sessions
          (lambda (a b)
            (let ((idle-a (or (plist-get a :idle) 0))
                  (idle-b (or (plist-get b :idle) 0)))
              (< idle-a idle-b))))))

(defun gptel-agent--format-session-annotation (session)
  "Format annotation string for SESSION in completion interface.
SESSION is a plist from `gptel-agent--get-active-sessions'."
  (let ((project (plist-get session :project))
        (model (plist-get session :model))
        (idle (plist-get session :idle))
        (idle-p (plist-get session :idle-p)))
    (format " [%s%s%s]"
            project
            (if model (format " %s" model) "")
            (cond
             ((null idle) "")
             (idle-p (format " idle:%ds" idle))
             (t (format " active:%ds-ago" idle))))))

;;;; Idle Detection

(defun gptel-agent--session-idle-p (&optional buffer)
  "Return non-nil if BUFFER (default: current) is idle.
A session is idle if it has no activity within `gptel-agent-idle-timeout'
seconds, or if idle timeout is disabled, never considered idle."
  (unless buffer (setq buffer (current-buffer)))
  (when (and gptel-agent-idle-timeout
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((last-activity gptel-agent--last-activity))
        (and last-activity
             (> (float-time (time-since last-activity))
                gptel-agent-idle-timeout))))))

;;;; Commands

;;;###autoload
(defun gptel-agent-switch ()
  "Switch to another active gptel-agent session.

Display a completion interface showing all active sessions with:
- Session name
- Project directory
- Current model
- Idle status

Select a session to switch to that buffer."
  (interactive)
  (let* ((sessions (gptel-agent--get-active-sessions))
         (choices (mapcar
                   (lambda (s)
                     (let ((name (plist-get s :name)))
                       (cons (concat name
                                   (gptel-agent--format-session-annotation s))
                             (plist-get s :buffer))))
                   sessions)))
    (unless choices
      (user-error "No active sessions"))
    (let* ((choice (completing-read "Switch to session: " choices nil t))
           (buffer (cdr (assoc choice choices))))
      (when buffer
        (switch-to-buffer buffer)))))

;;;###autoload
(defun gptel-agent-close-idle-sessions (&optional force)
  "Close idle agent sessions.

Sessions are considered idle if they have no activity within
`gptel-agent-idle-timeout' seconds.

With prefix argument FORCE, close all idle sessions without
confirmation.  Otherwise, prompt for each idle session.

Returns the number of sessions closed."
  (interactive "P")
  (let* ((sessions (gptel-agent--get-active-sessions))
         (idle-sessions (cl-remove-if-not
                        (lambda (s) (plist-get s :idle-p))
                        sessions))
         (closed 0))
    (unless idle-sessions
      (message "No idle sessions found")
      (cl-return-from gptel-agent-close-idle-sessions 0))
    (dolist (session idle-sessions)
      (let* ((buffer (plist-get session :buffer))
             (name (plist-get session :name))
             (idle (plist-get session :idle)))
        (when (or force
                  (y-or-n-p (format "Close idle session '%s' (idle %ds)? "
                                   name idle)))
          (kill-buffer buffer)
          (cl-incf closed))))
    (message "Closed %d idle session%s" closed (if (= closed 1) "" "s"))
    closed))

;;;###autoload
(defun gptel-agent-rename-session (new-name)
  "Rename the current agent session to NEW-NAME.

Updates both the session name and the buffer name while maintaining
the \"*gptel-agent:*\" prefix format."
  (interactive
   (list (read-string "New session name: "
                     (when gptel-agent--session-name
                       ;; Strip prefix if present
                       (string-remove-prefix "*gptel-agent:"
                                           (string-remove-suffix "*"
                                                               gptel-agent--session-name))))))
  (unless (memq (current-buffer) gptel-agent--active-sessions)
    (user-error "Current buffer is not an active agent session"))
  (let ((new-buffer-name (format "*gptel-agent:%s*" new-name)))
    (setq gptel-agent--session-name new-name)
    (rename-buffer new-buffer-name t)
    (message "Session renamed to: %s" new-name)))

;;;###autoload
(defun gptel-agent-list-sessions ()
  "Display list of active agent sessions with status.

Shows session name, project, model, and idle status for all
active sessions in the echo area or in a separate buffer if
there are many sessions."
  (interactive)
  (let ((sessions (gptel-agent--get-active-sessions)))
    (unless sessions
      (message "No active sessions")
      (cl-return-from gptel-agent-list-sessions))
    (if (<= (length sessions) 5)
        ;; Short list - display in echo area
        (message "Active sessions:\n%s"
                 (mapconcat
                  (lambda (s)
                    (format "  %s%s"
                            (plist-get s :name)
                            (gptel-agent--format-session-annotation s)))
                  sessions
                  "\n"))
      ;; Long list - use dedicated buffer
      (with-current-buffer (get-buffer-create "*GPTel Agent Sessions*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Active GPTel Agent Sessions\n")
          (insert (make-string 50 ?=) "\n\n")
          (dolist (session sessions)
            (insert (format "%-30s %s\n"
                           (plist-get session :name)
                           (gptel-agent--format-session-annotation session))))
          (insert "\n" (make-string 50 ?-) "\n")
          (insert (format "Total: %d session%s"
                         (length sessions)
                         (if (= (length sessions) 1) "" "s")))
          (when gptel-agent-max-sessions
            (insert (format " (limit: %d)" gptel-agent-max-sessions)))
          (insert "\n\n")
          (insert "Commands:\n")
          (insert "  M-x gptel-agent-switch - Switch to a session\n")
          (insert "  M-x gptel-agent-close-idle-sessions - Close idle sessions\n")
          (goto-char (point-min))
          (view-mode 1))
        (pop-to-buffer (current-buffer))))))

;;;; Buffer Cleanup Hook

(defun gptel-agent--cleanup-on-kill ()
  "Clean up session registry when buffer is killed.
Add this to `kill-buffer-hook' in agent buffers."
  (gptel-agent--unregister-session (current-buffer)))

;;;; Integration Point

;; This should be called from gptel-agent.el after creating an agent buffer:
;;
;; (with-current-buffer gptel-buf
;;   (add-hook 'kill-buffer-hook #'gptel-agent--cleanup-on-kill nil t)
;;   (gptel-agent--register-session (current-buffer)))

(provide 'gptel-agent-multi)
;;; gptel-agent-multi.el ends here
