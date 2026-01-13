;;; gptel-agent-mcp.el --- Project-level MCP server management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: January 2025
;; Version: 0.1.0
;; Keywords: tools, convenience
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5") (mcp "0.1.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides project-level MCP server management for gptel-agent,
;; enabling per-project MCP server configuration via .gptel-agent.el files.
;;
;; Features:
;; - Per-project MCP server configuration in .gptel-agent.el
;; - Environment variable expansion in server commands ($VAR, ${VAR})
;; - Automatic server connection on project load (optional)
;; - Health monitoring and status display
;; - Integration with gptel-agent permissions system
;;
;; Configuration Format:
;;
;; Add MCP server configuration to .gptel-agent.el:
;;
;;   (gptel-agent-project-config
;;    :mcp-servers
;;    ((server-name . (:command "npx"
;;                     :args ("-y" "package-name")
;;                     :env (("API_KEY" . "value"))
;;                     :auto-start t
;;                     :restart-on-failure t))))
;;
;; MCP Server Configuration:
;; - :command - Command to start server (required for stdio servers)
;; - :args - List of command arguments (supports $VAR expansion)
;; - :env - Environment variables as alist
;; - :url - URL for HTTP/SSE servers (alternative to :command)
;; - :auto-start - Auto-connect when project loads (default: nil)
;; - :restart-on-failure - Auto-reconnect on failure (default: nil)
;; - :roots - List of root directories to expose to server
;;
;; Interactive Commands:
;;
;;   M-x gptel-agent-mcp-list           ; Show server status in list view
;;   M-x gptel-agent-mcp-connect        ; Connect project MCP servers
;;   M-x gptel-agent-mcp-disconnect     ; Disconnect project servers
;;   M-x gptel-agent-mcp-status         ; Display connection status
;;
;; Permission Integration:
;;
;; MCP tools are prefixed with "mcp-servername:" in permission checks.
;; Use pattern-based rules in .gptel-agent.el:
;;
;;   :permissions '((* . allow)
;;                  (mcp-github . allow)
;;                  (* . ask))
;;
;; Or use glob patterns for specific MCP tools:
;;
;;   :permissions '((* . ((pattern "mcp-github:*" . allow)
;;                        (pattern "mcp-*:dangerous-*" . deny)
;;                        (* . ask))))

;;; Code:

(require 'cl-lib)
(require 'gptel-agent-permissions)

(declare-function mcp-connect-server "mcp")
(declare-function mcp-stop-server "mcp")
(declare-function mcp-hub-get-servers "mcp-hub")
(declare-function mcp--status "mcp")
(declare-function mcp--tools "mcp")
(declare-function project-current "project")
(declare-function project-root "project")

(defvar mcp-server-connections)
(defvar mcp-hub-servers)

(defgroup gptel-agent-mcp nil
  "Project-level MCP server management for gptel-agent."
  :group 'gptel-agent
  :prefix "gptel-agent-mcp-")

;;;; Customization Options

(defcustom gptel-agent-mcp-auto-connect nil
  "Whether to automatically connect project MCP servers.

When non-nil, MCP servers configured in .gptel-agent.el with
:auto-start t will be automatically connected when the project
is loaded."
  :type 'boolean
  :group 'gptel-agent-mcp)

(defcustom gptel-agent-mcp-merge-strategy 'combine
  "Strategy for merging project and global MCP servers.

- `combine' - Project servers are added to global servers
- `replace' - Project servers replace global servers
- `project-only' - Only project servers are used"
  :type '(choice (const :tag "Combine project and global" combine)
                 (const :tag "Project replaces global" replace)
                 (const :tag "Project servers only" project-only))
  :group 'gptel-agent-mcp)

;;;; Internal State

(defvar gptel-agent--mcp-project-servers nil
  "Alist of project root to MCP server configurations.

Each entry is (PROJECT-ROOT . SERVER-CONFIGS) where SERVER-CONFIGS
is an alist of (SERVER-NAME . CONFIG-PLIST).")

;;;; Configuration Loading

(defun gptel-agent--expand-env-var (string)
  "Expand environment variables in STRING.

Supports both $VAR and ${VAR} syntax. Returns STRING with all
environment variables replaced by their values."
  (when (stringp string)
    (replace-regexp-in-string
     "\\$\\({\\([^}]+\\)}\\|\\([A-Za-z_][A-Za-z0-9_]*\\)\\)"
     (lambda (match)
       (let ((var-name (or (match-string 2 match)
                           (match-string 3 match))))
         (or (getenv var-name) "")))
     string
     t)))  ; FIXEDCASE - preserve original case of replacement

(defun gptel-agent--expand-mcp-config (config)
  "Expand environment variables in MCP server CONFIG.

CONFIG is a plist with server configuration. Returns a new plist
with environment variables expanded in :args, :command, and :url."
  (let ((expanded (copy-sequence config)))
    ;; Expand command
    (when-let ((command (plist-get expanded :command)))
      (setq expanded (plist-put expanded :command
                                (gptel-agent--expand-env-var command))))
    ;; Expand args
    (when-let ((args (plist-get expanded :args)))
      (setq expanded (plist-put expanded :args
                                (mapcar #'gptel-agent--expand-env-var args))))
    ;; Expand URL
    (when-let ((url (plist-get expanded :url)))
      (setq expanded (plist-put expanded :url
                                (gptel-agent--expand-env-var url))))
    expanded))

(defun gptel-agent--load-mcp-config (project-root)
  "Load MCP server configuration for PROJECT-ROOT.

Returns an alist of (SERVER-NAME . CONFIG-PLIST) from the
.gptel-agent.el file, or nil if no MCP configuration exists."
  (when-let* ((config-path (gptel-agent--locate-config project-root))
              (config-plist (condition-case err
                                (with-temp-buffer
                                  (insert-file-contents config-path)
                                  (let ((form (read (current-buffer))))
                                    (unless (and (consp form)
                                                 (eq (car form) 'gptel-agent-project-config))
                                      (error "Invalid config format"))
                                    (cdr form)))
                              (error
                               (message "Failed to load MCP config from %s: %s"
                                        config-path (error-message-string err))
                               nil)))
              (mcp-servers (plist-get config-plist :mcp-servers)))
    ;; Expand environment variables in each server config
    (mapcar (lambda (server-entry)
              (cons (car server-entry)
                    (gptel-agent--expand-mcp-config (cdr server-entry))))
            mcp-servers)))

(defun gptel-agent--get-project-servers (&optional project-root)
  "Get MCP server configurations for PROJECT-ROOT.

PROJECT-ROOT defaults to the current project root. Returns an alist
of (SERVER-NAME . CONFIG-PLIST)."
  (unless project-root
    (when-let ((project (project-current)))
      (setq project-root (project-root project))))
  (when project-root
    (or (alist-get project-root gptel-agent--mcp-project-servers
                   nil nil #'equal)
        (when-let ((servers (gptel-agent--load-mcp-config project-root)))
          (setf (alist-get project-root gptel-agent--mcp-project-servers
                           nil nil #'equal)
                servers)
          servers))))

(defun gptel-agent--merge-server-configs (project-servers)
  "Merge PROJECT-SERVERS with global mcp-hub-servers.

Uses `gptel-agent-mcp-merge-strategy' to determine how to combine
configurations. Returns the merged server configuration list."
  (pcase gptel-agent-mcp-merge-strategy
    ('combine
     (append project-servers mcp-hub-servers))
    ('replace
     (or project-servers mcp-hub-servers))
    ('project-only
     project-servers)))

;;;; Server Connection Management

(defun gptel-agent--mcp-server-status (name)
  "Get the connection status for MCP server NAME.

Returns one of: `connected', `disconnected', `error', or `unknown'."
  (if-let ((connection (gethash name mcp-server-connections)))
      (condition-case nil
          (mcp--status connection)
        (error 'unknown))
    'disconnected))

(defun gptel-agent--mcp-server-running-p (name)
  "Return non-nil if MCP server NAME is running."
  (eq (gptel-agent--mcp-server-status name) 'connected))

;;;###autoload
(defun gptel-agent-mcp-connect (&optional project-root)
  "Connect MCP servers configured for PROJECT-ROOT.

PROJECT-ROOT defaults to the current project root. Servers are
connected according to their :auto-start configuration.

This command loads server configurations from .gptel-agent.el and
starts any servers that aren't already running."
  (interactive)
  (unless (require 'mcp nil t)
    (user-error "MCP package not available. Please install mcp.el"))
  (unless project-root
    (unless (project-current)
      (user-error "Not in a project"))
    (setq project-root (project-root (project-current))))

  (if-let ((servers (gptel-agent--get-project-servers project-root)))
      (let ((connected 0)
            (failed 0)
            (skipped 0))
        (dolist (server-entry servers)
          (let ((name (car server-entry))
                (config (cdr server-entry)))
            (cond
             ;; Already running
             ((gptel-agent--mcp-server-running-p name)
              (cl-incf skipped)
              (message "MCP server %s already running" name))

             ;; Try to connect
             (t
              (condition-case err
                  (progn
                    (apply #'mcp-connect-server
                           (cons name config))
                    (cl-incf connected)
                    (message "Connected MCP server: %s" name))
                (error
                 (cl-incf failed)
                 (message "Failed to connect MCP server %s: %s"
                          name (error-message-string err))))))))

        (message "MCP servers - Connected: %d, Failed: %d, Skipped: %d"
                 connected failed skipped))
    (message "No MCP servers configured for project: %s" project-root)))

;;;###autoload
(defun gptel-agent-mcp-disconnect (&optional project-root)
  "Disconnect MCP servers for PROJECT-ROOT.

PROJECT-ROOT defaults to the current project root. Stops all
project-configured MCP servers that are currently running."
  (interactive)
  (unless (require 'mcp nil t)
    (user-error "MCP package not available"))
  (unless project-root
    (unless (project-current)
      (user-error "Not in a project"))
    (setq project-root (project-root (project-current))))

  (if-let ((servers (gptel-agent--get-project-servers project-root)))
      (let ((disconnected 0))
        (dolist (server-entry servers)
          (let ((name (car server-entry)))
            (when (gptel-agent--mcp-server-running-p name)
              (condition-case err
                  (progn
                    (mcp-stop-server name)
                    (cl-incf disconnected)
                    (message "Disconnected MCP server: %s" name))
                (error
                 (message "Failed to disconnect MCP server %s: %s"
                          name (error-message-string err)))))))
        (message "Disconnected %d MCP server%s"
                 disconnected (if (= disconnected 1) "" "s")))
    (message "No MCP servers configured for project: %s" project-root)))

;;;###autoload
(defun gptel-agent-mcp-status (&optional project-root)
  "Display connection status for PROJECT-ROOT MCP servers.

PROJECT-ROOT defaults to the current project root. Shows the
status of all configured MCP servers in the minibuffer."
  (interactive)
  (unless project-root
    (unless (project-current)
      (user-error "Not in a project"))
    (setq project-root (project-root (project-current))))

  (if-let ((servers (gptel-agent--get-project-servers project-root)))
      (let ((status-lines
             (mapcar
              (lambda (server-entry)
                (let* ((name (car server-entry))
                       (status (gptel-agent--mcp-server-status name))
                       (status-str
                        (pcase status
                          ('connected
                           (propertize "connected" 'face 'success))
                          ('disconnected
                           (propertize "disconnected" 'face 'warning))
                          ('error
                           (propertize "error" 'face 'error))
                          (_
                           (symbol-name status)))))
                  (format "  %s: %s" name status-str)))
              servers)))
        (message "MCP servers for %s:\n%s"
                 (file-name-nondirectory (directory-file-name project-root))
                 (string-join status-lines "\n")))
    (message "No MCP servers configured for project: %s" project-root)))

;;;; Server List UI

(defvar gptel-agent-mcp-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'gptel-agent-mcp-list-connect)
    (define-key map (kbd "d") #'gptel-agent-mcp-list-disconnect)
    (define-key map (kbd "r") #'gptel-agent-mcp-list-restart)
    (define-key map (kbd "g") #'gptel-agent-mcp-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `gptel-agent-mcp-list-mode'.")

(define-derived-mode gptel-agent-mcp-list-mode tabulated-list-mode
  "MCP-Servers"
  "Major mode for listing gptel-agent MCP servers.

\\{gptel-agent-mcp-list-mode-map}"
  (setq tabulated-list-format
        [("Name" 20 t)
         ("Status" 12 t)
         ("Command" 30 t)
         ("Tools" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(defun gptel-agent-mcp--list-entries ()
  "Generate entries for MCP server list display.

Returns a list suitable for `tabulated-list-entries'."
  (when-let ((project (project-current))
             (project-root (project-root project))
             (servers (gptel-agent--get-project-servers project-root)))
    (mapcar
     (lambda (server-entry)
       (let* ((name (car server-entry))
              (config (cdr server-entry))
              (status (gptel-agent--mcp-server-status name))
              (status-str
               (pcase status
                 ('connected (propertize "connected" 'face 'success))
                 ('disconnected (propertize "disconnected" 'face 'warning))
                 ('error (propertize "error" 'face 'error))
                 (_ (symbol-name status))))
              (command (or (plist-get config :command)
                          (plist-get config :url)
                          ""))
              (tools-count
               (if (eq status 'connected)
                   (condition-case nil
                       (when-let ((conn (gethash name mcp-server-connections)))
                         (format "%d" (length (mcp--tools conn))))
                     (error "?"))
                 "-")))
         (list name
               (vector name status-str command tools-count))))
     servers)))

;;;###autoload
(defun gptel-agent-mcp-list ()
  "Display a list of MCP servers for the current project.

Shows server name, connection status, command, and available tools
count. Use the following keys:

  c - Connect server
  d - Disconnect server
  r - Restart server
  g - Refresh list
  q - Quit window"
  (interactive)
  (unless (require 'mcp nil t)
    (user-error "MCP package not available"))
  (unless (project-current)
    (user-error "Not in a project"))

  (let ((buffer (get-buffer-create "*gptel-agent MCP Servers*")))
    (with-current-buffer buffer
      (gptel-agent-mcp-list-mode)
      (setq tabulated-list-entries #'gptel-agent-mcp--list-entries)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

(defun gptel-agent-mcp-list-connect ()
  "Connect the MCP server at point in the server list."
  (interactive nil gptel-agent-mcp-list-mode)
  (when-let* ((id (tabulated-list-get-id))
              (project (project-current))
              (project-root (project-root project))
              (servers (gptel-agent--get-project-servers project-root))
              (config (alist-get id servers nil nil #'equal)))
    (condition-case err
        (progn
          (apply #'mcp-connect-server (cons id config))
          (message "Connecting MCP server: %s" id)
          (run-with-timer 2 nil #'gptel-agent-mcp-list-refresh))
      (error
       (message "Failed to connect MCP server %s: %s"
                id (error-message-string err))))))

(defun gptel-agent-mcp-list-disconnect ()
  "Disconnect the MCP server at point in the server list."
  (interactive nil gptel-agent-mcp-list-mode)
  (when-let ((id (tabulated-list-get-id)))
    (condition-case err
        (progn
          (mcp-stop-server id)
          (message "Disconnected MCP server: %s" id)
          (gptel-agent-mcp-list-refresh))
      (error
       (message "Failed to disconnect MCP server %s: %s"
                id (error-message-string err))))))

(defun gptel-agent-mcp-list-restart ()
  "Restart the MCP server at point in the server list."
  (interactive nil gptel-agent-mcp-list-mode)
  (when-let ((id (tabulated-list-get-id)))
    (gptel-agent-mcp-list-disconnect)
    (run-with-timer 1 nil
                    (lambda ()
                      (with-current-buffer "*gptel-agent MCP Servers*"
                        (gptel-agent-mcp-list-connect))))))

(defun gptel-agent-mcp-list-refresh ()
  "Refresh the MCP server list display."
  (interactive nil gptel-agent-mcp-list-mode)
  (tabulated-list-print t))

;;;; Auto-connection Hook

(defun gptel-agent--maybe-connect-mcp ()
  "Auto-connect MCP servers if configured.

Called when opening a project file. Connects servers with
:auto-start t if `gptel-agent-mcp-auto-connect' is enabled."
  (when (and gptel-agent-mcp-auto-connect
             (require 'mcp nil t))
    (when-let* ((project (project-current))
                (project-root (project-root project))
                (servers (gptel-agent--get-project-servers project-root)))
      (dolist (server-entry servers)
        (let ((name (car server-entry))
              (config (cdr server-entry)))
          (when (and (plist-get config :auto-start)
                     (not (gptel-agent--mcp-server-running-p name)))
            (condition-case err
                (apply #'mcp-connect-server (cons name config))
              (error
               (message "Failed to auto-connect MCP server %s: %s"
                        name (error-message-string err))))))))))

;; Add hook for auto-connection
(add-hook 'find-file-hook #'gptel-agent--maybe-connect-mcp)

;;;; Permission Integration

(defun gptel-agent--mcp-tool-matches-p (tool-name pattern)
  "Check if MCP TOOL-NAME matches PATTERN.

TOOL-NAME should be in format \"mcp-server:tool\".
PATTERN can use glob syntax (e.g., \"mcp-github:*\")."
  (let ((regexp (wildcard-to-regexp pattern)))
    (string-match-p regexp tool-name)))

(provide 'gptel-agent-mcp)
;;; gptel-agent-mcp.el ends here
