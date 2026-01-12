;;; gptel-agent-permissions.el --- Per-project permissions for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: January 2025
;; Version: 0.1.0
;; Keywords: tools, convenience
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module implements a comprehensive per-project tool permissions system
;; for gptel-agent, allowing fine-grained control over what AI-initiated tool
;; calls are allowed within a project.
;;
;; Features:
;; - Per-project configuration via .gptel-agent.el files
;; - Pattern-based rules for tools like bash commands (glob matching)
;; - Three permission levels: allow, deny, ask
;; - Permission caching with automatic invalidation on file changes
;; - Global defaults with project-specific overrides
;; - Tool-specific and universal permission rules
;;
;; Configuration Format:
;;
;; Create a .gptel-agent.el file in your project root:
;;
;;   (gptel-agent-project-config
;;    :permissions '((* . allow)
;;                   (bash . ((pattern "git *" . allow)
;;                            (pattern "rm *" . deny)
;;                            (* . ask)))
;;                   (edit . ask)))
;;
;; Permission Rules:
;;
;; - `(* . allow)' - Universal default for all tools
;; - `(tool-name . decision)' - Simple per-tool permission
;; - `(tool-name . ((pattern "..." . decision) ...))' - Pattern-based rules
;;
;; Decisions: `allow', `deny', `ask' (prompt user)
;;
;; Usage:
;;
;;   (gptel-agent--check-permission "bash" '("ls -la"))  ; => 'allow/'deny/'ask
;;   (gptel-agent-reload-permissions)                     ; Reload config

;;; Code:

(require 'cl-lib)

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

(defgroup gptel-agent-permissions nil
  "Per-project tool permissions for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-permission-")

;;;; Customization Options

(defcustom gptel-agent-permission-config-filename ".gptel-agent.el"
  "Name of per-project permission configuration file.

This file should be placed in the project root directory and contain
a `gptel-agent-project-config' form specifying permission rules."
  :type 'string
  :group 'gptel-agent-permissions)

(defcustom gptel-agent-default-permissions
  '((* . ask))
  "Global default permissions for tool calls.

This is a nested alist with the following structure:

  ((TOOL-NAME . RULES) ...)

Where RULES can be:
- A symbol: `allow', `deny', or `ask' for simple per-tool permission
- An alist for pattern-based rules:
    ((pattern GLOB-PATTERN . DECISION)
     (pattern GLOB-PATTERN . DECISION)
     (* . FALLBACK-DECISION))

The special key `*' matches any tool or any pattern.

Examples:

  ;; Ask for everything by default
  \\='((* . ask))

  ;; Allow all by default, but restrict bash
  \\='((* . allow)
    (bash . ((pattern \"git *\" . allow)
             (pattern \"rm *\" . deny)
             (* . ask))))

  ;; Specific tool permissions
  \\='((* . deny)
    (read . allow)
    (edit . ask)
    (bash . deny))"
  :type '(alist :key-type symbol
                :value-type (choice symbol
                                    (alist :key-type symbol
                                           :value-type (choice symbol string))))
  :group 'gptel-agent-permissions)

;;;; Internal State

(defvar gptel-agent--permission-cache (make-hash-table :test 'equal)
  "Cache of loaded permission configurations keyed by project root.

Each entry is a plist with keys:
  :permissions - The permission alist
  :mtime       - File modification time
  :config-path - Full path to config file")

;;;; Config File Location

(defun gptel-agent--locate-config (dir)
  "Find the permission config file starting from DIR.

Searches upward through the directory hierarchy until finding
`gptel-agent-permission-config-filename' or reaching the project root.

Returns the full path to the config file if found, or nil otherwise."
  (when-let* ((project (project-current nil dir))
              (project-root (project-root project)))
    (let ((config-path (expand-file-name
                        gptel-agent-permission-config-filename
                        project-root)))
      (when (file-readable-p config-path)
        config-path))))

;;;; Config Loading and Parsing

(defun gptel-agent--load-project-permissions (project-root)
  "Load permission configuration for PROJECT-ROOT.

Returns a plist with keys:
  :permissions - The permission alist from the config file
  :config-path - Full path to the config file
  :mtime       - File modification time

Returns nil if no config file is found or if loading fails."
  (when-let ((config-path (gptel-agent--locate-config project-root)))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents config-path)
          (let ((form (read (current-buffer))))
            (unless (and (consp form)
                         (eq (car form) 'gptel-agent-project-config))
              (error "Invalid config format: expected gptel-agent-project-config form"))
            (let ((permissions (plist-get (cdr form) :permissions)))
              (unless permissions
                (error "Missing :permissions key in config"))
              (list :permissions permissions
                    :config-path config-path
                    :mtime (file-attribute-modification-time
                            (file-attributes config-path))))))
      (error
       (message "Failed to load gptel-agent config from %s: %s"
                config-path (error-message-string err))
       nil))))

;;;; Permission Caching

(defun gptel-agent--get-cached-permissions (project-root)
  "Get cached permissions for PROJECT-ROOT.

Returns the cached plist if valid (mtime matches), or nil if the cache
is invalid or doesn't exist."
  (when-let ((cached (gethash project-root gptel-agent--permission-cache)))
    (let ((config-path (plist-get cached :config-path))
          (cached-mtime (plist-get cached :mtime)))
      (if (and config-path
               (file-exists-p config-path)
               (time-equal-p cached-mtime
                             (file-attribute-modification-time
                              (file-attributes config-path))))
          cached
        ;; Cache invalid, remove it
        (remhash project-root gptel-agent--permission-cache)
        nil))))

(defun gptel-agent--cache-permissions (project-root permissions config-path)
  "Cache PERMISSIONS for PROJECT-ROOT with CONFIG-PATH.

PERMISSIONS should be the permission alist.
CONFIG-PATH is the full path to the config file."
  (puthash project-root
           (list :permissions permissions
                 :config-path config-path
                 :mtime (file-attribute-modification-time
                         (file-attributes config-path)))
           gptel-agent--permission-cache))

(defun gptel-agent--invalidate-permission-cache (&optional project-root)
  "Invalidate permission cache for PROJECT-ROOT.

If PROJECT-ROOT is nil, clear the entire cache."
  (if project-root
      (remhash project-root gptel-agent--permission-cache)
    (clrhash gptel-agent--permission-cache)))

;;;###autoload
(defun gptel-agent-reload-permissions ()
  "Reload permission configuration for the current project.

Invalidates the cache and reloads the config file from disk.
Use this after modifying .gptel-agent.el files."
  (interactive)
  (when-let* ((project (project-current))
              (project-root (project-root project)))
    (gptel-agent--invalidate-permission-cache project-root)
    (if (gptel-agent--load-project-permissions project-root)
        (message "Reloaded permissions for project: %s" project-root)
      (message "No permission config found for project: %s" project-root))))

;;;; Pattern Matching

(defun gptel-agent--build-tool-call-string (tool-name args)
  "Build a string representation of a tool call for pattern matching.

TOOL-NAME is the tool name symbol or string.
ARGS is the argument list/plist for the tool call.

For bash tools, extracts the command string. For other tools,
formats as \"tool-name arg1 arg2 ...\"."
  (let ((tool-str (if (symbolp tool-name)
                      (symbol-name tool-name)
                    tool-name)))
    (cond
     ;; Bash tool - extract command
     ((string-equal tool-str "bash")
      (or (plist-get args :command)
          (plist-get args :cmd)
          (and (consp args) (car args))
          ""))

     ;; Other tools - format with args
     (t
      (let ((arg-strings
             (cond
              ;; Plist args
              ((and (consp args)
                    (keywordp (car args)))
               (cl-loop for (key val) on args by #'cddr
                        when (and key val)
                        collect (format "%s" val)))
              ;; List args
              ((consp args)
               (mapcar (lambda (arg) (format "%s" arg)) args))
              ;; Single arg
              (args
               (list (format "%s" args)))
              ;; No args
              (t nil))))
        (if arg-strings
            (concat tool-str " " (mapconcat #'identity arg-strings " "))
          tool-str))))))

(defun gptel-agent--permission-matches-p (tool-call pattern)
  "Check if TOOL-CALL string matches PATTERN using glob-style matching.

TOOL-CALL is the formatted tool call string.
PATTERN is a glob pattern (e.g., \"git *\", \"rm -rf *\").

Returns non-nil if the pattern matches."
  (let ((regexp (wildcard-to-regexp pattern)))
    (string-match-p regexp tool-call)))

;;;; Permission Resolution

(defun gptel-agent--resolve-tool-permission (tool-name args permissions)
  "Resolve permission for TOOL-NAME with ARGS against PERMISSIONS alist.

Returns one of the symbols: `allow', `deny', or `ask'.

Resolution order:
1. Check tool-specific pattern rules (if present)
2. Check simple tool-specific permission
3. Check universal default permission (*)
4. Fall back to `ask'"
  (let ((tool-rules (alist-get tool-name permissions))
        (universal (alist-get '* permissions)))
    (cond
     ;; Tool has pattern-based rules
     ((and (consp tool-rules)
           (not (memq tool-rules '(allow deny ask))))
      (let ((tool-call (gptel-agent--build-tool-call-string tool-name args)))
        (cl-loop for rule in tool-rules
                 when (and (consp rule)
                           (eq (car rule) 'pattern))
                 do (let ((pattern (cadr rule))
                          (decision (cddr rule)))
                      (when (gptel-agent--permission-matches-p tool-call pattern)
                        (cl-return decision)))
                 finally return (or (alist-get '* tool-rules) universal 'ask))))

     ;; Simple tool permission
     ((memq tool-rules '(allow deny ask))
      tool-rules)

     ;; Universal default
     ((memq universal '(allow deny ask))
      universal)

     ;; Ultimate fallback
     (t 'ask))))

;;;; Main Permission Check

(defun gptel-agent--normalize-tool-name (tool-name)
  "Normalize TOOL-NAME to a lowercase symbol for permission matching.

TOOL-NAME can be a string or symbol. This ensures case-insensitive
matching since tools are registered with capitalized names like
\"Bash\", \"Edit\" but users may write lowercase in configs."
  (intern (downcase (if (symbolp tool-name)
                        (symbol-name tool-name)
                      tool-name))))

(defun gptel-agent--check-permission (tool-name args)
  "Check permission for TOOL-NAME with ARGS in the current project.

TOOL-NAME can be a string or symbol.
ARGS is the argument list/plist for the tool call.

Returns one of the symbols: `allow', `deny', or `ask'.

Resolution order:
1. Project-specific permissions (from .gptel-agent.el)
2. Global default permissions (`gptel-agent-default-permissions')
3. Ultimate fallback (`ask')

This function handles caching and automatically reloads configurations
when files are modified."
  (let* ((tool-sym (gptel-agent--normalize-tool-name tool-name))
         (project (project-current))
         (project-root (when project (project-root project))))

    (if project-root
        ;; Project-specific resolution
        (let* ((cached (gptel-agent--get-cached-permissions project-root))
               (config (or cached
                           (gptel-agent--load-project-permissions project-root)))
               (permissions (if config
                                (plist-get config :permissions)
                              gptel-agent-default-permissions)))
          ;; Cache if we just loaded
          (when (and config (not cached))
            (gptel-agent--cache-permissions project-root
                                            (plist-get config :permissions)
                                            (plist-get config :config-path)))

          (gptel-agent--resolve-tool-permission tool-sym args permissions))

      ;; No project - use global defaults
      (gptel-agent--resolve-tool-permission tool-sym
                                            args
                                            gptel-agent-default-permissions))))

;;;; gptel Integration

(defun gptel-agent--make-permission-confirm (tool-name)
  "Create a confirmation predicate for TOOL-NAME based on permissions.

Returns a function suitable for the `:confirm' slot of `gptel-make-tool'.
The returned function takes the tool's argument values and returns:
- nil if permission is `allow' (no confirmation needed, proceed)
- t if permission is `ask' (show confirmation UI)
- Signals an error if permission is `deny' (prevent execution)

TOOL-NAME should be a string or symbol matching the tool's name."
  (lambda (&rest arg-values)
    (let* ((args (if (and (= (length arg-values) 1)
                          (listp (car arg-values)))
                     (car arg-values)
                   arg-values))
           (permission (gptel-agent--check-permission tool-name args)))
      (pcase permission
        ('allow nil)                    ; No confirmation needed
        ('ask t)                        ; Show confirmation UI
        ('deny
         (error "Permission denied for tool `%s'" tool-name))))))

(defun gptel-agent-permission-enforcer (tool-name)
  "Create a permission enforcement wrapper for TOOL-NAME.

Returns a function that checks permissions before tool execution.
This can be used to wrap a tool's `:function' for permission enforcement.

The returned function takes the tool's original function as its first
argument, followed by the tool's arguments. It checks permissions and
either calls the original function or signals an error.

Usage in tool registration:
  (gptel-make-tool
   :name \"MyTool\"
   :function (lambda (arg) ...)
   :confirm (gptel-agent--make-permission-confirm \"MyTool\")
   ...)"
  (lambda (original-fn &rest args)
    (let ((permission (gptel-agent--check-permission tool-name args)))
      (pcase permission
        ('deny (error "Permission denied for tool `%s'" tool-name))
        (_ (apply original-fn args))))))

(defun gptel-agent-wrap-tool-confirm (tool-spec)
  "Wrap TOOL-SPEC's confirm function to include permission checking.

TOOL-SPEC is a gptel tool specification (plist or struct).
This function is intended to be called during gptel-agent initialization
to retrofit existing tools with permission checking.

Returns the modified TOOL-SPEC."
  (when-let* ((tool-name (plist-get tool-spec :name))
              (original-confirm (plist-get tool-spec :confirm)))
    (plist-put tool-spec :confirm
               (lambda (&rest arg-values)
                 (let* ((args (if (and (= (length arg-values) 1)
                                       (listp (car arg-values)))
                                  (car arg-values)
                                arg-values))
                        (permission (gptel-agent--check-permission tool-name args)))
                   (pcase permission
                     ('allow nil)
                     ('deny (error "Permission denied for tool `%s'" tool-name))
                     ('ask
                      ;; Defer to original confirm behavior
                      (if (functionp original-confirm)
                          (apply original-confirm arg-values)
                        original-confirm)))))))
  tool-spec)

(defun gptel-agent-permission-confirm (tool-name args)
  "Check and potentially prompt for permission to execute TOOL-NAME with ARGS.

This is a convenience function for manual permission checking.

TOOL-NAME is the tool name string or symbol.
ARGS is the argument list/plist for the tool call.

Returns non-nil if execution should proceed, nil otherwise."
  (let ((permission (gptel-agent--check-permission tool-name args)))
    (pcase permission
      ('allow t)
      ('deny
       (message "Permission denied for tool `%s'" tool-name)
       nil)
      ('ask
       (let ((tool-call (gptel-agent--build-tool-call-string tool-name args)))
         (yes-or-no-p (format "Allow tool call: %s? " tool-call)))))))

(provide 'gptel-agent-permissions)
;;; gptel-agent-permissions.el ends here
