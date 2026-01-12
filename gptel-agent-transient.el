;;; gptel-agent-transient.el --- Permission approval UI for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: January 2025
;; Version: 0.1.0
;; Keywords: tools, convenience
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5") (gptel "0.9.9"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module implements the interactive permission approval flow for
;; gptel-agent, providing a UI for real-time approval when tools require
;; 'ask' permission.
;;
;; Features:
;; - Transient-based approval menu (when transient.el is available)
;; - Fallback to read-char-choice for lightweight operation
;; - Session permission caching (buffer-local)
;; - Project permission persistence (writes to .gptel-agent.el)
;; - Integration with existing tool preview overlays
;;
;; Usage:
;;
;;   (require 'gptel-agent-transient)
;;
;;   ;; Request approval for a tool call
;;   (gptel-agent-request-approval
;;    'Bash
;;    '("git status")
;;    (lambda (decision) (message "User decided: %s" decision)))
;;
;; The approval flow is automatically integrated with gptel-agent-permissions.el
;; when a tool permission resolves to 'ask'.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

;; Optional transient dependency
(declare-function transient-define-prefix "transient")
(declare-function transient-setup "transient")

;; Forward declarations
(declare-function gptel-agent--project-root "gptel-agent")
(declare-function gptel-agent--confirm-overlay "gptel-agent-tools")

(defvar gptel-tool-call-actions-map)

;;;; Customization

(defgroup gptel-agent-approval nil
  "Permission approval UI for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-approval-")

(defcustom gptel-agent-approval-use-transient t
  "Use transient.el for approval menu when available.

If nil, always use the simpler read-char-choice interface."
  :type 'boolean
  :group 'gptel-agent-approval)

(defcustom gptel-agent-approval-show-preview t
  "Show operation preview when requesting approval.

When non-nil, displays a preview of what the tool will do
before requesting approval."
  :type 'boolean
  :group 'gptel-agent-approval)

;;;; Session Permission Cache

(defvar-local gptel-agent--session-permissions nil
  "Hash table of permission decisions for current session.

Keys are tool names (symbols), values are plists:
  (:decision approve|deny :args-pattern REGEXP|nil :timestamp TIME)")

(defun gptel-agent--init-session-permissions ()
  "Initialize session permissions hash table if needed."
  (unless gptel-agent--session-permissions
    (setq gptel-agent--session-permissions
          (make-hash-table :test 'eq))))

(defun gptel-agent--check-session-permission (tool-name args)
  "Check if TOOL-NAME with ARGS has a session-level permission.

Returns nil (no cached decision), `approve', or `deny'.
TOOL-NAME should be a symbol."
  (gptel-agent--init-session-permissions)
  (let ((tool-sym (if (symbolp tool-name) tool-name (intern tool-name))))
    (when-let* ((entry (gethash tool-sym gptel-agent--session-permissions)))
      (let ((decision (plist-get entry :decision))
            (pattern (plist-get entry :args-pattern)))
        (if (or (null pattern)
                (let ((args-str (format "%S" args)))
                  (string-match-p pattern args-str)))
            decision
          nil)))))

(defun gptel-agent--store-session-permission (tool-name decision &optional args-pattern)
  "Store DECISION for TOOL-NAME in session cache.

DECISION should be `approve' or `deny'.
Optional ARGS-PATTERN is a regexp to match specific argument patterns."
  (gptel-agent--init-session-permissions)
  (let ((tool-sym (if (symbolp tool-name) tool-name (intern tool-name))))
    (puthash tool-sym
             (list :decision decision
                   :args-pattern args-pattern
                   :timestamp (current-time))
             gptel-agent--session-permissions)))

(defun gptel-agent-clear-session-permissions ()
  "Clear all session-level permission decisions.

This resets permissions to their default state for the current session."
  (interactive)
  (setq gptel-agent--session-permissions nil)
  (message "Session permissions cleared"))

;;;; Project Permission Persistence

(defun gptel-agent--load-project-permission-config ()
  "Load permission configuration from project .gptel-agent.el.

Returns the :permissions plist value or nil."
  (when-let* ((root (and (fboundp 'gptel-agent--project-root)
                         (gptel-agent--project-root)))
              (file (expand-file-name ".gptel-agent.el" root))
              ((file-exists-p file)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((config (read (current-buffer))))
            (when (and (listp config)
                       (eq (car config) 'gptel-agent-project-config))
              (plist-get (cdr config) :permissions))))
      (error nil))))

(defun gptel-agent--save-project-permission (tool-name decision &optional pattern)
  "Save TOOL-NAME permission DECISION to project .gptel-agent.el.

DECISION should be `approve' or `deny'.
Optional PATTERN limits the permission to matching arguments."
  (let* ((root (and (fboundp 'gptel-agent--project-root)
                    (gptel-agent--project-root)))
         (file (when root (expand-file-name ".gptel-agent.el" root))))
    (unless file
      (user-error "Not in a project; cannot save project permission"))

    (let* ((config (if (file-exists-p file)
                       (with-temp-buffer
                         (insert-file-contents file)
                         (condition-case nil
                             (read (current-buffer))
                           (error '(gptel-agent-project-config))))
                     '(gptel-agent-project-config)))
           (config-body (if (eq (car config) 'gptel-agent-project-config)
                            (cdr config)
                          config))
           (perms (plist-get config-body :permissions))
           (tool-sym (if (symbolp tool-name) tool-name (intern tool-name)))
           (new-entry (pcase decision
                        ('approve (if pattern
                                      `((pattern ,pattern . allow))
                                    'allow))
                        ('deny (if pattern
                                   `((pattern ,pattern . deny))
                                 'deny))
                        (_ decision))))

      ;; Update permissions alist
      (setf (alist-get tool-sym perms) new-entry)
      (setq config-body (plist-put config-body :permissions perms))

      ;; Write back to file
      (with-temp-file file
        (insert ";;; .gptel-agent.el --- Project configuration -*- lexical-binding: t; -*-\n\n")
        (pp `(gptel-agent-project-config ,@config-body) (current-buffer))
        (insert "\n;;; .gptel-agent.el ends here\n"))

      (message "Saved %s permission for %s to project config"
               decision tool-name))))

;;;; Approval Decision Types

(defconst gptel-agent--approval-decisions
  '(approve-once approve-session approve-project deny-once deny-always)
  "List of valid approval decision types.")

(defun gptel-agent--decision-to-symbol (char)
  "Convert approval CHAR to decision symbol."
  (pcase char
    (?y 'approve-once)
    (?a 'approve-session)
    (?p 'approve-project)
    (?n 'deny-once)
    (?d 'deny-always)
    (_ nil)))

;;;; Fallback Approval UI

(defun gptel-agent--approval-fallback (tool-name args)
  "Request approval using read-char-choice when transient unavailable.

TOOL-NAME is the tool requiring approval.
ARGS are the tool arguments.
Returns a decision symbol."
  (let* ((args-preview (if (stringp (car args))
                           (truncate-string-to-width (car args) 40)
                         (format "%S" args)))
         (prompt (format "%s %s? [y]once [a]lways [p]roject [n]o [d]eny-always: "
                         tool-name args-preview))
         (char (read-char-choice prompt '(?y ?a ?p ?n ?d))))
    (gptel-agent--decision-to-symbol char)))

;;;; Transient Approval Menu

(defvar gptel-agent--pending-approval nil
  "Plist containing pending approval request data.
Contains: :tool-name :args :callback")

(defun gptel-agent--transient-available-p ()
  "Check if transient.el is available and should be used."
  (and gptel-agent-approval-use-transient
       (require 'transient nil t)))

(defun gptel-agent--approval-menu-description ()
  "Generate description for transient approval menu."
  (let ((tool-name (plist-get gptel-agent--pending-approval :tool-name))
        (args (plist-get gptel-agent--pending-approval :args)))
    (format "Permission required for: %s\nArguments: %S"
            tool-name
            (if (and args (> (length (format "%S" args)) 60))
                (truncate-string-to-width (format "%S" args) 60)
              args))))

(defun gptel-agent--complete-approval (decision)
  "Complete pending approval with DECISION and call callback."
  (when gptel-agent--pending-approval
    (let ((callback (plist-get gptel-agent--pending-approval :callback))
          (tool-name (plist-get gptel-agent--pending-approval :tool-name)))
      ;; Store session/project decisions
      (pcase decision
        ('approve-session
         (gptel-agent--store-session-permission tool-name 'approve))
        ('approve-project
         (gptel-agent--save-project-permission tool-name 'approve))
        ('deny-always
         (gptel-agent--store-session-permission tool-name 'deny)))
      ;; Clear pending and call callback
      (setq gptel-agent--pending-approval nil)
      (when callback
        (funcall callback decision)))))

(defun gptel-agent--approve-once ()
  "Approve tool execution once."
  (interactive)
  (gptel-agent--complete-approval 'approve-once))

(defun gptel-agent--approve-session ()
  "Approve tool execution for this session."
  (interactive)
  (gptel-agent--complete-approval 'approve-session))

(defun gptel-agent--approve-project ()
  "Approve tool execution for this project."
  (interactive)
  (gptel-agent--complete-approval 'approve-project))

(defun gptel-agent--deny-once ()
  "Deny tool execution once."
  (interactive)
  (gptel-agent--complete-approval 'deny-once))

(defun gptel-agent--deny-always ()
  "Deny tool execution always (for this session)."
  (interactive)
  (gptel-agent--complete-approval 'deny-always))

;; Define transient menu when transient is available
(with-eval-after-load 'transient
  (transient-define-prefix gptel-agent-approval-menu ()
    "Permission approval menu for tool execution."
    [:description gptel-agent--approval-menu-description
     ["Approve"
      ("y" "Once" gptel-agent--approve-once)
      ("a" "Always (session)" gptel-agent--approve-session)
      ("p" "Always (project)" gptel-agent--approve-project)]
     ["Deny"
      ("n" "Once" gptel-agent--deny-once)
      ("d" "Always" gptel-agent--deny-always)]
     ["Other"
      ("q" "Quit (deny)" gptel-agent--deny-once)]]))

;;;; Main Approval Entry Point

(defun gptel-agent-request-approval (tool-name args callback)
  "Request user approval for TOOL-NAME with ARGS.

CALLBACK is called with the decision symbol:
- `approve-once': Execute this once
- `approve-session': Execute and remember for session
- `approve-project': Execute and save to project config
- `deny-once': Deny this execution
- `deny-always': Deny and remember for session

Returns immediately; CALLBACK is called when user decides."
  (let ((tool-sym (if (symbolp tool-name) tool-name (intern tool-name))))
    ;; Check session cache first
    (let ((cached (gptel-agent--check-session-permission tool-sym args)))
      (if cached
          ;; Use cached decision
          (funcall callback (if (eq cached 'approve) 'approve-once 'deny-once))
        ;; Need user input
        (if (gptel-agent--transient-available-p)
            ;; Use transient menu
            (progn
              (setq gptel-agent--pending-approval
                    (list :tool-name tool-sym :args args :callback callback))
              (gptel-agent-approval-menu))
          ;; Use fallback
          (let ((decision (gptel-agent--approval-fallback tool-sym args)))
            ;; Store session/project decisions
            (pcase decision
              ('approve-session
               (gptel-agent--store-session-permission tool-sym 'approve))
              ('approve-project
               (gptel-agent--save-project-permission tool-sym 'approve))
              ('deny-always
               (gptel-agent--store-session-permission tool-sym 'deny)))
            (funcall callback decision)))))))

;;;; Overlay Integration

(defvar gptel-agent--approval-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'gptel-agent--overlay-approve-once)
    (define-key map (kbd "a") #'gptel-agent--overlay-approve-session)
    (define-key map (kbd "p") #'gptel-agent--overlay-approve-project)
    (define-key map (kbd "n") #'gptel-agent--overlay-deny-once)
    (define-key map (kbd "d") #'gptel-agent--overlay-deny-always)
    map)
  "Keymap for permission approval in tool preview overlays.")

(defvar-local gptel-agent--overlay-approval-callback nil
  "Callback for current overlay approval.")

(defun gptel-agent--overlay-approve-once ()
  "Approve execution once from overlay."
  (interactive)
  (when gptel-agent--overlay-approval-callback
    (funcall gptel-agent--overlay-approval-callback 'approve-once)))

(defun gptel-agent--overlay-approve-session ()
  "Approve execution for session from overlay."
  (interactive)
  (let ((ov (cdr (get-char-property-and-overlay (point) 'gptel-agent-tool))))
    (when (and ov gptel-agent--overlay-approval-callback)
      (let ((tool-name (overlay-get ov 'gptel-agent-tool-name)))
        (gptel-agent--store-session-permission tool-name 'approve)
        (funcall gptel-agent--overlay-approval-callback 'approve-session)))))

(defun gptel-agent--overlay-approve-project ()
  "Approve execution for project from overlay."
  (interactive)
  (let ((ov (cdr (get-char-property-and-overlay (point) 'gptel-agent-tool))))
    (when (and ov gptel-agent--overlay-approval-callback)
      (let ((tool-name (overlay-get ov 'gptel-agent-tool-name)))
        (gptel-agent--save-project-permission tool-name 'approve)
        (funcall gptel-agent--overlay-approval-callback 'approve-project)))))

(defun gptel-agent--overlay-deny-once ()
  "Deny execution once from overlay."
  (interactive)
  (when gptel-agent--overlay-approval-callback
    (funcall gptel-agent--overlay-approval-callback 'deny-once)))

(defun gptel-agent--overlay-deny-always ()
  "Deny execution always from overlay."
  (interactive)
  (let ((ov (cdr (get-char-property-and-overlay (point) 'gptel-agent-tool))))
    (when (and ov gptel-agent--overlay-approval-callback)
      (let ((tool-name (overlay-get ov 'gptel-agent-tool-name)))
        (gptel-agent--store-session-permission tool-name 'deny)
        (funcall gptel-agent--overlay-approval-callback 'deny-always)))))

(defun gptel-agent--add-approval-to-overlay (overlay tool-name args callback)
  "Add permission approval UI to OVERLAY for TOOL-NAME with ARGS.

CALLBACK is called with the user's decision."
  ;; Store approval context
  (overlay-put overlay 'gptel-agent-permission t)
  (overlay-put overlay 'gptel-agent-tool-name tool-name)
  (overlay-put overlay 'gptel-agent-tool-args args)
  (setq-local gptel-agent--overlay-approval-callback callback)

  ;; Compose approval keymap with existing
  (let ((existing-keymap (overlay-get overlay 'keymap)))
    (overlay-put overlay 'keymap
                 (if existing-keymap
                     (make-composed-keymap gptel-agent--approval-keymap
                                           existing-keymap)
                   gptel-agent--approval-keymap)))

  ;; Add visual indicator
  (let ((before-str (overlay-get overlay 'before-string)))
    (overlay-put overlay 'before-string
                 (concat (propertize "[APPROVAL NEEDED] "
                                     'face '(:foreground "orange" :weight bold))
                         (or before-str "")))))

;;;; Permission Resolution Integration

(defun gptel-agent--resolve-with-approval (tool-name args execute-fn)
  "Execute TOOL-NAME with ARGS using EXECUTE-FN after permission check.

This function integrates with gptel-agent-permissions.el to handle
tools that require 'ask' permission."
  (let ((decision (if (fboundp 'gptel-agent--check-permission)
                      (gptel-agent--check-permission tool-name args)
                    'ask)))
    (pcase decision
      ('allow (funcall execute-fn))
      ('deny (gptel-agent--tool-denied tool-name args))
      ('ask
       (gptel-agent-request-approval
        tool-name args
        (lambda (user-decision)
          (pcase user-decision
            ((or 'approve-once 'approve-session 'approve-project)
             (funcall execute-fn))
            ((or 'deny-once 'deny-always)
             (gptel-agent--tool-denied tool-name args)))))))))

(defun gptel-agent--tool-denied (tool-name args)
  "Handle denied tool execution for TOOL-NAME with ARGS.

Returns an error result suitable for gptel's tool response handling."
  (message "Tool %s denied by user" tool-name)
  (list :error (format "User denied permission to execute %s with args: %S"
                       tool-name args)))

(provide 'gptel-agent-transient)
;;; gptel-agent-transient.el ends here
