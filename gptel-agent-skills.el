;;; gptel-agent-skills.el --- Skill/prompt library system for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: January 2025
;; Version: 0.1.0
;; Keywords: tools, convenience, ai, llm
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module implements a skill loading system for gptel-agent that supports
;; injecting specialized prompts and configuring tool restrictions.
;;
;; Features:
;; - Automatic discovery of SKILL.md/SKILL.org files in project roots
;; - Global skills directory support (~/.emacs.d/gptel-agent/skills/)
;; - Project-local skills override global skills of same name
;; - Skill composition with multiple active skills
;; - Tool allow/deny lists per skill
;; - Context file auto-loading
;; - Skill dependencies with topological sorting
;; - Version compatibility checking
;; - Interactive skill management via transient menu
;;
;; Skill Definition Format (YAML frontmatter in Markdown):
;;
;;   ---
;;   name: code-review
;;   description: Specialized code review assistant
;;   prompt-position: append  # or 'prepend'
;;   tools:
;;     allow: [Read, Grep, Glob]
;;     deny: [Write, Edit, Bash]
;;   context-files:
;;     - .eslintrc.json
;;     - CODE_STYLE.md
;;   requires: [general-assistant]  # skill dependencies
;;   min-version: 0.1.0
;;   ---
;;   You are a code reviewer. Focus on...
;;
;; Usage:
;;   (gptel-agent-load-skill "code-review")
;;   (gptel-agent-unload-skill "code-review")
;;   (gptel-agent-active-skills)
;;
;; The system automatically loads SKILL.md/SKILL.org from project roots
;; when `gptel-agent' is invoked if `gptel-agent-auto-load-skills' is non-nil.

;;; Code:

(require 'cl-lib)

(declare-function gptel-agent-parse-markdown-frontmatter "gptel-agent")
(declare-function gptel-agent-parse-org-properties "gptel-agent")
(declare-function project-current "project")
(declare-function project-root "project")

(defgroup gptel-agent-skills nil
  "Skill/prompt library system for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-skills-")

;;;; Customization Options

(defcustom gptel-agent-skills-dir
  (expand-file-name "gptel-agent/skills/" user-emacs-directory)
  "Global directory containing skill definition files.

Markdown (.md) and Org (.org) files in this directory will be
available as skills that can be loaded into any gptel-agent session."
  :type 'directory
  :group 'gptel-agent-skills)

(defcustom gptel-agent-skill-file-names '("SKILL.md" "SKILL.org")
  "File names to search for project-local skill definitions.

These files are searched in the project root directory."
  :type '(repeat string)
  :group 'gptel-agent-skills)

(defcustom gptel-agent-auto-load-skills t
  "Whether to automatically load project skills on gptel-agent invocation.

When non-nil, gptel-agent will check for SKILL.md/SKILL.org in the
project root and load it automatically if found."
  :type 'boolean
  :group 'gptel-agent-skills)

(defcustom gptel-agent-skills-version "0.1.0"
  "Current version for skill compatibility checking.

Skills can specify min-version and max-version in their metadata
to ensure compatibility."
  :type 'string
  :group 'gptel-agent-skills)

;;;; Internal State

(defvar-local gptel-agent--active-skills nil
  "List of currently active skill plists in this buffer.

Each skill is a plist containing parsed skill metadata and prompt content.")

(defvar-local gptel-agent--skill-load-order nil
  "List of skill names in the order they were loaded.

Used for resolving precedence in tool restrictions and prompt composition.")

(defvar gptel-agent--skill-cache nil
  "Cache of parsed skills from global skills directory.

Alist of (skill-name . skill-plist).")

(defvar gptel-agent--skill-cache-time nil
  "Time when the skill cache was last updated.")

;;;; Skill File Discovery

(defun gptel-agent--ensure-skills-dir ()
  "Ensure the global skills directory exists."
  (unless (file-directory-p gptel-agent-skills-dir)
    (make-directory gptel-agent-skills-dir t)))

(defun gptel-agent--project-root ()
  "Get the current project root directory."
  (when-let* ((proj (and (fboundp 'project-current)
                         (project-current nil))))
    (when (fboundp 'project-root)
      (project-root proj))))

(defun gptel-agent--discover-project-skill ()
  "Discover a skill file in the current project root.

Searches for SKILL.md or SKILL.org in the project root.
Returns the file path if found, nil otherwise."
  (when-let ((root (gptel-agent--project-root)))
    (cl-some
     (lambda (name)
       (let ((path (expand-file-name name root)))
         (when (file-exists-p path)
           path)))
     gptel-agent-skill-file-names)))

(defun gptel-agent--discover-global-skills ()
  "Discover all skill files in the global skills directory.

Returns an alist of (skill-name . file-path)."
  (gptel-agent--ensure-skills-dir)
  (let ((skills nil))
    (dolist (file (directory-files gptel-agent-skills-dir t "\\.\\(md\\|org\\)$"))
      (when (file-regular-p file)
        (let ((name (file-name-base file)))
          (push (cons name file) skills))))
    (nreverse skills)))

(defun gptel-agent--resolve-skill-path (skill-name)
  "Resolve SKILL-NAME to a file path.

Project-local skills take precedence over global skills.
SKILL-NAME can be:
- A skill name (string) to look up
- An absolute path to a skill file

Returns the file path or nil if not found."
  (cond
   ;; Absolute path
   ((and (stringp skill-name)
         (file-name-absolute-p skill-name)
         (file-exists-p skill-name))
    skill-name)

   ;; Check project root first
   ((let* ((root (gptel-agent--project-root))
           (project-skill (when root
                           (cl-some
                            (lambda (ext)
                              (let ((path (expand-file-name
                                          (concat skill-name ext) root)))
                                (when (file-exists-p path) path)))
                            '(".md" ".org" "")))))
      project-skill))

   ;; Check global skills directory
   (t (let ((global-skills (gptel-agent--discover-global-skills)))
        (cdr (assoc skill-name global-skills))))))

;;;; Skill Parsing

(defun gptel-agent--parse-skill-file (file-path)
  "Parse a skill file at FILE-PATH.

Returns a skill plist with parsed metadata and prompt content.
The plist includes:
  :name            - Skill name (string)
  :description     - Optional description (string)
  :prompt-position - 'append or 'prepend (default: 'append)
  :tools-allow     - List of allowed tool names
  :tools-deny      - List of denied tool names
  :context-files   - List of files to auto-include
  :requires        - List of required skill names (dependencies)
  :min-version     - Optional minimum version string
  :max-version     - Optional maximum version string
  :prompt-content  - The skill's prompt text"
  (unless (file-exists-p file-path)
    (error "Skill file not found: %s" file-path))

  (let* ((ext (file-name-extension file-path))
         (raw-plist (pcase ext
                      ("md" (gptel-agent-parse-markdown-frontmatter file-path))
                      ("org" (gptel-agent-parse-org-properties file-path))
                      (_ (error "Unsupported skill file type: %s" ext))))
         (tools (plist-get raw-plist :tools)))

    ;; Build normalized skill plist
    (list :name (or (plist-get raw-plist :name)
                    (file-name-base file-path))
          :description (plist-get raw-plist :description)
          :prompt-position (let ((pos (plist-get raw-plist :prompt-position)))
                            (if (stringp pos)
                                (intern pos)
                              (or pos 'append)))
          :tools-allow (when tools
                        (let ((allow (plist-get tools :allow)))
                          (if (vectorp allow)
                              (append allow nil)
                            allow)))
          :tools-deny (when tools
                       (let ((deny (plist-get tools :deny)))
                         (if (vectorp deny)
                             (append deny nil)
                           deny)))
          :context-files (let ((files (plist-get raw-plist :context-files)))
                          (if (vectorp files)
                              (append files nil)
                            files))
          :requires (let ((reqs (plist-get raw-plist :requires)))
                     (if (vectorp reqs)
                         (append reqs nil)
                       reqs))
          :min-version (plist-get raw-plist :min-version)
          :max-version (plist-get raw-plist :max-version)
          :prompt-content (plist-get raw-plist :system)
          :file-path file-path)))

(defun gptel-agent--validate-skill (skill-plist)
  "Validate SKILL-PLIST and return it if valid.

Signals an error if validation fails.
Returns the validated skill plist."
  (let ((name (plist-get skill-plist :name)))
    (unless name
      (error "Skill missing required field: name"))

    ;; Validate version compatibility if specified
    (when-let ((min-ver (plist-get skill-plist :min-version)))
      (when (version< gptel-agent-skills-version min-ver)
        (warn "Skill '%s' requires version %s, current is %s"
              name min-ver gptel-agent-skills-version)))

    (when-let ((max-ver (plist-get skill-plist :max-version)))
      (when (version< max-ver gptel-agent-skills-version)
        (warn "Skill '%s' is not compatible with version %s (max: %s)"
              name gptel-agent-skills-version max-ver)))

    skill-plist))

;;;; Skill Dependency Resolution

(defun gptel-agent--resolve-skill-dependencies (skill-names)
  "Resolve dependencies for SKILL-NAMES.

Returns a topologically sorted list of skill names including
all dependencies.  Signals an error if circular dependencies detected
or if a required skill is not found."
  (let ((visited (make-hash-table :test 'equal))
        (visiting (make-hash-table :test 'equal))
        (result nil))

    (cl-labels ((visit (name)
                  (cond
                   ((gethash name visited) nil)
                   ((gethash name visiting)
                    (error "Circular skill dependency detected: %s" name))
                   (t
                    (puthash name t visiting)
                    (let ((path (gptel-agent--resolve-skill-path name)))
                      (unless path
                        (error "Skill not found: %s" name))
                      (let* ((skill (gptel-agent--parse-skill-file path))
                             (deps (plist-get skill :requires)))
                        (dolist (dep deps)
                          (visit dep))))
                    (remhash name visiting)
                    (puthash name t visited)
                    (push name result)))))

      (dolist (name (if (listp skill-names) skill-names (list skill-names)))
        (visit name)))

    (nreverse result)))

;;;; Skill Composition

(defun gptel-agent--compose-skill-prompts (base-prompt)
  "Compose skill prompts with BASE-PROMPT.

Returns the composed prompt string with all active skill prompts
applied according to their prompt-position setting."
  (let ((prepended nil)
        (appended nil))

    (dolist (skill-name gptel-agent--skill-load-order)
      (when-let ((skill (cl-find skill-name gptel-agent--active-skills
                                :key (lambda (s) (plist-get s :name))
                                :test #'equal)))
        (let ((content (plist-get skill :prompt-content))
              (position (plist-get skill :prompt-position)))
          (when content
            (pcase position
              ('prepend (push content prepended))
              (_ (push content appended)))))))

    (concat
     (when prepended
       (concat (mapconcat #'identity (nreverse prepended) "\n\n") "\n\n"))
     base-prompt
     (when appended
       (concat "\n\n" (mapconcat #'identity (nreverse appended) "\n\n"))))))

(defun gptel-agent--compute-tool-restrictions ()
  "Compute effective tool restrictions from all active skills.

Returns a cons cell (allowed-tools . denied-tools).
Later-loaded skills have higher precedence for conflicts."
  (let ((allowed nil)
        (denied nil))

    (dolist (skill-name gptel-agent--skill-load-order)
      (when-let ((skill (cl-find skill-name gptel-agent--active-skills
                                :key (lambda (s) (plist-get s :name))
                                :test #'equal)))
        (when-let ((allow (plist-get skill :tools-allow)))
          (setq allowed (if allowed
                           (cl-intersection allowed allow :test #'equal)
                         allow)))
        (when-let ((deny (plist-get skill :tools-deny)))
          (setq denied (cl-union denied deny :test #'equal)))))

    (cons allowed denied)))

(defun gptel-agent--tool-allowed-p (tool-name)
  "Check if TOOL-NAME is allowed by current skill restrictions.

Returns non-nil if the tool is allowed."
  (let* ((restrictions (gptel-agent--compute-tool-restrictions))
         (allowed (car restrictions))
         (denied (cdr restrictions))
         (tool-str (if (symbolp tool-name)
                      (symbol-name tool-name)
                    tool-name)))
    (and (or (null allowed)
             (member tool-str allowed))
         (not (member tool-str denied)))))

(defun gptel-agent--load-context-files ()
  "Collect and format context files from all active skills.

Returns a string with the content of all context files,
or nil if no context files are specified."
  (let ((context-parts nil)
        (root (or (gptel-agent--project-root) default-directory)))

    (dolist (skill gptel-agent--active-skills)
      (dolist (file (plist-get skill :context-files))
        (let ((path (expand-file-name file root)))
          (when (file-exists-p path)
            (push (format "=== %s ===\n%s"
                         file
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string)))
                  context-parts)))))

    (when context-parts
      (mapconcat #'identity (nreverse context-parts) "\n\n"))))

;;;; Skill Loading API

;;;###autoload
(defun gptel-agent-load-skill (skill-name)
  "Load skill SKILL-NAME into the current buffer.

Interactively, prompts for skill name with completion.
If skill has dependencies, they are loaded first.
Project-local skills take precedence over global skills."
  (interactive
   (list (completing-read "Load skill: "
                         (gptel-agent--skill-completion-table)
                         nil t)))

  ;; Resolve dependencies
  (let ((to-load (gptel-agent--resolve-skill-dependencies skill-name)))

    (dolist (name to-load)
      (unless (gptel-agent--skill-loaded-p name)
        (let* ((path (gptel-agent--resolve-skill-path name))
               (skill (gptel-agent--validate-skill
                      (gptel-agent--parse-skill-file path))))
          (push skill gptel-agent--active-skills)
          (push name gptel-agent--skill-load-order)
          (message "Loaded skill: %s" name))))))

;;;###autoload
(defun gptel-agent-unload-skill (skill-name)
  "Unload skill SKILL-NAME from the current buffer.

Interactively, prompts for skill name with completion from active skills."
  (interactive
   (list (completing-read "Unload skill: "
                         (mapcar (lambda (s) (plist-get s :name))
                                gptel-agent--active-skills)
                         nil t)))

  (setq gptel-agent--active-skills
        (cl-remove skill-name gptel-agent--active-skills
                  :key (lambda (s) (plist-get s :name))
                  :test #'equal))
  (setq gptel-agent--skill-load-order
        (remove skill-name gptel-agent--skill-load-order))
  (message "Unloaded skill: %s" skill-name))

(defun gptel-agent--skill-loaded-p (skill-name)
  "Check if SKILL-NAME is currently loaded."
  (member skill-name gptel-agent--skill-load-order))

;;;###autoload
(defun gptel-agent-active-skills ()
  "Return list of currently active skill names."
  (mapcar (lambda (s) (plist-get s :name)) gptel-agent--active-skills))

;;;###autoload
(defun gptel-agent-available-skills ()
  "Return alist of all available skills (name . source).

Source is either 'project or 'global."
  (let ((skills nil))
    ;; Project skill
    (when-let ((project-skill (gptel-agent--discover-project-skill)))
      (let ((name (file-name-base project-skill)))
        (push (cons name 'project) skills)))
    ;; Global skills
    (dolist (entry (gptel-agent--discover-global-skills))
      (push (cons (car entry) 'global) skills))
    (nreverse skills)))

(defun gptel-agent--skill-completion-table ()
  "Return completion table for skill selection."
  (mapcar #'car (gptel-agent-available-skills)))

;;;; Interactive Commands

;;;###autoload
(defun gptel-agent-describe-skill (skill-name)
  "Display detailed information about SKILL-NAME."
  (interactive
   (list (completing-read "Describe skill: "
                         (gptel-agent--skill-completion-table)
                         nil t)))

  (let* ((path (gptel-agent--resolve-skill-path skill-name))
         (skill (when path (gptel-agent--parse-skill-file path))))
    (if (not skill)
        (message "Skill not found: %s" skill-name)
      (with-help-window "*Skill Info*"
        (with-current-buffer standard-output
          (insert (propertize (format "Skill: %s\n" (plist-get skill :name))
                             'face 'bold))
          (insert (make-string 60 ?-) "\n\n")

          (when-let ((desc (plist-get skill :description)))
            (insert (propertize "Description: " 'face 'bold))
            (insert desc "\n\n"))

          (insert (propertize "File: " 'face 'bold))
          (insert (plist-get skill :file-path) "\n\n")

          (insert (propertize "Prompt Position: " 'face 'bold))
          (insert (symbol-name (plist-get skill :prompt-position)) "\n\n")

          (when-let ((allow (plist-get skill :tools-allow)))
            (insert (propertize "Allowed Tools: " 'face 'bold))
            (insert (mapconcat #'identity allow ", ") "\n"))

          (when-let ((deny (plist-get skill :tools-deny)))
            (insert (propertize "Denied Tools: " 'face 'bold))
            (insert (mapconcat #'identity deny ", ") "\n"))

          (when-let ((files (plist-get skill :context-files)))
            (insert "\n" (propertize "Context Files:\n" 'face 'bold))
            (dolist (f files)
              (insert "  - " f "\n")))

          (when-let ((reqs (plist-get skill :requires)))
            (insert "\n" (propertize "Dependencies: " 'face 'bold))
            (insert (mapconcat #'identity reqs ", ") "\n"))

          (when-let ((content (plist-get skill :prompt-content)))
            (insert "\n" (propertize "Prompt Content:\n" 'face 'bold))
            (insert (make-string 40 ?-) "\n")
            (insert content)))))))

;;;###autoload
(defun gptel-agent-skills-status ()
  "Display current skill status for the buffer."
  (interactive)
  ;; Capture buffer-local variables before switching to help window
  (let ((skill-load-order gptel-agent--skill-load-order)
        (active-skills gptel-agent--active-skills)
        (restrictions (gptel-agent--compute-tool-restrictions))
        (available (gptel-agent-available-skills)))
    (with-help-window "*Skill Status*"
      (with-current-buffer standard-output
        (insert (propertize "GPTel Agent Skills Status\n" 'face 'bold))
        (insert (make-string 40 ?=) "\n\n")

        (insert (propertize "Active Skills:\n" 'face 'bold))
        (if skill-load-order
            (dolist (name skill-load-order)
              (let ((skill (cl-find name active-skills
                                   :key (lambda (s) (plist-get s :name))
                                   :test #'equal)))
                (insert (format "  - %s" name))
                (when-let ((desc (plist-get skill :description)))
                  (insert (format " (%s)" desc)))
                (insert "\n")))
          (insert "  (none)\n"))

        (insert "\n" (propertize "Tool Restrictions:\n" 'face 'bold))
        (if (and (null (car restrictions)) (null (cdr restrictions)))
            (insert "  (no restrictions)\n")
          (when (car restrictions)
            (insert "  Allowed: " (mapconcat #'identity (car restrictions) ", ") "\n"))
          (when (cdr restrictions)
            (insert "  Denied: " (mapconcat #'identity (cdr restrictions) ", ") "\n")))

        (insert "\n" (propertize "Available Skills:\n" 'face 'bold))
        (if available
            (dolist (entry available)
              (insert (format "  - %s [%s]\n" (car entry) (cdr entry))))
          (insert "  (none)\n"))))))

;;;; Automatic Loading

(defun gptel-agent--auto-load-project-skill ()
  "Automatically load project skill if present and auto-loading is enabled.

Called when gptel-agent is invoked."
  (when gptel-agent-auto-load-skills
    (when-let ((skill-file (gptel-agent--discover-project-skill)))
      (let ((name (file-name-base skill-file)))
        (unless (gptel-agent--skill-loaded-p name)
          (gptel-agent-load-skill name)
          (message "Auto-loaded project skill: %s" name))))))

;;;###autoload
(defun gptel-agent-reload-skills ()
  "Reload all currently active skills from their files."
  (interactive)
  (let ((names (copy-sequence gptel-agent--skill-load-order)))
    (setq gptel-agent--active-skills nil)
    (setq gptel-agent--skill-load-order nil)
    (dolist (name names)
      (gptel-agent-load-skill name))
    (message "Reloaded %d skill(s)" (length names))))

;;;; Clear skill cache

(defun gptel-agent-clear-skill-cache ()
  "Clear the skill cache to force re-parsing."
  (interactive)
  (setq gptel-agent--skill-cache nil)
  (setq gptel-agent--skill-cache-time nil)
  (message "Skill cache cleared"))

(provide 'gptel-agent-skills)
;;; gptel-agent-skills.el ends here
