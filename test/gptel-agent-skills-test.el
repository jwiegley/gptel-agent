;;; gptel-agent-skills-test.el --- Tests for gptel-agent-skills -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Keywords: tests
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tests for gptel-agent-skills.el skill loading system.
;;
;; Test coverage:
;; - Skill file discovery (project and global)
;; - Skill parsing from markdown and org formats
;; - Skill composition with multiple active skills
;; - Tool restriction application
;; - Context file loading
;; - Skill dependency resolution
;; - Version compatibility checking
;; - Interactive commands

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'gptel-agent-skills)

;;;; Test Helpers

(defvar gptel-agent-skills-test--temp-dirs nil
  "List of temporary directories to clean up after tests.")

(defun gptel-agent-skills-test--make-temp-dir ()
  "Create a temporary directory for testing.
Returns the path to the created directory."
  (let ((dir (make-temp-file "gptel-agent-skills-test-" t)))
    (push dir gptel-agent-skills-test--temp-dirs)
    dir))

(defun gptel-agent-skills-test--cleanup ()
  "Clean up temporary directories."
  (dolist (dir gptel-agent-skills-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq gptel-agent-skills-test--temp-dirs nil))

(defmacro gptel-agent-skills-test--with-temp-dir (dir-var &rest body)
  "Create temp directory, bind to DIR-VAR, execute BODY, cleanup."
  (declare (indent 1))
  `(unwind-protect
       (let ((,dir-var (gptel-agent-skills-test--make-temp-dir)))
         ,@body)
     (gptel-agent-skills-test--cleanup)))

(defun gptel-agent-skills-test--write-skill (dir name content)
  "Write skill CONTENT to NAME.md in DIR."
  (let ((file (expand-file-name (concat name ".md") dir)))
    (with-temp-file file
      (insert content))
    file))

(defun gptel-agent-skills-test--write-project-skill (dir content)
  "Write project skill CONTENT to SKILL.md in DIR."
  (gptel-agent-skills-test--write-skill dir "SKILL" content))

(defun gptel-agent-skills-test--setup ()
  "Set up test environment with clean state."
  (setq gptel-agent--active-skills nil)
  (setq gptel-agent--skill-load-order nil)
  (setq gptel-agent--skill-cache nil)
  (setq gptel-agent--skill-cache-time nil))

(defmacro gptel-agent-skills-test--with-clean-state (&rest body)
  "Execute BODY with clean test state."
  `(with-temp-buffer
     (gptel-agent-skills-test--setup)
     ,@body))

;;;; Skill File Discovery Tests

(ert-deftest gptel-agent-skills-test-ensure-skills-dir ()
  "Test skills directory creation."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir (expand-file-name "skills/" temp-dir)))
      (should-not (file-directory-p gptel-agent-skills-dir))
      (gptel-agent--ensure-skills-dir)
      (should (file-directory-p gptel-agent-skills-dir)))))

(ert-deftest gptel-agent-skills-test-discover-global-skills ()
  "Test discovery of skills in global directory."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      ;; Create some skill files
      (gptel-agent-skills-test--write-skill temp-dir "code-review"
        "---\nname: code-review\n---\nContent")
      (gptel-agent-skills-test--write-skill temp-dir "documentation"
        "---\nname: documentation\n---\nContent")
      ;; Discover skills
      (let ((skills (gptel-agent--discover-global-skills)))
        (should (= (length skills) 2))
        (should (assoc "code-review" skills))
        (should (assoc "documentation" skills))))))

(ert-deftest gptel-agent-skills-test-discover-global-skills-empty ()
  "Test discovery when no skills exist."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (let ((skills (gptel-agent--discover-global-skills)))
        (should (null skills))))))

(ert-deftest gptel-agent-skills-test-resolve-skill-path-absolute ()
  "Test skill resolution with absolute path."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "test"
                  "---\nname: test\n---\nContent")))
      (should (string= (gptel-agent--resolve-skill-path file) file)))))

(ert-deftest gptel-agent-skills-test-resolve-skill-path-global ()
  "Test skill resolution from global directory."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (gptel-agent-skills-test--write-skill temp-dir "my-skill"
        "---\nname: my-skill\n---\nContent")
      (let ((path (gptel-agent--resolve-skill-path "my-skill")))
        (should path)
        (should (string-suffix-p "my-skill.md" path))))))

(ert-deftest gptel-agent-skills-test-resolve-skill-path-not-found ()
  "Test skill resolution returns nil for missing skill."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (should (null (gptel-agent--resolve-skill-path "nonexistent"))))))

;;;; Skill Parsing Tests

(ert-deftest gptel-agent-skills-test-parse-basic ()
  "Test parsing a basic skill file."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "test"
                  "---
name: test-skill
description: A test skill
---
This is the prompt content.")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (string= (plist-get skill :name) "test-skill"))
        (should (string= (plist-get skill :description) "A test skill"))
        (should (string-match-p "prompt content" (plist-get skill :prompt-content)))))))

(ert-deftest gptel-agent-skills-test-parse-prompt-position-append ()
  "Test parsing prompt-position: append."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "test"
                  "---
name: test
prompt-position: append
---
Content")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (eq (plist-get skill :prompt-position) 'append))))))

(ert-deftest gptel-agent-skills-test-parse-prompt-position-prepend ()
  "Test parsing prompt-position: prepend."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "test"
                  "---
name: test
prompt-position: prepend
---
Content")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (eq (plist-get skill :prompt-position) 'prepend))))))

(ert-deftest gptel-agent-skills-test-parse-tools-allow ()
  "Test parsing tools allow list."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "test"
                  "---
name: test
tools:
  allow:
    - Read
    - Grep
---
Content")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (member "Read" (plist-get skill :tools-allow)))
        (should (member "Grep" (plist-get skill :tools-allow)))))))

(ert-deftest gptel-agent-skills-test-parse-tools-deny ()
  "Test parsing tools deny list."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "test"
                  "---
name: test
tools:
  deny:
    - Bash
    - Write
---
Content")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (member "Bash" (plist-get skill :tools-deny)))
        (should (member "Write" (plist-get skill :tools-deny)))))))

(ert-deftest gptel-agent-skills-test-parse-context-files ()
  "Test parsing context-files list."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "test"
                  "---
name: test
context-files:
  - README.md
  - .eslintrc.json
---
Content")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (member "README.md" (plist-get skill :context-files)))
        (should (member ".eslintrc.json" (plist-get skill :context-files)))))))

(ert-deftest gptel-agent-skills-test-parse-requires ()
  "Test parsing skill dependencies."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "test"
                  "---
name: test
requires:
  - base-skill
  - common-utils
---
Content")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (member "base-skill" (plist-get skill :requires)))
        (should (member "common-utils" (plist-get skill :requires)))))))

(ert-deftest gptel-agent-skills-test-parse-version-constraints ()
  "Test parsing version constraints."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "test"
                  "---
name: test
min-version: 0.1.0
max-version: 1.0.0
---
Content")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (string= (plist-get skill :min-version) "0.1.0"))
        (should (string= (plist-get skill :max-version) "1.0.0"))))))

(ert-deftest gptel-agent-skills-test-parse-name-from-filename ()
  "Test that name defaults to filename if not specified."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "fallback-name"
                  "---
description: No name specified
---
Content")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (string= (plist-get skill :name) "fallback-name"))))))

;;;; Skill Validation Tests

(ert-deftest gptel-agent-skills-test-validate-success ()
  "Test validation of valid skill."
  (let ((skill (list :name "valid-skill"
                    :description "A valid skill"
                    :prompt-position 'append)))
    (should (gptel-agent--validate-skill skill))))

(ert-deftest gptel-agent-skills-test-validate-missing-name ()
  "Test validation fails for skill without name."
  (let ((skill (list :description "No name")))
    (should-error (gptel-agent--validate-skill skill))))

;;;; Dependency Resolution Tests

(ert-deftest gptel-agent-skills-test-resolve-deps-no-deps ()
  "Test dependency resolution with no dependencies."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (gptel-agent-skills-test--write-skill temp-dir "standalone"
        "---
name: standalone
---
Content")
      (let ((order (gptel-agent--resolve-skill-dependencies "standalone")))
        (should (equal order '("standalone")))))))

(ert-deftest gptel-agent-skills-test-resolve-deps-linear ()
  "Test dependency resolution with linear chain."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (gptel-agent-skills-test--write-skill temp-dir "base"
        "---
name: base
---
Base content")
      (gptel-agent-skills-test--write-skill temp-dir "derived"
        "---
name: derived
requires:
  - base
---
Derived content")
      (let ((order (gptel-agent--resolve-skill-dependencies "derived")))
        (should (= (length order) 2))
        (should (member "base" order))
        (should (member "derived" order))
        ;; base should come before derived
        (should (< (cl-position "base" order :test #'equal)
                   (cl-position "derived" order :test #'equal)))))))

(ert-deftest gptel-agent-skills-test-resolve-deps-circular ()
  "Test that circular dependencies are detected."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (gptel-agent-skills-test--write-skill temp-dir "skill-a"
        "---
name: skill-a
requires:
  - skill-b
---
Content A")
      (gptel-agent-skills-test--write-skill temp-dir "skill-b"
        "---
name: skill-b
requires:
  - skill-a
---
Content B")
      (should-error (gptel-agent--resolve-skill-dependencies "skill-a")))))

;;;; Skill Composition Tests

(ert-deftest gptel-agent-skills-test-compose-empty ()
  "Test prompt composition with no skills."
  (gptel-agent-skills-test--with-clean-state
    (let ((result (gptel-agent--compose-skill-prompts "base prompt")))
      (should (string= result "base prompt")))))

(ert-deftest gptel-agent-skills-test-compose-append ()
  "Test prompt composition with append position."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "test"
               :prompt-position 'append
               :prompt-content "Appended content")
          gptel-agent--active-skills)
    (push "test" gptel-agent--skill-load-order)
    (let ((result (gptel-agent--compose-skill-prompts "base")))
      (should (string-prefix-p "base" result))
      (should (string-match-p "Appended content" result)))))

(ert-deftest gptel-agent-skills-test-compose-prepend ()
  "Test prompt composition with prepend position."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "test"
               :prompt-position 'prepend
               :prompt-content "Prepended content")
          gptel-agent--active-skills)
    (push "test" gptel-agent--skill-load-order)
    (let ((result (gptel-agent--compose-skill-prompts "base")))
      (should (string-prefix-p "Prepended content" result))
      (should (string-match-p "base" result)))))

(ert-deftest gptel-agent-skills-test-compose-multiple ()
  "Test prompt composition with multiple skills."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "prepend-skill"
               :prompt-position 'prepend
               :prompt-content "PREPEND")
          gptel-agent--active-skills)
    (push (list :name "append-skill"
               :prompt-position 'append
               :prompt-content "APPEND")
          gptel-agent--active-skills)
    (setq gptel-agent--skill-load-order '("prepend-skill" "append-skill"))
    (let ((result (gptel-agent--compose-skill-prompts "BASE")))
      (should (string-match-p "^PREPEND" result))
      (should (string-match-p "BASE" result))
      (should (string-match-p "APPEND$" result)))))

;;;; Tool Restriction Tests

(ert-deftest gptel-agent-skills-test-tool-restrictions-empty ()
  "Test tool restrictions with no skills."
  (gptel-agent-skills-test--with-clean-state
    (let ((restrictions (gptel-agent--compute-tool-restrictions)))
      (should (null (car restrictions)))
      (should (null (cdr restrictions))))))

(ert-deftest gptel-agent-skills-test-tool-restrictions-allow ()
  "Test tool allow list restrictions."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "test"
               :tools-allow '("Read" "Grep"))
          gptel-agent--active-skills)
    (push "test" gptel-agent--skill-load-order)
    (let ((restrictions (gptel-agent--compute-tool-restrictions)))
      (should (member "Read" (car restrictions)))
      (should (member "Grep" (car restrictions)))
      (should-not (member "Write" (car restrictions))))))

(ert-deftest gptel-agent-skills-test-tool-restrictions-deny ()
  "Test tool deny list restrictions."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "test"
               :tools-deny '("Bash" "Write"))
          gptel-agent--active-skills)
    (push "test" gptel-agent--skill-load-order)
    (let ((restrictions (gptel-agent--compute-tool-restrictions)))
      (should (member "Bash" (cdr restrictions)))
      (should (member "Write" (cdr restrictions))))))

(ert-deftest gptel-agent-skills-test-tool-allowed-p-no-restrictions ()
  "Test tool allowed with no restrictions."
  (gptel-agent-skills-test--with-clean-state
    (should (gptel-agent--tool-allowed-p "Read"))
    (should (gptel-agent--tool-allowed-p "Bash"))))

(ert-deftest gptel-agent-skills-test-tool-allowed-p-whitelist ()
  "Test tool allowed with whitelist."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "test"
               :tools-allow '("Read" "Grep"))
          gptel-agent--active-skills)
    (push "test" gptel-agent--skill-load-order)
    (should (gptel-agent--tool-allowed-p "Read"))
    (should-not (gptel-agent--tool-allowed-p "Bash"))))

(ert-deftest gptel-agent-skills-test-tool-allowed-p-blacklist ()
  "Test tool allowed with blacklist."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "test"
               :tools-deny '("Bash"))
          gptel-agent--active-skills)
    (push "test" gptel-agent--skill-load-order)
    (should (gptel-agent--tool-allowed-p "Read"))
    (should-not (gptel-agent--tool-allowed-p "Bash"))))

;;;; Context File Loading Tests

(ert-deftest gptel-agent-skills-test-load-context-empty ()
  "Test context loading with no context files."
  (gptel-agent-skills-test--with-clean-state
    (should (null (gptel-agent--load-context-files)))))

(ert-deftest gptel-agent-skills-test-load-context-files ()
  "Test context file loading."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--with-clean-state
      (let ((default-directory temp-dir))
        ;; Create context file
        (with-temp-file (expand-file-name "context.txt" temp-dir)
          (insert "Context content"))
        ;; Add skill with context file
        (push (list :name "test"
                   :context-files '("context.txt"))
              gptel-agent--active-skills)
        (let ((context (gptel-agent--load-context-files)))
          (should context)
          (should (string-match-p "Context content" context)))))))

;;;; Skill Loading API Tests

(ert-deftest gptel-agent-skills-test-active-skills-empty ()
  "Test active skills returns empty list initially."
  (gptel-agent-skills-test--with-clean-state
    (should (null (gptel-agent-active-skills)))))

(ert-deftest gptel-agent-skills-test-skill-loaded-p ()
  "Test checking if skill is loaded."
  (gptel-agent-skills-test--with-clean-state
    (should-not (gptel-agent--skill-loaded-p "test"))
    (push "test" gptel-agent--skill-load-order)
    (should (gptel-agent--skill-loaded-p "test"))))

(ert-deftest gptel-agent-skills-test-available-skills ()
  "Test listing available skills."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (gptel-agent-skills-test--write-skill temp-dir "available"
        "---\nname: available\n---\nContent")
      (let ((skills (gptel-agent-available-skills)))
        (should (assoc "available" skills))
        (should (eq (cdr (assoc "available" skills)) 'global))))))

(ert-deftest gptel-agent-skills-test-completion-table ()
  "Test completion table returns skill names."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (gptel-agent-skills-test--write-skill temp-dir "complete-me"
        "---\nname: complete-me\n---\nContent")
      (let ((table (gptel-agent--skill-completion-table)))
        (should (member "complete-me" table))))))

;;;; Customization Tests

(ert-deftest gptel-agent-skills-test-auto-load-default ()
  "Test default value for auto-load-skills."
  (should (default-value 'gptel-agent-auto-load-skills)))

(ert-deftest gptel-agent-skills-test-skill-file-names-default ()
  "Test default skill file names."
  (let ((names (default-value 'gptel-agent-skill-file-names)))
    (should (member "SKILL.md" names))
    (should (member "SKILL.org" names))))

(ert-deftest gptel-agent-skills-test-skills-version-default ()
  "Test skills version is set."
  (should (stringp gptel-agent-skills-version)))

(ert-deftest gptel-agent-skills-test-custom-group-exists ()
  "Test customization group is defined."
  (should (get 'gptel-agent-skills 'custom-group)))

(ert-deftest gptel-agent-skills-test-skills-dir-default ()
  "Test default skills directory."
  (should (stringp (default-value 'gptel-agent-skills-dir)))
  (should (string-match-p "skills" (default-value 'gptel-agent-skills-dir))))

;;;; Project Root Tests

(ert-deftest gptel-agent-skills-test-project-root-with-project ()
  "Test project root detection with project."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    ;; Create a simple git repo
    (let ((default-directory temp-dir))
      (make-directory (expand-file-name ".git" temp-dir))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&optional _maybe-prompt) (cons 'vc temp-dir)))
                ((symbol-function 'project-root)
                 (lambda (proj) (cdr proj))))
        (let ((root (gptel-agent--project-root)))
          (should root)
          (should (string= root temp-dir)))))))

(ert-deftest gptel-agent-skills-test-project-root-no-project ()
  "Test project root detection without project."
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _maybe-prompt) nil)))
    (should (null (gptel-agent--project-root)))))

;;;; Project Skill Discovery Tests

(ert-deftest gptel-agent-skills-test-discover-project-skill ()
  "Test discovering project skill file."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--write-project-skill temp-dir
      "---\nname: project-skill\n---\nContent")
    (cl-letf (((symbol-function 'gptel-agent--project-root)
               (lambda () temp-dir)))
      (let ((skill-file (gptel-agent--discover-project-skill)))
        (should skill-file)
        (should (string-match-p "SKILL.md" skill-file))))))

(ert-deftest gptel-agent-skills-test-discover-project-skill-org ()
  "Test discovering project skill as org file."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    ;; Write as org file
    (with-temp-file (expand-file-name "SKILL.org" temp-dir)
      (insert "* Skill\n:PROPERTIES:\n:name: org-skill\n:END:\nContent"))
    (cl-letf (((symbol-function 'gptel-agent--project-root)
               (lambda () temp-dir)))
      (let ((skill-file (gptel-agent--discover-project-skill)))
        (should skill-file)
        (should (string-match-p "SKILL.org" skill-file))))))

(ert-deftest gptel-agent-skills-test-discover-project-skill-none ()
  "Test discovering project skill when none exists."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (cl-letf (((symbol-function 'gptel-agent--project-root)
               (lambda () temp-dir)))
      (should (null (gptel-agent--discover-project-skill))))))

(ert-deftest gptel-agent-skills-test-discover-project-skill-no-project ()
  "Test discovering project skill without project."
  (cl-letf (((symbol-function 'gptel-agent--project-root)
             (lambda () nil)))
    (should (null (gptel-agent--discover-project-skill)))))

;;;; Skill Loading Tests

(ert-deftest gptel-agent-skills-test-load-skill ()
  "Test loading a skill."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--with-clean-state
      (let ((gptel-agent-skills-dir temp-dir))
        (gptel-agent-skills-test--write-skill temp-dir "loadable"
          "---
name: loadable
description: A loadable skill
---
Loadable content")
        (gptel-agent-load-skill "loadable")
        (should (gptel-agent--skill-loaded-p "loadable"))
        (should (member "loadable" (gptel-agent-active-skills)))))))

(ert-deftest gptel-agent-skills-test-load-skill-with-deps ()
  "Test loading a skill with dependencies."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--with-clean-state
      (let ((gptel-agent-skills-dir temp-dir))
        ;; Create base skill
        (gptel-agent-skills-test--write-skill temp-dir "base"
          "---\nname: base\n---\nBase")
        ;; Create derived skill
        (gptel-agent-skills-test--write-skill temp-dir "derived"
          "---
name: derived
requires:
  - base
---
Derived")
        (gptel-agent-load-skill "derived")
        (should (gptel-agent--skill-loaded-p "base"))
        (should (gptel-agent--skill-loaded-p "derived"))))))

(ert-deftest gptel-agent-skills-test-load-skill-already-loaded ()
  "Test loading an already loaded skill is idempotent."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--with-clean-state
      (let ((gptel-agent-skills-dir temp-dir))
        (gptel-agent-skills-test--write-skill temp-dir "once"
          "---\nname: once\n---\nContent")
        (gptel-agent-load-skill "once")
        (gptel-agent-load-skill "once")  ; Load again
        ;; Should only be loaded once
        (should (= (length gptel-agent--skill-load-order) 1))))))

;;;; Skill Unloading Tests

(ert-deftest gptel-agent-skills-test-unload-skill ()
  "Test unloading a skill."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--with-clean-state
      (let ((gptel-agent-skills-dir temp-dir))
        (gptel-agent-skills-test--write-skill temp-dir "unloadable"
          "---\nname: unloadable\n---\nContent")
        (gptel-agent-load-skill "unloadable")
        (should (gptel-agent--skill-loaded-p "unloadable"))
        (gptel-agent-unload-skill "unloadable")
        (should-not (gptel-agent--skill-loaded-p "unloadable"))
        (should (null (gptel-agent-active-skills)))))))

(ert-deftest gptel-agent-skills-test-unload-updates-lists ()
  "Test unloading skill updates both lists."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "test-skill") gptel-agent--active-skills)
    (push "test-skill" gptel-agent--skill-load-order)
    (gptel-agent-unload-skill "test-skill")
    (should (null gptel-agent--active-skills))
    (should (null gptel-agent--skill-load-order))))

;;;; Reload Skills Tests

(ert-deftest gptel-agent-skills-test-reload-skills ()
  "Test reloading all skills."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--with-clean-state
      (let ((gptel-agent-skills-dir temp-dir)
            (load-count 0))
        (gptel-agent-skills-test--write-skill temp-dir "reload-test"
          "---\nname: reload-test\n---\nContent")
        (gptel-agent-load-skill "reload-test")
        (cl-letf (((symbol-function 'gptel-agent-load-skill)
                   (lambda (_name) (cl-incf load-count))))
          (gptel-agent-reload-skills))
        (should (= load-count 1))))))

(ert-deftest gptel-agent-skills-test-reload-skills-empty ()
  "Test reloading with no active skills."
  (gptel-agent-skills-test--with-clean-state
    ;; Should not error
    (gptel-agent-reload-skills)))

;;;; Auto Load Tests

(ert-deftest gptel-agent-skills-test-auto-load-when-enabled ()
  "Test auto-load project skill when enabled."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--with-clean-state
      (let ((gptel-agent-auto-load-skills t)
            (gptel-agent-skills-dir temp-dir)
            (skill-loaded nil))
        (gptel-agent-skills-test--write-project-skill temp-dir
          "---\nname: project\n---\nContent")
        (cl-letf (((symbol-function 'gptel-agent--discover-project-skill)
                   (lambda () (expand-file-name "SKILL.md" temp-dir)))
                  ((symbol-function 'gptel-agent-load-skill)
                   (lambda (_name) (setq skill-loaded t))))
          (gptel-agent--auto-load-project-skill)
          (should skill-loaded))))))

(ert-deftest gptel-agent-skills-test-auto-load-when-disabled ()
  "Test auto-load disabled."
  (gptel-agent-skills-test--with-clean-state
    (let ((gptel-agent-auto-load-skills nil)
          (skill-loaded nil))
      (cl-letf (((symbol-function 'gptel-agent-load-skill)
                 (lambda (_name) (setq skill-loaded t))))
        (gptel-agent--auto-load-project-skill)
        (should-not skill-loaded)))))

(ert-deftest gptel-agent-skills-test-auto-load-no-project-skill ()
  "Test auto-load when no project skill exists."
  (gptel-agent-skills-test--with-clean-state
    (let ((gptel-agent-auto-load-skills t)
          (skill-loaded nil))
      (cl-letf (((symbol-function 'gptel-agent--discover-project-skill)
                 (lambda () nil))
                ((symbol-function 'gptel-agent-load-skill)
                 (lambda (_name) (setq skill-loaded t))))
        (gptel-agent--auto-load-project-skill)
        (should-not skill-loaded)))))

(ert-deftest gptel-agent-skills-test-auto-load-already-loaded ()
  "Test auto-load doesn't reload if already loaded."
  (gptel-agent-skills-test--with-clean-state
    (push "SKILL" gptel-agent--skill-load-order)
    (let ((gptel-agent-auto-load-skills t)
          (skill-loaded nil))
      (cl-letf (((symbol-function 'gptel-agent--discover-project-skill)
                 (lambda () "/path/to/SKILL.md"))
                ((symbol-function 'gptel-agent-load-skill)
                 (lambda (_name) (setq skill-loaded t))))
        (gptel-agent--auto-load-project-skill)
        (should-not skill-loaded)))))

;;;; Cache Tests

(ert-deftest gptel-agent-skills-test-clear-skill-cache ()
  "Test clearing skill cache."
  (setq gptel-agent--skill-cache '(("test" . (:name "test"))))
  (setq gptel-agent--skill-cache-time (current-time))
  (gptel-agent-clear-skill-cache)
  (should (null gptel-agent--skill-cache))
  (should (null gptel-agent--skill-cache-time)))

;;;; Tool Allowed Symbol Name Tests

(ert-deftest gptel-agent-skills-test-tool-allowed-symbol ()
  "Test tool-allowed-p with symbol name."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "test"
               :tools-allow '("Read"))
          gptel-agent--active-skills)
    (push "test" gptel-agent--skill-load-order)
    ;; Symbol should be converted to string
    (should (gptel-agent--tool-allowed-p 'Read))
    (should-not (gptel-agent--tool-allowed-p 'Bash))))

;;;; Multiple Skills Tool Intersection Tests

(ert-deftest gptel-agent-skills-test-tool-intersection ()
  "Test tool allow list intersection with multiple skills."
  (gptel-agent-skills-test--with-clean-state
    ;; First skill allows Read, Grep, Write
    (push (list :name "skill1"
               :tools-allow '("Read" "Grep" "Write"))
          gptel-agent--active-skills)
    ;; Second skill only allows Read, Grep
    (push (list :name "skill2"
               :tools-allow '("Read" "Grep"))
          gptel-agent--active-skills)
    (setq gptel-agent--skill-load-order '("skill1" "skill2"))
    (let ((restrictions (gptel-agent--compute-tool-restrictions)))
      ;; Intersection should be Read, Grep only
      (should (member "Read" (car restrictions)))
      (should (member "Grep" (car restrictions)))
      (should-not (member "Write" (car restrictions))))))

(ert-deftest gptel-agent-skills-test-tool-deny-union ()
  "Test tool deny list union with multiple skills."
  (gptel-agent-skills-test--with-clean-state
    ;; First skill denies Bash
    (push (list :name "skill1"
               :tools-deny '("Bash"))
          gptel-agent--active-skills)
    ;; Second skill denies Write
    (push (list :name "skill2"
               :tools-deny '("Write"))
          gptel-agent--active-skills)
    (setq gptel-agent--skill-load-order '("skill1" "skill2"))
    (let ((restrictions (gptel-agent--compute-tool-restrictions)))
      ;; Union should include both Bash and Write
      (should (member "Bash" (cdr restrictions)))
      (should (member "Write" (cdr restrictions))))))

;;;; Describe Skill Tests

(ert-deftest gptel-agent-skills-test-describe-skill-not-found ()
  "Test describe skill when skill not found."
  (cl-letf (((symbol-function 'gptel-agent--resolve-skill-path)
             (lambda (_name) nil)))
    (with-temp-buffer
      (let ((message-called nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest _) (setq message-called t))))
          (gptel-agent-describe-skill "nonexistent")
          (should message-called))))))

;;;; Skills Status Tests

(ert-deftest gptel-agent-skills-test-status-empty ()
  "Test status display with no skills."
  (gptel-agent-skills-test--with-clean-state
    ;; Since with-help-window is a macro, we can't mock it directly.
    ;; Instead, let the function run and check the resulting buffer.
    (gptel-agent-skills-status)
    (let ((buf (get-buffer "*Skill Status*")))
      (should buf)
      (unwind-protect
          (with-current-buffer buf
            (should (string-match-p "Active Skills" (buffer-string)))
            (should (string-match-p "(none)" (buffer-string))))
        (kill-buffer buf)))))

(ert-deftest gptel-agent-skills-test-status-with-skills ()
  "Test status display with active skills."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "active-skill"
               :description "An active skill"
               :tools-allow '("Read")
               :tools-deny '("Bash"))
          gptel-agent--active-skills)
    (push "active-skill" gptel-agent--skill-load-order)
    ;; Since with-help-window is a macro, we can't mock it directly.
    ;; Instead, let the function run and check the resulting buffer.
    (gptel-agent-skills-status)
    (let ((buf (get-buffer "*Skill Status*")))
      (should buf)
      (unwind-protect
          (with-current-buffer buf
            (should (string-match-p "active-skill" (buffer-string)))
            (should (string-match-p "An active skill" (buffer-string))))
        (kill-buffer buf)))))

;;;; Version Compatibility Tests

(ert-deftest gptel-agent-skills-test-validate-min-version-ok ()
  "Test validation passes when min-version is satisfied."
  (let ((gptel-agent-skills-version "1.0.0"))
    (let ((skill (list :name "test"
                      :min-version "0.5.0")))
      (should (gptel-agent--validate-skill skill)))))

(ert-deftest gptel-agent-skills-test-validate-min-version-warning ()
  "Test validation warns when min-version not satisfied."
  (let ((gptel-agent-skills-version "0.1.0")
        (warning-displayed nil))
    (cl-letf (((symbol-function 'warn)
               (lambda (&rest _) (setq warning-displayed t))))
      (let ((skill (list :name "test"
                        :min-version "1.0.0")))
        (gptel-agent--validate-skill skill)
        (should warning-displayed)))))

(ert-deftest gptel-agent-skills-test-validate-max-version-warning ()
  "Test validation warns when max-version exceeded."
  (let ((gptel-agent-skills-version "2.0.0")
        (warning-displayed nil))
    (cl-letf (((symbol-function 'warn)
               (lambda (&rest _) (setq warning-displayed t))))
      (let ((skill (list :name "test"
                        :max-version "1.0.0")))
        (gptel-agent--validate-skill skill)
        (should warning-displayed)))))

;;;; Parsing Edge Cases

(ert-deftest gptel-agent-skills-test-parse-file-not-found ()
  "Test parsing nonexistent file."
  (should-error (gptel-agent--parse-skill-file "/nonexistent/skill.md")))

(ert-deftest gptel-agent-skills-test-parse-unsupported-extension ()
  "Test parsing unsupported file type."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (expand-file-name "skill.txt" temp-dir)))
      (with-temp-file file (insert "content"))
      (should-error (gptel-agent--parse-skill-file file)))))

(ert-deftest gptel-agent-skills-test-parse-default-prompt-position ()
  "Test default prompt position is append."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((file (gptel-agent-skills-test--write-skill temp-dir "no-pos"
                  "---\nname: no-pos\n---\nContent")))
      (let ((skill (gptel-agent--parse-skill-file file)))
        (should (eq (plist-get skill :prompt-position) 'append))))))

;;;; Context File Edge Cases

(ert-deftest gptel-agent-skills-test-context-file-not-found ()
  "Test context loading when file doesn't exist."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--with-clean-state
      (let ((default-directory temp-dir))
        ;; Add skill with missing context file
        (push (list :name "test"
                   :context-files '("nonexistent.txt"))
              gptel-agent--active-skills)
        ;; Should not include missing file
        (let ((context (gptel-agent--load-context-files)))
          (should (null context)))))))

(ert-deftest gptel-agent-skills-test-context-multiple-files ()
  "Test context loading with multiple files."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (gptel-agent-skills-test--with-clean-state
      (let ((default-directory temp-dir))
        ;; Create context files
        (with-temp-file (expand-file-name "file1.txt" temp-dir)
          (insert "Content 1"))
        (with-temp-file (expand-file-name "file2.txt" temp-dir)
          (insert "Content 2"))
        ;; Add skill
        (push (list :name "test"
                   :context-files '("file1.txt" "file2.txt"))
              gptel-agent--active-skills)
        (let ((context (gptel-agent--load-context-files)))
          (should context)
          (should (string-match-p "Content 1" context))
          (should (string-match-p "Content 2" context)))))))

;;;; Buffer Local Variable Tests

(ert-deftest gptel-agent-skills-test-active-skills-buffer-local ()
  "Test active skills is buffer-local."
  (with-temp-buffer
    (push (list :name "local") gptel-agent--active-skills)
    (with-temp-buffer
      (should (null gptel-agent--active-skills)))
    (should gptel-agent--active-skills)))

(ert-deftest gptel-agent-skills-test-load-order-buffer-local ()
  "Test skill load order is buffer-local."
  (with-temp-buffer
    (push "local-skill" gptel-agent--skill-load-order)
    (with-temp-buffer
      (should (null gptel-agent--skill-load-order)))
    (should (member "local-skill" gptel-agent--skill-load-order))))

;;;; Dependency Resolution Edge Cases

(ert-deftest gptel-agent-skills-test-resolve-deps-missing ()
  "Test dependency resolution with missing dependency."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (gptel-agent-skills-test--write-skill temp-dir "needs-missing"
        "---
name: needs-missing
requires:
  - nonexistent
---
Content")
      ;; Should error when dependency not found
      (should-error (gptel-agent--resolve-skill-dependencies "needs-missing")))))

(ert-deftest gptel-agent-skills-test-resolve-deps-list-input ()
  "Test dependency resolution accepts list input."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir))
      (gptel-agent-skills-test--write-skill temp-dir "skill1"
        "---\nname: skill1\n---\nContent")
      (gptel-agent-skills-test--write-skill temp-dir "skill2"
        "---\nname: skill2\n---\nContent")
      (let ((order (gptel-agent--resolve-skill-dependencies '("skill1" "skill2"))))
        (should (= (length order) 2))
        (should (member "skill1" order))
        (should (member "skill2" order))))))

;;;; Skill Resolution Precedence Tests

(ert-deftest gptel-agent-skills-test-project-skill-precedence ()
  "Test project skills take precedence over global."
  (gptel-agent-skills-test--with-temp-dir temp-dir
    (let ((gptel-agent-skills-dir temp-dir)
          (project-dir (gptel-agent-skills-test--make-temp-dir)))
      ;; Create global skill
      (gptel-agent-skills-test--write-skill temp-dir "common"
        "---\nname: common-global\n---\nGlobal")
      ;; Create project skill with same filename
      (gptel-agent-skills-test--write-skill project-dir "common"
        "---\nname: common-project\n---\nProject")
      (cl-letf (((symbol-function 'gptel-agent--project-root)
                 (lambda () project-dir)))
        (let ((path (gptel-agent--resolve-skill-path "common")))
          (should path)
          (should (string-prefix-p project-dir path)))))))

;;;; Compose Skills Empty Content Tests

(ert-deftest gptel-agent-skills-test-compose-nil-content ()
  "Test prompt composition with nil content."
  (gptel-agent-skills-test--with-clean-state
    (push (list :name "nil-content"
               :prompt-position 'append
               :prompt-content nil)
          gptel-agent--active-skills)
    (push "nil-content" gptel-agent--skill-load-order)
    (let ((result (gptel-agent--compose-skill-prompts "base")))
      ;; Should just return base with no extra content
      (should (string= result "base")))))

(provide 'gptel-agent-skills-test)
;;; gptel-agent-skills-test.el ends here
