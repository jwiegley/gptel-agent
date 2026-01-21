;;; gptel-agent-permissions-test.el --- Tests for gptel-agent-permissions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: January 2025
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ert "0.1"))
;; Keywords: tests

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive test suite for gptel-agent-permissions module.
;; Tests pattern matching, permission resolution, configuration loading,
;; caching, and integration scenarios.

;;; Code:

(require 'ert)
(require 'gptel-agent-permissions)

;;; Test Helpers

(defvar gptel-agent-permissions-test--temp-dirs nil
  "List of temporary directories to clean up after tests.")

(defun gptel-agent-permissions-test--make-temp-project ()
  "Create a temporary directory simulating a project root.
Returns the absolute path to the directory."
  (let ((dir (make-temp-file "gptel-agent-test-" t)))
    (push dir gptel-agent-permissions-test--temp-dirs)
    dir))

(defun gptel-agent-permissions-test--write-config (dir config-form)
  "Write CONFIG-FORM to .gptel-agent.el in DIR."
  (let ((config-path (expand-file-name ".gptel-agent.el" dir)))
    (with-temp-file config-path
      (prin1 config-form (current-buffer)))
    config-path))

(defun gptel-agent-permissions-test--cleanup ()
  "Clean up temporary directories and reset state."
  (dolist (dir gptel-agent-permissions-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq gptel-agent-permissions-test--temp-dirs nil)
  (gptel-agent--invalidate-permission-cache))

(defmacro gptel-agent-permissions-test--with-cleanup (&rest body)
  "Execute BODY with automatic cleanup afterward."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (gptel-agent-permissions-test--cleanup)))

;;; Pattern Matching Tests

(ert-deftest gptel-agent-permissions-test-build-tool-call-string-bash ()
  "Test tool call string building for bash commands."
  (should (string-equal
           (gptel-agent--build-tool-call-string "bash" '(:command "ls -la"))
           "ls -la"))

  (should (string-equal
           (gptel-agent--build-tool-call-string "bash" '(:cmd "git status"))
           "git status"))

  (should (string-equal
           (gptel-agent--build-tool-call-string 'bash '("rm -rf /tmp/test"))
           "rm -rf /tmp/test"))

  ;; Empty command
  (should (string-equal
           (gptel-agent--build-tool-call-string "bash" '())
           "")))

(ert-deftest gptel-agent-permissions-test-build-tool-call-string-other-tools ()
  "Test tool call string building for non-bash tools."
  ;; Plist args
  (should (string-equal
           (gptel-agent--build-tool-call-string "edit" '(:file "/tmp/test.txt" :content "hello"))
           "edit /tmp/test.txt hello"))

  ;; List args
  (should (string-equal
           (gptel-agent--build-tool-call-string "read" '("/tmp/file.txt" "100"))
           "read /tmp/file.txt 100"))

  ;; Single arg
  (should (string-equal
           (gptel-agent--build-tool-call-string "grep" "pattern")
           "grep pattern"))

  ;; No args
  (should (string-equal
           (gptel-agent--build-tool-call-string "status" nil)
           "status")))

(ert-deftest gptel-agent-permissions-test-permission-matches-p ()
  "Test glob pattern matching for tool calls."
  ;; Exact matches
  (should (gptel-agent--permission-matches-p "git status" "git status"))

  ;; Wildcard matches
  (should (gptel-agent--permission-matches-p "git status" "git *"))
  (should (gptel-agent--permission-matches-p "git commit -m \"test\"" "git *"))
  (should (gptel-agent--permission-matches-p "rm -rf /tmp" "rm *"))

  ;; Non-matches
  (should-not (gptel-agent--permission-matches-p "ls -la" "git *"))
  (should-not (gptel-agent--permission-matches-p "git status" "git commit*"))

  ;; Universal wildcard
  (should (gptel-agent--permission-matches-p "anything" "*"))

  ;; Complex patterns
  (should (gptel-agent--permission-matches-p "npm install package" "npm install *"))
  (should (gptel-agent--permission-matches-p "docker run image" "docker *"))

  ;; Empty pattern and string
  (should (gptel-agent--permission-matches-p "" ""))
  (should (gptel-agent--permission-matches-p "test" "*")))

(ert-deftest gptel-agent-permissions-test-permission-matches-p-edge-cases ()
  "Test edge cases in pattern matching."
  ;; Multiple wildcards
  (should (gptel-agent--permission-matches-p "git commit -m test" "git * -m *"))

  ;; Pattern with dot (dot is literal in glob patterns)
  (should (gptel-agent--permission-matches-p "file.txt" "file.txt"))

  ;; Character class patterns - [1] is a character class matching digit 1
  (should (gptel-agent--permission-matches-p "file1.txt" "file[1].txt"))
  (should-not (gptel-agent--permission-matches-p "file2.txt" "file[1].txt"))

  ;; Case sensitivity
  (should-not (gptel-agent--permission-matches-p "Git Status" "git *"))
  (should (gptel-agent--permission-matches-p "git status" "git *")))

;;; Permission Resolution Tests

(ert-deftest gptel-agent-permissions-test-resolve-simple-allow ()
  "Test resolution of simple allow permission."
  (let ((permissions '((read . allow))))
    (should (eq (gptel-agent--resolve-tool-permission 'read '("/tmp/file") permissions)
                'allow))))

(ert-deftest gptel-agent-permissions-test-resolve-simple-deny ()
  "Test resolution of simple deny permission."
  (let ((permissions '((delete . deny))))
    (should (eq (gptel-agent--resolve-tool-permission 'delete '("/tmp/file") permissions)
                'deny))))

(ert-deftest gptel-agent-permissions-test-resolve-simple-ask ()
  "Test resolution of simple ask permission."
  (let ((permissions '((edit . ask))))
    (should (eq (gptel-agent--resolve-tool-permission 'edit '("/tmp/file") permissions)
                'ask))))

(ert-deftest gptel-agent-permissions-test-resolve-universal-default ()
  "Test universal default permission (*) fallback."
  (let ((permissions '((* . allow))))
    (should (eq (gptel-agent--resolve-tool-permission 'any-tool '("args") permissions)
                'allow)))

  (let ((permissions '((* . deny))))
    (should (eq (gptel-agent--resolve-tool-permission 'unknown-tool '() permissions)
                'deny))))

(ert-deftest gptel-agent-permissions-test-resolve-tool-overrides-universal ()
  "Test that tool-specific rules override universal default."
  (let ((permissions '((* . allow)
                       (bash . deny))))
    (should (eq (gptel-agent--resolve-tool-permission 'bash '("ls") permissions)
                'deny))
    (should (eq (gptel-agent--resolve-tool-permission 'read '("file") permissions)
                'allow))))

(ert-deftest gptel-agent-permissions-test-resolve-pattern-matching ()
  "Test pattern-based permission resolution for bash commands."
  (let ((permissions '((bash . ((pattern "git *" . allow)
                                (pattern "rm *" . deny)
                                (* . ask))))))
    ;; Pattern matches
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "git status") permissions)
                'allow))
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "rm -rf /tmp") permissions)
                'deny))

    ;; Pattern fallback
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "ls -la") permissions)
                'ask))))

(ert-deftest gptel-agent-permissions-test-resolve-pattern-priority ()
  "Test that first matching pattern wins."
  (let ((permissions '((bash . ((pattern "git *" . allow)
                                (pattern "git commit*" . deny)
                                (* . ask))))))
    ;; First pattern should match
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "git commit -m test") permissions)
                'allow))))

(ert-deftest gptel-agent-permissions-test-resolve-no-match-fallback ()
  "Test fallback to 'ask when no rules match."
  (let ((permissions '((other-tool . allow))))
    ;; No rule for 'unknown-tool, no universal default
    (should (eq (gptel-agent--resolve-tool-permission 'unknown-tool '() permissions)
                'ask))))

(ert-deftest gptel-agent-permissions-test-resolve-hierarchy ()
  "Test the complete permission resolution hierarchy."
  (let ((permissions '((* . deny)
                       (bash . ((pattern "git *" . allow)
                                (pattern "npm *" . ask)
                                (* . deny)))
                       (read . allow))))
    ;; Tool-specific pattern match (highest priority)
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "git status") permissions)
                'allow))

    ;; Tool-specific pattern fallback
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "ls") permissions)
                'deny))

    ;; Simple tool permission
    (should (eq (gptel-agent--resolve-tool-permission 'read '("file") permissions)
                'allow))

    ;; Universal default
    (should (eq (gptel-agent--resolve-tool-permission 'other-tool '() permissions)
                'deny))))

;;; Tool Name Normalization Tests

(ert-deftest gptel-agent-permissions-test-normalize-tool-name ()
  "Test tool name normalization for case-insensitive matching."
  (should (eq (gptel-agent--normalize-tool-name "Bash") 'bash))
  (should (eq (gptel-agent--normalize-tool-name "BASH") 'bash))
  (should (eq (gptel-agent--normalize-tool-name "bash") 'bash))
  (should (eq (gptel-agent--normalize-tool-name 'Bash) 'bash))
  (should (eq (gptel-agent--normalize-tool-name 'BASH) 'bash))
  (should (eq (gptel-agent--normalize-tool-name 'bash) 'bash))
  (should (eq (gptel-agent--normalize-tool-name "Edit") 'edit))
  (should (eq (gptel-agent--normalize-tool-name 'Read) 'read)))

;;; Configuration Loading Tests

(ert-deftest gptel-agent-permissions-test-load-valid-config ()
  "Test loading a valid configuration file."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir (gptel-agent-permissions-test--make-temp-project))
           (config-form '(gptel-agent-project-config
                          :permissions ((* . allow)
                                        (bash . deny))))
           (config-path (gptel-agent-permissions-test--write-config dir config-form)))

      (let ((loaded (gptel-agent--load-project-permissions dir)))
        (should loaded)
        (should (equal (plist-get loaded :permissions)
                       '((* . allow) (bash . deny))))
        (should (string-equal (plist-get loaded :config-path) config-path))
        (should (plist-get loaded :mtime))))))

(ert-deftest gptel-agent-permissions-test-load-config-with-patterns ()
  "Test loading configuration with pattern-based rules."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir (gptel-agent-permissions-test--make-temp-project))
           (config-form '(gptel-agent-project-config
                          :permissions ((bash . ((pattern "git *" . allow)
                                                 (pattern "rm *" . deny)
                                                 (* . ask)))))))
      (gptel-agent-permissions-test--write-config dir config-form)

      (let ((loaded (gptel-agent--load-project-permissions dir)))
        (should loaded)
        (should (equal (plist-get loaded :permissions)
                       '((bash . ((pattern "git *" . allow)
                                  (pattern "rm *" . deny)
                                  (* . ask))))))))))

(ert-deftest gptel-agent-permissions-test-load-invalid-form ()
  "Test handling of invalid configuration form."
  (gptel-agent-permissions-test--with-cleanup
    (let ((dir (gptel-agent-permissions-test--make-temp-project)))
      (with-temp-file (expand-file-name ".gptel-agent.el" dir)
        (insert "(not-the-right-form :permissions ((* . allow)))"))

      ;; Should return nil and not error
      (should-not (gptel-agent--load-project-permissions dir)))))

(ert-deftest gptel-agent-permissions-test-load-missing-permissions-key ()
  "Test handling of config missing :permissions key."
  (gptel-agent-permissions-test--with-cleanup
    (let ((dir (gptel-agent-permissions-test--make-temp-project)))
      (with-temp-file (expand-file-name ".gptel-agent.el" dir)
        (insert "(gptel-agent-project-config :other-key value)"))

      ;; Should return nil
      (should-not (gptel-agent--load-project-permissions dir)))))

(ert-deftest gptel-agent-permissions-test-load-malformed-elisp ()
  "Test handling of syntactically invalid elisp."
  (gptel-agent-permissions-test--with-cleanup
    (let ((dir (gptel-agent-permissions-test--make-temp-project)))
      (with-temp-file (expand-file-name ".gptel-agent.el" dir)
        (insert "(incomplete form"))

      ;; Should return nil and not error
      (should-not (gptel-agent--load-project-permissions dir)))))

(ert-deftest gptel-agent-permissions-test-load-nonexistent-config ()
  "Test behavior when config file doesn't exist."
  (gptel-agent-permissions-test--with-cleanup
    (let ((dir (gptel-agent-permissions-test--make-temp-project)))
      ;; Don't create config file
      (should-not (gptel-agent--load-project-permissions dir)))))

;;; Cache Tests

(ert-deftest gptel-agent-permissions-test-cache-basic ()
  "Test basic permission caching."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir (gptel-agent-permissions-test--make-temp-project))
           (config-form '(gptel-agent-project-config
                          :permissions ((* . allow))))
           (config-path (gptel-agent-permissions-test--write-config dir config-form)))

      ;; Load and cache
      (let ((loaded (gptel-agent--load-project-permissions dir)))
        (gptel-agent--cache-permissions dir
                                        (plist-get loaded :permissions)
                                        config-path))

      ;; Retrieve from cache
      (let ((cached (gptel-agent--get-cached-permissions dir)))
        (should cached)
        (should (equal (plist-get cached :permissions)
                       '((* . allow))))))))

(ert-deftest gptel-agent-permissions-test-cache-invalidation-on-mtime-change ()
  "Test cache invalidation when config file is modified."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir (gptel-agent-permissions-test--make-temp-project))
           (config-form-1 '(gptel-agent-project-config
                            :permissions ((* . allow))))
           (config-path (gptel-agent-permissions-test--write-config dir config-form-1)))

      ;; Initial load and cache
      (let ((loaded (gptel-agent--load-project-permissions dir)))
        (gptel-agent--cache-permissions dir
                                        (plist-get loaded :permissions)
                                        config-path))

      ;; Verify cache hit
      (should (gptel-agent--get-cached-permissions dir))

      ;; Wait a bit to ensure mtime changes
      (sleep-for 0.1)

      ;; Modify config file
      (let ((config-form-2 '(gptel-agent-project-config
                             :permissions ((* . deny)))))
        (gptel-agent-permissions-test--write-config dir config-form-2))

      ;; Cache should be invalidated (mtime mismatch)
      (should-not (gptel-agent--get-cached-permissions dir)))))

(ert-deftest gptel-agent-permissions-test-cache-invalidation-manual ()
  "Test manual cache invalidation."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir (gptel-agent-permissions-test--make-temp-project))
           (config-form '(gptel-agent-project-config
                          :permissions ((* . allow))))
           (config-path (gptel-agent-permissions-test--write-config dir config-form)))

      ;; Load and cache
      (let ((loaded (gptel-agent--load-project-permissions dir)))
        (gptel-agent--cache-permissions dir
                                        (plist-get loaded :permissions)
                                        config-path))

      ;; Verify cached
      (should (gptel-agent--get-cached-permissions dir))

      ;; Invalidate
      (gptel-agent--invalidate-permission-cache dir)

      ;; Should be gone
      (should-not (gptel-agent--get-cached-permissions dir)))))

(ert-deftest gptel-agent-permissions-test-cache-clear-all ()
  "Test clearing entire cache."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir1 (gptel-agent-permissions-test--make-temp-project))
           (dir2 (gptel-agent-permissions-test--make-temp-project))
           (config-form '(gptel-agent-project-config
                          :permissions ((* . allow)))))

      ;; Create configs for both
      (gptel-agent-permissions-test--write-config dir1 config-form)
      (gptel-agent-permissions-test--write-config dir2 config-form)

      ;; Load and cache both
      (let ((loaded1 (gptel-agent--load-project-permissions dir1))
            (loaded2 (gptel-agent--load-project-permissions dir2)))
        (gptel-agent--cache-permissions dir1
                                        (plist-get loaded1 :permissions)
                                        (plist-get loaded1 :config-path))
        (gptel-agent--cache-permissions dir2
                                        (plist-get loaded2 :permissions)
                                        (plist-get loaded2 :config-path)))

      ;; Both should be cached
      (should (gptel-agent--get-cached-permissions dir1))
      (should (gptel-agent--get-cached-permissions dir2))

      ;; Clear all
      (gptel-agent--invalidate-permission-cache)

      ;; Both should be gone
      (should-not (gptel-agent--get-cached-permissions dir1))
      (should-not (gptel-agent--get-cached-permissions dir2)))))

(ert-deftest gptel-agent-permissions-test-cache-returns-nil-on-missing-file ()
  "Test cache returns nil when config file is deleted."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir (gptel-agent-permissions-test--make-temp-project))
           (config-form '(gptel-agent-project-config
                          :permissions ((* . allow))))
           (config-path (gptel-agent-permissions-test--write-config dir config-form)))

      ;; Load and cache
      (let ((loaded (gptel-agent--load-project-permissions dir)))
        (gptel-agent--cache-permissions dir
                                        (plist-get loaded :permissions)
                                        config-path))

      ;; Delete config file
      (delete-file config-path)

      ;; Cache should return nil (file doesn't exist)
      (should-not (gptel-agent--get-cached-permissions dir)))))

;;; Integration Tests

(ert-deftest gptel-agent-permissions-test-check-permission-with-defaults ()
  "Test permission checking using global defaults."
  (let ((gptel-agent-default-permissions '((* . allow)
                                            (bash . deny))))
    ;; No project context - should use defaults
    (should (eq (gptel-agent--check-permission "read" '("file"))
                'allow))
    (should (eq (gptel-agent--check-permission "bash" '(:command "ls"))
                'deny))))

(ert-deftest gptel-agent-permissions-test-check-permission-ultimate-fallback ()
  "Test ultimate fallback to 'ask when no rules match."
  (let ((gptel-agent-default-permissions '()))
    (should (eq (gptel-agent--check-permission "unknown-tool" '())
                'ask))))

(ert-deftest gptel-agent-permissions-test-check-permission-normalization ()
  "Test that tool name normalization works in permission checking."
  (let ((gptel-agent-default-permissions '((bash . deny))))
    (should (eq (gptel-agent--check-permission "Bash" '(:command "ls"))
                'deny))
    (should (eq (gptel-agent--check-permission 'BASH '(:command "ls"))
                'deny))
    (should (eq (gptel-agent--check-permission 'bash '(:command "ls"))
                'deny))))

(ert-deftest gptel-agent-permissions-test-make-permission-confirm-allow ()
  "Test permission confirm function with 'allow decision."
  (let ((gptel-agent-default-permissions '((* . allow))))
    (let ((confirm-fn (gptel-agent--make-permission-confirm "read")))
      ;; Allow should return nil (no confirmation needed)
      (should-not (funcall confirm-fn "/tmp/file")))))

(ert-deftest gptel-agent-permissions-test-make-permission-confirm-ask ()
  "Test permission confirm function with 'ask decision."
  (let ((gptel-agent-default-permissions '((* . ask))))
    (let ((confirm-fn (gptel-agent--make-permission-confirm "read")))
      ;; Ask should return t (show confirmation)
      (should (eq (funcall confirm-fn "/tmp/file") t)))))

(ert-deftest gptel-agent-permissions-test-make-permission-confirm-deny ()
  "Test permission confirm function with 'deny decision."
  (let ((gptel-agent-default-permissions '((* . deny))))
    (let ((confirm-fn (gptel-agent--make-permission-confirm "read")))
      ;; Deny should signal an error
      (should-error (funcall confirm-fn "/tmp/file")
                    :type 'error))))

(ert-deftest gptel-agent-permissions-test-make-permission-confirm-plist-args ()
  "Test permission confirm with plist arguments."
  (let ((gptel-agent-default-permissions '((bash . ((pattern "git *" . allow)
                                                     (* . deny))))))
    (let ((confirm-fn (gptel-agent--make-permission-confirm "bash")))
      ;; Git command should be allowed
      (should-not (funcall confirm-fn :command "git status"))

      ;; Other command should be denied
      (should-error (funcall confirm-fn :command "rm -rf /")
                    :type 'error))))

(ert-deftest gptel-agent-permissions-test-permission-confirm-allow ()
  "Test convenience permission confirm function with 'allow."
  (let ((gptel-agent-default-permissions '((* . allow))))
    (should (gptel-agent-permission-confirm "read" '("/tmp/file")))))

(ert-deftest gptel-agent-permissions-test-permission-confirm-deny ()
  "Test convenience permission confirm function with 'deny."
  (let ((gptel-agent-default-permissions '((* . deny))))
    (should-not (gptel-agent-permission-confirm "read" '("/tmp/file")))))

;;; Complex Integration Scenarios

(ert-deftest gptel-agent-permissions-test-complex-bash-patterns ()
  "Test complex bash command pattern matching scenarios."
  (let ((permissions '((bash . ((pattern "git status" . allow)
                                (pattern "git commit*" . ask)
                                (pattern "git *" . allow)
                                (pattern "rm -rf *" . deny)
                                (pattern "npm install*" . ask)
                                (* . deny))))))

    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "git status") permissions)
                'allow))
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "git log") permissions)
                'allow))
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "git commit -m test") permissions)
                'ask))
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "rm -rf /tmp") permissions)
                'deny))
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "npm install lodash") permissions)
                'ask))
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "ls -la") permissions)
                'deny))))

(ert-deftest gptel-agent-permissions-test-multi-tool-permissions ()
  "Test permissions for multiple tools with different rules."
  (let ((permissions '((* . deny)
                       (read . allow)
                       (edit . ask)
                       (bash . ((pattern "git *" . allow)
                                (* . deny)))
                       (grep . allow))))

    (should (eq (gptel-agent--resolve-tool-permission 'read '("file") permissions)
                'allow))
    (should (eq (gptel-agent--resolve-tool-permission 'edit '("file") permissions)
                'ask))
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "git status") permissions)
                'allow))
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "ls") permissions)
                'deny))
    (should (eq (gptel-agent--resolve-tool-permission 'grep '("pattern") permissions)
                'allow))
    (should (eq (gptel-agent--resolve-tool-permission 'delete '("file") permissions)
                'deny))))

;;; Config Location Tests

(ert-deftest gptel-agent-permissions-test-locate-config-found ()
  "Test locating config file when it exists."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir (gptel-agent-permissions-test--make-temp-project))
           (config-form '(gptel-agent-project-config
                          :permissions ((* . allow))))
           (expected-path (expand-file-name ".gptel-agent.el" dir)))
      (gptel-agent-permissions-test--write-config dir config-form)
      ;; Use cl-letf to mock project-current and project-root
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&optional _maybe-prompt _directory) dir))
                ((symbol-function 'project-root)
                 (lambda (_project) dir)))
        (let ((found (gptel-agent--locate-config dir)))
          (should found)
          (should (string-equal found expected-path)))))))

(ert-deftest gptel-agent-permissions-test-locate-config-not-found ()
  "Test locating config file when it doesn't exist."
  (gptel-agent-permissions-test--with-cleanup
    (let ((dir (gptel-agent-permissions-test--make-temp-project)))
      ;; Don't create config file
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&optional _maybe-prompt _directory) dir))
                ((symbol-function 'project-root)
                 (lambda (_project) dir)))
        (should-not (gptel-agent--locate-config dir))))))

(ert-deftest gptel-agent-permissions-test-locate-config-no-project ()
  "Test locating config file when no project is detected."
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _maybe-prompt _directory) nil)))
    (should-not (gptel-agent--locate-config "/nonexistent"))))

;;; Reload Permissions Tests

(ert-deftest gptel-agent-permissions-test-reload-permissions ()
  "Test interactive permission reload."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir (gptel-agent-permissions-test--make-temp-project))
           (config-form '(gptel-agent-project-config
                          :permissions ((* . allow)))))
      (gptel-agent-permissions-test--write-config dir config-form)

      ;; Pre-populate cache
      (let ((loaded (gptel-agent--load-project-permissions dir)))
        (gptel-agent--cache-permissions dir
                                        (plist-get loaded :permissions)
                                        (plist-get loaded :config-path)))
      (should (gptel-agent--get-cached-permissions dir))

      ;; Mock project functions and call reload
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&optional _maybe-prompt _directory) dir))
                ((symbol-function 'project-root)
                 (lambda (_project) dir)))
        (gptel-agent-reload-permissions))

      ;; Cache should be repopulated (not nil after reload)
      (should (gptel-agent--get-cached-permissions dir)))))

(ert-deftest gptel-agent-permissions-test-reload-no-project ()
  "Test reload permissions when no project context."
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _maybe-prompt _directory) nil)))
    ;; Should not error
    (gptel-agent-reload-permissions)))

;;; Permission Enforcer Tests

(ert-deftest gptel-agent-permissions-test-permission-enforcer-allow ()
  "Test permission enforcer allows execution."
  (let ((gptel-agent-default-permissions '((* . allow)))
        (original-fn (lambda (&rest args) (list 'executed args)))
        (enforcer (gptel-agent-permission-enforcer "test-tool")))
    (let ((result (funcall enforcer original-fn "arg1" "arg2")))
      (should (equal result '(executed ("arg1" "arg2")))))))

(ert-deftest gptel-agent-permissions-test-permission-enforcer-deny ()
  "Test permission enforcer blocks execution."
  (let ((gptel-agent-default-permissions '((* . deny)))
        (original-fn (lambda (&rest args) (list 'executed args)))
        (enforcer (gptel-agent-permission-enforcer "test-tool")))
    (should-error (funcall enforcer original-fn "arg1")
                  :type 'error)))

(ert-deftest gptel-agent-permissions-test-permission-enforcer-ask ()
  "Test permission enforcer with ask allows through."
  (let ((gptel-agent-default-permissions '((* . ask)))
        (original-fn (lambda (&rest args) (list 'executed args)))
        (enforcer (gptel-agent-permission-enforcer "test-tool")))
    ;; Ask permission still allows execution (it's the confirm function that prompts)
    (let ((result (funcall enforcer original-fn "arg1")))
      (should (equal result '(executed ("arg1")))))))

;;; Wrap Tool Confirm Tests

(ert-deftest gptel-agent-permissions-test-wrap-tool-confirm-basic ()
  "Test wrapping a tool spec with permission checking."
  (let ((gptel-agent-default-permissions '((* . allow)))
        (tool-spec (list :name "TestTool"
                         :confirm (lambda (&rest _) t)
                         :function #'identity)))
    (let ((wrapped (gptel-agent-wrap-tool-confirm tool-spec)))
      (should wrapped)
      (should (functionp (plist-get wrapped :confirm)))
      ;; With allow permission, should return nil (no confirmation)
      (should-not (funcall (plist-get wrapped :confirm) "arg")))))

(ert-deftest gptel-agent-permissions-test-wrap-tool-confirm-deny ()
  "Test wrapped tool confirm signals error on deny."
  (let ((gptel-agent-default-permissions '((testtool . deny)))
        (tool-spec (list :name "TestTool"
                         :confirm (lambda (&rest _) t)
                         :function #'identity)))
    (let ((wrapped (gptel-agent-wrap-tool-confirm tool-spec)))
      (should-error (funcall (plist-get wrapped :confirm) "arg")
                    :type 'error))))

(ert-deftest gptel-agent-permissions-test-wrap-tool-confirm-ask-defers ()
  "Test wrapped tool confirm defers to original on ask."
  (let* ((gptel-agent-default-permissions '((testtool . ask)))
         (original-called nil)
         (tool-spec (list :name "TestTool"
                          :confirm (lambda (&rest _)
                                     (setq original-called t)
                                     'original-result)
                          :function #'identity)))
    (let ((wrapped (gptel-agent-wrap-tool-confirm tool-spec)))
      (let ((result (funcall (plist-get wrapped :confirm) "arg")))
        (should original-called)
        (should (eq result 'original-result))))))

(ert-deftest gptel-agent-permissions-test-wrap-tool-confirm-nil-name ()
  "Test wrap-tool-confirm handles nil name gracefully."
  (let ((tool-spec (list :confirm (lambda (&rest _) t)
                         :function #'identity)))
    (let ((result (gptel-agent-wrap-tool-confirm tool-spec)))
      ;; Should return original spec unchanged when name is nil
      (should (eq result tool-spec)))))

;;; Additional Edge Cases

(ert-deftest gptel-agent-permissions-test-empty-pattern-list ()
  "Test resolution with empty pattern list for tool."
  (let ((permissions '((bash . ()))))
    ;; No patterns means no match, should fall back
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "ls") permissions)
                'ask))))

(ert-deftest gptel-agent-permissions-test-nested-plist-args ()
  "Test tool call string building with nested plist args."
  (should (string-equal
           (gptel-agent--build-tool-call-string "edit" '(:file "/path" :content "test" :mode "append"))
           "edit /path test append")))

(ert-deftest gptel-agent-permissions-test-symbol-in-args ()
  "Test tool call string building with symbols in args."
  (should (string-equal
           (gptel-agent--build-tool-call-string "test" '(foo bar baz))
           "test foo bar baz")))

(ert-deftest gptel-agent-permissions-test-bash-with-cmd-key ()
  "Test bash tool call string extraction with :cmd key."
  (should (string-equal
           (gptel-agent--build-tool-call-string 'bash '(:cmd "echo hello"))
           "echo hello")))

(ert-deftest gptel-agent-permissions-test-bash-with-direct-string ()
  "Test bash tool call string extraction with direct string arg."
  (should (string-equal
           (gptel-agent--build-tool-call-string 'bash '("pwd"))
           "pwd")))

(ert-deftest gptel-agent-permissions-test-glob-question-mark ()
  "Test glob pattern with question mark wildcard."
  (should (gptel-agent--permission-matches-p "git" "gi?"))
  (should-not (gptel-agent--permission-matches-p "gitt" "gi?")))

(ert-deftest gptel-agent-permissions-test-glob-character-class ()
  "Test glob pattern with character class."
  (should (gptel-agent--permission-matches-p "file1.txt" "file[0-9].txt"))
  (should-not (gptel-agent--permission-matches-p "filea.txt" "file[0-9].txt")))

(ert-deftest gptel-agent-permissions-test-config-custom-filename ()
  "Test config location with custom filename."
  (gptel-agent-permissions-test--with-cleanup
    (let* ((dir (gptel-agent-permissions-test--make-temp-project))
           (gptel-agent-permission-config-filename ".custom-config.el")
           (config-path (expand-file-name ".custom-config.el" dir)))
      (with-temp-file config-path
        (prin1 '(gptel-agent-project-config :permissions ((* . allow)))
               (current-buffer)))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&optional _maybe-prompt _directory) dir))
                ((symbol-function 'project-root)
                 (lambda (_project) dir)))
        (let ((found (gptel-agent--locate-config dir)))
          (should found)
          (should (string-equal found config-path)))))))

(ert-deftest gptel-agent-permissions-test-permission-confirm-ask-yes ()
  "Test permission confirm with ask decision and user approving."
  (let ((gptel-agent-default-permissions '((* . ask))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (gptel-agent-permission-confirm "read" '("/tmp/file"))))))

(ert-deftest gptel-agent-permissions-test-permission-confirm-ask-no ()
  "Test permission confirm with ask decision and user denying."
  (let ((gptel-agent-default-permissions '((* . ask))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
      (should-not (gptel-agent-permission-confirm "read" '("/tmp/file"))))))

(ert-deftest gptel-agent-permissions-test-default-permissions-ask ()
  "Test default value of gptel-agent-default-permissions."
  (should (equal (default-value 'gptel-agent-default-permissions)
                 '((* . ask)))))

(ert-deftest gptel-agent-permissions-test-customization-group ()
  "Test that customization group is defined."
  (should (get 'gptel-agent-permissions 'group-documentation)))

(ert-deftest gptel-agent-permissions-test-config-filename-default ()
  "Test default config filename."
  (should (string-equal (default-value 'gptel-agent-permission-config-filename)
                        ".gptel-agent.el")))

(ert-deftest gptel-agent-permissions-test-cache-hash-table-exists ()
  "Test that permission cache is initialized."
  (should (hash-table-p gptel-agent--permission-cache)))

(ert-deftest gptel-agent-permissions-test-pattern-with-asterisk-middle ()
  "Test pattern matching with asterisk in middle of pattern."
  (should (gptel-agent--permission-matches-p "git commit -m 'message'" "git * -m *"))
  (should (gptel-agent--permission-matches-p "npm run build" "npm * build")))

(ert-deftest gptel-agent-permissions-test-resolve-pattern-only-wildcard-fallback ()
  "Test pattern resolution uses wildcard fallback in tool rules."
  (let ((permissions '((bash . ((pattern "git *" . allow)
                                (* . deny))))))
    ;; Non-matching command should use (* . deny) fallback in bash rules
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "ls") permissions)
                'deny))))

(ert-deftest gptel-agent-permissions-test-resolve-pattern-no-wildcard-fallback ()
  "Test pattern resolution without wildcard fallback."
  (let ((permissions '((bash . ((pattern "git *" . allow)))
                       (* . deny))))
    ;; Non-matching should fall back to universal (* . deny)
    (should (eq (gptel-agent--resolve-tool-permission 'bash '(:command "ls") permissions)
                'deny))))

(ert-deftest gptel-agent-permissions-test-make-permission-confirm-single-list-arg ()
  "Test permission confirm with single list as argument."
  (let ((gptel-agent-default-permissions '((bash . ((pattern "git *" . allow)
                                                     (* . ask))))))
    (let ((confirm-fn (gptel-agent--make-permission-confirm "bash")))
      ;; When called with a single list argument
      (should-not (funcall confirm-fn '(:command "git status"))))))

(provide 'gptel-agent-permissions-test)
;;; gptel-agent-permissions-test.el ends here
