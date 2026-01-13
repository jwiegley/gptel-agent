;;; gptel-agent-mcp-test.el --- Tests for gptel-agent-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for gptel-agent MCP server management.

;;; Code:

(require 'ert)
(require 'gptel-agent-mcp)

;;;; Test Fixtures

(defconst gptel-agent-mcp-test--sample-config
  '((github . (:command "npx"
               :args ("-y" "@modelcontextprotocol/server-github")
               :env (("GITHUB_TOKEN" . "$GITHUB_TOKEN"))
               :auto-start t))
    (filesystem . (:command "npx"
                   :args ("-y" "@modelcontextprotocol/server-filesystem"
                          "${PROJECT_ROOT}/docs")
                   :auto-start nil))
    (remote . (:url "https://mcp.example.com/api"
               :token "$MCP_TOKEN"
               :auto-start nil)))
  "Sample MCP server configuration for testing.")

;;;; Environment Variable Expansion Tests

(ert-deftest gptel-agent-mcp-test-expand-env-var ()
  "Test environment variable expansion in strings."
  (let ((process-environment '("TEST_VAR=hello"
                               "ANOTHER_VAR=world")))
    ;; Simple $VAR syntax
    (should (equal (gptel-agent--expand-env-var "$TEST_VAR")
                   "hello"))

    ;; ${VAR} syntax
    (should (equal (gptel-agent--expand-env-var "${TEST_VAR}")
                   "hello"))

    ;; Multiple variables
    (should (equal (gptel-agent--expand-env-var "$TEST_VAR $ANOTHER_VAR")
                   "hello world"))

    ;; Mixed syntax
    (should (equal (gptel-agent--expand-env-var "$TEST_VAR/${ANOTHER_VAR}")
                   "hello/world"))

    ;; Undefined variable
    (should (equal (gptel-agent--expand-env-var "$UNDEFINED")
                   ""))

    ;; No variables
    (should (equal (gptel-agent--expand-env-var "no vars here")
                   "no vars here"))

    ;; Partial match (should not expand)
    (should (equal (gptel-agent--expand-env-var "prefix$TEST_VARsuffix")
                   "prefixhellosuffix"))))

(ert-deftest gptel-agent-mcp-test-expand-env-var-edge-cases ()
  "Test edge cases in environment variable expansion."
  (let ((process-environment '("VAR=value")))
    ;; Empty string
    (should (equal (gptel-agent--expand-env-var "")
                   ""))

    ;; Just dollar sign
    (should (equal (gptel-agent--expand-env-var "$")
                   "$"))

    ;; Empty braces
    (should (equal (gptel-agent--expand-env-var "${}")
                   ""))

    ;; Non-string input
    (should (null (gptel-agent--expand-env-var nil)))

    ;; Numbers in variable names
    (let ((process-environment '("VAR123=test")))
      (should (equal (gptel-agent--expand-env-var "$VAR123")
                     "test")))))

(ert-deftest gptel-agent-mcp-test-expand-mcp-config ()
  "Test expansion of environment variables in MCP config."
  (let ((process-environment '("API_KEY=secret123"
                               "SERVER_URL=https://api.example.com"
                               "PROJECT=/home/user/project")))
    (let* ((config '(:command "mcp-server"
                     :args ("--key" "$API_KEY" "--path" "${PROJECT}/data")
                     :url "${SERVER_URL}/endpoint"
                     :other-field "not-expanded"))
           (expanded (gptel-agent--expand-mcp-config config)))

      ;; Command expanded
      (should (equal (plist-get expanded :command) "mcp-server"))

      ;; Args expanded
      (should (equal (plist-get expanded :args)
                     '("--key" "secret123" "--path" "/home/user/project/data")))

      ;; URL expanded
      (should (equal (plist-get expanded :url)
                     "https://api.example.com/endpoint"))

      ;; Other fields unchanged
      (should (equal (plist-get expanded :other-field)
                     "not-expanded")))))

(ert-deftest gptel-agent-mcp-test-expand-config-missing-vars ()
  "Test config expansion with missing environment variables."
  (let ((process-environment '()))
    (let* ((config '(:command "cmd"
                     :args ("$MISSING" "${ALSO_MISSING}")
                     :url "$UNDEFINED_URL"))
           (expanded (gptel-agent--expand-mcp-config config)))

      ;; Missing variables become empty strings
      (should (equal (plist-get expanded :args) '("" "")))
      (should (equal (plist-get expanded :url) "")))))

;;;; Configuration Loading Tests

(ert-deftest gptel-agent-mcp-test-load-config ()
  "Test loading MCP configuration from file."
  (let ((temp-dir (make-temp-file "gptel-agent-test-" t)))
    (unwind-protect
        (let* ((config-file (expand-file-name ".gptel-agent.el" temp-dir))
               (config-content
                "(gptel-agent-project-config
                  :mcp-servers
                  ((test-server . (:command \"test\"
                                   :args (\"arg1\" \"arg2\")))))"))
          ;; Write config file
          (with-temp-file config-file
            (insert config-content))

          ;; Mock gptel-agent--locate-config to return our temp file
          (cl-letf (((symbol-function 'gptel-agent--locate-config)
                     (lambda (_) config-file)))
            (let ((servers (gptel-agent--load-mcp-config temp-dir)))
              ;; Should load server config
              (should servers)
              (should (= (length servers) 1))
              (should (equal (car (car servers)) 'test-server))
              (should (equal (plist-get (cdr (car servers)) :command)
                           "test")))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest gptel-agent-mcp-test-load-config-with-expansion ()
  "Test loading config with environment variable expansion."
  (let ((temp-dir (make-temp-file "gptel-agent-test-" t))
        (process-environment '("TEST_CMD=my-command"
                              "TEST_ARG=my-arg")))
    (unwind-protect
        (let* ((config-file (expand-file-name ".gptel-agent.el" temp-dir))
               (config-content
                "(gptel-agent-project-config
                  :mcp-servers
                  ((test-server . (:command \"$TEST_CMD\"
                                   :args (\"${TEST_ARG}\")))))"))
          (with-temp-file config-file
            (insert config-content))

          (cl-letf (((symbol-function 'gptel-agent--locate-config)
                     (lambda (_) config-file)))
            (let ((servers (gptel-agent--load-mcp-config temp-dir)))
              (should servers)
              (let ((config (cdr (car servers))))
                ;; Variables should be expanded
                (should (equal (plist-get config :command) "my-command"))
                (should (equal (plist-get config :args) '("my-arg")))))))
      (delete-directory temp-dir t))))

(ert-deftest gptel-agent-mcp-test-load-config-invalid ()
  "Test handling of invalid configuration."
  (let ((temp-dir (make-temp-file "gptel-agent-test-" t)))
    (unwind-protect
        (let* ((config-file (expand-file-name ".gptel-agent.el" temp-dir))
               (invalid-content "(invalid-form :foo :bar)"))
          (with-temp-file config-file
            (insert invalid-content))

          (cl-letf (((symbol-function 'gptel-agent--locate-config)
                     (lambda (_) config-file)))
            ;; Should return nil for invalid config
            (should-not (gptel-agent--load-mcp-config temp-dir))))
      (delete-directory temp-dir t))))

;;;; Server Status Tests

(ert-deftest gptel-agent-mcp-test-server-status ()
  "Test MCP server status checking."
  ;; Mock mcp-server-connections
  (let ((mcp-server-connections (make-hash-table :test 'equal)))

    ;; No connection
    (should (eq (gptel-agent--mcp-server-status "nonexistent")
                'disconnected))

    ;; Mock connection with status
    (let ((mock-connection (list :status 'connected)))
      (puthash "test-server" mock-connection mcp-server-connections)

      ;; Mock mcp--status to return the status
      (cl-letf (((symbol-function 'mcp--status)
                 (lambda (_) 'connected)))
        (should (eq (gptel-agent--mcp-server-status "test-server")
                    'connected))
        (should (gptel-agent--mcp-server-running-p "test-server"))))

    ;; Error case
    (puthash "error-server" (list :status 'error) mcp-server-connections)
    (cl-letf (((symbol-function 'mcp--status)
               (lambda (_) (error "Connection error"))))
      (should (eq (gptel-agent--mcp-server-status "error-server")
                  'unknown)))))

;;;; Server List Generation Tests

(ert-deftest gptel-agent-mcp-test-list-entries ()
  "Test generation of server list entries for display."
  (let ((mcp-server-connections (make-hash-table :test 'equal))
        (gptel-agent--mcp-project-servers
         '(("/test/project" . ((test-server . (:command "cmd" :args ("arg"))))))))

    ;; Mock project-current and project-root
    (cl-letf (((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--mcp-server-status)
               (lambda (_) 'disconnected)))

      (let ((entries (gptel-agent-mcp--list-entries)))
        (should entries)
        (should (= (length entries) 1))
        (let ((entry (car entries)))
          (should (equal (car entry) 'test-server))
          (should (vectorp (cadr entry)))
          (should (= (length (cadr entry)) 4)))))))

;;;; Permission Matching Tests

(ert-deftest gptel-agent-mcp-test-tool-matches ()
  "Test MCP tool name pattern matching."
  ;; Exact match
  (should (gptel-agent--mcp-tool-matches-p
           "mcp-github:list-repos"
           "mcp-github:list-repos"))

  ;; Wildcard match
  (should (gptel-agent--mcp-tool-matches-p
           "mcp-github:list-repos"
           "mcp-github:*"))

  ;; Wildcard all MCP tools
  (should (gptel-agent--mcp-tool-matches-p
           "mcp-github:list-repos"
           "mcp-*:*"))

  ;; Specific tool across servers
  (should (gptel-agent--mcp-tool-matches-p
           "mcp-github:search"
           "mcp-*:search"))

  ;; No match
  (should-not (gptel-agent--mcp-tool-matches-p
               "mcp-github:list-repos"
               "mcp-gitlab:*"))

  ;; Partial wildcard
  (should (gptel-agent--mcp-tool-matches-p
           "mcp-github:list-repos"
           "mcp-github:list*")))

;;;; Merge Strategy Tests

(ert-deftest gptel-agent-mcp-test-merge-strategies ()
  "Test different server configuration merge strategies."
  (let ((project-servers '((proj-server . (:command "proj"))))
        (mcp-hub-servers '((global-server . (:command "global")))))

    ;; Combine strategy
    (let ((gptel-agent-mcp-merge-strategy 'combine))
      (let ((merged (gptel-agent--merge-server-configs project-servers)))
        (should (= (length merged) 2))
        (should (assoc 'proj-server merged))
        (should (assoc 'global-server merged))))

    ;; Replace strategy
    (let ((gptel-agent-mcp-merge-strategy 'replace))
      (let ((merged (gptel-agent--merge-server-configs project-servers)))
        (should (= (length merged) 1))
        (should (assoc 'proj-server merged))))

    ;; Project-only strategy
    (let ((gptel-agent-mcp-merge-strategy 'project-only))
      (let ((merged (gptel-agent--merge-server-configs project-servers)))
        (should (= (length merged) 1))
        (should (assoc 'proj-server merged))
        (should-not (assoc 'global-server merged))))))

;;;; Integration Tests

(ert-deftest gptel-agent-mcp-test-connect-disconnect-flow ()
  "Test the connect/disconnect flow."
  (let ((mcp-server-connections (make-hash-table :test 'equal))
        (gptel-agent--mcp-project-servers
         '(("/test/project" . ((test-server . (:command "test"))))))
        (connect-called nil)
        (stop-called nil))

    ;; Mock functions
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'mcp-connect-server)
               (lambda (&rest _) (setq connect-called t)))
              ((symbol-function 'mcp-stop-server)
               (lambda (_) (setq stop-called t)))
              ((symbol-function 'gptel-agent--mcp-server-running-p)
               (lambda (_) nil)))

      ;; Test connect
      (gptel-agent-mcp-connect "/test/project")
      (should connect-called)

      ;; Reset and test disconnect
      (setq connect-called nil)
      (cl-letf (((symbol-function 'gptel-agent--mcp-server-running-p)
                 (lambda (_) t)))
        (gptel-agent-mcp-disconnect "/test/project")
        (should stop-called)))))

(provide 'gptel-agent-mcp-test)
;;; gptel-agent-mcp-test.el ends here
