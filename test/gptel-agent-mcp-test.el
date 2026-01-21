;;; gptel-agent-mcp-test.el --- Tests for gptel-agent-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for gptel-agent MCP server management.

;;; Code:

(require 'ert)
(require 'gptel-agent-mcp)

;; Declare mcp.el variables for testing without requiring the full library
(defvar mcp-server-connections nil
  "Hash table of MCP server connections (defined in mcp.el).")
(defvar mcp-hub-servers nil
  "List of global MCP hub servers (defined in mcp-hub.el).")

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

    ;; Greedy match - $TEST_VARsuffix is treated as one variable name
    ;; (matches standard shell behavior - use ${VAR}suffix for embedded vars)
    (should (equal (gptel-agent--expand-env-var "prefix$TEST_VARsuffix")
                   "prefix"))

    ;; Brace syntax works for embedded variables
    (should (equal (gptel-agent--expand-env-var "prefix${TEST_VAR}suffix")
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

    ;; Empty braces - invalid syntax, left unchanged
    (should (equal (gptel-agent--expand-env-var "${}")
                   "${}"))

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

;;;; Customization Tests

(ert-deftest gptel-agent-mcp-test-customization-group ()
  "Test that customization group is properly defined."
  (should (get 'gptel-agent-mcp 'group-documentation)))

(ert-deftest gptel-agent-mcp-test-auto-connect-default ()
  "Test default value of auto-connect option."
  (should (null (default-value 'gptel-agent-mcp-auto-connect))))

(ert-deftest gptel-agent-mcp-test-merge-strategy-default ()
  "Test default value of merge strategy option."
  (should (eq (default-value 'gptel-agent-mcp-merge-strategy) 'combine)))

(ert-deftest gptel-agent-mcp-test-merge-strategy-type ()
  "Test merge strategy custom type validation."
  (let ((type (get 'gptel-agent-mcp-merge-strategy 'custom-type)))
    (should type)
    ;; Should be a choice type
    (should (eq (car type) 'choice))))

;;;; Get Project Servers Tests

(ert-deftest gptel-agent-mcp-test-get-project-servers-no-project ()
  "Test get-project-servers returns nil when not in a project."
  (cl-letf (((symbol-function 'project-current)
             (lambda () nil)))
    (should-not (gptel-agent--get-project-servers))))

(ert-deftest gptel-agent-mcp-test-get-project-servers-cached ()
  "Test get-project-servers returns cached config."
  (let ((gptel-agent--mcp-project-servers
         '(("/test/project" . ((cached-server . (:command "cached")))))))
    (cl-letf (((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project")))
      (let ((servers (gptel-agent--get-project-servers)))
        (should servers)
        (should (assoc 'cached-server servers))))))

(ert-deftest gptel-agent-mcp-test-get-project-servers-loads-on-miss ()
  "Test get-project-servers loads config when not cached."
  (let ((gptel-agent--mcp-project-servers nil)
        (load-called nil))
    (cl-letf (((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--load-mcp-config)
               (lambda (_)
                 (setq load-called t)
                 '((loaded-server . (:command "loaded"))))))
      (let ((servers (gptel-agent--get-project-servers)))
        (should load-called)
        (should servers)
        (should (assoc 'loaded-server servers))))))

(ert-deftest gptel-agent-mcp-test-get-project-servers-explicit-root ()
  "Test get-project-servers with explicit project root."
  (let ((gptel-agent--mcp-project-servers
         '(("/explicit/root" . ((explicit-server . (:command "explicit")))))))
    ;; Should not call project-current when root is provided
    (let ((servers (gptel-agent--get-project-servers "/explicit/root")))
      (should servers)
      (should (assoc 'explicit-server servers)))))

;;;; Config Loading Extended Tests

(ert-deftest gptel-agent-mcp-test-load-config-no-file ()
  "Test loading when no config file exists."
  (cl-letf (((symbol-function 'gptel-agent--locate-config)
             (lambda (_) nil)))
    (should-not (gptel-agent--load-mcp-config "/nonexistent"))))

(ert-deftest gptel-agent-mcp-test-load-config-empty-servers ()
  "Test loading config with empty mcp-servers."
  (let ((temp-dir (make-temp-file "gptel-agent-test-" t)))
    (unwind-protect
        (let* ((config-file (expand-file-name ".gptel-agent.el" temp-dir))
               (config-content "(gptel-agent-project-config :mcp-servers nil)"))
          (with-temp-file config-file
            (insert config-content))

          (cl-letf (((symbol-function 'gptel-agent--locate-config)
                     (lambda (_) config-file)))
            ;; Should return nil for empty server list
            (should-not (gptel-agent--load-mcp-config temp-dir))))
      (delete-directory temp-dir t))))

(ert-deftest gptel-agent-mcp-test-load-config-multiple-servers ()
  "Test loading config with multiple servers."
  (let ((temp-dir (make-temp-file "gptel-agent-test-" t)))
    (unwind-protect
        (let* ((config-file (expand-file-name ".gptel-agent.el" temp-dir))
               (config-content
                "(gptel-agent-project-config
                  :mcp-servers
                  ((server1 . (:command \"cmd1\"))
                   (server2 . (:command \"cmd2\" :args (\"arg\")))
                   (server3 . (:url \"https://example.com\"))))"))
          (with-temp-file config-file
            (insert config-content))

          (cl-letf (((symbol-function 'gptel-agent--locate-config)
                     (lambda (_) config-file)))
            (let ((servers (gptel-agent--load-mcp-config temp-dir)))
              (should servers)
              (should (= (length servers) 3))
              (should (assoc 'server1 servers))
              (should (assoc 'server2 servers))
              (should (assoc 'server3 servers)))))
      (delete-directory temp-dir t))))

;;;; Server Status Extended Tests

(ert-deftest gptel-agent-mcp-test-server-status-disconnected ()
  "Test status returns disconnected for unknown server."
  (let ((mcp-server-connections (make-hash-table :test 'equal)))
    (should (eq (gptel-agent--mcp-server-status "unknown-server")
                'disconnected))))

(ert-deftest gptel-agent-mcp-test-server-running-false ()
  "Test server-running-p returns nil for disconnected server."
  (let ((mcp-server-connections (make-hash-table :test 'equal)))
    (should-not (gptel-agent--mcp-server-running-p "unknown-server"))))

(ert-deftest gptel-agent-mcp-test-server-running-true ()
  "Test server-running-p returns t for connected server."
  (let ((mcp-server-connections (make-hash-table :test 'equal)))
    (puthash "connected-server" t mcp-server-connections)
    (cl-letf (((symbol-function 'mcp--status)
               (lambda (_) 'connected)))
      (should (gptel-agent--mcp-server-running-p "connected-server")))))

;;;; MCP Status Command Tests

(ert-deftest gptel-agent-mcp-test-status-no-project ()
  "Test mcp-status fails gracefully when not in project."
  (cl-letf (((symbol-function 'project-current)
             (lambda () nil)))
    (should-error (gptel-agent-mcp-status) :type 'user-error)))

(ert-deftest gptel-agent-mcp-test-status-no-servers ()
  "Test mcp-status handles no configured servers."
  (let ((messages nil))
    (cl-letf (((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (gptel-agent-mcp-status "/test/project")
      (should (car messages))
      (should (string-match-p "No MCP servers" (car messages))))))

(ert-deftest gptel-agent-mcp-test-status-shows-all-servers ()
  "Test mcp-status shows status for all servers."
  (let ((mcp-server-connections (make-hash-table :test 'equal))
        (messages nil))
    (puthash "server1" t mcp-server-connections)

    (cl-letf (((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((server1 . (:command "cmd1"))
                   (server2 . (:command "cmd2")))))
              ((symbol-function 'gptel-agent--mcp-server-status)
               (lambda (name)
                 (if (string= name "server1") 'connected 'disconnected)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (gptel-agent-mcp-status "/test/project")
      (let ((output (car messages)))
        (should (string-match-p "server1" output))
        (should (string-match-p "server2" output))))))

;;;; MCP List Command Tests

(ert-deftest gptel-agent-mcp-test-list-no-mcp-package ()
  "Test mcp-list fails when mcp package unavailable."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) nil)))
    (should-error (gptel-agent-mcp-list) :type 'user-error)))

(ert-deftest gptel-agent-mcp-test-list-no-project ()
  "Test mcp-list fails when not in project."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) t))
            ((symbol-function 'project-current)
             (lambda () nil)))
    (should-error (gptel-agent-mcp-list) :type 'user-error)))

(ert-deftest gptel-agent-mcp-test-list-creates-buffer ()
  "Test mcp-list creates the server list buffer."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) t))
            ((symbol-function 'project-current)
             (lambda () t))
            ((symbol-function 'project-root)
             (lambda (_) "/test/project"))
            ((symbol-function 'gptel-agent--get-project-servers)
             (lambda (_) nil))  ;; Return nil to avoid list printing issues
            ((symbol-function 'pop-to-buffer)
             (lambda (buf) buf)))
    (let ((buf (gptel-agent-mcp-list)))
      (unwind-protect
          (progn
            (should buf)
            (should (string= (buffer-name buf) "*gptel-agent MCP Servers*")))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;;;; List Entries Extended Tests

(ert-deftest gptel-agent-mcp-test-list-entries-no-project ()
  "Test list entries returns nil when not in project."
  (cl-letf (((symbol-function 'project-current)
             (lambda () nil)))
    (should-not (gptel-agent-mcp--list-entries))))

(ert-deftest gptel-agent-mcp-test-list-entries-no-servers ()
  "Test list entries returns nil when no servers configured."
  (cl-letf (((symbol-function 'project-current)
             (lambda () t))
            ((symbol-function 'project-root)
             (lambda (_) "/test/project"))
            ((symbol-function 'gptel-agent--get-project-servers)
             (lambda (_) nil)))
    (should-not (gptel-agent-mcp--list-entries))))

(ert-deftest gptel-agent-mcp-test-list-entries-formats-correctly ()
  "Test list entries format is correct for tabulated-list."
  (let ((mcp-server-connections (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((test-server . (:command "npx" :args ("-y" "package"))))))
              ((symbol-function 'gptel-agent--mcp-server-status)
               (lambda (_) 'disconnected)))
      (let ((entries (gptel-agent-mcp--list-entries)))
        (should entries)
        (should (= (length entries) 1))
        (let ((entry (car entries)))
          ;; Entry format: (id [name status command tools])
          (should (eq (car entry) 'test-server))
          (should (vectorp (cadr entry)))
          (should (= (length (cadr entry)) 4)))))))

(ert-deftest gptel-agent-mcp-test-list-entries-url-server ()
  "Test list entries handles URL-based servers."
  (let ((mcp-server-connections (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((remote-server . (:url "https://api.example.com")))))
              ((symbol-function 'gptel-agent--mcp-server-status)
               (lambda (_) 'disconnected)))
      (let* ((entries (gptel-agent-mcp--list-entries))
             (entry (car entries))
             (columns (cadr entry)))
        ;; Command column should show URL
        (should (string= (aref columns 2) "https://api.example.com"))))))

(ert-deftest gptel-agent-mcp-test-list-entries-connected-tools-count ()
  "Test list entries shows tool count for connected servers."
  (let ((mcp-server-connections (make-hash-table :test 'equal))
        (mock-conn t))
    ;; Use symbol key to match how the code looks up connections
    (puthash 'connected-server mock-conn mcp-server-connections)

    (cl-letf (((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((connected-server . (:command "cmd")))))
              ((symbol-function 'gptel-agent--mcp-server-status)
               (lambda (_) 'connected))
              ((symbol-function 'gethash)
               (lambda (key _table) (when (eq key 'connected-server) mock-conn)))
              ((symbol-function 'mcp--tools)
               (lambda (_) '(tool1 tool2 tool3))))
      (let* ((entries (gptel-agent-mcp--list-entries))
             (entry (car entries))
             (columns (cadr entry)))
        ;; Tools count should be "3"
        (should (string= (aref columns 3) "3"))))))

;;;; List Mode Command Tests

(ert-deftest gptel-agent-mcp-test-list-connect-no-id ()
  "Test list-connect does nothing when no entry at point."
  (with-temp-buffer
    (gptel-agent-mcp-list-mode)
    (cl-letf (((symbol-function 'tabulated-list-get-id)
               (lambda () nil)))
      ;; Should not error
      (gptel-agent-mcp-list-connect))))

(ert-deftest gptel-agent-mcp-test-list-disconnect-no-id ()
  "Test list-disconnect does nothing when no entry at point."
  (with-temp-buffer
    (gptel-agent-mcp-list-mode)
    (cl-letf (((symbol-function 'tabulated-list-get-id)
               (lambda () nil)))
      ;; Should not error
      (gptel-agent-mcp-list-disconnect))))

(ert-deftest gptel-agent-mcp-test-list-mode-keymap ()
  "Test list mode has correct keybindings."
  (should (keymapp gptel-agent-mcp-list-mode-map))
  (should (eq (lookup-key gptel-agent-mcp-list-mode-map "c")
              'gptel-agent-mcp-list-connect))
  (should (eq (lookup-key gptel-agent-mcp-list-mode-map "d")
              'gptel-agent-mcp-list-disconnect))
  (should (eq (lookup-key gptel-agent-mcp-list-mode-map "r")
              'gptel-agent-mcp-list-restart))
  (should (eq (lookup-key gptel-agent-mcp-list-mode-map "g")
              'gptel-agent-mcp-list-refresh))
  (should (eq (lookup-key gptel-agent-mcp-list-mode-map "q")
              'quit-window)))

(ert-deftest gptel-agent-mcp-test-list-mode-format ()
  "Test list mode has correct column format."
  (with-temp-buffer
    (gptel-agent-mcp-list-mode)
    (should (equal tabulated-list-format
                   [("Name" 20 t)
                    ("Status" 12 t)
                    ("Command" 30 t)
                    ("Tools" 10 t)]))))

;;;; Auto-Connection Hook Tests

(ert-deftest gptel-agent-mcp-test-maybe-connect-disabled ()
  "Test auto-connect hook does nothing when disabled."
  (let ((gptel-agent-mcp-auto-connect nil)
        (connect-called nil))
    (cl-letf (((symbol-function 'mcp-connect-server)
               (lambda (&rest _) (setq connect-called t))))
      (gptel-agent--maybe-connect-mcp)
      (should-not connect-called))))

(ert-deftest gptel-agent-mcp-test-maybe-connect-no-mcp ()
  "Test auto-connect hook does nothing when mcp unavailable."
  (let ((gptel-agent-mcp-auto-connect t)
        (connect-called nil))
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _) nil))
              ((symbol-function 'mcp-connect-server)
               (lambda (&rest _) (setq connect-called t))))
      (gptel-agent--maybe-connect-mcp)
      (should-not connect-called))))

(ert-deftest gptel-agent-mcp-test-maybe-connect-no-project ()
  "Test auto-connect hook does nothing when not in project."
  (let ((gptel-agent-mcp-auto-connect t)
        (connect-called nil))
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'project-current)
               (lambda () nil))
              ((symbol-function 'mcp-connect-server)
               (lambda (&rest _) (setq connect-called t))))
      (gptel-agent--maybe-connect-mcp)
      (should-not connect-called))))

(ert-deftest gptel-agent-mcp-test-maybe-connect-only-auto-start ()
  "Test auto-connect only connects servers with :auto-start t."
  (let ((gptel-agent-mcp-auto-connect t)
        (connected-servers nil))
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((auto-server . (:command "auto" :auto-start t))
                   (manual-server . (:command "manual" :auto-start nil))
                   (default-server . (:command "default")))))
              ((symbol-function 'gptel-agent--mcp-server-running-p)
               (lambda (_) nil))
              ((symbol-function 'mcp-connect-server)
               (lambda (name &rest _)
                 (push name connected-servers))))
      (gptel-agent--maybe-connect-mcp)
      ;; Only auto-server should be connected
      (should (= (length connected-servers) 1))
      (should (eq (car connected-servers) 'auto-server)))))

(ert-deftest gptel-agent-mcp-test-maybe-connect-skips-running ()
  "Test auto-connect skips already running servers."
  (let ((gptel-agent-mcp-auto-connect t)
        (connected-servers nil))
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((running-server . (:command "running" :auto-start t))
                   (stopped-server . (:command "stopped" :auto-start t)))))
              ((symbol-function 'gptel-agent--mcp-server-running-p)
               (lambda (name)
                 (eq name 'running-server)))
              ((symbol-function 'mcp-connect-server)
               (lambda (name &rest _)
                 (push name connected-servers))))
      (gptel-agent--maybe-connect-mcp)
      ;; Only stopped-server should be connected
      (should (= (length connected-servers) 1))
      (should (eq (car connected-servers) 'stopped-server)))))

(ert-deftest gptel-agent-mcp-test-maybe-connect-handles-errors ()
  "Test auto-connect continues after connection errors."
  (let ((gptel-agent-mcp-auto-connect t)
        (connect-attempts 0)
        (messages nil))
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((fail-server . (:command "fail" :auto-start t))
                   (ok-server . (:command "ok" :auto-start t)))))
              ((symbol-function 'gptel-agent--mcp-server-running-p)
               (lambda (_) nil))
              ((symbol-function 'mcp-connect-server)
               (lambda (name &rest _)
                 (cl-incf connect-attempts)
                 (when (eq name 'fail-server)
                   (error "Connection failed"))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (gptel-agent--maybe-connect-mcp)
      ;; Both servers should be attempted
      (should (= connect-attempts 2))
      ;; Should have error message
      (should (cl-some (lambda (m) (string-match-p "Failed to auto-connect" m))
                       messages)))))

;;;; Merge Strategy Extended Tests

(ert-deftest gptel-agent-mcp-test-merge-combine-empty-project ()
  "Test combine strategy with nil project servers."
  (let ((mcp-hub-servers '((global-server . (:command "global"))))
        (gptel-agent-mcp-merge-strategy 'combine))
    (let ((merged (gptel-agent--merge-server-configs nil)))
      (should (= (length merged) 1))
      (should (assoc 'global-server merged)))))

(ert-deftest gptel-agent-mcp-test-merge-replace-empty-project ()
  "Test replace strategy with nil project servers."
  (let ((mcp-hub-servers '((global-server . (:command "global"))))
        (gptel-agent-mcp-merge-strategy 'replace))
    ;; With nil project servers, falls back to global
    (let ((merged (gptel-agent--merge-server-configs nil)))
      (should (= (length merged) 1))
      (should (assoc 'global-server merged)))))

(ert-deftest gptel-agent-mcp-test-merge-project-only-empty ()
  "Test project-only strategy with nil project servers."
  (let ((mcp-hub-servers '((global-server . (:command "global"))))
        (gptel-agent-mcp-merge-strategy 'project-only))
    (let ((merged (gptel-agent--merge-server-configs nil)))
      (should-not merged))))

(ert-deftest gptel-agent-mcp-test-merge-combine-no-global ()
  "Test combine strategy with nil global servers."
  (let ((mcp-hub-servers nil)
        (gptel-agent-mcp-merge-strategy 'combine)
        (project-servers '((proj-server . (:command "proj")))))
    (let ((merged (gptel-agent--merge-server-configs project-servers)))
      (should (= (length merged) 1))
      (should (assoc 'proj-server merged)))))

;;;; Permission Matching Extended Tests

(ert-deftest gptel-agent-mcp-test-tool-matches-case-mismatch ()
  "Test tool matching behavior with different case.
Note: wildcard-to-regexp is case-insensitive by default."
  ;; This matches because wildcard-to-regexp is case-insensitive
  (should (gptel-agent--mcp-tool-matches-p
           "mcp-GitHub:list-repos"
           "mcp-github:*")))

(ert-deftest gptel-agent-mcp-test-tool-matches-double-wildcard ()
  "Test tool matching with multiple wildcards."
  (should (gptel-agent--mcp-tool-matches-p
           "mcp-github-org:list-repos"
           "mcp-*-*:*")))

(ert-deftest gptel-agent-mcp-test-tool-matches-question-mark ()
  "Test tool matching with single character wildcard."
  (should (gptel-agent--mcp-tool-matches-p
           "mcp-github:list"
           "mcp-github:lis?")))

(ert-deftest gptel-agent-mcp-test-tool-matches-empty-pattern ()
  "Test tool matching with empty pattern."
  (should-not (gptel-agent--mcp-tool-matches-p
               "mcp-github:list"
               "")))

;;;; Connect Command Extended Tests

(ert-deftest gptel-agent-mcp-test-connect-no-mcp ()
  "Test connect fails when mcp package unavailable."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) nil)))
    (should-error (gptel-agent-mcp-connect) :type 'user-error)))

(ert-deftest gptel-agent-mcp-test-connect-no-project ()
  "Test connect fails when not in project."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) t))
            ((symbol-function 'project-current)
             (lambda () nil)))
    (should-error (gptel-agent-mcp-connect) :type 'user-error)))

(ert-deftest gptel-agent-mcp-test-connect-reports-counts ()
  "Test connect reports connected/failed/skipped counts."
  (let ((messages nil)
        (mcp-server-connections (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((ok-server . (:command "ok"))
                   (fail-server . (:command "fail"))
                   (running-server . (:command "running")))))
              ((symbol-function 'gptel-agent--mcp-server-running-p)
               (lambda (name) (eq name 'running-server)))
              ((symbol-function 'mcp-connect-server)
               (lambda (name &rest _)
                 (when (eq name 'fail-server)
                   (error "Failed"))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (gptel-agent-mcp-connect "/test/project")
      ;; Final message should have counts
      (let ((summary (car messages)))
        (should (string-match-p "Connected: 1" summary))
        (should (string-match-p "Failed: 1" summary))
        (should (string-match-p "Skipped: 1" summary))))))

;;;; Disconnect Command Extended Tests

(ert-deftest gptel-agent-mcp-test-disconnect-no-mcp ()
  "Test disconnect fails when mcp package unavailable."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) nil)))
    (should-error (gptel-agent-mcp-disconnect) :type 'user-error)))

(ert-deftest gptel-agent-mcp-test-disconnect-no-project ()
  "Test disconnect fails when not in project."
  (cl-letf (((symbol-function 'require)
             (lambda (&rest _) t))
            ((symbol-function 'project-current)
             (lambda () nil)))
    (should-error (gptel-agent-mcp-disconnect) :type 'user-error)))

(ert-deftest gptel-agent-mcp-test-disconnect-only-running ()
  "Test disconnect only stops running servers."
  (let ((stopped-servers nil))
    (cl-letf (((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((running . (:command "running"))
                   (stopped . (:command "stopped")))))
              ((symbol-function 'gptel-agent--mcp-server-running-p)
               (lambda (name) (eq name 'running)))
              ((symbol-function 'mcp-stop-server)
               (lambda (name) (push name stopped-servers)))
              ((symbol-function 'message) #'ignore))
      (gptel-agent-mcp-disconnect "/test/project")
      (should (= (length stopped-servers) 1))
      (should (eq (car stopped-servers) 'running)))))

;;;; Buffer Local Variable Tests

(ert-deftest gptel-agent-mcp-test-project-servers-variable ()
  "Test project servers variable exists."
  (should (boundp 'gptel-agent--mcp-project-servers)))

(ert-deftest gptel-agent-mcp-test-project-servers-alist ()
  "Test project servers is an alist."
  (let ((gptel-agent--mcp-project-servers
         '(("/project1" . ((server1 . (:command "cmd1"))))
           ("/project2" . ((server2 . (:command "cmd2")))))))
    (should (listp gptel-agent--mcp-project-servers))
    (should (consp (car gptel-agent--mcp-project-servers)))
    (should (stringp (caar gptel-agent--mcp-project-servers)))))

;;;; Edge Case Tests

(ert-deftest gptel-agent-mcp-test-expand-config-nil-values ()
  "Test config expansion handles nil values."
  (let ((config '(:command nil :args nil :url nil)))
    (let ((expanded (gptel-agent--expand-mcp-config config)))
      (should (null (plist-get expanded :command)))
      (should (null (plist-get expanded :args)))
      (should (null (plist-get expanded :url))))))

(ert-deftest gptel-agent-mcp-test-expand-config-preserves-other-keys ()
  "Test config expansion preserves non-expanded keys."
  (let ((config '(:command "cmd"
                  :auto-start t
                  :restart-on-failure t
                  :roots ("/path/1" "/path/2"))))
    (let ((expanded (gptel-agent--expand-mcp-config config)))
      (should (eq (plist-get expanded :auto-start) t))
      (should (eq (plist-get expanded :restart-on-failure) t))
      (should (equal (plist-get expanded :roots) '("/path/1" "/path/2"))))))

(ert-deftest gptel-agent-mcp-test-list-entries-error-status ()
  "Test list entries handles error status."
  (let ((mcp-server-connections (make-hash-table :test 'equal)))
    (puthash "error-server" t mcp-server-connections)

    (cl-letf (((symbol-function 'project-current)
               (lambda () t))
              ((symbol-function 'project-root)
               (lambda (_) "/test/project"))
              ((symbol-function 'gptel-agent--get-project-servers)
               (lambda (_)
                 '((error-server . (:command "cmd")))))
              ((symbol-function 'gptel-agent--mcp-server-status)
               (lambda (_) 'error)))
      (let* ((entries (gptel-agent-mcp--list-entries))
             (entry (car entries))
             (columns (cadr entry))
             (status-cell (aref columns 1)))
        ;; Status should show "error" with error face
        (should (string-match-p "error" status-cell))))))

(ert-deftest gptel-agent-mcp-test-find-file-hook-registered ()
  "Test that maybe-connect-mcp is registered on find-file-hook."
  (should (memq 'gptel-agent--maybe-connect-mcp find-file-hook)))

(provide 'gptel-agent-mcp-test)
;;; gptel-agent-mcp-test.el ends here
