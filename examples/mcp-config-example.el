;;; mcp-config-example.el --- Example MCP configuration -*- lexical-binding: t; -*-

;; This is an example .gptel-agent.el file demonstrating MCP server
;; configuration for gptel-agent.

(gptel-agent-project-config

 ;; Define MCP servers for this project
 :mcp-servers
 (
  ;; GitHub MCP server - provides GitHub API access
  (github . (:command "npx"
             :args ("-y" "@modelcontextprotocol/server-github")
             :env (("GITHUB_TOKEN" . "$GITHUB_TOKEN"))
             :auto-start t
             :restart-on-failure t))

  ;; Filesystem MCP server - provides file access
  (filesystem . (:command "npx"
                 :args ("-y" "@modelcontextprotocol/server-filesystem"
                        "${PROJECT_ROOT}/docs"
                        "${PROJECT_ROOT}/src")
                 :auto-start t))

  ;; Slack MCP server - provides Slack integration
  (slack . (:command "npx"
            :args ("-y" "@modelcontextprotocol/server-slack")
            :env (("SLACK_BOT_TOKEN" . "$SLACK_BOT_TOKEN")
                  ("SLACK_TEAM_ID" . "$SLACK_TEAM_ID"))
            :auto-start nil))

  ;; HTTP/SSE MCP server - remote server connection
  (remote-api . (:url "https://mcp.example.com/api"
                 :token "$MCP_API_TOKEN"
                 :auto-start nil))

  ;; Custom local MCP server
  (custom-tools . (:command "/usr/local/bin/custom-mcp-server"
                   :args ("--config" "${PROJECT_ROOT}/.mcp-config.json")
                   :env (("LOG_LEVEL" . "info"))
                   :roots ("${PROJECT_ROOT}"
                          "${HOME}/shared-data")
                   :auto-start nil)))

 ;; Define tool permissions
 :permissions
 '((* . ask)  ; Ask by default for all tools

   ;; Allow all GitHub MCP tools
   (mcp-github . allow)

   ;; Fine-grained control for filesystem tools
   (mcp-filesystem . ((pattern "mcp-filesystem:read-*" . allow)
                      (pattern "mcp-filesystem:write-*" . ask)
                      (pattern "mcp-filesystem:delete-*" . deny)
                      (* . ask)))

   ;; Pattern-based rules for all MCP tools
   (* . ((pattern "mcp-*:list-*" . allow)     ; Allow listing operations
         (pattern "mcp-*:read-*" . allow)     ; Allow read operations
         (pattern "mcp-*:create-*" . ask)     ; Ask for creation
         (pattern "mcp-*:update-*" . ask)     ; Ask for updates
         (pattern "mcp-*:delete-*" . deny)    ; Deny deletions
         (pattern "mcp-*:dangerous-*" . deny) ; Deny dangerous ops
         (* . ask)))                           ; Ask for everything else

   ;; Standard tool permissions
   (bash . ((pattern "git *" . allow)
            (pattern "npm *" . allow)
            (pattern "rm *" . deny)
            (* . ask)))
   (read . allow)
   (edit . ask)))

;;; Environment Variables Used:
;;
;; - GITHUB_TOKEN: Personal access token for GitHub API
;; - SLACK_BOT_TOKEN: Slack bot token for workspace access
;; - SLACK_TEAM_ID: Slack team/workspace identifier
;; - MCP_API_TOKEN: Authentication token for remote MCP server
;; - PROJECT_ROOT: Automatically set to project root directory
;; - HOME: User home directory
;;
;; Set these in your shell environment or in .envrc (if using direnv):
;;
;;   export GITHUB_TOKEN=ghp_xxxxx
;;   export SLACK_BOT_TOKEN=xoxb-xxxxx
;;   export MCP_API_TOKEN=xxxxx

;;; Usage:
;;
;; 1. Copy this file to your project root as .gptel-agent.el
;;
;; 2. Customize the server configurations for your needs
;;
;; 3. Set required environment variables
;;
;; 4. Enable auto-connection (optional):
;;
;;    (setq gptel-agent-mcp-auto-connect t)
;;
;; 5. Use interactive commands:
;;
;;    M-x gptel-agent-mcp-list           ; View server status
;;    M-x gptel-agent-mcp-connect        ; Connect servers
;;    M-x gptel-agent-mcp-disconnect     ; Disconnect servers
;;    M-x gptel-agent-mcp-status         ; Show status in minibuffer
;;
;; 6. Or use programmatically:
;;
;;    (gptel-agent-mcp-connect)          ; Connect project servers
;;    (gptel-agent-mcp-status)           ; Check status

;;; Notes:
;;
;; - Servers with :auto-start t will connect automatically when
;;   gptel-agent-mcp-auto-connect is enabled
;;
;; - Environment variables in :args and :env are expanded using
;;   $VAR or ${VAR} syntax
;;
;; - The :roots field specifies directories the MCP server can access
;;
;; - Permission rules are checked in order, first match wins
;;
;; - MCP tool names have the format "mcp-servername:toolname"
;;
;; - Use wildcards (*) in permission patterns for flexible matching

;;; mcp-config-example.el ends here
