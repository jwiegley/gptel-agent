;;; sample-gptel-agent.el --- Sample project permission file -*- lexical-binding: t; -*-

;; This file demonstrates project-level gptel-agent configuration.
;; Place this as .gptel-agent.el in your project root.

;;; Tool Permissions
;;
;; Control which tools the agent can use in this project.
;; Patterns support glob matching (* matches any characters).

(:tool-permissions
 (:allow ("Read" "Grep" "Glob" "FetchURL")  ; Always allowed without prompt
  :deny ("Bash")                             ; Never allowed in this project
  :ask ("Write" "Edit" "Agent")))            ; Require confirmation

;;; Permission Examples
;;
;; Restrictive (documentation project):
;; (:tool-permissions
;;  (:allow ("Read" "Grep" "Glob")
;;   :deny ("Bash" "Write" "Edit")
;;   :ask nil))
;;
;; Permissive (personal project):
;; (:tool-permissions
;;  (:allow ("*")
;;   :deny nil
;;   :ask nil))
;;
;; Web-focused (research project):
;; (:tool-permissions
;;  (:allow ("Read" "WebSearch" "FetchURL" "YouTube")
;;   :deny ("Bash" "Write" "Edit")
;;   :ask ("Agent")))

;;; Pattern Examples
;;
;; Allow all read operations:
;; (:allow ("Read*"))
;;
;; Deny anything that writes:
;; (:deny ("*Write*" "*Edit*"))
;;
;; Ask for any MCP tools:
;; (:ask ("mcp-*"))
