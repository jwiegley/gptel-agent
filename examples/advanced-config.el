;;; advanced-config.el --- Advanced gptel-agent configuration -*- lexical-binding: t; -*-

;; This is a comprehensive configuration example showcasing all
;; gptel-agent features including sessions, checkpoints, and statistics.

;;; Code:

(use-package gptel-agent
  :ensure t
  :init
  ;; Pre-configuration settings
  (setq gptel-agent-dirs
        '("~/.emacs.d/agents/"           ; Personal agents
          "~/work/team-agents/"))         ; Team-shared agents

  :config
  ;; Load all agent definitions
  (gptel-agent-update)

  ;; Session persistence
  (setq gptel-agent-session-auto-save t)
  (setq gptel-agent-session-idle-delay 30)  ; Auto-save after 30s idle
  (setq gptel-agent-session-retention-days 30)

  ;; Checkpoint configuration
  (setq gptel-agent-checkpoint-frequency 5)   ; Checkpoint every 5 tool calls
  (setq gptel-agent-checkpoint-retention 10)  ; Keep 10 checkpoints per session
  (setq gptel-agent-checkpoint-auto-recover t)

  ;; Token statistics and budget
  (setq gptel-agent-stats-header-line t)
  (setq gptel-agent-budget-limit 10.0)         ; $10 budget limit
  (setq gptel-agent-budget-action 'confirm)    ; Require confirmation at limit

  ;; Custom model pricing (if using non-standard models)
  (setq gptel-agent-model-pricing
        '(("claude-opus-4" . (:input 15.00 :output 75.00))
          ("claude-sonnet-4" . (:input 3.00 :output 15.00))
          ("gpt-4o" . (:input 2.50 :output 10.00))
          ("gpt-4o-mini" . (:input 0.15 :output 0.60))
          ("my-custom-model" . (:input 1.00 :output 2.00))))

  ;; Context compaction
  (setq gptel-agent-compaction-threshold 100000)
  (setq gptel-agent-compaction-strategy 'hybrid)
  (setq gptel-agent-compaction-preserved-count 10)

  ;; Safety settings
  (setq gptel-agent-external-access-policy 'ask)
  (setq gptel-agent-external-whitelist
        '("~/common-libs/"
          "/usr/local/share/docs/"))

  ;; Custom modes
  (setq gptel-agent-custom-modes
        '((review . (:tools (Read Grep Glob)
                     :prompt "You are a code reviewer. Analyze code quality, security, and performance."
                     :face font-lock-warning-face))
          (research . (:tools (WebSearch FetchURL Read)
                       :prompt "You are a research assistant. Find and summarize information."
                       :face font-lock-doc-face))))

  ;; Enable tool preview mode by default
  (add-hook 'gptel-agent-mode-hook #'gptel-agent-tool-preview-mode)

  ;; Global keybindings
  :bind (("C-c g a" . gptel-agent)
         ("C-c g s" . gptel-agent-sessions)
         ("C-c g r" . gptel-agent-resume)
         ("C-c g c" . gptel-agent-checkpoint)
         ("C-c g S" . gptel-agent-stats)))

;; Per-project settings via directory-local variables
;; Add to .dir-locals.el in your project:
;;
;; ((nil . ((gptel-agent-auto-approve . ("Read" "Grep" "Glob")))))

;; gptel backend configuration
(use-package gptel
  :ensure t
  :config
  ;; Configure multiple backends
  (gptel-make-anthropic "Claude"
    :key 'gptel-api-key-from-auth-source
    :stream t)

  (gptel-make-openai "GPT"
    :key 'gptel-api-key-from-auth-source
    :stream t)

  ;; Set default
  (setq gptel-backend (alist-get "Claude" gptel--known-backends nil nil #'equal))
  (setq gptel-model 'claude-sonnet-4))

;;; Integration with other packages

;; Project.el integration - agent uses project root automatically
(with-eval-after-load 'project
  (add-to-list 'project-switch-commands
               '(gptel-agent "Start gptel-agent" ?g)))

;; Embark integration for agent actions
(with-eval-after-load 'embark
  (defun embark-gptel-agent-send-region (beg end)
    "Send region to gptel-agent."
    (gptel-agent default-directory)
    (insert (buffer-substring beg end))
    (gptel-send))

  (define-key embark-region-map (kbd "G") #'embark-gptel-agent-send-region))

;;; advanced-config.el ends here
