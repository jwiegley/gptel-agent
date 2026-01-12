;;; basic-config.el --- Basic gptel-agent configuration -*- lexical-binding: t; -*-

;; This is a minimal configuration example for gptel-agent.
;; Add this to your Emacs configuration (init.el or similar).

;;; Code:

;; Install gptel-agent from MELPA
(use-package gptel-agent
  :ensure t
  :config
  ;; Load agent definitions from directories
  (gptel-agent-update)

  ;; Optional: Add your own agent directory
  ;; (add-to-list 'gptel-agent-dirs "~/my-agents/")

  ;; Optional: Keybinding for quick access
  (global-set-key (kbd "C-c g a") #'gptel-agent)
  (global-set-key (kbd "C-c g s") #'gptel-agent-sessions)
  (global-set-key (kbd "C-c g r") #'gptel-agent-resume))

;; Ensure gptel is configured with your preferred backend
(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'claude-sonnet-4)  ; Or your preferred model
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :key 'gptel-api-key-from-auth-source
                        :stream t)))

;;; Usage:
;; 1. M-x gptel-agent to start a session
;; 2. Type your request and press C-c RET to send
;; 3. The agent will use tools automatically as needed

;;; basic-config.el ends here
