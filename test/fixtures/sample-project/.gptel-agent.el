;;; .gptel-agent.el --- Sample project configuration -*- lexical-binding: t; -*-

;; Sample gptel-agent configuration for testing purposes

(gptel-agent-project-config
 :permissions '((* . allow)
                (bash . ((pattern "git *" . allow)
                         (pattern "npm *" . allow)
                         (pattern "rm *" . deny)
                         (pattern "sudo *" . deny)
                         (pattern "chmod *" . deny)
                         (* . ask)))
                (edit . ask)
                (write . ask)
                (eval . deny))
 :external-paths '("/tmp/" "/var/log/")
 :mcp-servers '((github . (:command "github-mcp" :args nil))))

;;; .gptel-agent.el ends here
