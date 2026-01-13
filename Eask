(package "gptel-agent"
         "0.2.0"
         "Agent mode extensions for gptel with tool use")

(website-url "https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent")
(keywords "tools" "convenience" "ai" "llm")

(package-file "gptel-agent.el")

(files "gptel-agent.el"
       "gptel-agent-tools.el"
       "gptel-agent-tools-introspection.el"
       "gptel-agent-lsp.el"
       "gptel-agent-permissions.el"
       "gptel-agent-safety.el"
       "gptel-agent-compaction.el"
       "gptel-agent-transient.el"
       "gptel-agent-skills.el"
       "gptel-agent-modes.el"
       "gptel-agent-sessions.el"
       "gptel-agent-multi.el"
       "gptel-agent-stats.el"
       "gptel-agent-checkpoints.el"
       "gptel-agent.texi"
       "CHANGELOG.md")

(script "test" "eask test ert test/*.el")
(script "lint" "eask lint package")
(script "compile" "eask compile")
(script "info" "makeinfo gptel-agent.texi")

(source "gnu")
(source "melpa")

(depends-on "emacs" "29.1")
(depends-on "compat" "30.1.0.0")
(depends-on "gptel" "0.9.9")

(development
 (depends-on "package-lint")
 (depends-on "ert"))
