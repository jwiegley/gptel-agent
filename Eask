(package "gptel-agent"
         "0.1.0"
         "Agent mode extensions for gptel with tool use")

(website-url "https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent")
(keywords "tools" "convenience" "ai" "llm")

(package-file "gptel-agent.el")

(files "gptel-agent.el"
       "gptel-agent-tools.el"
       "gptel-agent-tools-introspection.el"
       "gptel-agent-permissions.el"
       "gptel-agent-safety.el"
       "gptel-agent-compaction.el")

(script "test" "eask test ert test/*.el")
(script "lint" "eask lint package")
(script "compile" "eask compile")

(source "gnu")
(source "melpa")

(depends-on "emacs" "29.1")
(depends-on "compat" "30.1.0.0")
(depends-on "gptel" "0.9.9")

(development
 (depends-on "package-lint")
 (depends-on "ert"))
