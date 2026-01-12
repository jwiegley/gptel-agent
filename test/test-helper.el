;;; test-helper.el --- Shared test utilities for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared test utilities and macros for gptel-agent test suite.
;;
;; This file provides:
;; - Temporary project directory creation and cleanup
;; - Mock LLM response injection
;; - Mock tool execution helpers
;; - Common test fixtures and setup/teardown
;;
;; Usage:
;;   (require 'test-helper)
;;   (gptel-agent-test-with-temp-project
;;     (gptel-agent-test-write-config "(:permissions '((* . allow)))")
;;     ;; ... test code ...
;;     )

;;; Code:

(require 'cl-lib)
(require 'ert)

;;;; Temporary Directory Management

(defvar gptel-agent-test--temp-dirs nil
  "List of temporary directories created by tests for cleanup.")

(defun gptel-agent-test-make-temp-project (&optional name)
  "Create a temporary project directory for testing.
Optional NAME provides a prefix for the directory name.
Returns the path to the created directory.
The directory is registered for automatic cleanup."
  (let ((dir (make-temp-file (concat "gptel-agent-test-" (or name "project") "-") t)))
    (push dir gptel-agent-test--temp-dirs)
    dir))

(defun gptel-agent-test-cleanup ()
  "Clean up all temporary test directories.
Call this after tests complete or in unwind-protect."
  (dolist (dir gptel-agent-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq gptel-agent-test--temp-dirs nil))

(defmacro gptel-agent-test-with-cleanup (&rest body)
  "Execute BODY with automatic cleanup of test resources.
Ensures temp directories are deleted even if tests fail."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn ,@body)
     (gptel-agent-test-cleanup)))

(defmacro gptel-agent-test-with-temp-project (dir-var &rest body)
  "Create temp project, bind to DIR-VAR, execute BODY, then cleanup.
DIR-VAR is bound to the project root directory path."
  (declare (indent 1) (debug (symbolp body)))
  `(gptel-agent-test-with-cleanup
     (let ((,dir-var (gptel-agent-test-make-temp-project)))
       ,@body)))

;;;; Configuration File Helpers

(defun gptel-agent-test-write-config (dir content)
  "Write CONTENT to .gptel-agent.el in project DIR.
CONTENT should be an elisp form (without outer parens) or string."
  (let ((config-file (expand-file-name ".gptel-agent.el" dir)))
    (with-temp-file config-file
      (insert (if (stringp content)
                  content
                (prin1-to-string content))))
    config-file))

(defun gptel-agent-test-write-skill (dir name content)
  "Write a SKILL.md file with NAME and CONTENT in DIR.
NAME is the skill name (without extension).
CONTENT should be the full markdown content including frontmatter."
  (let ((skill-file (expand-file-name (concat name ".md") dir)))
    (with-temp-file skill-file
      (insert content))
    skill-file))

(defun gptel-agent-test-write-file (dir filename content)
  "Write CONTENT to FILENAME in DIR.
Returns the full path to the created file."
  (let ((file-path (expand-file-name filename dir)))
    (make-directory (file-name-directory file-path) t)
    (with-temp-file file-path
      (insert content))
    file-path))

;;;; Mock Response Helpers

(defvar gptel-agent-test--mock-responses nil
  "Queue of mock responses to return from LLM calls.")

(defun gptel-agent-test-queue-response (response)
  "Queue a mock RESPONSE to be returned by next LLM call.
RESPONSE should be a string (the LLM response text)."
  (push response gptel-agent-test--mock-responses))

(defun gptel-agent-test-clear-responses ()
  "Clear all queued mock responses."
  (setq gptel-agent-test--mock-responses nil))

(defun gptel-agent-test--pop-response ()
  "Pop and return the next mock response, or nil if empty."
  (pop gptel-agent-test--mock-responses))

(defmacro gptel-agent-test-with-mock-responses (responses &rest body)
  "Execute BODY with RESPONSES queued for LLM calls.
RESPONSES is a list of response strings to return in order."
  (declare (indent 1) (debug (form body)))
  `(unwind-protect
       (progn
         (setq gptel-agent-test--mock-responses (reverse ,responses))
         ,@body)
     (gptel-agent-test-clear-responses)))

;;;; Mock Tool Execution Helpers

(defvar gptel-agent-test--tool-call-log nil
  "Log of tool calls made during testing.
Each entry is a plist: (:tool NAME :args ARGS :result RESULT)")

(defun gptel-agent-test-log-tool-call (tool-name args result)
  "Log a tool call with TOOL-NAME, ARGS, and RESULT."
  (push (list :tool tool-name :args args :result result)
        gptel-agent-test--tool-call-log))

(defun gptel-agent-test-clear-tool-log ()
  "Clear the tool call log."
  (setq gptel-agent-test--tool-call-log nil))

(defun gptel-agent-test-get-tool-calls (&optional tool-name)
  "Get logged tool calls, optionally filtered by TOOL-NAME.
Returns list in chronological order (oldest first)."
  (let ((calls (reverse gptel-agent-test--tool-call-log)))
    (if tool-name
        (cl-remove-if-not
         (lambda (call)
           (eq (plist-get call :tool) tool-name))
         calls)
      calls)))

(defmacro gptel-agent-test-with-tool-logging (&rest body)
  "Execute BODY with tool call logging enabled."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
         (gptel-agent-test-clear-tool-log)
         ,@body)
     (gptel-agent-test-clear-tool-log)))

;;;; Message Creation Helpers

(defun gptel-agent-test-make-message (role content &optional props)
  "Create a test message plist with ROLE and CONTENT.
Optional PROPS is additional properties to merge."
  (append (list :role role :content content) props))

(defun gptel-agent-test-make-user-message (content)
  "Create a user message with CONTENT."
  (gptel-agent-test-make-message 'user content))

(defun gptel-agent-test-make-assistant-message (content)
  "Create an assistant message with CONTENT."
  (gptel-agent-test-make-message 'assistant content))

(defun gptel-agent-test-make-system-message (content)
  "Create a system message with CONTENT."
  (gptel-agent-test-make-message 'system content))

(defun gptel-agent-test-make-tool-result (tool-call-id content)
  "Create a tool result message with TOOL-CALL-ID and CONTENT."
  (gptel-agent-test-make-message 'tool content
                                  (list :tool-call-id tool-call-id)))

;;;; Test Environment Setup

(defun gptel-agent-test-setup ()
  "Set up test environment before running tests.
Call this in test setup or at start of test file."
  (gptel-agent-test-cleanup)
  (gptel-agent-test-clear-responses)
  (gptel-agent-test-clear-tool-log))

(defun gptel-agent-test-teardown ()
  "Clean up test environment after running tests.
Call this in test teardown or at end of test file."
  (gptel-agent-test-cleanup)
  (gptel-agent-test-clear-responses)
  (gptel-agent-test-clear-tool-log))

;;;; Assertion Helpers

(defun gptel-agent-test-assert-file-exists (path)
  "Assert that file at PATH exists."
  (should (file-exists-p path)))

(defun gptel-agent-test-assert-file-contains (path regexp)
  "Assert that file at PATH contains text matching REGEXP."
  (with-temp-buffer
    (insert-file-contents path)
    (should (string-match-p regexp (buffer-string)))))

(defun gptel-agent-test-assert-no-file (path)
  "Assert that no file exists at PATH."
  (should-not (file-exists-p path)))

;;;; Common Test Fixtures

(defconst gptel-agent-test-sample-permissions
  '(gptel-agent-project-config
    :permissions '((* . allow)
                   (bash . ((pattern "git *" . allow)
                            (pattern "rm *" . deny)
                            (* . ask)))
                   (edit . ask)))
  "Sample permission configuration for testing.")

(defconst gptel-agent-test-sample-skill
  "---
name: test-skill
description: A test skill for testing
prompt-position: append
---
You are a test assistant. This is test content."
  "Sample SKILL.md content for testing.")

(defconst gptel-agent-test-sample-conversation
  '((:role system :content "You are a helpful assistant.")
    (:role user :content "Hello, how are you?")
    (:role assistant :content "I'm doing well, thank you!"))
  "Sample conversation history for testing.")

(provide 'test-helper)
;;; test-helper.el ends here
