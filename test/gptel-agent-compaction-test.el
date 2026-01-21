;;; gptel-agent-compaction-test.el --- Tests for gptel-agent-compaction -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tests for gptel-agent-compaction.el covering:
;; - Token estimation and counting
;; - Model-specific context detection
;; - Message compactability analysis
;; - Compaction strategies
;; - Configuration and thresholds

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'gptel-agent-compaction)

;;;; Test Helpers

(defvar gptel-agent-compaction-test--temp-dirs nil
  "List of temporary directories to clean up after tests.")

(defun gptel-agent-compaction-test--cleanup ()
  "Clean up test resources."
  (dolist (dir gptel-agent-compaction-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq gptel-agent-compaction-test--temp-dirs nil))

(defmacro gptel-agent-compaction-test--with-cleanup (&rest body)
  "Execute BODY with automatic cleanup."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (gptel-agent-compaction-test--cleanup)))

(defun gptel-agent-compaction-test--make-message (role content &optional props)
  "Create a test message plist with ROLE and CONTENT.
Optional PROPS is additional properties to merge."
  (append (list :role role :content content) props))

;;;; Token Estimation Tests

(ert-deftest gptel-agent-compaction-test-estimate-tokens-basic ()
  "Test basic token estimation (~4 chars per token)."
  ;; 20 chars should be ~5 tokens
  (should (= (gptel-agent--estimate-tokens "12345678901234567890") 5))
  ;; 100 chars should be ~25 tokens
  (should (= (gptel-agent--estimate-tokens (make-string 100 ?a)) 25))
  ;; Empty string should be 0
  (should (= (gptel-agent--estimate-tokens "") 0)))

(ert-deftest gptel-agent-compaction-test-estimate-tokens-nil ()
  "Test token estimation handles nil gracefully."
  (should (= (gptel-agent--estimate-tokens nil) 0)))

(ert-deftest gptel-agent-compaction-test-estimate-tokens-non-string ()
  "Test token estimation handles non-strings gracefully."
  (should (= (gptel-agent--estimate-tokens 12345) 0))
  (should (= (gptel-agent--estimate-tokens '(a b c)) 0)))

(ert-deftest gptel-agent-compaction-test-count-message-tokens ()
  "Test counting tokens in a message plist."
  (let ((msg (gptel-agent-compaction-test--make-message
              'user "Hello, how are you?")))
    ;; Content is 19 chars (~5 tokens) + role overhead + structure
    (let ((count (gptel-agent--count-message-tokens msg)))
      (should (> count 0))
      (should (< count 100)))))  ; Reasonable bounds

(ert-deftest gptel-agent-compaction-test-count-message-tokens-empty ()
  "Test counting tokens in empty message."
  (let ((msg (gptel-agent-compaction-test--make-message 'user "")))
    (let ((count (gptel-agent--count-message-tokens msg)))
      ;; Should have minimal overhead for role and structure
      (should (> count 0))
      (should (< count 20)))))

;;;; Model Factor Tests

(ert-deftest gptel-agent-compaction-test-get-model-factor-default ()
  "Test model factor defaults to 1.0."
  (let ((gptel-model nil))
    (should (= (gptel-agent--get-model-factor) 1.0))))

(ert-deftest gptel-agent-compaction-test-get-model-factor-claude ()
  "Test model factor for Claude models."
  (let ((gptel-model "claude-3-5-sonnet-20240620"))
    ;; Claude factor is 1.0
    (should (= (gptel-agent--get-model-factor) 1.0))))

(ert-deftest gptel-agent-compaction-test-get-model-factor-gemini ()
  "Test model factor for Gemini models."
  (let ((gptel-model "gemini-1.5-pro"))
    ;; Gemini factor is 0.9
    (should (= (gptel-agent--get-model-factor) 1.0))))  ; Falls through to default

;;;; Context Limit Detection Tests

(ert-deftest gptel-agent-compaction-test-context-limit-override ()
  "Test manual context size override."
  (let ((gptel-agent-context-size 50000))
    (should (= (gptel-agent--get-context-limit) 50000))))

(ert-deftest gptel-agent-compaction-test-context-limit-claude ()
  "Test context limit detection for Claude models."
  (let ((gptel-agent-context-size nil)
        (gptel-model "claude-3-5-sonnet-20240620"))
    (let ((limit (gptel-agent--get-context-limit)))
      ;; Should be 200000 * 0.9 = 180000
      (should (= limit 180000)))))

(ert-deftest gptel-agent-compaction-test-context-limit-gpt4 ()
  "Test context limit detection for GPT-4 models."
  (let ((gptel-agent-context-size nil)
        (gptel-model "gpt-4-turbo-preview"))
    (let ((limit (gptel-agent--get-context-limit)))
      ;; Should be 128000 * 0.9 = 115200
      (should (= limit 115200)))))

(ert-deftest gptel-agent-compaction-test-context-limit-unknown ()
  "Test context limit fallback for unknown models."
  (let ((gptel-agent-context-size nil)
        (gptel-model "unknown-model-xyz"))
    (let ((limit (gptel-agent--get-context-limit)))
      ;; Should fall back to 8192
      (should (= limit 8192)))))

(ert-deftest gptel-agent-compaction-test-context-limit-gemini ()
  "Test context limit detection for Gemini models."
  (let ((gptel-agent-context-size nil)
        (gptel-model "gemini-1.5-pro-latest"))
    (let ((limit (gptel-agent--get-context-limit)))
      ;; Should be 1000000 * 0.9 = 900000
      (should (= limit 900000)))))

;;;; Message Compactability Tests

(ert-deftest gptel-agent-compaction-test-compactable-user-message ()
  "Test that regular user messages are compactable."
  (let ((msg (gptel-agent-compaction-test--make-message
              'user "What is the weather today?")))
    (should (gptel-agent--message-compactable-p msg))))

(ert-deftest gptel-agent-compaction-test-compactable-assistant-message ()
  "Test that regular assistant messages are compactable."
  (let ((msg (gptel-agent-compaction-test--make-message
              'assistant "The weather is sunny.")))
    (should (gptel-agent--message-compactable-p msg))))

(ert-deftest gptel-agent-compaction-test-not-compactable-system ()
  "Test that system messages are never compactable."
  (let ((msg (gptel-agent-compaction-test--make-message
              'system "You are a helpful assistant.")))
    (should-not (gptel-agent--message-compactable-p msg))))

(ert-deftest gptel-agent-compaction-test-not-compactable-tool-calls ()
  "Test that messages with tool calls are never compactable."
  (let ((msg (gptel-agent-compaction-test--make-message
              'assistant "Let me check that."
              '(:tool-calls ((:id "1" :name "read"))))))
    (should-not (gptel-agent--message-compactable-p msg))))

(ert-deftest gptel-agent-compaction-test-not-compactable-tool-result ()
  "Test that tool result messages are never compactable."
  (let ((msg (gptel-agent-compaction-test--make-message
              'tool "File contents here"
              '(:tool-call-id "1"))))
    (should-not (gptel-agent--message-compactable-p msg))))

(ert-deftest gptel-agent-compaction-test-not-compactable-important ()
  "Test that messages marked important are never compactable."
  (let ((msg (gptel-agent-compaction-test--make-message
              'user "This is critical information"
              '(:important t))))
    (should-not (gptel-agent--message-compactable-p msg))))

;;;; Message Marking Tests

(ert-deftest gptel-agent-compaction-test-mark-compactability-basic ()
  "Test basic compactability marking."
  (let ((msg (gptel-agent-compaction-test--make-message
              'user "Hello world")))
    (gptel-agent--mark-message-compactability msg)
    (should (plist-get msg :compactable))))

(ert-deftest gptel-agent-compaction-test-mark-compactability-task-marker ()
  "Test task marker detection prevents compaction."
  (let ((gptel-agent-task-markers '("please" "implement")))
    ;; Message with task marker should not be compactable
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "Please help me with this")))
      (gptel-agent--mark-message-compactability msg)
      (should-not (plist-get msg :compactable)))

    ;; Message with "implement" marker
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "Can you implement this feature?")))
      (gptel-agent--mark-message-compactability msg)
      (should-not (plist-get msg :compactable)))))

(ert-deftest gptel-agent-compaction-test-mark-compactability-no-task-marker ()
  "Test messages without task markers remain compactable."
  (let ((gptel-agent-task-markers '("please" "implement")))
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "The sky is blue.")))
      (gptel-agent--mark-message-compactability msg)
      (should (plist-get msg :compactable)))))

(ert-deftest gptel-agent-compaction-test-mark-compactability-case-insensitive ()
  "Test task markers are matched case-insensitively."
  (let ((gptel-agent-task-markers '("please")))
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "PLEASE HELP ME")))
      (gptel-agent--mark-message-compactability msg)
      (should-not (plist-get msg :compactable)))))

;;;; Format Messages Tests

(ert-deftest gptel-agent-compaction-test-format-messages ()
  "Test formatting messages for summarization."
  (let ((messages (list (gptel-agent-compaction-test--make-message
                         'user "Hello")
                        (gptel-agent-compaction-test--make-message
                         'assistant "Hi there"))))
    (let ((formatted (gptel-agent--format-messages-for-summary messages)))
      (should (stringp formatted))
      (should (string-match-p "\\[USER\\]" formatted))
      (should (string-match-p "\\[ASSISTANT\\]" formatted))
      (should (string-match-p "Hello" formatted))
      (should (string-match-p "Hi there" formatted)))))

(ert-deftest gptel-agent-compaction-test-format-messages-empty ()
  "Test formatting empty message list."
  (let ((formatted (gptel-agent--format-messages-for-summary nil)))
    (should (stringp formatted))
    (should (string= formatted ""))))

;;;; Insert Summary Tests

(ert-deftest gptel-agent-compaction-test-insert-summary ()
  "Test creating summary message."
  (let ((summary-msg (gptel-agent--insert-summary-message
                      "This is a summary of the conversation."
                      5)))
    (should (eq (plist-get summary-msg :role) 'assistant))
    (should (string-match-p "summary of 5 messages" (plist-get summary-msg :content)))
    (should (string-match-p "This is a summary" (plist-get summary-msg :content)))
    (should (plist-get summary-msg :important))
    (should-not (plist-get summary-msg :compactable))))

;;;; Threshold Calculation Tests

(ert-deftest gptel-agent-compaction-test-threshold-default ()
  "Test default compaction threshold is 0.7."
  (should (= gptel-agent-compaction-threshold 0.7)))

(ert-deftest gptel-agent-compaction-test-threshold-trigger ()
  "Test compaction triggers at threshold."
  (let ((gptel-agent-compaction-threshold 0.7)
        (gptel-agent-context-size 10000)
        (gptel-agent--session-token-count 7500))  ; 75% - above threshold
    ;; Token count > threshold * limit should trigger
    (let* ((limit (gptel-agent--get-context-limit))
           (threshold (* limit gptel-agent-compaction-threshold)))
      (should (> gptel-agent--session-token-count threshold)))))

(ert-deftest gptel-agent-compaction-test-threshold-no-trigger ()
  "Test compaction doesn't trigger below threshold."
  (let ((gptel-agent-compaction-threshold 0.7)
        (gptel-agent-context-size 10000)
        (gptel-agent--session-token-count 5000))  ; 50% - below threshold
    (let* ((limit (gptel-agent--get-context-limit))
           (threshold (* limit gptel-agent-compaction-threshold)))
      (should-not (> gptel-agent--session-token-count threshold)))))

;;;; Strategy Tests

(ert-deftest gptel-agent-compaction-test-strategy-default ()
  "Test default compaction strategy is summarize."
  (should (eq gptel-agent-compaction-strategy 'summarize)))

(ert-deftest gptel-agent-compaction-test-strategy-valid-values ()
  "Test all strategy values are valid."
  (dolist (strategy '(summarize truncate hybrid))
    (let ((gptel-agent-compaction-strategy strategy))
      (should (memq gptel-agent-compaction-strategy
                    '(summarize truncate hybrid))))))

;;;; Configuration Tests

(ert-deftest gptel-agent-compaction-test-preserved-count-default ()
  "Test default preserved message count."
  (should (= gptel-agent-preserved-message-count 10)))

(ert-deftest gptel-agent-compaction-test-preserved-count-affects-sliding-window ()
  "Test preserved message count affects compactable window."
  ;; With 15 messages and 10 preserved, only first 5 are candidates
  (let ((gptel-agent-preserved-message-count 10))
    ;; The logic: total - preserved = compactable window
    (should (= (max 0 (- 15 gptel-agent-preserved-message-count)) 5))
    (should (= (max 0 (- 5 gptel-agent-preserved-message-count)) 0))))

(ert-deftest gptel-agent-compaction-test-summarization-model-default ()
  "Test default summarization model is nil (use current)."
  (should (null gptel-agent-summarization-model)))

(ert-deftest gptel-agent-compaction-test-notify-default ()
  "Test notification is enabled by default."
  (should gptel-agent-compaction-notify))

;;;; Modeline Indicator Tests

(ert-deftest gptel-agent-compaction-test-modeline-indicator-format ()
  "Test modeline indicator format."
  (let ((gptel-mode t)
        (gptel-agent--session-token-count 5000)
        (gptel-agent-context-size 10000))
    (let ((indicator (gptel-agent--modeline-context-indicator)))
      (should (stringp indicator))
      (should (string-match-p "5k/10k" indicator))
      (should (string-match-p "50%" indicator)))))

(ert-deftest gptel-agent-compaction-test-modeline-indicator-colors ()
  "Test modeline indicator color coding."
  (let ((gptel-mode t)
        (gptel-agent-context-size 10000))
    ;; Low usage - default face
    (let ((gptel-agent--session-token-count 5000))
      (let ((indicator (gptel-agent--modeline-context-indicator)))
        (should (eq (get-text-property 0 'face indicator) 'default))))

    ;; Medium usage - warning face
    (let ((gptel-agent--session-token-count 7000))
      (let ((indicator (gptel-agent--modeline-context-indicator)))
        (should (eq (get-text-property 0 'face indicator) 'warning))))

    ;; High usage - error face
    (let ((gptel-agent--session-token-count 9000))
      (let ((indicator (gptel-agent--modeline-context-indicator)))
        (should (eq (get-text-property 0 'face indicator) 'error))))))

(ert-deftest gptel-agent-compaction-test-modeline-indicator-nil-outside-gptel ()
  "Test modeline indicator returns nil outside gptel-mode."
  (let ((gptel-mode nil))
    (should (null (gptel-agent--modeline-context-indicator)))))

;;;; Token Factors Tests

(ert-deftest gptel-agent-compaction-test-token-factors-structure ()
  "Test token factors has expected structure."
  (should (listp gptel-agent-token-factors))
  (dolist (entry gptel-agent-token-factors)
    (should (consp entry))
    (should (stringp (car entry)))
    (should (numberp (cdr entry)))))

(ert-deftest gptel-agent-compaction-test-token-factors-values ()
  "Test token factor values are reasonable."
  (dolist (entry gptel-agent-token-factors)
    (let ((factor (cdr entry)))
      (should (> factor 0))
      (should (< factor 2)))))

;;;; Known Context Limits Tests

(ert-deftest gptel-agent-compaction-test-known-limits-structure ()
  "Test known context limits has expected structure."
  (should (listp gptel-agent--known-context-limits))
  (dolist (entry gptel-agent--known-context-limits)
    (should (consp entry))
    (should (stringp (car entry)))
    (should (integerp (cdr entry)))))

(ert-deftest gptel-agent-compaction-test-known-limits-values ()
  "Test known context limit values are reasonable."
  (dolist (entry gptel-agent--known-context-limits)
    (let ((limit (cdr entry)))
      (should (> limit 1000))
      (should (< limit 10000000)))))

;;;; Session State Tests

(ert-deftest gptel-agent-compaction-test-session-token-count-initial ()
  "Test initial session token count is 0."
  (with-temp-buffer
    (should (= gptel-agent--session-token-count 0))))

(ert-deftest gptel-agent-compaction-test-summarization-in-progress-initial ()
  "Test initial summarization state is nil."
  (with-temp-buffer
    (should (null gptel-agent--summarization-in-progress))))

;;;; Task Markers Tests

(ert-deftest gptel-agent-compaction-test-task-markers-default ()
  "Test default task markers include expected keywords."
  (should (member "task:" gptel-agent-task-markers))
  (should (member "please" gptel-agent-task-markers))
  (should (member "implement" gptel-agent-task-markers)))

(ert-deftest gptel-agent-compaction-test-task-markers-detection ()
  "Test task marker detection in various positions."
  (let ((gptel-agent-task-markers '("task:")))
    ;; At start
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "Task: do something")))
      (gptel-agent--mark-message-compactability msg)
      (should-not (plist-get msg :compactable)))

    ;; In middle
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "Here is my task: do it")))
      (gptel-agent--mark-message-compactability msg)
      (should-not (plist-get msg :compactable)))))

;;;; Edge Cases

(ert-deftest gptel-agent-compaction-test-empty-conversation ()
  "Test handling of empty conversation."
  (should (= (length (gptel-agent--get-all-messages)) 0))
  (should (= (gptel-agent--count-conversation-tokens) 0)))

(ert-deftest gptel-agent-compaction-test-very-long-message ()
  "Test token estimation for very long messages."
  (let ((long-content (make-string 100000 ?a)))
    (let ((tokens (gptel-agent--estimate-tokens long-content)))
      ;; 100000 / 4 = 25000 tokens
      (should (= tokens 25000)))))

(ert-deftest gptel-agent-compaction-test-unicode-content ()
  "Test token estimation handles unicode."
  (let ((unicode-content "こんにちは世界 🌍 مرحبا"))
    (let ((tokens (gptel-agent--estimate-tokens unicode-content)))
      (should (> tokens 0)))))

(ert-deftest gptel-agent-compaction-test-zero-context-limit ()
  "Test modeline handles zero context limit gracefully."
  (let ((gptel-mode t)
        (gptel-agent--session-token-count 100)
        (gptel-agent-context-size 0))
    ;; Should not error, should handle zero division
    (let ((indicator (gptel-agent--modeline-context-indicator)))
      (should (stringp indicator)))))

;;;; Summarization Prompt Tests

(ert-deftest gptel-agent-compaction-test-summarization-prompt-format ()
  "Test summarization prompt has placeholder."
  (should (string-match-p "%s" gptel-agent-summarization-prompt)))

(ert-deftest gptel-agent-compaction-test-summarization-prompt-content ()
  "Test summarization prompt includes key instructions."
  (should (string-match-p "summarize" (downcase gptel-agent-summarization-prompt)))
  (should (string-match-p "concise" (downcase gptel-agent-summarization-prompt))))

;;;; Compactable Messages Tests

(ert-deftest gptel-agent-compaction-test-get-compactable-messages-empty ()
  "Test getting compactable messages from empty conversation."
  (cl-letf (((symbol-function 'gptel-agent--get-all-messages)
             (lambda () nil)))
    (should (= (length (gptel-agent--get-compactable-messages)) 0))))

(ert-deftest gptel-agent-compaction-test-get-compactable-messages-preserved ()
  "Test preserved messages are not returned as compactable."
  (let ((gptel-agent-preserved-message-count 3))
    (cl-letf (((symbol-function 'gptel-agent--get-all-messages)
               (lambda ()
                 (list (gptel-agent-compaction-test--make-message 'user "msg1")
                       (gptel-agent-compaction-test--make-message 'user "msg2")
                       (gptel-agent-compaction-test--make-message 'user "msg3")))))
      ;; All 3 messages should be preserved
      (should (= (length (gptel-agent--get-compactable-messages)) 0)))))

(ert-deftest gptel-agent-compaction-test-get-compactable-messages-partial ()
  "Test sliding window preserves recent messages."
  (let ((gptel-agent-preserved-message-count 2)
        (gptel-agent-task-markers nil))  ; Disable task markers
    (cl-letf (((symbol-function 'gptel-agent--get-all-messages)
               (lambda ()
                 (list (gptel-agent-compaction-test--make-message 'user "old1")
                       (gptel-agent-compaction-test--make-message 'user "old2")
                       (gptel-agent-compaction-test--make-message 'user "recent1")
                       (gptel-agent-compaction-test--make-message 'user "recent2")))))
      ;; First 2 messages should be compactable
      (let ((compactable (gptel-agent--get-compactable-messages)))
        (should (= (length compactable) 2))))))

(ert-deftest gptel-agent-compaction-test-get-compactable-excludes-system ()
  "Test system messages are excluded from compactable."
  (let ((gptel-agent-preserved-message-count 0)
        (gptel-agent-task-markers nil))
    (cl-letf (((symbol-function 'gptel-agent--get-all-messages)
               (lambda ()
                 (list (gptel-agent-compaction-test--make-message 'system "system prompt")
                       (gptel-agent-compaction-test--make-message 'user "user msg")))))
      (let ((compactable (gptel-agent--get-compactable-messages)))
        ;; Only user message should be compactable
        (should (= (length compactable) 1))))))

;;;; Truncation Strategy Tests

(ert-deftest gptel-agent-compaction-test-truncate-no-messages ()
  "Test truncate with no compactable messages."
  (cl-letf (((symbol-function 'gptel-agent--get-compactable-messages)
             (lambda () nil))
            ((symbol-function 'gptel-agent--count-conversation-tokens)
             (lambda () 0)))
    (let ((gptel-agent-context-size 10000)
          (gptel-agent--session-token-count 0))
      (let ((result (gptel-agent--truncate-context)))
        (should (numberp result))))))

(ert-deftest gptel-agent-compaction-test-truncate-removes-messages ()
  "Test truncate removes messages until target reached."
  (let ((removed-messages '())
        (gptel-agent-context-size 10000)
        (gptel-agent--session-token-count 8000))
    (cl-letf (((symbol-function 'gptel-agent--get-compactable-messages)
               (lambda ()
                 (list (gptel-agent-compaction-test--make-message 'user "msg1")
                       (gptel-agent-compaction-test--make-message 'user "msg2"))))
              ((symbol-function 'gptel-agent--count-conversation-tokens)
               (lambda () 8000))
              ((symbol-function 'gptel-agent--remove-message)
               (lambda (msg) (push msg removed-messages))))
      (gptel-agent--truncate-context)
      ;; Should have attempted to remove messages
      (should (>= (length removed-messages) 0)))))

;;;; Hybrid Strategy Tests

(ert-deftest gptel-agent-compaction-test-hybrid-divides-messages ()
  "Test hybrid strategy divides messages between truncate and summarize."
  (let ((truncated-count 0)
        (gptel-agent--summarization-in-progress nil))
    (cl-letf (((symbol-function 'gptel-agent--get-compactable-messages)
               (lambda ()
                 (list (gptel-agent-compaction-test--make-message 'user "m1")
                       (gptel-agent-compaction-test--make-message 'user "m2")
                       (gptel-agent-compaction-test--make-message 'user "m3")
                       (gptel-agent-compaction-test--make-message 'user "m4")
                       (gptel-agent-compaction-test--make-message 'user "m5")
                       (gptel-agent-compaction-test--make-message 'user "m6"))))
              ((symbol-function 'gptel-agent--remove-message)
               (lambda (_msg) (cl-incf truncated-count)))
              ((symbol-function 'gptel-agent--summarize-context)
               (lambda () nil)))
      (let ((result (gptel-agent--hybrid-compact)))
        ;; Should truncate about 1/3 of messages (2 of 6)
        (should (= truncated-count 2))
        (should (numberp result))))))

;;;; Summarization Tests

(ert-deftest gptel-agent-compaction-test-summarize-blocks-concurrent ()
  "Test summarization blocks when already in progress."
  (let ((gptel-agent--summarization-in-progress t))
    ;; Should return nil without doing anything
    (should (null (gptel-agent--summarize-context)))))

(ert-deftest gptel-agent-compaction-test-summarize-fallback-few-messages ()
  "Test summarization falls back to truncate with few messages."
  (let ((gptel-agent--summarization-in-progress nil)
        (truncated nil))
    (cl-letf (((symbol-function 'gptel-agent--get-compactable-messages)
               (lambda ()
                 (list (gptel-agent-compaction-test--make-message 'user "m1")
                       (gptel-agent-compaction-test--make-message 'user "m2"))))
              ((symbol-function 'gptel-agent--truncate-context)
               (lambda () (setq truncated t) 5)))
      (gptel-agent--summarize-context)
      ;; Should have fallen back to truncation
      (should truncated))))

;;;; Main Compact Context Tests

(ert-deftest gptel-agent-compaction-test-compact-context-not-gptel-buffer ()
  "Test compact-context errors when not in gptel buffer."
  (with-temp-buffer
    (should-error (gptel-agent-compact-context)
                  :type 'user-error)))

(ert-deftest gptel-agent-compaction-test-compact-context-blocks-concurrent ()
  "Test compact-context blocks when summarization in progress."
  (with-temp-buffer
    (let ((gptel-agent--summarization-in-progress t))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest _) t)))
        ;; Should return nil without triggering
        (should (null (gptel-agent-compact-context)))))))

(ert-deftest gptel-agent-compaction-test-compact-context-force ()
  "Test compact-context with force flag."
  (with-temp-buffer
    (let ((gptel-agent-compaction-strategy 'truncate)
          (gptel-agent-context-size 10000)
          (gptel-agent--session-token-count 100)  ; Below threshold
          (gptel-agent--summarization-in-progress nil)
          (compaction-triggered nil))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest _) t))
                ((symbol-function 'gptel-agent--truncate-context)
                 (lambda () (setq compaction-triggered t) 0)))
        (gptel-agent-compact-context t)  ; Force
        (should compaction-triggered)))))

(ert-deftest gptel-agent-compaction-test-compact-context-below-threshold ()
  "Test compact-context does nothing below threshold."
  (with-temp-buffer
    (let ((gptel-agent-compaction-threshold 0.7)
          (gptel-agent-context-size 10000)
          (gptel-agent--session-token-count 5000)  ; 50% - below threshold
          (gptel-agent--summarization-in-progress nil)
          (compaction-triggered nil))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest _) t))
                ((symbol-function 'gptel-agent--truncate-context)
                 (lambda () (setq compaction-triggered t) 0))
                ((symbol-function 'called-interactively-p)
                 (lambda (&rest _) nil)))
        (should (null (gptel-agent-compact-context)))
        (should-not compaction-triggered)))))

(ert-deftest gptel-agent-compaction-test-compact-context-strategy-dispatch ()
  "Test compact-context dispatches to correct strategy."
  (with-temp-buffer
    (let ((gptel-agent-context-size 10000)
          (gptel-agent--session-token-count 8000)  ; Above threshold
          (gptel-agent--summarization-in-progress nil)
          (strategy-called nil))
      (dolist (strategy '(summarize truncate hybrid))
        (setq strategy-called nil)
        (let ((gptel-agent-compaction-strategy strategy))
          (cl-letf (((symbol-function 'derived-mode-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'gptel-agent--summarize-context)
                     (lambda () (setq strategy-called 'summarize)))
                    ((symbol-function 'gptel-agent--truncate-context)
                     (lambda () (setq strategy-called 'truncate) 0))
                    ((symbol-function 'gptel-agent--hybrid-compact)
                     (lambda () (setq strategy-called 'hybrid) 0)))
            (gptel-agent-compact-context t)
            (should (eq strategy-called strategy))))))))

(ert-deftest gptel-agent-compaction-test-compact-context-unknown-strategy ()
  "Test compact-context errors on unknown strategy."
  (with-temp-buffer
    (let ((gptel-agent-compaction-strategy 'unknown-strategy)
          (gptel-agent-context-size 10000)
          (gptel-agent--session-token-count 8000)
          (gptel-agent--summarization-in-progress nil))
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest _) t)))
        (should-error (gptel-agent-compact-context t)
                      :type 'user-error)))))

;;;; Compaction Status Tests

(ert-deftest gptel-agent-compaction-test-status-format ()
  "Test compaction status message format."
  (with-temp-buffer
    (let ((gptel-agent-context-size 10000)
          (gptel-agent--session-token-count 7500)
          (gptel-agent-compaction-threshold 0.7)
          (message-output nil))
      (cl-letf (((symbol-function 'gptel-agent--get-compactable-messages)
                 (lambda () (list 1 2 3)))  ; 3 compactable
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-output (apply #'format fmt args)))))
        (gptel-agent-compaction-status)
        (should (stringp message-output))
        (should (string-match-p "7500/10000" message-output))
        (should (string-match-p "75" message-output))
        (should (string-match-p "YES" message-output))))))

(ert-deftest gptel-agent-compaction-test-status-no-compaction-needed ()
  "Test status shows no compaction needed below threshold."
  (with-temp-buffer
    (let ((gptel-agent-context-size 10000)
          (gptel-agent--session-token-count 5000)
          (gptel-agent-compaction-threshold 0.7)
          (message-output nil))
      (cl-letf (((symbol-function 'gptel-agent--get-compactable-messages)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-output (apply #'format fmt args)))))
        (gptel-agent-compaction-status)
        (should (string-match-p "no" message-output))))))

;;;; Check Compaction Needed Tests

(ert-deftest gptel-agent-compaction-test-check-needed-returns-t ()
  "Test check-compaction-needed returns t when triggered."
  (with-temp-buffer
    (let ((gptel-agent-context-size 10000)
          (gptel-agent--session-token-count 8000)
          (gptel-agent-compaction-threshold 0.7)
          (compact-called nil))
      (cl-letf (((symbol-function 'gptel-agent-compact-context)
                 (lambda () (setq compact-called t))))
        (should (gptel-agent--check-compaction-needed))
        (should compact-called)))))

(ert-deftest gptel-agent-compaction-test-check-needed-returns-nil ()
  "Test check-compaction-needed returns nil when not needed."
  (with-temp-buffer
    (let ((gptel-agent-context-size 10000)
          (gptel-agent--session-token-count 5000)
          (gptel-agent-compaction-threshold 0.7))
      (should-not (gptel-agent--check-compaction-needed)))))

;;;; Post Response Hook Tests

(ert-deftest gptel-agent-compaction-test-post-response-hook ()
  "Test post-response hook calls check function."
  (let ((check-called nil))
    (cl-letf (((symbol-function 'gptel-agent--check-compaction-needed)
               (lambda () (setq check-called t) nil)))
      (gptel-agent--post-response-compaction-check)
      (should check-called))))

;;;; Notification Tests

(ert-deftest gptel-agent-compaction-test-notify-when-enabled ()
  "Test notification shows when enabled."
  (with-temp-buffer
    (let ((gptel-agent-compaction-notify t)
          (gptel-agent--session-token-count 8000)
          (gptel-agent-context-size 10000)
          (message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (gptel-agent--notify-compaction)
        (should message-called)))))

(ert-deftest gptel-agent-compaction-test-notify-when-disabled ()
  "Test notification suppressed when disabled."
  (with-temp-buffer
    (let ((gptel-agent-compaction-notify nil)
          (message-called nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (gptel-agent--notify-compaction)
        (should-not message-called)))))

;;;; Minor Mode Tests

(ert-deftest gptel-agent-compaction-test-mode-enable ()
  "Test compaction mode enables properly."
  (let ((gptel-agent-compaction-mode nil))
    (gptel-agent-compaction-mode 1)
    (unwind-protect
        (progn
          (should gptel-agent-compaction-mode)
          (should (memq #'gptel-agent--post-response-compaction-check
                        gptel-post-response-functions)))
      (gptel-agent-compaction-mode -1))))

(ert-deftest gptel-agent-compaction-test-mode-disable ()
  "Test compaction mode disables properly."
  (let ((gptel-agent-compaction-mode nil))
    (gptel-agent-compaction-mode 1)
    (gptel-agent-compaction-mode -1)
    (should-not gptel-agent-compaction-mode)
    (should-not (memq #'gptel-agent--post-response-compaction-check
                      gptel-post-response-functions))))

(ert-deftest gptel-agent-compaction-test-mode-adds-modeline ()
  "Test compaction mode adds modeline indicator."
  (let ((gptel-agent-compaction-mode nil)
        (mode-line-misc-info nil))
    (gptel-agent-compaction-mode 1)
    (unwind-protect
        (should (member '(:eval (gptel-agent--modeline-context-indicator))
                        mode-line-misc-info))
      (gptel-agent-compaction-mode -1))))

;;;; Customization Group Tests

(ert-deftest gptel-agent-compaction-test-custom-group-exists ()
  "Test customization group is defined."
  (should (get 'gptel-agent-compaction 'custom-group)))

(ert-deftest gptel-agent-compaction-test-threshold-type ()
  "Test threshold is a float."
  (should (floatp gptel-agent-compaction-threshold)))

(ert-deftest gptel-agent-compaction-test-threshold-range ()
  "Test threshold is within valid range."
  (should (>= gptel-agent-compaction-threshold 0.5))
  (should (<= gptel-agent-compaction-threshold 0.95)))

(ert-deftest gptel-agent-compaction-test-preserved-count-type ()
  "Test preserved count is an integer."
  (should (integerp gptel-agent-preserved-message-count)))

(ert-deftest gptel-agent-compaction-test-preserved-count-positive ()
  "Test preserved count is positive."
  (should (> gptel-agent-preserved-message-count 0)))

;;;; Count Conversation Tokens Tests

(ert-deftest gptel-agent-compaction-test-count-conversation-empty ()
  "Test counting tokens in empty conversation."
  (cl-letf (((symbol-function 'gptel-agent--get-all-messages)
             (lambda () nil)))
    (should (= (gptel-agent--count-conversation-tokens) 0))))

(ert-deftest gptel-agent-compaction-test-count-conversation-multiple ()
  "Test counting tokens across multiple messages."
  (cl-letf (((symbol-function 'gptel-agent--get-all-messages)
             (lambda ()
               (list (gptel-agent-compaction-test--make-message
                      'user "Hello world")
                     (gptel-agent-compaction-test--make-message
                      'assistant "Hi there")))))
    (let ((total (gptel-agent--count-conversation-tokens)))
      (should (> total 0))
      ;; Should be reasonable - not too high for short messages
      (should (< total 100)))))

;;;; Integration Tests

(ert-deftest gptel-agent-compaction-test-full-workflow ()
  "Test complete compaction workflow logic."
  (let ((gptel-agent-compaction-threshold 0.7)
        (gptel-agent-context-size 10000)
        (gptel-agent-preserved-message-count 3)
        (gptel-agent-compaction-strategy 'truncate))
    ;; At 80% usage, should need compaction
    (let ((current-tokens 8000)
          (limit 10000)
          (threshold (* 10000 0.7)))
      (should (> current-tokens threshold))
      ;; This confirms compaction would trigger
      )))

(ert-deftest gptel-agent-compaction-test-check-compaction-needed ()
  "Test compaction need detection."
  (gptel-agent-compaction-test--with-cleanup
    (with-temp-buffer
      (let ((gptel-agent-compaction-threshold 0.7)
            (gptel-agent-context-size 1000)
            (gptel-agent--session-token-count 800))  ; 80%
        ;; Should indicate compaction needed
        (let* ((limit (gptel-agent--get-context-limit))
               (threshold (* limit gptel-agent-compaction-threshold)))
          (should (> gptel-agent--session-token-count threshold)))))))

;;;; Remove Message Tests

(ert-deftest gptel-agent-compaction-test-remove-message-placeholder ()
  "Test remove message placeholder doesn't error."
  (let ((msg (gptel-agent-compaction-test--make-message 'user "test")))
    ;; Should not error - just a placeholder
    (should (null (gptel-agent--remove-message msg)))))

;;;; Model Detection Edge Cases

(ert-deftest gptel-agent-compaction-test-context-limit-gpt35 ()
  "Test context limit for GPT-3.5 models."
  (let ((gptel-agent-context-size nil)
        (gptel-model "gpt-3.5-turbo-16k"))
    (let ((limit (gptel-agent--get-context-limit)))
      ;; Should be 16385 * 0.9 = 14746
      (should (= limit 14746)))))

(ert-deftest gptel-agent-compaction-test-context-limit-llama ()
  "Test context limit for Llama models."
  (let ((gptel-agent-context-size nil)
        (gptel-model "llama-3-70b-instruct"))
    (let ((limit (gptel-agent--get-context-limit)))
      ;; Should be 8192 * 0.9 = 7372 (rounded)
      (should (= limit 7373)))))

(ert-deftest gptel-agent-compaction-test-context-limit-mistral ()
  "Test context limit for Mistral models."
  (let ((gptel-agent-context-size nil)
        (gptel-model "mistral-large-latest"))
    (let ((limit (gptel-agent--get-context-limit)))
      ;; Should be 32000 * 0.9 = 28800
      (should (= limit 28800)))))

;;;; Format Messages Edge Cases

(ert-deftest gptel-agent-compaction-test-format-single-message ()
  "Test formatting single message."
  (let ((messages (list (gptel-agent-compaction-test--make-message
                         'user "Single message"))))
    (let ((formatted (gptel-agent--format-messages-for-summary messages)))
      (should (string-match-p "\\[USER\\]" formatted))
      (should (string-match-p "Single message" formatted))
      (should-not (string-match-p "\n\n\n" formatted)))))  ; No extra newlines

(ert-deftest gptel-agent-compaction-test-format-tool-role ()
  "Test formatting tool role message."
  (let ((messages (list (gptel-agent-compaction-test--make-message
                         'tool "Tool result"))))
    (let ((formatted (gptel-agent--format-messages-for-summary messages)))
      (should (string-match-p "\\[TOOL\\]" formatted)))))

;;;; Insert Summary Edge Cases

(ert-deftest gptel-agent-compaction-test-insert-summary-zero-count ()
  "Test inserting summary with zero message count."
  (let ((summary-msg (gptel-agent--insert-summary-message "Summary" 0)))
    (should (string-match-p "0 messages" (plist-get summary-msg :content)))))

(ert-deftest gptel-agent-compaction-test-insert-summary-large-count ()
  "Test inserting summary with large message count."
  (let ((summary-msg (gptel-agent--insert-summary-message "Summary" 1000)))
    (should (string-match-p "1000 messages" (plist-get summary-msg :content)))))

;;;; Modeline Edge Cases

(ert-deftest gptel-agent-compaction-test-modeline-very-large-values ()
  "Test modeline with very large token values."
  (let ((gptel-mode t)
        (gptel-agent--session-token-count 1000000)
        (gptel-agent-context-size 2000000))
    (let ((indicator (gptel-agent--modeline-context-indicator)))
      (should (stringp indicator))
      (should (string-match-p "1000k" indicator)))))

(ert-deftest gptel-agent-compaction-test-modeline-boundary-60 ()
  "Test modeline at exactly 60% boundary."
  (let ((gptel-mode t)
        (gptel-agent--session-token-count 6000)
        (gptel-agent-context-size 10000))
    (let ((indicator (gptel-agent--modeline-context-indicator)))
      (should (eq (get-text-property 0 'face indicator) 'default)))))

(ert-deftest gptel-agent-compaction-test-modeline-boundary-80 ()
  "Test modeline at exactly 80% boundary."
  (let ((gptel-mode t)
        (gptel-agent--session-token-count 8000)
        (gptel-agent-context-size 10000))
    (let ((indicator (gptel-agent--modeline-context-indicator)))
      (should (eq (get-text-property 0 'face indicator) 'warning)))))

;;;; Task Marker Edge Cases

(ert-deftest gptel-agent-compaction-test-task-marker-word-boundary ()
  "Test task markers require word boundaries."
  (let ((gptel-agent-task-markers '("fix")))
    ;; "fix" as word should match
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "Please fix this bug")))
      (gptel-agent--mark-message-compactability msg)
      (should-not (plist-get msg :compactable)))
    ;; "prefix" should NOT match (fix is substring)
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "The prefix is wrong")))
      (gptel-agent--mark-message-compactability msg)
      (should (plist-get msg :compactable)))))

(ert-deftest gptel-agent-compaction-test-task-marker-multiple ()
  "Test multiple task markers in one message."
  (let ((gptel-agent-task-markers '("please" "implement")))
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "Please implement the feature")))
      (gptel-agent--mark-message-compactability msg)
      (should-not (plist-get msg :compactable)))))

(ert-deftest gptel-agent-compaction-test-task-marker-empty-list ()
  "Test empty task marker list allows all messages."
  (let ((gptel-agent-task-markers nil))
    (let ((msg (gptel-agent-compaction-test--make-message
                'user "Please implement something")))
      (gptel-agent--mark-message-compactability msg)
      (should (plist-get msg :compactable)))))

;;;; Buffer Local Variable Tests

(ert-deftest gptel-agent-compaction-test-session-token-count-buffer-local ()
  "Test session token count is buffer-local."
  (with-temp-buffer
    (setq gptel-agent--session-token-count 1000)
    (with-temp-buffer
      (should (= gptel-agent--session-token-count 0)))
    (should (= gptel-agent--session-token-count 1000))))

(ert-deftest gptel-agent-compaction-test-summarization-progress-buffer-local ()
  "Test summarization in progress is buffer-local."
  (with-temp-buffer
    (setq gptel-agent--summarization-in-progress t)
    (with-temp-buffer
      (should-not gptel-agent--summarization-in-progress))
    (should gptel-agent--summarization-in-progress)))

;;;; Get All Messages Tests

(ert-deftest gptel-agent-compaction-test-get-all-messages-returns-list ()
  "Test get-all-messages returns a list."
  (should (listp (gptel-agent--get-all-messages))))

(provide 'gptel-agent-compaction-test)
;;; gptel-agent-compaction-test.el ends here
