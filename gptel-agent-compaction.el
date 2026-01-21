;;; gptel-agent-compaction.el --- Automatic prompt compaction for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: January 2025
;; Version: 0.1.0
;; Keywords: tools, convenience
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5") (gptel "0.9.9"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module implements automatic prompt compaction for gptel-agent,
;; preventing context window overflow during long conversations.
;;
;; Features:
;; - Token counting with model-specific adjustments
;; - Automatic threshold-based compaction triggering
;; - Multiple compaction strategies: summarize, truncate, hybrid
;; - Async LLM-based summarization of old messages
;; - Smart message preservation (system prompts, tool calls, recent msgs)
;; - Configurable sliding window for recent message retention
;; - Non-blocking UI during compaction operations
;; - Modeline indicator for context usage
;;
;; Usage:
;;
;;   (require 'gptel-agent-compaction)
;;   (gptel-agent-compaction-mode 1)  ; Enable globally
;;
;; The compaction system automatically monitors token usage and triggers
;; compaction when the context approaches the model's limit. You can also
;; manually trigger compaction:
;;
;;   M-x gptel-agent-compact-context
;;
;; Configuration:
;;
;;   ;; Set compaction threshold (default 0.7 = 70%)
;;   (setq gptel-agent-compaction-threshold 0.8)
;;
;;   ;; Choose strategy: 'summarize, 'truncate, or 'hybrid
;;   (setq gptel-agent-compaction-strategy 'hybrid)
;;
;;   ;; Number of recent messages to always preserve
;;   (setq gptel-agent-preserved-message-count 10)
;;
;; The system preserves:
;; - System prompts (never compacted)
;; - Tool calls and their results (never compacted)
;; - User task requests (marked by task keywords)
;; - N most recent messages (sliding window)
;;
;; Performance Requirements (NFR-001):
;; - Compaction completes in < 2000ms for 100 messages
;; - Non-blocking UI during compaction
;; - Async summarization using gptel-request

;;; Code:

(require 'cl-lib)
(require 'gptel)

(declare-function gptel-request "gptel")
(declare-function gptel-backend-name "gptel")
(defvar gptel-model)
(defvar gptel-backend)

;;;; Customization

(defgroup gptel-agent-compaction nil
  "Automatic prompt compaction for gptel-agent."
  :group 'gptel
  :prefix "gptel-agent-compaction-")

(defcustom gptel-agent-compaction-threshold 0.7
  "Threshold for triggering automatic compaction as fraction of context size.

When the token count reaches this percentage of the model's context window,
compaction is triggered. For example, 0.7 means compaction starts at 70%
of the context limit.

Value should be between 0.5 and 0.95."
  :type 'float
  :group 'gptel-agent-compaction)

(defcustom gptel-agent-context-size nil
  "Manual override for model context window size in tokens.

If nil, the system attempts to auto-detect the context size from
`gptel-model'. Set this to a specific number to override detection.

Common values:
  Claude 3.5 Sonnet: 200000
  GPT-4 Turbo: 128000
  GPT-4: 8192
  GPT-3.5 Turbo: 16385"
  :type '(choice (const :tag "Auto-detect" nil)
                 (integer :tag "Token count"))
  :group 'gptel-agent-compaction)

(defcustom gptel-agent-preserved-message-count 10
  "Number of most recent messages to always preserve during compaction.

These messages form a sliding window that is never compacted, ensuring
recent conversation context is always available. Increase this value
for longer-term memory at the cost of more token usage."
  :type 'integer
  :group 'gptel-agent-compaction)

(defcustom gptel-agent-compaction-strategy 'summarize
  "Strategy to use for context compaction.

- `summarize': Use LLM to create concise summary of old messages (recommended)
- `truncate': Simply remove oldest compactable messages
- `hybrid': Combine both - summarize some, truncate others for speed"
  :type '(choice (const :tag "Summarize old messages" summarize)
                 (const :tag "Truncate old messages" truncate)
                 (const :tag "Hybrid approach" hybrid))
  :group 'gptel-agent-compaction)

(defcustom gptel-agent-summarization-model nil
  "Model to use for summarization during compaction.

If nil, uses the current `gptel-model'. You may want to set this to a
faster/cheaper model for summarization, such as:
  - claude-3-haiku-20240307
  - gpt-3.5-turbo
  - gpt-4o-mini"
  :type '(choice (const :tag "Use current model" nil)
                 (string :tag "Model name"))
  :group 'gptel-agent-compaction)

(defcustom gptel-agent-compaction-notify t
  "Whether to show notifications when compaction occurs.

If non-nil, displays a message when compaction is triggered and completed."
  :type 'boolean
  :group 'gptel-agent-compaction)

(defcustom gptel-agent-task-markers
  '("task:" "todo:" "please" "can you" "could you" "implement" "create" "fix")
  "Keywords that identify user task requests in messages.

Messages containing these keywords are considered important and are
given priority during compaction. The matching is case-insensitive."
  :type '(repeat string)
  :group 'gptel-agent-compaction)

(defcustom gptel-agent-summarization-prompt
  "You are a conversation summarizer. Condense the following conversation messages into a concise summary that preserves all critical information, decisions, and context needed for future reference.

Focus on:
- Key decisions and outcomes
- Important technical details
- User requirements and constraints
- Context needed to understand later messages

Omit:
- Redundant information
- Verbose explanations already acted upon
- Conversational pleasantries

MESSAGES TO SUMMARIZE:
%s

CONCISE SUMMARY:"
  "Prompt template for LLM-based summarization.

The string should contain a single %s placeholder where the messages
to be summarized will be inserted."
  :type 'string
  :group 'gptel-agent-compaction)

;;;; Token Counting

(defcustom gptel-agent-token-factors
  '(("claude" . 1.0)
    ("gpt" . 1.0)
    ("gemini" . 0.9)
    ("llama" . 1.1)
    ("mistral" . 1.0))
  "Model-specific token count adjustment factors.

This is an alist mapping model name prefixes to multiplication factors.
Factors adjust the basic 4-chars-per-token estimate to better match
actual tokenization for different model families."
  :type '(alist :key-type string :value-type float)
  :group 'gptel-agent-compaction)

(defvar-local gptel-agent--session-token-count 0
  "Running token count for the current conversation.

This is updated incrementally as messages are added to avoid
recounting the entire conversation on every check.")

(defvar-local gptel-agent--summarization-in-progress nil
  "Non-nil when async summarization is currently in progress.

Prevents concurrent summarization operations.")

(defun gptel-agent--estimate-tokens (text)
  "Estimate token count for TEXT using ~4 chars per token.

This is a fast approximation suitable for triggering compaction.
The actual token count may vary by ±20% depending on the model
and content characteristics."
  (if (stringp text)
      (/ (length text) 4)
    0))

(defun gptel-agent--get-model-factor ()
  "Get token count adjustment factor for current model.

Returns a multiplication factor based on `gptel-model' and
`gptel-agent-token-factors'. Defaults to 1.0 if no match found."
  (when-let* ((model-name (if (boundp 'gptel-model)
                              (format "%s" gptel-model)
                            "gpt"))
              (factor (cl-loop for (prefix . factor) in gptel-agent-token-factors
                               when (string-match-p (regexp-quote prefix) model-name)
                               return factor)))
    factor)
  1.0)

(defun gptel-agent--count-message-tokens (message)
  "Count estimated tokens in MESSAGE.

MESSAGE should be a message plist from gptel with :role and :content keys.
Applies model-specific adjustment factors."
  (let* ((content (plist-get message :content))
         (role (plist-get message :role))
         (base-tokens (+ (gptel-agent--estimate-tokens content)
                         (gptel-agent--estimate-tokens (format "%s" role))
                         5)))  ; Overhead for message structure
    (round (* base-tokens (gptel-agent--get-model-factor)))))

(defun gptel-agent--count-conversation-tokens ()
  "Count total tokens in the current gptel conversation buffer.

This scans all messages in the buffer and returns the sum of their
estimated token counts. For performance, prefer using
`gptel-agent--session-token-count' which tracks incrementally."
  (let ((total 0)
        (messages (gptel-agent--get-all-messages)))
    (dolist (msg messages)
      (cl-incf total (gptel-agent--count-message-tokens msg)))
    total))

;;;; Model Context Detection

(defconst gptel-agent--known-context-limits
  '(("claude-3-5-sonnet" . 200000)
    ("claude-3-5-haiku" . 200000)
    ("claude-3-opus" . 200000)
    ("claude-3-sonnet" . 200000)
    ("claude-3-haiku" . 200000)
    ("gpt-4-turbo" . 128000)
    ("gpt-4o" . 128000)
    ("gpt-4-32k" . 32768)
    ("gpt-4" . 8192)
    ("gpt-3.5-turbo-16k" . 16385)
    ("gpt-3.5-turbo" . 16385)
    ("gemini-1.5-pro" . 1000000)
    ("gemini-1.5-flash" . 1000000)
    ("gemini-pro" . 32760)
    ("llama-3-70b" . 8192)
    ("llama-3-8b" . 8192)
    ("mistral-large" . 32000)
    ("mistral-medium" . 32000))
  "Known context window sizes for various models.

This is an alist mapping model name prefixes to their context
limits in tokens.")

(defun gptel-agent--get-context-limit ()
  "Get context window size for current model.

Returns token limit based on:
1. `gptel-agent-context-size' if set (manual override)
2. Detected from `gptel-model' using known limits
3. Default of 8192 tokens if detection fails

The detected limit is reduced by 10% to provide a safety margin."
  (or gptel-agent-context-size
      (when-let* ((model-name (and (boundp 'gptel-model)
                                    (format "%s" gptel-model)))
                  (limit (cl-loop for (prefix . size) in gptel-agent--known-context-limits
                                  when (string-match-p (regexp-quote prefix) model-name)
                                  return size)))
        ;; Reduce by 10% for safety margin
        (round (* limit 0.9)))
      ;; Fallback default
      8192))

;;;; Message Analysis and Marking

(defun gptel-agent--get-all-messages ()
  "Extract all messages from current gptel buffer.

Returns a list of message plists with :role, :content, and other keys.
This is a placeholder - actual implementation depends on gptel internals."
  ;; TODO: Integrate with actual gptel message storage
  ;; For now, return empty list - this needs to hook into gptel's
  ;; conversation tracking mechanism
  (let ((messages '()))
    ;; This would scan buffer for gptel message markers
    ;; and extract structured message data
    messages))

(defun gptel-agent--message-compactable-p (message)
  "Check if MESSAGE can be safely compacted.

Returns nil if message should never be compacted:
- System prompts (role is 'system)
- Tool calls and results (contains :tool-calls or :tool-call-id)
- Messages marked as :important t

Returns non-nil if message is eligible for compaction."
  (let ((role (plist-get message :role))
        (important (plist-get message :important))
        (has-tools (or (plist-get message :tool-calls)
                       (plist-get message :tool-call-id))))
    (not (or (eq role 'system)
             important
             has-tools))))

(defun gptel-agent--mark-message-compactability (message)
  "Add :compactable property to MESSAGE based on analysis.

Examines message content and metadata to determine if it can be
safely compacted. Mutates MESSAGE by adding :compactable property.

Messages are marked non-compactable if they:
- Are system prompts
- Contain tool calls/results
- Match task marker keywords
- Are within the recent message window

Returns MESSAGE with :compactable property set."
  (let ((compactable (gptel-agent--message-compactable-p message))
        (content (plist-get message :content)))
    ;; Check for task markers
    ;; Use word boundary at start but not at end, since markers often
    ;; include punctuation (e.g., "task:")
    (when (and compactable (stringp content))
      (dolist (marker gptel-agent-task-markers)
        (when (string-match-p (concat "\\<" (regexp-quote marker))
                              (downcase content))
          (setq compactable nil))))
    (plist-put message :compactable compactable)
    message))

(defun gptel-agent--get-compactable-messages ()
  "Get list of messages eligible for compaction.

Returns messages from the conversation, excluding:
- The N most recent messages (sliding window)
- Non-compactable messages (system, tools, tasks)

Returns a list of message plists in chronological order."
  (let* ((all-messages (gptel-agent--get-all-messages))
         (total (length all-messages))
         (preserve-count gptel-agent-preserved-message-count)
         (compactable-end (max 0 (- total preserve-count))))
    ;; Get messages before the preserved window
    (cl-loop for msg in (cl-subseq all-messages 0 compactable-end)
             when (plist-get (gptel-agent--mark-message-compactability msg)
                             :compactable)
             collect msg)))

;;;; Compaction Strategies

(defun gptel-agent--truncate-context ()
  "Remove oldest compactable messages from conversation.

This is the fastest compaction strategy. Removes messages until
token count is reduced to 50% of the context limit.

Returns the number of messages removed."
  (let* ((target-tokens (* (gptel-agent--get-context-limit) 0.5))
         (current-tokens (gptel-agent--count-conversation-tokens))
         (tokens-to-remove (- current-tokens target-tokens))
         (compactable (gptel-agent--get-compactable-messages))
         (removed 0))
    (catch 'done
      (dolist (msg compactable)
        (let ((msg-tokens (gptel-agent--count-message-tokens msg)))
          (when (>= removed tokens-to-remove)
            (throw 'done removed))
          ;; Remove message from buffer/storage
          (gptel-agent--remove-message msg)
          (cl-incf removed msg-tokens)
          (cl-decf gptel-agent--session-token-count msg-tokens))))
    (/ removed (gptel-agent--estimate-tokens "token"))))

(defun gptel-agent--remove-message (message)
  "Remove MESSAGE from the conversation buffer.

This is a placeholder for actual message removal.
Implementation depends on gptel's message storage mechanism."
  ;; TODO: Integrate with gptel's message management
  (ignore message))

(defun gptel-agent--format-messages-for-summary (messages)
  "Format MESSAGES into text for summarization.

Returns a string with all messages concatenated in a readable format."
  (mapconcat
   (lambda (msg)
     (let ((role (plist-get msg :role))
           (content (plist-get msg :content)))
       (format "[%s]: %s" (upcase (symbol-name role)) content)))
   messages
   "\n\n"))

(defun gptel-agent--insert-summary-message (summary original-count)
  "Insert SUMMARY message replacing ORIGINAL-COUNT compacted messages.

Creates a new assistant message containing the summary and inserts it
at the beginning of the conversation (after system prompt).

Returns the summary message plist."
  (let ((summary-msg (list :role 'assistant
                           :content (format "[Conversation summary of %d messages]\n\n%s"
                                            original-count
                                            summary)
                           :compactable nil
                           :important t)))
    ;; TODO: Insert into actual gptel buffer at appropriate position
    summary-msg))

(cl-defun gptel-agent--summarize-context ()
  "Summarize old messages using LLM asynchronously.

Uses `gptel-request' to generate a concise summary of compactable
messages. The summary replaces the original messages upon completion.

This is non-blocking - returns immediately and processes async."
  (when gptel-agent--summarization-in-progress
    (message "Summarization already in progress")
    (cl-return-from gptel-agent--summarize-context nil))

  (let* ((compactable (gptel-agent--get-compactable-messages))
         (message-count (length compactable)))
    (when (< message-count 3)
      ;; Not enough messages to summarize
      (message "Too few messages to summarize, using truncation")
      (cl-return-from gptel-agent--summarize-context
        (gptel-agent--truncate-context)))

    (setq gptel-agent--summarization-in-progress t)

    (let* ((messages-text (gptel-agent--format-messages-for-summary compactable))
           (prompt (format gptel-agent-summarization-prompt messages-text))
           (model (or gptel-agent-summarization-model gptel-model)))

      (gptel-request
       prompt
       :stream nil
       :model model
       :callback
       (lambda (response info)
         (setq gptel-agent--summarization-in-progress nil)
         (if (plist-get info :error)
             (progn
               (message "Summarization failed: %s. Falling back to truncation."
                        (plist-get info :error))
               (gptel-agent--truncate-context))
           ;; Success - insert summary and remove old messages
           (dolist (msg compactable)
             (gptel-agent--remove-message msg))
           (gptel-agent--insert-summary-message response message-count)
           (when gptel-agent-compaction-notify
             (message "Compaction complete: %d messages summarized" message-count))
           ;; Recount tokens
           (setq gptel-agent--session-token-count
                 (gptel-agent--count-conversation-tokens))))))))

(defun gptel-agent--hybrid-compact ()
  "Hybrid compaction strategy combining summarization and truncation.

Summarizes some old messages while truncating others for faster
completion. Aims to balance quality and performance.

Returns when initial truncation completes. Summarization continues async."
  ;; First, do quick truncation to create immediate space
  (let* ((compactable (gptel-agent--get-compactable-messages))
         (total (length compactable))
         (truncate-count (/ total 3))
         (summarize-msgs (cl-subseq compactable 0 (- total truncate-count))))

    ;; Remove oldest third immediately
    (dotimes (i truncate-count)
      (gptel-agent--remove-message (nth i compactable)))

    ;; Summarize the middle portion async
    (when (> (length summarize-msgs) 3)
      (let ((gptel-agent--get-compactable-messages
             (lambda () summarize-msgs)))
        (gptel-agent--summarize-context)))

    truncate-count))

;;;; Main Compaction Entry Point

(cl-defun gptel-agent-compact-context (&optional force)
  "Compact the conversation context using configured strategy.

If FORCE is non-nil (interactively, with prefix arg), perform
compaction even if under threshold.

Uses the strategy specified in `gptel-agent-compaction-strategy':
- `summarize': LLM-based summarization (best quality, slower)
- `truncate': Simple message removal (fastest)
- `hybrid': Combined approach (balanced)

This function is non-blocking when using summarization."
  (interactive "P")

  (unless (derived-mode-p 'gptel-mode)
    (user-error "Not in a gptel buffer"))

  (when (and gptel-agent--summarization-in-progress
             (not force))
    (message "Compaction already in progress")
    (cl-return-from gptel-agent-compact-context nil))

  (let* ((current-tokens (or gptel-agent--session-token-count
                             (gptel-agent--count-conversation-tokens)))
         (limit (gptel-agent--get-context-limit))
         (threshold (* limit gptel-agent-compaction-threshold))
         (needs-compaction (or force (> current-tokens threshold))))

    (unless needs-compaction
      (when (called-interactively-p 'any)
        (message "Context usage: %d/%d tokens (%.1f%%) - no compaction needed"
                 current-tokens limit
                 (* 100.0 (/ (float current-tokens) limit))))
      (cl-return-from gptel-agent-compact-context nil))

    (when gptel-agent-compaction-notify
      (message "Compacting context: %d/%d tokens (%.1f%%)..."
               current-tokens limit
               (* 100.0 (/ (float current-tokens) limit))))

    (pcase gptel-agent-compaction-strategy
      ('summarize (gptel-agent--summarize-context))
      ('truncate (gptel-agent--truncate-context))
      ('hybrid (gptel-agent--hybrid-compact))
      (_ (user-error "Unknown compaction strategy: %s"
                     gptel-agent-compaction-strategy)))))

;;;###autoload
(defun gptel-agent-compaction-status ()
  "Display current context usage and compaction status.

Shows:
- Current token count and limit
- Percentage of context used
- Whether compaction would trigger
- Number of compactable messages"
  (interactive)
  (let* ((current-tokens (or gptel-agent--session-token-count
                             (gptel-agent--count-conversation-tokens)))
         (limit (gptel-agent--get-context-limit))
         (percentage (* 100.0 (/ (float current-tokens) limit)))
         (threshold (* limit gptel-agent-compaction-threshold))
         (would-compact (> current-tokens threshold))
         (compactable-count (length (gptel-agent--get-compactable-messages))))
    (message (concat "Context: %d/%d tokens (%.1f%%) | "
                     "Threshold: %.1f%% | "
                     "Would compact: %s | "
                     "Compactable messages: %d")
             current-tokens limit percentage
             (* 100.0 gptel-agent-compaction-threshold)
             (if would-compact "YES" "no")
             compactable-count)))

;;;; Hook Integration

(defun gptel-agent--check-compaction-needed ()
  "Check if compaction is needed and trigger if so.

Returns non-nil if compaction was triggered."
  (let* ((current-tokens (or gptel-agent--session-token-count
                             (gptel-agent--count-conversation-tokens)))
         (limit (gptel-agent--get-context-limit))
         (threshold (* limit gptel-agent-compaction-threshold)))
    (when (> current-tokens threshold)
      (gptel-agent-compact-context)
      t)))

(defun gptel-agent--post-response-compaction-check ()
  "Hook function for `gptel-post-response-functions'.

Checks context usage after each LLM response and triggers
compaction if threshold is exceeded."
  (gptel-agent--check-compaction-needed))

(defun gptel-agent--notify-compaction ()
  "Show user notification about compaction.

Called when compaction is triggered to inform user."
  (when gptel-agent-compaction-notify
    (let* ((current (or gptel-agent--session-token-count 0))
           (limit (gptel-agent--get-context-limit))
           (pct (* 100.0 (/ (float current) limit))))
      (message "Context compaction triggered at %.1f%% usage" pct))))

;;;; Modeline Indicator

(defun gptel-agent--modeline-context-indicator ()
  "Return modeline string showing context usage.

Format: [TOKENS/LIMIT PCT%]
Color codes by usage:
  < 60% : default
  60-80%: warning
  > 80% : critical"
  (when (and (boundp 'gptel-mode) gptel-mode)
    (let* ((current (or gptel-agent--session-token-count 0))
           (limit (gptel-agent--get-context-limit))
           (pct (if (> limit 0)
                    (* 100.0 (/ (float current) limit))
                  0))
           (face (cond
                  ((> pct 80) 'error)
                  ((> pct 60) 'warning)
                  (t 'default))))
      (propertize (format " [%dk/%dk %.0f%%]"
                          (/ current 1000)
                          (/ limit 1000)
                          pct)
                  'face face
                  'help-echo "Context window usage"))))

;;;; Minor Mode

;;;###autoload
(define-minor-mode gptel-agent-compaction-mode
  "Toggle automatic context compaction for gptel-agent.

When enabled, monitors token usage in gptel conversations and
automatically triggers compaction when approaching context limits.

This is a global minor mode that affects all gptel buffers."
  :global t
  :group 'gptel-agent-compaction
  :lighter nil
  (if gptel-agent-compaction-mode
      (progn
        (add-hook 'gptel-post-response-functions
                  #'gptel-agent--post-response-compaction-check)
        ;; Add modeline indicator
        (unless (member '(:eval (gptel-agent--modeline-context-indicator))
                        mode-line-misc-info)
          (push '(:eval (gptel-agent--modeline-context-indicator))
                mode-line-misc-info)))
    (remove-hook 'gptel-post-response-functions
                 #'gptel-agent--post-response-compaction-check)))

(provide 'gptel-agent-compaction)
;;; gptel-agent-compaction.el ends here
