;;; gptel-agent-safety-test.el --- Tests for gptel-agent-safety -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Keywords: tests
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tests for gptel-agent-safety.el doom loop detection
;; and external directory access control.
;;
;; Test coverage - Doom Loop Detection:
;; - Ring buffer management (tracking, retrieval, overflow, clearing)
;; - Pattern detection (identical, similar, alternating, oscillating)
;; - Argument normalization (paths, whitespace, types)
;; - Token estimation
;; - Score calculation
;; - Configuration effects
;;
;; Test coverage - External Directory Access Control (FR-014):
;; - Path normalization (~, relative, absolute)
;; - Boundary detection and caching
;; - Whitelist matching (exact, directory, glob patterns)
;; - Access policies (allow, ask, deny)
;; - Path extraction from bash commands
;; - Customization defaults

;;; Code:

(require 'ert)
(require 'gptel-agent-safety)
(require 'cl-lib)

;;;; Test Helpers

(defun gptel-agent-safety-test--make-call (tool args &optional result)
  "Create a test tool call plist.
TOOL is the tool name, ARGS are arguments, RESULT is optional result."
  (list :tool tool
        :args args
        :result (or result "test-result")
        :timestamp (current-time)))

(defun gptel-agent-safety-test--setup ()
  "Set up test environment with clean state."
  ;; Clear ring buffer
  (setq gptel-agent--recent-tool-calls nil)
  (setq gptel-agent--doom-loop-log nil)
  ;; Set standard test configuration
  (setq gptel-agent-doom-loop-threshold 3)
  (setq gptel-agent-doom-loop-similarity 0.8)
  (setq gptel-agent-doom-loop-buffer-size 20)
  (setq gptel-agent-doom-loop-enabled t))

(defun gptel-agent-safety-test--teardown ()
  "Clean up test environment."
  (setq gptel-agent--recent-tool-calls nil)
  (setq gptel-agent--doom-loop-log nil))

(defmacro gptel-agent-safety-test-with-clean-state (&rest body)
  "Execute BODY with clean test state."
  `(unwind-protect
       (progn
         (gptel-agent-safety-test--setup)
         ,@body)
     (gptel-agent-safety-test--teardown)))

;;;; Ring Buffer Tests

(ert-deftest gptel-agent-safety-test-ring-buffer-creation ()
  "Test ring buffer is created on first use."
  (gptel-agent-safety-test-with-clean-state
   (should-not gptel-agent--recent-tool-calls)
   (gptel-agent--track-tool-call "Read" '(:file "/tmp/test.txt") "success")
   (should gptel-agent--recent-tool-calls)
   (should (ring-p gptel-agent--recent-tool-calls))
   (should (= (ring-size gptel-agent--recent-tool-calls)
              gptel-agent-doom-loop-buffer-size))))

(ert-deftest gptel-agent-safety-test-track-tool-call-adds-entry ()
  "Test tracking tool call adds entry to ring buffer."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Bash" "ls -la" "files listed")
   (let ((calls (gptel-agent--get-recent-calls)))
     (should (= (length calls) 1))
     (let ((call (car calls)))
       (should (equal (plist-get call :tool) "Bash"))
       (should (equal (plist-get call :args) "ls -la"))
       (should (equal (plist-get call :result) "files listed"))
       (should (plist-get call :timestamp))))))

(ert-deftest gptel-agent-safety-test-track-symbol-tool-name ()
  "Test tracking converts symbol tool names to strings."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call 'Read '(:file "test.el") "content")
   (let ((call (car (gptel-agent--get-recent-calls))))
     (should (stringp (plist-get call :tool)))
     (should (equal (plist-get call :tool) "Read")))))

(ert-deftest gptel-agent-safety-test-get-recent-calls-order ()
  "Test retrieval returns most recent calls first."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Tool1" "args1" "result1")
   (gptel-agent--track-tool-call "Tool2" "args2" "result2")
   (gptel-agent--track-tool-call "Tool3" "args3" "result3")
   (let ((calls (gptel-agent--get-recent-calls)))
     (should (= (length calls) 3))
     ;; Most recent first (reversed order)
     (should (equal (plist-get (nth 0 calls) :tool) "Tool3"))
     (should (equal (plist-get (nth 1 calls) :tool) "Tool2"))
     (should (equal (plist-get (nth 2 calls) :tool) "Tool1")))))

(ert-deftest gptel-agent-safety-test-get-recent-calls-limit ()
  "Test retrieving limited number of calls."
  (gptel-agent-safety-test-with-clean-state
   (dotimes (i 10)
     (gptel-agent--track-tool-call (format "Tool%d" i) nil "result"))
   (let ((calls (gptel-agent--get-recent-calls 3)))
     (should (= (length calls) 3))
     (should (equal (plist-get (car calls) :tool) "Tool9")))))

(ert-deftest gptel-agent-safety-test-ring-buffer-overflow ()
  "Test ring buffer discards oldest entries when full."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-buffer-size 5)
   (dotimes (i 10)
     (gptel-agent--track-tool-call (format "Tool%d" i) nil "result"))
   (let ((calls (gptel-agent--get-recent-calls)))
     (should (= (length calls) 5))
     ;; Should have entries 5-9 (most recent)
     (should (equal (plist-get (nth 0 calls) :tool) "Tool9"))
     (should (equal (plist-get (nth 4 calls) :tool) "Tool5")))))

(ert-deftest gptel-agent-safety-test-clear-tool-history ()
  "Test clearing tool call history resets ring buffer."
  (gptel-agent-safety-test-with-clean-state
   (dotimes (i 5)
     (gptel-agent--track-tool-call "Tool" nil "result"))
   (should (= (ring-length gptel-agent--recent-tool-calls) 5))
   (gptel-agent--clear-tool-history)
   (should (= (ring-length gptel-agent--recent-tool-calls) 0))))

(ert-deftest gptel-agent-safety-test-disabled-mode ()
  "Test tracking is disabled when mode is off."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-enabled nil)
   (gptel-agent--track-tool-call "Tool" "args" "result")
   (should-not gptel-agent--recent-tool-calls)))

;;;; Normalization Tests

(ert-deftest gptel-agent-safety-test-normalize-string-args ()
  "Test normalization trims whitespace from strings."
  (gptel-agent-safety-test-with-clean-state
   (let ((normalized (gptel-agent--normalize-tool-args
                      "Read" "  /tmp/test.txt  ")))
     (should (equal normalized "/tmp/test.txt")))))

(ert-deftest gptel-agent-safety-test-normalize-list-args ()
  "Test normalization handles list arguments."
  (gptel-agent-safety-test-with-clean-state
   (let ((normalized (gptel-agent--normalize-tool-args
                      "Bash" '("ls " "  -la  " "  /tmp"))))
     (should (equal normalized '("ls" "-la" "/tmp"))))))

(ert-deftest gptel-agent-safety-test-normalize-plist-args ()
  "Test normalization handles plist arguments."
  (gptel-agent-safety-test-with-clean-state
   (let ((normalized (gptel-agent--normalize-tool-args
                      "Read" '(:file "  test.el  " :offset 10))))
     (should (equal normalized '(:file "test.el" :offset 10))))))

(ert-deftest gptel-agent-safety-test-normalize-path-expansion ()
  "Test normalization expands file paths for file tools."
  (gptel-agent-safety-test-with-clean-state
   (let ((normalized (gptel-agent--normalize-tool-args
                      "Read" "~/test.txt")))
     (should (string-prefix-p "/" normalized))
     (should-not (string-prefix-p "~" normalized)))))

(ert-deftest gptel-agent-safety-test-normalize-no-path-for-non-file-tools ()
  "Test normalization doesn't expand paths for non-file tools."
  (gptel-agent-safety-test-with-clean-state
   (let ((normalized (gptel-agent--normalize-tool-args
                      "Calculate" "~/test.txt")))
     ;; Non-file tool should not expand path
     (should (equal normalized "~/test.txt")))))

(ert-deftest gptel-agent-safety-test-normalize-plist-paths ()
  "Test normalization expands paths in plists for file tools."
  (gptel-agent-safety-test-with-clean-state
   (let ((normalized (gptel-agent--normalize-tool-args
                      "Edit" '(:file "~/test.el" :old "foo" :new "bar"))))
     (should (string-prefix-p "/" (plist-get normalized :file)))
     (should (equal (plist-get normalized :old) "foo")))))

;;;; Pattern Detection Tests - Identical Calls

(ert-deftest gptel-agent-safety-test-calls-identical-same-tool-args ()
  "Test identical call detection for exact matches."
  (gptel-agent-safety-test-with-clean-state
   (let ((call1 (gptel-agent-safety-test--make-call "Read" '(:file "test.el")))
         (call2 (gptel-agent-safety-test--make-call "Read" '(:file "test.el"))))
     (should (gptel-agent--calls-identical-p call1 call2)))))

(ert-deftest gptel-agent-safety-test-calls-identical-different-tools ()
  "Test identical call detection rejects different tools."
  (gptel-agent-safety-test-with-clean-state
   (let ((call1 (gptel-agent-safety-test--make-call "Read" '(:file "test.el")))
         (call2 (gptel-agent-safety-test--make-call "Write" '(:file "test.el"))))
     (should-not (gptel-agent--calls-identical-p call1 call2)))))

(ert-deftest gptel-agent-safety-test-calls-identical-different-args ()
  "Test identical call detection rejects different arguments."
  (gptel-agent-safety-test-with-clean-state
   (let ((call1 (gptel-agent-safety-test--make-call "Read" '(:file "test1.el")))
         (call2 (gptel-agent-safety-test--make-call "Read" '(:file "test2.el"))))
     (should-not (gptel-agent--calls-identical-p call1 call2)))))

(ert-deftest gptel-agent-safety-test-calls-identical-whitespace-normalized ()
  "Test identical call detection normalizes whitespace."
  (gptel-agent-safety-test-with-clean-state
   (let ((call1 (gptel-agent-safety-test--make-call "Bash" "ls -la"))
         (call2 (gptel-agent-safety-test--make-call "Bash" "  ls -la  ")))
     (should (gptel-agent--calls-identical-p call1 call2)))))

(ert-deftest gptel-agent-safety-test-detect-identical-sequence ()
  "Test detection of identical call sequence."
  (gptel-agent-safety-test-with-clean-state
   (dotimes (_ 3)
     (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
   (let ((result (gptel-agent--detect-identical-sequence)))
     (should result)
     (should (eq (plist-get result :type) 'identical))
     (should (= (plist-get result :count) 3)))))

(ert-deftest gptel-agent-safety-test-no-identical-sequence-different-tools ()
  "Test no detection when tools differ."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Read" '(:file "test.el") "content")
   (gptel-agent--track-tool-call "Write" '(:file "test.el") "success")
   (gptel-agent--track-tool-call "Edit" '(:file "test.el") "edited")
   (should-not (gptel-agent--detect-identical-sequence))))

(ert-deftest gptel-agent-safety-test-no-identical-sequence-below-threshold ()
  "Test no detection when below threshold."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-threshold 5)
   (dotimes (_ 3)
     (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
   (should-not (gptel-agent--detect-identical-sequence))))

;;;; Pattern Detection Tests - Similar Calls

(ert-deftest gptel-agent-safety-test-calls-similar-high-similarity ()
  "Test similar call detection with high similarity."
  (gptel-agent-safety-test-with-clean-state
   (let ((call1 (gptel-agent-safety-test--make-call "Read" '(:file "test1.el")))
         (call2 (gptel-agent-safety-test--make-call "Read" '(:file "test2.el"))))
     (should (gptel-agent--calls-similar-p call1 call2 0.7)))))

(ert-deftest gptel-agent-safety-test-calls-similar-different-tools ()
  "Test similar call detection requires same tool."
  (gptel-agent-safety-test-with-clean-state
   (let ((call1 (gptel-agent-safety-test--make-call "Read" '(:file "test.el")))
         (call2 (gptel-agent-safety-test--make-call "Write" '(:file "test.el"))))
     (should-not (gptel-agent--calls-similar-p call1 call2 0.8)))))

(ert-deftest gptel-agent-safety-test-calls-similar-low-similarity ()
  "Test similar call detection rejects low similarity."
  (gptel-agent-safety-test-with-clean-state
   (let ((call1 (gptel-agent-safety-test--make-call
                 "Read" '(:file "short.el")))
         (call2 (gptel-agent-safety-test--make-call
                 "Read" '(:file "completely-different-very-long-filename.el"))))
     (should-not (gptel-agent--calls-similar-p call1 call2 0.9)))))

(ert-deftest gptel-agent-safety-test-detect-similar-sequence ()
  "Test detection of similar call sequence."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Read" '(:file "test1.el") "content1")
   (gptel-agent--track-tool-call "Read" '(:file "test2.el") "content2")
   (gptel-agent--track-tool-call "Read" '(:file "test3.el") "content3")
   (let ((result (gptel-agent--detect-similar-sequence)))
     (should result)
     (should (eq (plist-get result :type) 'similar))
     (should (= (plist-get result :count) 3)))))

(ert-deftest gptel-agent-safety-test-no-similar-sequence-low-threshold ()
  "Test no detection with low similarity threshold."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-similarity 0.99)
   (gptel-agent--track-tool-call "Read" '(:file "test1.el") "content1")
   (gptel-agent--track-tool-call "Read" '(:file "test2.el") "content2")
   (gptel-agent--track-tool-call "Read" '(:file "test3.el") "content3")
   (should-not (gptel-agent--detect-similar-sequence))))

;;;; Pattern Detection Tests - Alternating Pattern

(ert-deftest gptel-agent-safety-test-detect-alternating-pattern ()
  "Test detection of A->B->A->B alternating pattern."
  (gptel-agent-safety-test-with-clean-state
   (dotimes (_ 3)
     (gptel-agent--track-tool-call "Read" '(:file "a.el") "content")
     (gptel-agent--track-tool-call "Write" '(:file "b.el") "written"))
   (let ((result (gptel-agent--detect-alternating-pattern)))
     (should result)
     (should (eq (plist-get result :type) 'alternating))
     (should (= (plist-get result :count) 6)))))

(ert-deftest gptel-agent-safety-test-no-alternating-pattern-different-pairs ()
  "Test no detection when pairs differ."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Read" '(:file "a.el") "content")
   (gptel-agent--track-tool-call "Write" '(:file "b.el") "written")
   (gptel-agent--track-tool-call "Edit" '(:file "c.el") "edited")
   (gptel-agent--track-tool-call "Bash" "ls" "files")
   (should-not (gptel-agent--detect-alternating-pattern))))

(ert-deftest gptel-agent-safety-test-no-alternating-pattern-insufficient-pairs ()
  "Test no detection with insufficient pair repetitions."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-threshold 5)
   (dotimes (_ 2)
     (gptel-agent--track-tool-call "Read" '(:file "a.el") "content")
     (gptel-agent--track-tool-call "Write" '(:file "b.el") "written"))
   (should-not (gptel-agent--detect-alternating-pattern))))

;;;; Pattern Detection Tests - Oscillating Results

(ert-deftest gptel-agent-safety-test-detect-oscillating-results ()
  "Test detection of same tool with varying results."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Bash" "echo test" "result1")
   (gptel-agent--track-tool-call "Bash" "echo test" "result2")
   (gptel-agent--track-tool-call "Bash" "echo test" "result3")
   (let ((result (gptel-agent--detect-oscillating-results)))
     (should result)
     (should (eq (plist-get result :type) 'oscillating))
     (should (= (plist-get result :count) 3)))))

(ert-deftest gptel-agent-safety-test-no-oscillating-same-results ()
  "Test no detection when results are identical."
  (gptel-agent-safety-test-with-clean-state
   (dotimes (_ 3)
     (gptel-agent--track-tool-call "Bash" "echo test" "same-result"))
   (should-not (gptel-agent--detect-oscillating-results))))

(ert-deftest gptel-agent-safety-test-no-oscillating-different-tools ()
  "Test no detection when tools differ."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Bash" "ls" "result1")
   (gptel-agent--track-tool-call "Read" "test.el" "result2")
   (gptel-agent--track-tool-call "Write" "test.el" "result3")
   (should-not (gptel-agent--detect-oscillating-results))))

;;;; Main Detection Function Tests

(ert-deftest gptel-agent-safety-test-detect-doom-loop-finds-identical ()
  "Test main detection function finds identical patterns."
  (gptel-agent-safety-test-with-clean-state
   (dotimes (_ 3)
     (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
   (let ((result (gptel-agent--detect-doom-loop)))
     (should result)
     (should (eq (plist-get result :type) 'identical)))))

(ert-deftest gptel-agent-safety-test-detect-doom-loop-finds-similar ()
  "Test main detection function finds similar patterns."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Read" '(:file "test1.el") "content1")
   (gptel-agent--track-tool-call "Read" '(:file "test2.el") "content2")
   (gptel-agent--track-tool-call "Read" '(:file "test3.el") "content3")
   (let ((result (gptel-agent--detect-doom-loop)))
     (should result)
     (should (eq (plist-get result :type) 'similar)))))

(ert-deftest gptel-agent-safety-test-detect-doom-loop-returns-nil ()
  "Test main detection returns nil when no loop."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Read" '(:file "a.el") "content")
   (gptel-agent--track-tool-call "Write" '(:file "b.el") "written")
   (should-not (gptel-agent--detect-doom-loop))))

(ert-deftest gptel-agent-safety-test-detect-doom-loop-prefers-identical ()
  "Test detection prioritizes identical over similar."
  (gptel-agent-safety-test-with-clean-state
   ;; Add identical calls
   (dotimes (_ 3)
     (gptel-agent--track-tool-call "Read" '(:file "same.el") "content"))
   (let ((result (gptel-agent--detect-doom-loop)))
     (should (eq (plist-get result :type) 'identical)))))

;;;; Score Calculation Tests

(ert-deftest gptel-agent-safety-test-doom-loop-score-zero-no-loop ()
  "Test score is 0.0 when no loop detected."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Read" '(:file "a.el") "content")
   (gptel-agent--track-tool-call "Write" '(:file "b.el") "written")
   (should (= (gptel-agent--doom-loop-score) 0.0))))

(ert-deftest gptel-agent-safety-test-doom-loop-score-identical ()
  "Test score is high for identical patterns."
  (gptel-agent-safety-test-with-clean-state
   (dotimes (_ 3)
     (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
   (let ((score (gptel-agent--doom-loop-score)))
     (should (>= score 0.9))
     (should (<= score 1.0)))))

(ert-deftest gptel-agent-safety-test-doom-loop-score-increases-with-count ()
  "Test score increases with more repetitions."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-threshold 3)
   ;; Add exactly threshold repetitions
   (dotimes (_ 3)
     (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
   (let ((score-min (gptel-agent--doom-loop-score)))
     ;; Add more repetitions
     (dotimes (_ 3)
       (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
     (let ((score-more (gptel-agent--doom-loop-score)))
       (should (> score-more score-min))))))

(ert-deftest gptel-agent-safety-test-doom-loop-score-bounded ()
  "Test score is always between 0.0 and 1.0."
  (gptel-agent-safety-test-with-clean-state
   ;; Add many identical calls
   (dotimes (_ 20)
     (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
   (let ((score (gptel-agent--doom-loop-score)))
     (should (>= score 0.0))
     (should (<= score 1.0)))))

;;;; Token Estimation Tests

(ert-deftest gptel-agent-safety-test-estimate-wasted-tokens-zero ()
  "Test token estimation with no calls."
  (gptel-agent-safety-test-with-clean-state
   (let ((estimate (gptel-agent--estimate-wasted-tokens nil)))
     (should (= estimate 0)))))

(ert-deftest gptel-agent-safety-test-estimate-wasted-tokens-positive ()
  "Test token estimation returns positive value for calls."
  (gptel-agent-safety-test-with-clean-state
   (let* ((calls (list
                  (gptel-agent-safety-test--make-call
                   "Read" '(:file "test.el") "file content")
                  (gptel-agent-safety-test--make-call
                   "Bash" "ls -la" "file list")))
          (estimate (gptel-agent--estimate-wasted-tokens calls)))
     (should (> estimate 0)))))

(ert-deftest gptel-agent-safety-test-estimate-wasted-tokens-reasonable ()
  "Test token estimation is in reasonable range."
  (gptel-agent-safety-test-with-clean-state
   (let* ((calls (list
                  (gptel-agent-safety-test--make-call
                   "Read" '(:file "test.el")
                   (make-string 1000 ?x))))
          (estimate (gptel-agent--estimate-wasted-tokens calls)))
     ;; Should be roughly 1000/4 + overhead = ~300 tokens
     (should (> estimate 200))
     (should (< estimate 500)))))

;;;; Configuration Tests

(ert-deftest gptel-agent-safety-test-threshold-affects-detection ()
  "Test changing threshold affects detection."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-threshold 5)
   (dotimes (_ 3)
     (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
   (should-not (gptel-agent--detect-doom-loop))
   ;; Add more to reach threshold
   (dotimes (_ 2)
     (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
   (should (gptel-agent--detect-doom-loop))))

(ert-deftest gptel-agent-safety-test-similarity-threshold-affects-detection ()
  "Test changing similarity threshold affects detection."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-similarity 0.99)
   (gptel-agent--track-tool-call "Read" '(:file "test1.el") "content1")
   (gptel-agent--track-tool-call "Read" '(:file "test2.el") "content2")
   (gptel-agent--track-tool-call "Read" '(:file "test3.el") "content3")
   (should-not (gptel-agent--detect-similar-sequence))
   ;; Lower threshold
   (setq gptel-agent-doom-loop-similarity 0.7)
   (should (gptel-agent--detect-similar-sequence))))

(ert-deftest gptel-agent-safety-test-buffer-size-resize ()
  "Test changing buffer size resizes ring buffer."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-buffer-size 5)
   (gptel-agent--ensure-ring)
   (should (= (ring-size gptel-agent--recent-tool-calls) 5))
   (setq gptel-agent-doom-loop-buffer-size 10)
   (gptel-agent--ensure-ring)
   (should (= (ring-size gptel-agent--recent-tool-calls) 10))))

(ert-deftest gptel-agent-safety-test-buffer-size-preserves-data ()
  "Test resizing buffer preserves existing data."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-buffer-size 5)
   (dotimes (i 3)
     (gptel-agent--track-tool-call (format "Tool%d" i) nil "result"))
   (setq gptel-agent-doom-loop-buffer-size 10)
   (gptel-agent--ensure-ring)
   (let ((calls (gptel-agent--get-recent-calls)))
     (should (= (length calls) 3)))))

;;;; Edge Cases and Boundary Tests

(ert-deftest gptel-agent-safety-test-empty-ring-buffer ()
  "Test operations on empty ring buffer."
  (gptel-agent-safety-test-with-clean-state
   (should-not (gptel-agent--get-recent-calls))
   (should-not (gptel-agent--detect-doom-loop))
   (should (= (gptel-agent--doom-loop-score) 0.0))))

(ert-deftest gptel-agent-safety-test-nil-args ()
  "Test handling of nil arguments."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Tool" nil "result")
   (let ((call (car (gptel-agent--get-recent-calls))))
     (should (null (plist-get call :args))))))

(ert-deftest gptel-agent-safety-test-complex-nested-args ()
  "Test normalization of complex nested structures."
  (gptel-agent-safety-test-with-clean-state
   (let* ((args '(:file "test.el" :options (:depth 3 :ignore ("*.pyc"))))
          (normalized (gptel-agent--normalize-tool-args "Tool" args)))
     (should normalized)
     (should (plist-get normalized :file)))))

(ert-deftest gptel-agent-safety-test-identical-with-different-results ()
  "Test identical calls can have different results."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Read" '(:file "test.el") "result1")
   (gptel-agent--track-tool-call "Read" '(:file "test.el") "result2")
   (gptel-agent--track-tool-call "Read" '(:file "test.el") "result3")
   ;; Should still detect as identical (results don't affect identity)
   (let ((result (gptel-agent--detect-identical-sequence)))
     (should result)
     (should (eq (plist-get result :type) 'identical)))))

(ert-deftest gptel-agent-safety-test-single-call ()
  "Test no false positive with single call."
  (gptel-agent-safety-test-with-clean-state
   (gptel-agent--track-tool-call "Read" '(:file "test.el") "content")
   (should-not (gptel-agent--detect-doom-loop))))

(ert-deftest gptel-agent-safety-test-exact-threshold-boundary ()
  "Test detection at exact threshold boundary."
  (gptel-agent-safety-test-with-clean-state
   (setq gptel-agent-doom-loop-threshold 3)
   ;; Add exactly threshold - 1 calls
   (dotimes (_ 2)
     (gptel-agent--track-tool-call "Read" '(:file "test.el") "content"))
   (should-not (gptel-agent--detect-doom-loop))
   ;; Add one more to reach threshold
   (gptel-agent--track-tool-call "Read" '(:file "test.el") "content")
   (should (gptel-agent--detect-doom-loop))))

;;;; =====================================================================
;;;; External Directory Access Control Tests (FR-014)
;;;; =====================================================================

;;;; Boundary Detection Tests

(defvar gptel-agent-safety-test--temp-dirs nil
  "List of temporary directories to clean up after tests.")

(defun gptel-agent-safety-test--make-temp-project ()
  "Create a temporary project directory for testing.
Returns the path to the created directory."
  (let ((dir (make-temp-file "gptel-agent-safety-test-" t)))
    (push dir gptel-agent-safety-test--temp-dirs)
    dir))

(defun gptel-agent-safety-test--cleanup-dirs ()
  "Clean up temporary directories."
  (dolist (dir gptel-agent-safety-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq gptel-agent-safety-test--temp-dirs nil))

(defmacro gptel-agent-safety-test--with-temp-project (dir-var &rest body)
  "Create temp project, bind to DIR-VAR, execute BODY, cleanup."
  (declare (indent 1))
  `(unwind-protect
       (let ((,dir-var (gptel-agent-safety-test--make-temp-project)))
         ,@body)
     (gptel-agent-safety-test--cleanup-dirs)))

(ert-deftest gptel-agent-safety-test-normalize-path-expands-tilde ()
  "Test path normalization expands ~ to home directory."
  (let ((result (gptel-agent--normalize-path "~/test.txt")))
    (should (stringp result))
    (should (string-prefix-p "/" result))
    (should-not (string-prefix-p "~" result))))

(ert-deftest gptel-agent-safety-test-normalize-path-relative ()
  "Test path normalization handles relative paths."
  (let ((result (gptel-agent--normalize-path "foo/bar.txt")))
    (should (stringp result))
    (should (string-prefix-p "/" result))
    (should (string-match-p "foo/bar.txt$" result))))

(ert-deftest gptel-agent-safety-test-normalize-path-nil ()
  "Test path normalization returns nil for nil input."
  (should (null (gptel-agent--normalize-path nil))))

(ert-deftest gptel-agent-safety-test-normalize-path-non-string ()
  "Test path normalization returns nil for non-string input."
  (should (null (gptel-agent--normalize-path 123))))

(ert-deftest gptel-agent-safety-test-path-within-boundary ()
  "Test path within boundary returns non-nil."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let* ((gptel-agent--project-boundary-cache nil)
           (default-directory project-dir)
           (subfile (expand-file-name "subdir/file.txt" project-dir)))
      ;; Mock the boundary detection to use our temp dir
      (setq gptel-agent--project-boundary-cache
            (list :boundary project-dir
                  :timestamp (current-time)
                  :whitelist nil))
      (should (gptel-agent--path-within-boundary-p subfile)))))

(ert-deftest gptel-agent-safety-test-path-outside-boundary ()
  "Test path outside boundary returns nil."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let* ((gptel-agent--project-boundary-cache nil)
           (external-path "/tmp/completely-outside.txt"))
      (setq gptel-agent--project-boundary-cache
            (list :boundary project-dir
                  :timestamp (current-time)
                  :whitelist nil))
      (should-not (gptel-agent--path-within-boundary-p external-path)))))

;;;; Whitelist Tests

(ert-deftest gptel-agent-safety-test-path-in-whitelist-exact ()
  "Test exact path match in whitelist."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let ((gptel-agent-external-path-whitelist '("/tmp/allowed.txt"))
          (gptel-agent--project-boundary-cache
           (list :boundary project-dir
                 :timestamp (current-time)
                 :whitelist nil)))
      (should (gptel-agent--path-in-whitelist-p "/tmp/allowed.txt"))
      (should-not (gptel-agent--path-in-whitelist-p "/tmp/notallowed.txt")))))

(ert-deftest gptel-agent-safety-test-path-in-whitelist-directory ()
  "Test directory prefix match in whitelist."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let ((gptel-agent-external-path-whitelist '("/tmp/allowed-dir/"))
          (gptel-agent--project-boundary-cache
           (list :boundary project-dir
                 :timestamp (current-time)
                 :whitelist nil)))
      (should (gptel-agent--path-in-whitelist-p "/tmp/allowed-dir/file.txt"))
      (should (gptel-agent--path-in-whitelist-p "/tmp/allowed-dir/sub/file.txt"))
      (should-not (gptel-agent--path-in-whitelist-p "/tmp/other-dir/file.txt")))))

(ert-deftest gptel-agent-safety-test-path-in-whitelist-glob ()
  "Test glob pattern match in whitelist."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let ((gptel-agent-external-path-whitelist '("/tmp/*.log"))
          (gptel-agent--project-boundary-cache
           (list :boundary project-dir
                 :timestamp (current-time)
                 :whitelist nil)))
      (should (gptel-agent--path-in-whitelist-p "/tmp/app.log"))
      (should (gptel-agent--path-in-whitelist-p "/tmp/error.log"))
      (should-not (gptel-agent--path-in-whitelist-p "/tmp/app.txt")))))

(ert-deftest gptel-agent-safety-test-path-in-whitelist-project ()
  "Test project-level whitelist from config."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let ((gptel-agent-external-path-whitelist nil)
          (gptel-agent--project-boundary-cache
           (list :boundary project-dir
                 :timestamp (current-time)
                 :whitelist '("/tmp/project-allowed/"))))
      (should (gptel-agent--path-in-whitelist-p "/tmp/project-allowed/file.txt"))
      (should-not (gptel-agent--path-in-whitelist-p "/tmp/other/file.txt")))))

;;;; Access Policy Tests

(ert-deftest gptel-agent-safety-test-check-access-within-boundary ()
  "Test access check for paths within boundary."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let* ((gptel-agent--project-boundary-cache
            (list :boundary project-dir
                  :timestamp (current-time)
                  :whitelist nil))
           (internal-file (expand-file-name "test.txt" project-dir)))
      (should (eq (gptel-agent--check-path-access internal-file 'read) 'allow))
      (should (eq (gptel-agent--check-path-access internal-file 'write) 'allow)))))

(ert-deftest gptel-agent-safety-test-check-access-whitelist ()
  "Test access check for whitelisted paths."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let ((gptel-agent-external-path-whitelist '("/tmp/allowed/"))
          (gptel-agent--project-boundary-cache
           (list :boundary project-dir
                 :timestamp (current-time)
                 :whitelist nil)))
      (should (eq (gptel-agent--check-path-access "/tmp/allowed/file.txt" 'read)
                  'allow-with-warning))
      (should (eq (gptel-agent--check-path-access "/tmp/allowed/file.txt" 'write)
                  'allow-with-warning)))))

(ert-deftest gptel-agent-safety-test-check-access-external-read-allow ()
  "Test external read access with allow policy."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let ((gptel-agent-external-path-whitelist nil)
          (gptel-agent-external-read-policy 'allow)
          (gptel-agent--project-boundary-cache
           (list :boundary project-dir
                 :timestamp (current-time)
                 :whitelist nil)))
      (should (eq (gptel-agent--check-path-access "/external/file.txt" 'read)
                  'allow-with-warning)))))

(ert-deftest gptel-agent-safety-test-check-access-external-read-ask ()
  "Test external read access with ask policy."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let ((gptel-agent-external-path-whitelist nil)
          (gptel-agent-external-read-policy 'ask)
          (gptel-agent--project-boundary-cache
           (list :boundary project-dir
                 :timestamp (current-time)
                 :whitelist nil)))
      (should (eq (gptel-agent--check-path-access "/external/file.txt" 'read)
                  'ask)))))

(ert-deftest gptel-agent-safety-test-check-access-external-read-deny ()
  "Test external read access with deny policy."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let ((gptel-agent-external-path-whitelist nil)
          (gptel-agent-external-read-policy 'deny)
          (gptel-agent--project-boundary-cache
           (list :boundary project-dir
                 :timestamp (current-time)
                 :whitelist nil)))
      (should (eq (gptel-agent--check-path-access "/external/file.txt" 'read)
                  'deny)))))

(ert-deftest gptel-agent-safety-test-check-access-external-write-deny ()
  "Test external write access with deny policy (default)."
  (gptel-agent-safety-test--with-temp-project project-dir
    (let ((gptel-agent-external-path-whitelist nil)
          (gptel-agent-external-write-policy 'deny)
          (gptel-agent--project-boundary-cache
           (list :boundary project-dir
                 :timestamp (current-time)
                 :whitelist nil)))
      (should (eq (gptel-agent--check-path-access "/external/file.txt" 'write)
                  'deny))
      (should (eq (gptel-agent--check-path-access "/external/file.txt" 'edit)
                  'deny)))))

;;;; Path Extraction Tests

(ert-deftest gptel-agent-safety-test-extract-paths-cat ()
  "Test path extraction from cat command."
  (let ((paths (gptel-agent--extract-paths-from-bash-command "cat /etc/passwd")))
    (should (member "/etc/passwd" paths))))

(ert-deftest gptel-agent-safety-test-extract-paths-head-tail ()
  "Test path extraction from head and tail commands."
  (let ((paths-head (gptel-agent--extract-paths-from-bash-command "head -n 10 /var/log/syslog"))
        (paths-tail (gptel-agent--extract-paths-from-bash-command "tail -f /var/log/messages")))
    (should (member "/var/log/syslog" paths-head))
    (should (member "/var/log/messages" paths-tail))))

(ert-deftest gptel-agent-safety-test-extract-paths-redirection ()
  "Test path extraction from output redirection."
  (let ((paths (gptel-agent--extract-paths-from-bash-command "echo test > /tmp/output.txt")))
    (should (member "/tmp/output.txt" paths))))

(ert-deftest gptel-agent-safety-test-extract-paths-cp-mv ()
  "Test path extraction from cp and mv commands."
  (let ((paths-cp (gptel-agent--extract-paths-from-bash-command "cp /source/file.txt /dest/file.txt"))
        (paths-mv (gptel-agent--extract-paths-from-bash-command "mv /old/path /new/path")))
    (should (member "/source/file.txt" paths-cp))
    (should (member "/dest/file.txt" paths-cp))
    (should (member "/old/path" paths-mv))
    (should (member "/new/path" paths-mv))))

(ert-deftest gptel-agent-safety-test-extract-paths-rm ()
  "Test path extraction from rm command."
  (let ((paths (gptel-agent--extract-paths-from-bash-command "rm -rf /tmp/delete-me")))
    (should (member "/tmp/delete-me" paths))))

(ert-deftest gptel-agent-safety-test-extract-paths-mkdir ()
  "Test path extraction from mkdir command."
  (let ((paths (gptel-agent--extract-paths-from-bash-command "mkdir -p /new/directory/path")))
    (should (member "/new/directory/path" paths))))

(ert-deftest gptel-agent-safety-test-extract-paths-none ()
  "Test path extraction returns empty for commands without paths."
  (let ((paths (gptel-agent--extract-paths-from-bash-command "echo hello world")))
    (should (or (null paths) (cl-every (lambda (p) (not (string-prefix-p "/" p))) paths)))))

;;;; Glob Pattern Tests

(ert-deftest gptel-agent-safety-test-glob-match-star ()
  "Test glob matching with single asterisk."
  (let ((gptel-agent-external-path-whitelist nil)
        (gptel-agent--project-boundary-cache nil))
    (should (gptel-agent--path-matches-glob-p "/tmp/test.log" "/tmp/*.log"))
    (should (gptel-agent--path-matches-glob-p "/tmp/app.log" "/tmp/*.log"))
    (should-not (gptel-agent--path-matches-glob-p "/tmp/test.txt" "/tmp/*.log"))
    (should-not (gptel-agent--path-matches-glob-p "/var/test.log" "/tmp/*.log"))))

(ert-deftest gptel-agent-safety-test-glob-match-question ()
  "Test glob matching with question mark."
  (let ((gptel-agent-external-path-whitelist nil)
        (gptel-agent--project-boundary-cache nil))
    (should (gptel-agent--path-matches-glob-p "/tmp/file1.txt" "/tmp/file?.txt"))
    (should (gptel-agent--path-matches-glob-p "/tmp/fileA.txt" "/tmp/file?.txt"))
    (should-not (gptel-agent--path-matches-glob-p "/tmp/file12.txt" "/tmp/file?.txt"))))

;;;; Cache Tests

(ert-deftest gptel-agent-safety-test-invalidate-cache ()
  "Test boundary cache invalidation."
  (gptel-agent-safety-test--with-temp-project project-dir
    (setq gptel-agent--project-boundary-cache
          (list :boundary project-dir
                :timestamp (current-time)
                :whitelist nil))
    (should gptel-agent--project-boundary-cache)
    (gptel-agent--invalidate-boundary-cache)
    (should (null gptel-agent--project-boundary-cache))))

;;;; Customization Tests

(ert-deftest gptel-agent-safety-test-external-path-whitelist-default ()
  "Test default value for external path whitelist."
  (should (null (default-value 'gptel-agent-external-path-whitelist))))

(ert-deftest gptel-agent-safety-test-external-read-policy-default ()
  "Test default value for external read policy."
  (should (eq (default-value 'gptel-agent-external-read-policy) 'ask)))

(ert-deftest gptel-agent-safety-test-external-write-policy-default ()
  "Test default value for external write policy."
  (should (eq (default-value 'gptel-agent-external-write-policy) 'deny)))

(ert-deftest gptel-agent-safety-test-resolve-symlinks-default ()
  "Test default value for symlink resolution."
  (should (default-value 'gptel-agent-resolve-symlinks)))

(ert-deftest gptel-agent-safety-test-show-warnings-default ()
  "Test default value for showing warnings."
  (should (default-value 'gptel-agent-show-external-warnings)))

(provide 'gptel-agent-safety-test)
;;; gptel-agent-safety-test.el ends here
