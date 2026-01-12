;;; gptel-agent-safety-test.el --- Tests for gptel-agent-safety -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Keywords: tests
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tests for gptel-agent-safety.el doom loop detection.
;;
;; Test coverage:
;; - Ring buffer management (tracking, retrieval, overflow, clearing)
;; - Pattern detection (identical, similar, alternating, oscillating)
;; - Argument normalization (paths, whitespace, types)
;; - Token estimation
;; - Score calculation
;; - Configuration effects

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

(provide 'gptel-agent-safety-test)
;;; gptel-agent-safety-test.el ends here
