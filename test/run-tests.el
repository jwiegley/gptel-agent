;;; run-tests.el --- Test runner for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Simple test runner script for gptel-agent tests.
;; Run with: emacs -batch -l test/run-tests.el

;;; Code:

;; Add parent directory to load path
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(add-to-list 'load-path (file-name-directory load-file-name))

;; Load dependencies
(require 'ert)

;; Load shared test utilities
(load-file (expand-file-name "test-helper.el"
                             (file-name-directory load-file-name)))

;; Load test files
(load-file (expand-file-name "gptel-agent-permissions-test.el"
                             (file-name-directory load-file-name)))
(load-file (expand-file-name "gptel-agent-safety-test.el"
                             (file-name-directory load-file-name)))
(load-file (expand-file-name "gptel-agent-compaction-test.el"
                             (file-name-directory load-file-name)))
(load-file (expand-file-name "gptel-agent-transient-test.el"
                             (file-name-directory load-file-name)))
(load-file (expand-file-name "gptel-agent-skills-test.el"
                             (file-name-directory load-file-name)))
(load-file (expand-file-name "gptel-agent-modes-test.el"
                             (file-name-directory load-file-name)))
(load-file (expand-file-name "gptel-agent-sessions-test.el"
                             (file-name-directory load-file-name)))
(load-file (expand-file-name "gptel-agent-stats-test.el"
                             (file-name-directory load-file-name)))
(load-file (expand-file-name "gptel-agent-checkpoints-test.el"
                             (file-name-directory load-file-name)))
(load-file (expand-file-name "gptel-agent-multi-test.el"
                             (file-name-directory load-file-name)))

;; Run all tests
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
