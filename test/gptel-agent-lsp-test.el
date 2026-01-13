;;; gptel-agent-lsp-test.el --- Tests for gptel-agent-lsp  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for gptel-agent-lsp.el

;;; Code:

(require 'ert)
(require 'gptel-agent-lsp)

;;; Helper Functions

(defun gptel-agent-lsp-test--get-tool ()
  "Get the LSP tool from the gptel registry.
Returns the tool struct or nil if not found."
  (when (boundp 'gptel--known-tools)
    (cdr (assoc "LSP" (cdr (assoc "gptel-agent" gptel--known-tools))))))

;;; Tool Registration Tests

(ert-deftest gptel-agent-lsp-test-tool-registered ()
  "Test that LSP tool is properly registered."
  (should (gptel-agent-lsp-test--get-tool)))

(ert-deftest gptel-agent-lsp-test-tool-category ()
  "Test that LSP tool has correct category."
  (let ((tool (gptel-agent-lsp-test--get-tool)))
    (should tool)
    (should (equal (gptel-tool-category tool) "gptel-agent"))))

(ert-deftest gptel-agent-lsp-test-tool-args ()
  "Test that LSP tool has correct arguments defined."
  (let* ((tool (gptel-agent-lsp-test--get-tool))
         (args (gptel-tool-args tool)))
    (should tool)
    (should (= (length args) 5))
    (should (equal (plist-get (nth 0 args) :name) "operation"))
    (should (equal (plist-get (nth 1 args) :name) "file"))
    (should (equal (plist-get (nth 2 args) :name) "symbol"))
    (should (equal (plist-get (nth 3 args) :name) "line"))
    (should (equal (plist-get (nth 4 args) :name) "column"))))

;;; Availability Checks

(ert-deftest gptel-agent-lsp-test-check-disabled ()
  "Test availability check when LSP is disabled."
  (let ((gptel-agent-lsp-enable nil))
    (should (stringp (gptel-agent-lsp--check-availability)))
    (should (string-match-p "disabled" (gptel-agent-lsp--check-availability)))))

(ert-deftest gptel-agent-lsp-test-check-no-eglot ()
  "Test availability check when Eglot is not available."
  (let ((gptel-agent-lsp-enable t))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature) (not (eq feature 'eglot)))))
      (should (stringp (gptel-agent-lsp--check-availability)))
      (should (string-match-p "not available" (gptel-agent-lsp--check-availability))))))

(ert-deftest gptel-agent-lsp-test-check-no-server ()
  "Test availability check when no LSP server is running."
  (let ((gptel-agent-lsp-enable t))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'eglot-current-server) (lambda () nil)))
      (should (stringp (gptel-agent-lsp--check-availability)))
      (should (string-match-p "No LSP server" (gptel-agent-lsp--check-availability))))))

;;; Position Conversion Tests

(ert-deftest gptel-agent-lsp-test-position-conversion ()
  "Test conversion of line/column to LSP position."
  (let ((pos (gptel-agent-lsp--position 10 5)))
    (should (= (plist-get pos :line) 9))
    (should (= (plist-get pos :character) 5))))

(ert-deftest gptel-agent-lsp-test-position-default-column ()
  "Test position conversion with nil column."
  (let ((pos (gptel-agent-lsp--position 1 nil)))
    (should (= (plist-get pos :line) 0))
    (should (= (plist-get pos :character) 0))))

(ert-deftest gptel-agent-lsp-test-position-default-line ()
  "Test position conversion with nil line."
  (let ((pos (gptel-agent-lsp--position nil 10)))
    (should (= (plist-get pos :line) 0))
    (should (= (plist-get pos :character) 10))))

;;; String Truncation Tests

(ert-deftest gptel-agent-lsp-test-truncate-short-string ()
  "Test that short strings are not truncated."
  (should (equal (gptel-agent-lsp--truncate-string "short" 100)
                 "short")))

(ert-deftest gptel-agent-lsp-test-truncate-long-string ()
  "Test that long strings are truncated with ellipsis."
  (let ((long-string (make-string 100 ?x)))
    (should (equal (gptel-agent-lsp--truncate-string long-string 20)
                   (concat (make-string 17 ?x) "...")))))

(ert-deftest gptel-agent-lsp-test-truncate-exact-length ()
  "Test truncation at exact length boundary."
  (let ((str (make-string 20 ?x)))
    (should (equal (gptel-agent-lsp--truncate-string str 20)
                   str))))

;;; Location Formatting Tests

(ert-deftest gptel-agent-lsp-test-format-location-inaccessible ()
  "Test formatting location when file is not accessible."
  (let ((location (list :uri "file:///nonexistent/file.el"
                        :range (list :start (list :line 9 :character 5)))))
    (should (string-match-p "file not accessible"
                            (gptel-agent-lsp--format-location location)))))

(ert-deftest gptel-agent-lsp-test-format-location-with-file ()
  "Test formatting location with accessible file."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (location (list :uri (concat "file://" test-file)
                         :range (list :start (list :line 0 :character 0)))))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";; Test file\n(defun test ())\n"))
          (let ((formatted (gptel-agent-lsp--format-location location)))
            (should (string-match-p test-file formatted))
            (should (string-match-p ":1:0" formatted))))
      (delete-file test-file))))

;;; Operation Tests with Mock LSP

(ert-deftest gptel-agent-lsp-test-definitions-disabled ()
  "Test definitions operation when LSP is disabled."
  (let ((gptel-agent-lsp-enable nil))
    (should (string-match-p "disabled"
                            (gptel-agent-lsp--definitions "test.el")))))

(ert-deftest gptel-agent-lsp-test-references-no-server ()
  "Test references operation with no LSP server."
  (let ((gptel-agent-lsp-enable t))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'eglot-current-server) (lambda () nil)))
      (should (string-match-p "No LSP server"
                              (gptel-agent-lsp--references "test.el"))))))

(ert-deftest gptel-agent-lsp-test-hover-file-not-found ()
  "Test hover operation when file doesn't exist."
  (let ((gptel-agent-lsp-enable t))
    (cl-letf (((symbol-function 'eglot-current-server) (lambda () t)))
      (should (string-match-p "not found"
                              (gptel-agent-lsp--hover "/nonexistent/file.el"))))))

(ert-deftest gptel-agent-lsp-test-symbols-error-handling ()
  "Test symbols operation error handling."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test ())"))
          (with-current-buffer (find-file-noselect test-file)
            (cl-letf (((symbol-function 'eglot-current-server)
                       (lambda () (error "Mock LSP error"))))
              (should (string-match-p "LSP error"
                                      (gptel-agent-lsp--symbols test-file))))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

;;; Tool Function Tests

(ert-deftest gptel-agent-lsp-test-tool-invalid-operation ()
  "Test tool function with invalid operation."
  (let* ((tool (gptel-agent-lsp-test--get-tool))
         (fn (gptel-tool-function tool))
         (result (funcall fn "invalid" "test.el")))
    (should (string-match-p "Unknown operation" result))))

(ert-deftest gptel-agent-lsp-test-tool-valid-operations ()
  "Test that all valid operations are recognized."
  (let* ((tool (gptel-agent-lsp-test--get-tool))
         (fn (gptel-tool-function tool))
         (operations '("definitions" "references" "hover" "symbols" "implementations"))
         (gptel-agent-lsp-enable nil))
    (dolist (op operations)
      (let ((result (funcall fn op "test.el")))
        (should-not (string-match-p "Unknown operation" result))))))

;;; Configuration Tests

(ert-deftest gptel-agent-lsp-test-max-results-default ()
  "Test that max-results has sensible default."
  (should (integerp gptel-agent-lsp-max-results))
  (should (> gptel-agent-lsp-max-results 0)))

(ert-deftest gptel-agent-lsp-test-hover-max-length-default ()
  "Test that hover-max-length has sensible default."
  (should (integerp gptel-agent-lsp-hover-max-length))
  (should (> gptel-agent-lsp-hover-max-length 0)))

(ert-deftest gptel-agent-lsp-test-context-lines-default ()
  "Test that context-lines has sensible default."
  (should (integerp gptel-agent-lsp-context-lines))
  (should (>= gptel-agent-lsp-context-lines 0)))

;;; Graceful Degradation Tests

(ert-deftest gptel-agent-lsp-test-error-suggests-grep ()
  "Test that errors suggest using Grep as alternative."
  (let ((gptel-agent-lsp-enable t))
    (cl-letf (((symbol-function 'eglot-current-server) (lambda () nil)))
      (should (string-match-p "Grep"
                              (gptel-agent-lsp--definitions "test.el" "foo"))))))

(ert-deftest gptel-agent-lsp-test-hover-suggests-read ()
  "Test that hover errors suggest Read tool."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test ())"))
          (with-current-buffer (find-file-noselect test-file)
            (cl-letf (((symbol-function 'eglot-current-server)
                       (lambda () (error "Mock error"))))
              (should (string-match-p "Grep\\|Read"
                                      (gptel-agent-lsp--hover test-file))))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(provide 'gptel-agent-lsp-test)
;;; gptel-agent-lsp-test.el ends here
