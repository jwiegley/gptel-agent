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
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'eglot-current-server) (lambda () t)))
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
            ;; Mock featurep to return t for eglot
            ;; Mock eglot-current-server to return a truthy value (passes availability check)
            ;; Mock eglot--request to throw an error (tests error handling in LSP operation)
            (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                      ((symbol-function 'eglot-current-server) (lambda () 'mock-server))
                      ((symbol-function 'eglot--request)
                       (lambda (&rest _) (error "Mock LSP error"))))
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

;;;; Additional Customization Tests

(ert-deftest gptel-agent-lsp-test-customization-group ()
  "Test customization group is defined."
  (should (get 'gptel-agent-lsp 'custom-group)))

(ert-deftest gptel-agent-lsp-test-enable-type ()
  "Test enable custom type."
  (let ((type (get 'gptel-agent-lsp-enable 'custom-type)))
    (should (eq type 'boolean))))

(ert-deftest gptel-agent-lsp-test-max-results-type ()
  "Test max-results custom type."
  (let ((type (get 'gptel-agent-lsp-max-results 'custom-type)))
    (should (eq type 'integer))))

(ert-deftest gptel-agent-lsp-test-hover-max-length-type ()
  "Test hover-max-length custom type."
  (let ((type (get 'gptel-agent-lsp-hover-max-length 'custom-type)))
    (should (eq type 'integer))))

(ert-deftest gptel-agent-lsp-test-context-lines-type ()
  "Test context-lines custom type."
  (let ((type (get 'gptel-agent-lsp-context-lines 'custom-type)))
    (should (eq type 'integer))))

;;;; Get Buffer Helper Tests

(ert-deftest gptel-agent-lsp-test-get-buffer-existing ()
  "Test get-buffer for existing file."
  (let ((test-file (make-temp-file "lsp-test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";; test"))
          (let ((buf (gptel-agent-lsp--get-buffer test-file)))
            (should buf)
            (should (buffer-live-p buf))
            (kill-buffer buf)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest gptel-agent-lsp-test-get-buffer-nonexistent ()
  "Test get-buffer for nonexistent file."
  (should (null (gptel-agent-lsp--get-buffer "/nonexistent/file.el"))))

(ert-deftest gptel-agent-lsp-test-get-buffer-already-open ()
  "Test get-buffer returns existing buffer."
  (let ((test-file (make-temp-file "lsp-test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";; test"))
          (let* ((buf1 (find-file-noselect test-file))
                 (buf2 (gptel-agent-lsp--get-buffer test-file)))
            (should (eq buf1 buf2))
            (kill-buffer buf1)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

;;;; Position Conversion Extended Tests

(ert-deftest gptel-agent-lsp-test-position-both-nil ()
  "Test position conversion with both values nil."
  (let ((pos (gptel-agent-lsp--position nil nil)))
    (should (= (plist-get pos :line) 0))
    (should (= (plist-get pos :character) 0))))

(ert-deftest gptel-agent-lsp-test-position-large-values ()
  "Test position conversion with large values."
  (let ((pos (gptel-agent-lsp--position 1000 500)))
    (should (= (plist-get pos :line) 999))
    (should (= (plist-get pos :character) 500))))

;;;; Truncation Extended Tests

(ert-deftest gptel-agent-lsp-test-truncate-empty-string ()
  "Test truncation of empty string."
  (should (equal (gptel-agent-lsp--truncate-string "" 100) "")))

(ert-deftest gptel-agent-lsp-test-truncate-very-short-max ()
  "Test truncation with very short max length."
  (let ((result (gptel-agent-lsp--truncate-string "hello" 4)))
    (should (= (length result) 4))
    (should (string-suffix-p "..." result))))

;;;; Location Formatting Extended Tests

(ert-deftest gptel-agent-lsp-test-format-location-with-context ()
  "Test formatting location shows context lines."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-context-lines 1)
         (location (list :uri (concat "file://" test-file)
                         :range (list :start (list :line 1 :character 0)))))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";; Line 1\n;; Line 2\n;; Line 3\n"))
          (let ((formatted (gptel-agent-lsp--format-location location)))
            ;; Should include context
            (should (string-match-p "Line 1" formatted))
            (should (string-match-p "Line 2" formatted))
            (should (string-match-p "Line 3" formatted))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

(ert-deftest gptel-agent-lsp-test-format-location-highlights-target ()
  "Test formatting location highlights target line."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-context-lines 0)
         (location (list :uri (concat "file://" test-file)
                         :range (list :start (list :line 0 :character 5)))))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (let ((formatted (gptel-agent-lsp--format-location location)))
            ;; Should have > marker for target line
            (should (string-match-p ">" formatted))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

;;;; Implementations Operation Tests

(ert-deftest gptel-agent-lsp-test-implementations-disabled ()
  "Test implementations operation when LSP is disabled."
  (let ((gptel-agent-lsp-enable nil))
    (should (string-match-p "disabled"
                            (gptel-agent-lsp--implementations "test.el")))))

(ert-deftest gptel-agent-lsp-test-implementations-no-server ()
  "Test implementations operation with no LSP server."
  (let ((gptel-agent-lsp-enable t))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'eglot-current-server) (lambda () nil)))
      (should (string-match-p "No LSP server"
                              (gptel-agent-lsp--implementations "test.el"))))))

(ert-deftest gptel-agent-lsp-test-implementations-file-not-found ()
  "Test implementations operation when file doesn't exist."
  (let ((gptel-agent-lsp-enable t))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'eglot-current-server) (lambda () t)))
      (should (string-match-p "not found"
                              (gptel-agent-lsp--implementations "/nonexistent/file.el"))))))

;;;; Operations with Symbol Search Tests

(ert-deftest gptel-agent-lsp-test-definitions-with-symbol ()
  "Test definitions operation with symbol name search."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun my-test-function () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _) nil)))
            ;; Should not error and should search for symbol
            (let ((result (gptel-agent-lsp--definitions test-file "my-test-function")))
              (should (stringp result)))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

;;;; Mock LSP Response Tests

(ert-deftest gptel-agent-lsp-test-definitions-with-response ()
  "Test definitions operation with mock LSP response."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _)
                       (list :uri (concat "file://" test-file)
                             :range (list :start (list :line 0 :character 0))))))
            (let ((result (gptel-agent-lsp--definitions test-file nil 1 0)))
              (should (stringp result))
              (should (string-match-p "defun" result)))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

(ert-deftest gptel-agent-lsp-test-references-with-vector-response ()
  "Test references operation with vector response."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)\n(test)\n(test)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _)
                       ;; Return vector (array) of locations
                       (vector
                        (list :uri (concat "file://" test-file)
                              :range (list :start (list :line 1 :character 0)))
                        (list :uri (concat "file://" test-file)
                              :range (list :start (list :line 2 :character 0)))))))
            (let ((result (gptel-agent-lsp--references test-file nil 1 0)))
              (should (stringp result))
              ;; Should format multiple locations
              (should (string-match-p "----" result)))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

(ert-deftest gptel-agent-lsp-test-hover-with-string-contents ()
  "Test hover operation with string contents."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _)
                       (list :contents "This is documentation"))))
            (let ((result (gptel-agent-lsp--hover test-file nil 1 0)))
              (should (string= result "This is documentation")))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

(ert-deftest gptel-agent-lsp-test-hover-with-value-contents ()
  "Test hover operation with :value contents."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _)
                       (list :contents (list :value "Markdown documentation"
                                             :kind "markdown")))))
            (let ((result (gptel-agent-lsp--hover test-file nil 1 0)))
              (should (string= result "Markdown documentation")))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

(ert-deftest gptel-agent-lsp-test-symbols-with-response ()
  "Test symbols operation with mock response."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentIdentifier)
                     (lambda () '(:uri "file://test")))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _)
                       (vector
                        (list :name "test"
                              :kind 12
                              :range (list :start (list :line 0 :character 0)))))))
            (let ((result (gptel-agent-lsp--symbols test-file)))
              (should (stringp result))
              (should (string-match-p "test" result))
              (should (string-match-p "kind: 12" result)))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

;;;; Max Results Limiting Tests

(ert-deftest gptel-agent-lsp-test-definitions-limits-results ()
  "Test definitions limits results to max."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t)
         (gptel-agent-lsp-max-results 2))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _)
                       ;; Return more results than max
                       (vector
                        (list :uri (concat "file://" test-file)
                              :range (list :start (list :line 0 :character 0)))
                        (list :uri (concat "file://" test-file)
                              :range (list :start (list :line 0 :character 0)))
                        (list :uri (concat "file://" test-file)
                              :range (list :start (list :line 0 :character 0)))
                        (list :uri (concat "file://" test-file)
                              :range (list :start (list :line 0 :character 0)))))))
            (let ((result (gptel-agent-lsp--definitions test-file nil 1 0)))
              ;; Should have only 2 separators (max-results = 2 means 1 separator)
              (should (stringp result))
              ;; Count separators
              (let ((count 0)
                    (start 0))
                (while (string-match "----" result start)
                  (setq count (1+ count)
                        start (match-end 0)))
                (should (<= count (1- gptel-agent-lsp-max-results)))))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

;;;; Tool Description Tests

(ert-deftest gptel-agent-lsp-test-tool-description ()
  "Test tool has comprehensive description."
  (let* ((tool (gptel-agent-lsp-test--get-tool))
         (desc (gptel-tool-description tool)))
    (should (stringp desc))
    (should (string-match-p "definitions" desc))
    (should (string-match-p "references" desc))
    (should (string-match-p "hover" desc))
    (should (string-match-p "symbols" desc))
    (should (string-match-p "implementations" desc))))

(ert-deftest gptel-agent-lsp-test-tool-arg-types ()
  "Test tool argument types."
  (let* ((tool (gptel-agent-lsp-test--get-tool))
         (args (gptel-tool-args tool)))
    ;; operation is string with enum
    ;; Note: gptel converts type symbols to strings internally
    (let ((op-arg (nth 0 args)))
      (should (equal (plist-get op-arg :type) "string"))
      (should (plist-get op-arg :enum)))
    ;; file is string
    (let ((file-arg (nth 1 args)))
      (should (equal (plist-get file-arg :type) "string")))
    ;; line/column are integers and optional
    (let ((line-arg (nth 3 args)))
      (should (equal (plist-get line-arg :type) "integer"))
      (should (plist-get line-arg :optional)))))

;;;; Availability Check Extended Tests

(ert-deftest gptel-agent-lsp-test-check-available ()
  "Test availability check returns nil when all conditions met."
  (let ((gptel-agent-lsp-enable t))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'eglot-current-server) (lambda () 'mock-server)))
      (should (null (gptel-agent-lsp--check-availability))))))

;;;; Error Message Tests

(ert-deftest gptel-agent-lsp-test-definitions-error-suggests-grep ()
  "Test definitions error message suggests Grep."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _) (error "LSP error"))))
            (let ((result (gptel-agent-lsp--definitions test-file "my-symbol" 1 0)))
              (should (string-match-p "Grep" result))
              (should (string-match-p "my-symbol" result)))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

;;;; No Results Tests

(ert-deftest gptel-agent-lsp-test-definitions-no-results ()
  "Test definitions with no results."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _) nil)))
            (let ((result (gptel-agent-lsp--definitions test-file nil 1 0)))
              (should (string-match-p "No definitions found" result)))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

(ert-deftest gptel-agent-lsp-test-references-no-results ()
  "Test references with no results."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _) nil)))
            (let ((result (gptel-agent-lsp--references test-file nil 1 0)))
              (should (string-match-p "No references found" result)))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

(ert-deftest gptel-agent-lsp-test-hover-no-results ()
  "Test hover with no results."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _) nil)))
            (let ((result (gptel-agent-lsp--hover test-file nil 1 0)))
              (should (string-match-p "No hover information" result)))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

(ert-deftest gptel-agent-lsp-test-implementations-no-results ()
  "Test implementations with no results."
  (let* ((test-file (make-temp-file "lsp-test" nil ".el"))
         (gptel-agent-lsp-enable t))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test () nil)"))
          (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                    ((symbol-function 'eglot-current-server) (lambda () 'mock))
                    ((symbol-function 'eglot--TextDocumentPositionParams)
                     (lambda () '(:textDocument (:uri "file://test"))))
                    ((symbol-function 'eglot--request)
                     (lambda (&rest _) nil)))
            (let ((result (gptel-agent-lsp--implementations test-file nil 1 0)))
              (should (string-match-p "No implementations found" result)))))
      (delete-file test-file)
      (when-let ((buf (find-buffer-visiting test-file)))
        (kill-buffer buf)))))

(provide 'gptel-agent-lsp-test)
;;; gptel-agent-lsp-test.el ends here
