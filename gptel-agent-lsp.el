;;; gptel-agent-lsp.el --- LSP integration for gptel-agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Keywords: convenience

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

;; LSP integration tool for gptel-agent.
;;
;; This module provides LLMs with access to Language Server Protocol (LSP)
;; information through Eglot (Emacs built-in LSP client in Emacs 29+):
;; - Jump to definitions
;; - Find references
;; - Hover documentation
;; - Document symbols
;; - Find implementations
;;
;; The tool gracefully degrades when Eglot is unavailable or no LSP server
;; is running, providing helpful error messages to guide the LLM toward
;; alternative approaches.

;;; Code:

(require 'gptel-request)
(eval-when-compile (require 'cl-lib))

(declare-function eglot--request "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot--TextDocumentIdentifier "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")

;;; Customization

(defgroup gptel-agent-lsp nil
  "LSP integration for gptel-agent."
  :group 'gptel-agent
  :prefix "gptel-agent-lsp-")

(defcustom gptel-agent-lsp-enable t
  "Enable LSP integration for gptel-agent.
When nil, the LSP tool will not be available."
  :type 'boolean
  :group 'gptel-agent-lsp)

(defcustom gptel-agent-lsp-max-results 20
  "Maximum number of LSP results to return.
Limits the number of locations, references, or symbols returned to prevent
overwhelming the LLM with too much information."
  :type 'integer
  :group 'gptel-agent-lsp)

(defcustom gptel-agent-lsp-hover-max-length 500
  "Maximum length of hover documentation to include.
Hover text longer than this will be truncated."
  :type 'integer
  :group 'gptel-agent-lsp)

(defcustom gptel-agent-lsp-context-lines 2
  "Number of context lines to include around LSP results.
Provides surrounding code context for definitions and references."
  :type 'integer
  :group 'gptel-agent-lsp)

;;; Helper functions

(defun gptel-agent-lsp--check-availability ()
  "Check if LSP functionality is available.
Returns nil if available, error string otherwise."
  (cond
   ((not gptel-agent-lsp-enable)
    "LSP integration is disabled (see `gptel-agent-lsp-enable')")
   ((not (featurep 'eglot))
    "Eglot is not available. Consider using Grep to search for definitions")
   ((not (eglot-current-server))
    "No LSP server is running for this buffer. Start an LSP server first or use Grep instead")
   (t nil)))

(defun gptel-agent-lsp--get-buffer (file)
  "Get or create buffer for FILE.
Returns buffer visiting FILE, or nil if FILE doesn't exist."
  (when (file-exists-p file)
    (or (find-buffer-visiting file)
        (find-file-noselect file))))

(defun gptel-agent-lsp--position (line column)
  "Convert LINE and COLUMN to LSP position.
LINE and COLUMN are 1-indexed as provided by the LLM.
Returns a plist with :line and :character keys (0-indexed)."
  (list :line (1- (or line 1))
        :character (or column 0)))

(defun gptel-agent-lsp--format-location (location)
  "Format LOCATION as a human-readable string with context.
LOCATION is a plist with :uri, :range keys from LSP.
Returns a string with file:line - context."
  (let* ((uri (plist-get location :uri))
         (file (string-remove-prefix "file://" uri))
         (range (plist-get location :range))
         (start (plist-get range :start))
         (line (1+ (plist-get start :line)))
         (col (plist-get start :character)))
    (if-let ((buf (gptel-agent-lsp--get-buffer file)))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (move-to-column col)
            (let* ((start-line (max 1 (- line gptel-agent-lsp-context-lines)))
                   (end-line (+ line gptel-agent-lsp-context-lines))
                   (context
                    (string-join
                     (cl-loop for i from start-line to end-line
                              collect
                              (progn
                                (goto-char (point-min))
                                (forward-line (1- i))
                                (format "%s%4d: %s"
                                        (if (= i line) ">" " ")
                                        i
                                        (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position)))))
                     "\n")))
              (format "%s:%d:%d\n%s" file line col context))))
      (format "%s:%d:%d [file not accessible]" file line col))))

(defun gptel-agent-lsp--truncate-string (str max-length)
  "Truncate STR to MAX-LENGTH, adding ellipsis if needed."
  (if (> (length str) max-length)
      (concat (substring str 0 (- max-length 3)) "...")
    str))

;;; LSP operation implementations

(defun gptel-agent-lsp--definitions (file &optional symbol line column)
  "Find definitions for SYMBOL in FILE at LINE, COLUMN.
Returns formatted string with definition locations and context."
  (if-let ((err (gptel-agent-lsp--check-availability)))
      (format "Error: %s" err)
    (if-let ((buf (gptel-agent-lsp--get-buffer file)))
        (with-current-buffer buf
          (save-excursion
            (when line
              (goto-char (point-min))
              (forward-line (1- line)))
            (when column
              (move-to-column column))
            (condition-case err
                (let* ((server (eglot-current-server))
                       (pos (if line
                                (gptel-agent-lsp--position line column)
                              (save-excursion
                                (when symbol
                                  (goto-char (point-min))
                                  (if (search-forward symbol nil t)
                                      (list :line (1- (line-number-at-pos))
                                            :character (current-column))
                                    (list :line 0 :character 0))))))
                       (params (eglot--TextDocumentPositionParams))
                       (response (eglot--request server
                                                 :textDocument/definition
                                                 params)))
                  (if response
                      (let ((locations (if (vectorp response)
                                           (append response nil)
                                         (list response))))
                        (if (> (length locations) gptel-agent-lsp-max-results)
                            (setq locations (seq-take locations gptel-agent-lsp-max-results)))
                        (mapconcat #'gptel-agent-lsp--format-location
                                   locations
                                   "\n\n----\n\n"))
                    "No definitions found"))
              (error
               (format "LSP error: %s\nConsider using Grep to search for '%s'"
                       (error-message-string err)
                       (or symbol "symbol"))))))
      (format "Error: File %s not found" file))))

(defun gptel-agent-lsp--references (file &optional symbol line column)
  "Find references for SYMBOL in FILE at LINE, COLUMN.
Returns formatted string with reference locations and context."
  (if-let ((err (gptel-agent-lsp--check-availability)))
      (format "Error: %s" err)
    (if-let ((buf (gptel-agent-lsp--get-buffer file)))
        (with-current-buffer buf
          (save-excursion
            (when line
              (goto-char (point-min))
              (forward-line (1- line)))
            (when column
              (move-to-column column))
            (condition-case err
                (let* ((server (eglot-current-server))
                       (pos (if line
                                (gptel-agent-lsp--position line column)
                              (save-excursion
                                (when symbol
                                  (goto-char (point-min))
                                  (if (search-forward symbol nil t)
                                      (list :line (1- (line-number-at-pos))
                                            :character (current-column))
                                    (list :line 0 :character 0))))))
                       (params (append (eglot--TextDocumentPositionParams)
                                       (list :context (list :includeDeclaration t))))
                       (response (eglot--request server
                                                 :textDocument/references
                                                 params)))
                  (if response
                      (let ((locations (if (vectorp response)
                                           (append response nil)
                                         nil)))
                        (if (> (length locations) gptel-agent-lsp-max-results)
                            (setq locations (seq-take locations gptel-agent-lsp-max-results)))
                        (if locations
                            (mapconcat #'gptel-agent-lsp--format-location
                                       locations
                                       "\n\n----\n\n")
                          "No references found"))
                    "No references found"))
              (error
               (format "LSP error: %s\nConsider using Grep to search for '%s'"
                       (error-message-string err)
                       (or symbol "symbol"))))))
      (format "Error: File %s not found" file))))

(defun gptel-agent-lsp--hover (file &optional symbol line column)
  "Get hover documentation for SYMBOL in FILE at LINE, COLUMN.
Returns formatted string with hover information."
  (if-let ((err (gptel-agent-lsp--check-availability)))
      (format "Error: %s" err)
    (if-let ((buf (gptel-agent-lsp--get-buffer file)))
        (with-current-buffer buf
          (save-excursion
            (when line
              (goto-char (point-min))
              (forward-line (1- line)))
            (when column
              (move-to-column column))
            (condition-case err
                (let* ((server (eglot-current-server))
                       (pos (if line
                                (gptel-agent-lsp--position line column)
                              (save-excursion
                                (when symbol
                                  (goto-char (point-min))
                                  (if (search-forward symbol nil t)
                                      (list :line (1- (line-number-at-pos))
                                            :character (current-column))
                                    (list :line 0 :character 0))))))
                       (params (eglot--TextDocumentPositionParams))
                       (response (eglot--request server
                                                 :textDocument/hover
                                                 params)))
                  (if response
                      (let* ((contents (plist-get response :contents))
                             (value (cond
                                     ((stringp contents) contents)
                                     ((and (listp contents)
                                           (plist-get contents :value))
                                      (plist-get contents :value))
                                     (t (format "%S" contents)))))
                        (gptel-agent-lsp--truncate-string
                         value gptel-agent-lsp-hover-max-length))
                    "No hover information available"))
              (error
               (format "LSP error: %s\nTry using 'Grep' or 'Read' tools instead"
                       (error-message-string err))))))
      (format "Error: File %s not found" file))))

(defun gptel-agent-lsp--symbols (file &optional _symbol _line _column)
  "Get document symbols from FILE.
Returns formatted string with symbol information."
  (if-let ((err (gptel-agent-lsp--check-availability)))
      (format "Error: %s" err)
    (if-let ((buf (gptel-agent-lsp--get-buffer file)))
        (with-current-buffer buf
          (condition-case err
              (let* ((server (eglot-current-server))
                     (params (eglot--TextDocumentIdentifier))
                     (response (eglot--request server
                                               :textDocument/documentSymbol
                                               params)))
                (if response
                    (let ((symbols (if (vectorp response)
                                       (append response nil)
                                     nil)))
                      (if (> (length symbols) gptel-agent-lsp-max-results)
                          (setq symbols (seq-take symbols gptel-agent-lsp-max-results)))
                      (if symbols
                          (mapconcat
                           (lambda (sym)
                             (let* ((name (plist-get sym :name))
                                    (kind (plist-get sym :kind))
                                    (range (plist-get sym :range))
                                    (start (plist-get range :start))
                                    (line (1+ (plist-get start :line))))
                               (format "%s:%d - %s (kind: %s)"
                                       file line name kind)))
                           symbols
                           "\n")
                        "No symbols found"))
                  "No symbols found"))
            (error
             (format "LSP error: %s\nTry using 'Grep' to search the file instead"
                     (error-message-string err)))))
      (format "Error: File %s not found" file))))

(defun gptel-agent-lsp--implementations (file &optional symbol line column)
  "Find implementations for SYMBOL in FILE at LINE, COLUMN.
Returns formatted string with implementation locations and context."
  (if-let ((err (gptel-agent-lsp--check-availability)))
      (format "Error: %s" err)
    (if-let ((buf (gptel-agent-lsp--get-buffer file)))
        (with-current-buffer buf
          (save-excursion
            (when line
              (goto-char (point-min))
              (forward-line (1- line)))
            (when column
              (move-to-column column))
            (condition-case err
                (let* ((server (eglot-current-server))
                       (pos (if line
                                (gptel-agent-lsp--position line column)
                              (save-excursion
                                (when symbol
                                  (goto-char (point-min))
                                  (if (search-forward symbol nil t)
                                      (list :line (1- (line-number-at-pos))
                                            :character (current-column))
                                    (list :line 0 :character 0))))))
                       (params (eglot--TextDocumentPositionParams))
                       (response (eglot--request server
                                                 :textDocument/implementation
                                                 params)))
                  (if response
                      (let ((locations (if (vectorp response)
                                           (append response nil)
                                         (list response))))
                        (if (> (length locations) gptel-agent-lsp-max-results)
                            (setq locations (seq-take locations gptel-agent-lsp-max-results)))
                        (mapconcat #'gptel-agent-lsp--format-location
                                   locations
                                   "\n\n----\n\n"))
                    "No implementations found"))
              (error
               (format "LSP error: %s\nConsider using Grep to search for implementations"
                       (error-message-string err))))))
      (format "Error: File %s not found" file))))

;;; Tool registration

(gptel-make-tool
 :name "LSP"
 :function
 (lambda (operation file &optional symbol line column)
   "Perform LSP OPERATION on FILE.
OPERATION is one of: definitions, references, hover, symbols, implementations.
FILE is the path to the file to query.
SYMBOL is the symbol name (optional, used for search if line/column not given).
LINE is the 1-indexed line number (optional).
COLUMN is the 0-indexed column number (optional)."
   (pcase operation
     ("definitions" (gptel-agent-lsp--definitions file symbol line column))
     ("references" (gptel-agent-lsp--references file symbol line column))
     ("hover" (gptel-agent-lsp--hover file symbol line column))
     ("symbols" (gptel-agent-lsp--symbols file symbol line column))
     ("implementations" (gptel-agent-lsp--implementations file symbol line column))
     (_ (format "Error: Unknown operation '%s'. \
Valid operations: definitions, references, hover, symbols, implementations"
                operation))))
 :description "Query Language Server Protocol (LSP) for code intelligence.

Provides access to LSP features through Eglot (Emacs built-in LSP client):

OPERATIONS:
- definitions: Jump to symbol definitions
- references: Find all references to a symbol
- hover: Get documentation and type information
- symbols: List all symbols in a document
- implementations: Find interface implementations

POSITIONING:
You can specify the target symbol in two ways:
1. Provide LINE and COLUMN for exact positioning (recommended)
2. Provide SYMBOL name for text search (less reliable)

USAGE NOTES:
- LSP must be running for the file (start with M-x eglot)
- Results are limited to `gptel-agent-lsp-max-results' entries
- Hover text truncated to `gptel-agent-lsp-hover-max-length' chars
- Context lines shown around results controlled by `gptel-agent-lsp-context-lines'

GRACEFUL DEGRADATION:
If LSP is unavailable or fails, error messages will suggest alternatives:
- Use 'Grep' to search for definitions and references
- Use 'Read' to view file contents
- Use introspection tools for Elisp code

EXAMPLES:
- Find definitions: operation=\"definitions\", file=\"main.py\", line=42, column=10
- List symbols: operation=\"symbols\", file=\"src/utils.rs\"
- Get hover info: operation=\"hover\", file=\"lib.go\", symbol=\"Handler\""
 :args '(( :name "operation"
           :type string
           :enum ["definitions" "references" "hover" "symbols" "implementations"]
           :description "LSP operation to perform")
         ( :name "file"
           :type string
           :description "Path to the file to query")
         ( :name "symbol"
           :type string
           :optional t
           :description "Symbol name (used for search if line/column not provided)")
         ( :name "line"
           :type integer
           :optional t
           :description "1-indexed line number")
         ( :name "column"
           :type integer
           :optional t
           :description "0-indexed column number"))
 :category "gptel-agent"
 :include (and gptel-agent-lsp-enable t))

(provide 'gptel-agent-lsp)
;;; gptel-agent-lsp.el ends here
