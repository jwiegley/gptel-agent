;;; gptel-agent-multi-test.el --- Tests for gptel-agent-multi -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Keywords: tests
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for gptel-agent-multi.el multi-session management.
;;
;; Test coverage:
;; - Session registration and tracking
;; - Session switching interface
;; - Session limit enforcement
;; - Idle detection and timeout
;; - Session renaming
;; - Idle session cleanup
;; - Activity tracking
;; - Session metadata queries

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'gptel-agent-multi)

;;;; Test Helpers

(defun gptel-agent-multi-test--create-mock-session (name project)
  "Create a mock agent session buffer with NAME and PROJECT directory.
Returns the buffer object with initialized session metadata."
  (let ((buf (generate-new-buffer (format "*gptel-agent:%s*" name))))
    (with-current-buffer buf
      (setq default-directory project)
      (setq gptel-agent--session-name name)
      (setq gptel-agent--last-activity (current-time)))
    buf))

(defun gptel-agent-multi-test--cleanup ()
  "Clean up all test buffers and reset session state."
  (dolist (buf gptel-agent--active-sessions)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq gptel-agent--active-sessions nil))

(defmacro gptel-agent-multi-test--with-cleanup (&rest body)
  "Execute BODY with automatic cleanup of test sessions."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn ,@body)
     (gptel-agent-multi-test--cleanup)))

;;;; Session Registration Tests

(ert-deftest gptel-agent-multi-test-register-session ()
  "Test session registration adds buffer to active sessions."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      (should (memq buf gptel-agent--active-sessions))
      (with-current-buffer buf
        (should gptel-agent--session-name)
        (should gptel-agent--last-activity)))))

(ert-deftest gptel-agent-multi-test-unregister-session ()
  "Test session unregistration removes buffer from registry."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      (should (memq buf gptel-agent--active-sessions))
      (gptel-agent--unregister-session buf)
      (should-not (memq buf gptel-agent--active-sessions)))))

(ert-deftest gptel-agent-multi-test-register-multiple-sessions ()
  "Test registering multiple sessions."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf1 (gptel-agent-multi-test--create-mock-session "session1" "/tmp/"))
          (buf2 (gptel-agent-multi-test--create-mock-session "session2" "/tmp/"))
          (buf3 (gptel-agent-multi-test--create-mock-session "session3" "/tmp/")))
      (gptel-agent--register-session buf1)
      (gptel-agent--register-session buf2)
      (gptel-agent--register-session buf3)
      (should (= (length gptel-agent--active-sessions) 3))
      (should (memq buf1 gptel-agent--active-sessions))
      (should (memq buf2 gptel-agent--active-sessions))
      (should (memq buf3 gptel-agent--active-sessions)))))

;;;; Activity Tracking Tests

(ert-deftest gptel-agent-multi-test-update-activity ()
  "Test activity timestamp updates."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      (with-current-buffer buf
        (let ((old-time gptel-agent--last-activity))
          (sleep-for 0.1)
          (gptel-agent--update-activity)
          (should (time-less-p old-time gptel-agent--last-activity)))))))

(ert-deftest gptel-agent-multi-test-update-activity-for-buffer ()
  "Test updating activity for a specific buffer."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      (let ((old-time (with-current-buffer buf gptel-agent--last-activity)))
        (sleep-for 0.1)
        (gptel-agent--update-activity buf)
        (with-current-buffer buf
          (should (time-less-p old-time gptel-agent--last-activity)))))))

;;;; Session Limit Tests

(ert-deftest gptel-agent-multi-test-session-limit-check ()
  "Test session limit checking and warnings."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-max-sessions 3)
          (warning-count 0))
      ;; Mock display-warning to count warnings
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _) (cl-incf warning-count))))
        ;; Register up to limit - should warn when approaching
        (dotimes (i 3)
          (let ((buf (gptel-agent-multi-test--create-mock-session
                     (format "session%d" i) "/tmp/")))
            (gptel-agent--register-session buf)))
        ;; Should have warned at least once (when at or near limit)
        (should (> warning-count 0))))))

(ert-deftest gptel-agent-multi-test-session-limit-disabled ()
  "Test session limit disabled when set to nil."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-max-sessions nil)
          (warning-count 0))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _) (cl-incf warning-count))))
        ;; Register many sessions - should not warn
        (dotimes (i 10)
          (let ((buf (gptel-agent-multi-test--create-mock-session
                     (format "session%d" i) "/tmp/")))
            (gptel-agent--register-session buf)))
        (should (= warning-count 0))))))

;;;; Idle Detection Tests

(ert-deftest gptel-agent-multi-test-session-idle-detection ()
  "Test idle session detection."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-idle-timeout 1)  ; 1 second for testing
          (buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      ;; Initially not idle
      (should-not (gptel-agent--session-idle-p buf))
      ;; Wait for timeout
      (sleep-for 1.5)
      ;; Now should be idle
      (should (gptel-agent--session-idle-p buf)))))

(ert-deftest gptel-agent-multi-test-idle-timeout-disabled ()
  "Test idle detection disabled when timeout is nil."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-idle-timeout nil)
          (buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      ;; Set old timestamp
      (with-current-buffer buf
        (setq gptel-agent--last-activity
              (time-subtract (current-time) 1000)))
      ;; Should never be idle when timeout disabled
      (should-not (gptel-agent--session-idle-p buf)))))

(ert-deftest gptel-agent-multi-test-activity-resets-idle ()
  "Test that activity updates reset idle status."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-idle-timeout 1)
          (buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      ;; Wait to become idle
      (sleep-for 1.5)
      (should (gptel-agent--session-idle-p buf))
      ;; Update activity
      (gptel-agent--update-activity buf)
      ;; Should no longer be idle
      (should-not (gptel-agent--session-idle-p buf)))))

;;;; Session Query Tests

(ert-deftest gptel-agent-multi-test-get-active-sessions ()
  "Test retrieving active session list with metadata."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf1 (gptel-agent-multi-test--create-mock-session "alpha" "/tmp/"))
          (buf2 (gptel-agent-multi-test--create-mock-session "beta" "/home/")))
      (gptel-agent--register-session buf1)
      (gptel-agent--register-session buf2)
      (let ((sessions (gptel-agent--get-active-sessions)))
        (should (= (length sessions) 2))
        ;; Check metadata structure
        (dolist (session sessions)
          (should (plist-get session :buffer))
          (should (plist-get session :name))
          (should (plist-get session :project))
          (should (plist-member session :idle))
          (should (plist-member session :idle-p)))))))

(ert-deftest gptel-agent-multi-test-session-sorting ()
  "Test sessions are sorted by most recent activity."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf1 (gptel-agent-multi-test--create-mock-session "old" "/tmp/"))
          (buf2 (gptel-agent-multi-test--create-mock-session "new" "/tmp/")))
      (gptel-agent--register-session buf1)
      ;; Sleep longer than 1 second so idle times differ after rounding
      (sleep-for 1.1)
      (gptel-agent--register-session buf2)
      ;; Update buf2 to make it most recent
      (gptel-agent--update-activity buf2)
      (let ((sessions (gptel-agent--get-active-sessions)))
        ;; Most recent (buf2 with name "new") should be first
        ;; Compare by name to avoid ERT's post-test buffer comparison issues
        (should (equal (plist-get (car sessions) :name) "new"))))))

;;;; Session Renaming Tests

(ert-deftest gptel-agent-multi-test-rename-session ()
  "Test session renaming updates both session name and buffer name."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "oldname" "/tmp/")))
      (gptel-agent--register-session buf)
      (with-current-buffer buf
        (gptel-agent-rename-session "newname")
        (should (string= gptel-agent--session-name "newname"))
        (should (string= (buffer-name) "*gptel-agent:newname*"))))))

(ert-deftest gptel-agent-multi-test-rename-non-session-buffer ()
  "Test renaming fails in non-session buffer."
  (gptel-agent-multi-test--with-cleanup
    (with-temp-buffer
      (should-error (gptel-agent-rename-session "test")
                   :type 'user-error))))

;;;; Idle Session Cleanup Tests

(ert-deftest gptel-agent-multi-test-close-idle-sessions ()
  "Test closing idle sessions."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-idle-timeout 1)
          (buf1 (gptel-agent-multi-test--create-mock-session "idle1" "/tmp/"))
          (buf2 (gptel-agent-multi-test--create-mock-session "active" "/tmp/"))
          (buf3 (gptel-agent-multi-test--create-mock-session "idle2" "/tmp/")))
      (gptel-agent--register-session buf1)
      (gptel-agent--register-session buf2)
      (gptel-agent--register-session buf3)
      ;; Wait for buf1 and buf3 to become idle
      (sleep-for 1.5)
      ;; Keep buf2 active
      (gptel-agent--update-activity buf2)
      ;; Close idle sessions with force
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (let ((closed (gptel-agent-close-idle-sessions t)))
          ;; Should close 2 idle sessions
          (should (= closed 2))
          ;; Active buffer should still exist
          (should (buffer-live-p buf2))
          ;; Idle buffers should be gone
          (should-not (buffer-live-p buf1))
          (should-not (buffer-live-p buf3)))))))

(ert-deftest gptel-agent-multi-test-close-no-idle-sessions ()
  "Test cleanup when no sessions are idle."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-idle-timeout 1000)  ; Very long timeout
          (buf (gptel-agent-multi-test--create-mock-session "active" "/tmp/")))
      (gptel-agent--register-session buf)
      (let ((closed (gptel-agent-close-idle-sessions t)))
        (should (= closed 0))
        (should (buffer-live-p buf))))))

;;;; Session Annotation Tests

(ert-deftest gptel-agent-multi-test-format-annotation ()
  "Test session annotation formatting."
  (gptel-agent-multi-test--with-cleanup
    (let ((session (list :buffer (current-buffer)
                        :name "test-session"
                        :project "/home/user/project/"
                        :model "gpt-4"
                        :idle 120
                        :idle-p t)))
      (let ((annotation (gptel-agent--format-session-annotation session)))
        (should (stringp annotation))
        (should (string-match-p "/home/user/project/" annotation))
        (should (string-match-p "gpt-4" annotation))
        (should (string-match-p "idle:120s" annotation))))))

(ert-deftest gptel-agent-multi-test-format-annotation-active ()
  "Test annotation formatting for active session."
  (gptel-agent-multi-test--with-cleanup
    (let ((session (list :buffer (current-buffer)
                        :name "test"
                        :project "/tmp/"
                        :model nil
                        :idle 30
                        :idle-p nil)))
      (let ((annotation (gptel-agent--format-session-annotation session)))
        (should (string-match-p "active:30s-ago" annotation))
        (should-not (string-match-p "idle:" annotation))))))

;;;; Buffer Cleanup Hook Tests

(ert-deftest gptel-agent-multi-test-cleanup-on-kill ()
  "Test session is unregistered when buffer is killed."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      (should (memq buf gptel-agent--active-sessions))
      ;; Add cleanup hook and kill buffer
      (with-current-buffer buf
        (add-hook 'kill-buffer-hook #'gptel-agent--cleanup-on-kill nil t))
      (kill-buffer buf)
      ;; Should be unregistered
      (should-not (memq buf gptel-agent--active-sessions)))))

;;;; Customization Tests

(ert-deftest gptel-agent-multi-test-default-max-sessions ()
  "Test default max sessions value."
  (should (integerp (default-value 'gptel-agent-max-sessions)))
  (should (> (default-value 'gptel-agent-max-sessions) 0)))

(ert-deftest gptel-agent-multi-test-default-idle-timeout ()
  "Test default idle timeout value."
  (should (integerp (default-value 'gptel-agent-idle-timeout)))
  (should (> (default-value 'gptel-agent-idle-timeout) 0)))

(provide 'gptel-agent-multi-test)
;;; gptel-agent-multi-test.el ends here
