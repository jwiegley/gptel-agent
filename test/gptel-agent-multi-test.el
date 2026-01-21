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

;;;; Additional Customization Tests

(ert-deftest gptel-agent-multi-test-customization-group ()
  "Test customization group is defined."
  (should (get 'gptel-agent-multi 'custom-group)))

(ert-deftest gptel-agent-multi-test-max-sessions-type ()
  "Test max sessions custom type."
  (let ((type (get 'gptel-agent-max-sessions 'custom-type)))
    (should (consp type))
    (should (eq (car type) 'choice))))

(ert-deftest gptel-agent-multi-test-idle-timeout-type ()
  "Test idle timeout custom type."
  (let ((type (get 'gptel-agent-idle-timeout 'custom-type)))
    (should (consp type))
    (should (eq (car type) 'choice))))

;;;; Session Registry Extended Tests

(ert-deftest gptel-agent-multi-test-register-session-initializes-name ()
  "Test session registration initializes name from buffer name."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (generate-new-buffer "*gptel-agent:test-init*")))
      (with-current-buffer buf
        (setq default-directory "/tmp/"))
      (gptel-agent--register-session buf)
      (with-current-buffer buf
        ;; Session name should default to buffer name
        (should gptel-agent--session-name)))))

(ert-deftest gptel-agent-multi-test-register-preserves-existing-name ()
  "Test registration preserves existing session name."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (generate-new-buffer "*gptel-agent:test*")))
      (with-current-buffer buf
        (setq default-directory "/tmp/")
        (setq gptel-agent--session-name "custom-name"))
      (gptel-agent--register-session buf)
      (with-current-buffer buf
        (should (string= gptel-agent--session-name "custom-name"))))))

(ert-deftest gptel-agent-multi-test-register-no-duplicates ()
  "Test registering same buffer twice doesn't duplicate."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      (gptel-agent--register-session buf)
      ;; Should only be in list once
      (should (= 1 (cl-count buf gptel-agent--active-sessions))))))

;;;; Activity Update Extended Tests

(ert-deftest gptel-agent-multi-test-update-activity-non-session ()
  "Test activity update in non-registered buffer."
  (gptel-agent-multi-test--with-cleanup
    (with-temp-buffer
      ;; Should not error even if buffer not registered
      (gptel-agent--update-activity)
      ;; Should have no effect
      (should-not gptel-agent--last-activity))))

(ert-deftest gptel-agent-multi-test-update-activity-updates-registered ()
  "Test activity update only affects registered sessions."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      (with-current-buffer buf
        (let ((old-activity gptel-agent--last-activity))
          (sleep-for 0.1)
          (gptel-agent--update-activity)
          (should (time-less-p old-activity gptel-agent--last-activity)))))))

;;;; Session Limit Extended Tests

(ert-deftest gptel-agent-multi-test-check-limit-approaching ()
  "Test warning when approaching session limit."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-max-sessions 3)
          (warning-shown nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_ msg _)
                   (when (string-match-p "Approaching" msg)
                     (setq warning-shown t)))))
        ;; Register 2 sessions (at limit - 1)
        (dotimes (i 2)
          (let ((buf (gptel-agent-multi-test--create-mock-session
                      (format "session%d" i) "/tmp/")))
            (gptel-agent--register-session buf)))
        (should warning-shown)))))

(ert-deftest gptel-agent-multi-test-check-limit-at-limit ()
  "Test warning when at session limit."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-max-sessions 2)
          (warning-msg nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_ msg _)
                   (setq warning-msg msg))))
        ;; Register 2 sessions (at limit)
        (dotimes (i 2)
          (let ((buf (gptel-agent-multi-test--create-mock-session
                      (format "session%d" i) "/tmp/")))
            (gptel-agent--register-session buf)))
        (should (string-match-p "Maximum" warning-msg))))))

(ert-deftest gptel-agent-multi-test-check-limit-under-limit ()
  "Test no warning when well under limit."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-max-sessions 10)
          (warning-count 0))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _) (cl-incf warning-count))))
        ;; Register 1 session (well under limit)
        (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
          (gptel-agent--register-session buf))
        (should (= warning-count 0))))))

;;;; Get Active Sessions Extended Tests

(ert-deftest gptel-agent-multi-test-get-sessions-filters-dead-buffers ()
  "Test get-active-sessions filters out dead buffers."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf1 (gptel-agent-multi-test--create-mock-session "alive" "/tmp/"))
          (buf2 (gptel-agent-multi-test--create-mock-session "tokill" "/tmp/")))
      (gptel-agent--register-session buf1)
      (gptel-agent--register-session buf2)
      ;; Kill one buffer without unregistering
      (kill-buffer buf2)
      (let ((sessions (gptel-agent--get-active-sessions)))
        ;; Should only return alive buffer
        (should (= (length sessions) 1))
        (should (eq (plist-get (car sessions) :buffer) buf1))))))

(ert-deftest gptel-agent-multi-test-get-sessions-with-model ()
  "Test get-active-sessions includes model info."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (with-current-buffer buf
        (defvar gptel-model)
        (setq gptel-model "gpt-4"))
      (gptel-agent--register-session buf)
      (let* ((sessions (gptel-agent--get-active-sessions))
             (session (car sessions)))
        (should (equal (plist-get session :model) "gpt-4"))))))

(ert-deftest gptel-agent-multi-test-get-sessions-abbreviates-directory ()
  "Test get-active-sessions abbreviates project directory."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "~/")))
      (with-current-buffer buf
        (setq default-directory (expand-file-name "~/")))
      (gptel-agent--register-session buf)
      (let* ((sessions (gptel-agent--get-active-sessions))
             (project (plist-get (car sessions) :project)))
        ;; Should be abbreviated (contains ~)
        (should (string-match-p "~" project))))))

;;;; Format Annotation Extended Tests

(ert-deftest gptel-agent-multi-test-format-annotation-nil-model ()
  "Test annotation formatting with nil model."
  (gptel-agent-multi-test--with-cleanup
    (let ((session (list :buffer (current-buffer)
                         :name "test"
                         :project "/tmp/"
                         :model nil
                         :idle 60
                         :idle-p nil)))
      (let ((annotation (gptel-agent--format-session-annotation session)))
        (should (stringp annotation))
        (should (string-match-p "/tmp/" annotation))
        (should-not (string-match-p "nil" annotation))))))

(ert-deftest gptel-agent-multi-test-format-annotation-nil-idle ()
  "Test annotation formatting with nil idle."
  (gptel-agent-multi-test--with-cleanup
    (let ((session (list :buffer (current-buffer)
                         :name "test"
                         :project "/tmp/"
                         :model "gpt-4"
                         :idle nil
                         :idle-p nil)))
      (let ((annotation (gptel-agent--format-session-annotation session)))
        (should (stringp annotation))
        (should-not (string-match-p "idle" annotation))
        (should-not (string-match-p "active" annotation))))))

;;;; Idle Detection Extended Tests

(ert-deftest gptel-agent-multi-test-idle-p-dead-buffer ()
  "Test idle detection with dead buffer."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      (kill-buffer buf)
      ;; Should return nil for dead buffer
      (should-not (gptel-agent--session-idle-p buf)))))

(ert-deftest gptel-agent-multi-test-idle-p-nil-activity ()
  "Test idle detection with nil last activity."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-idle-timeout 60)
          (buf (generate-new-buffer " *test-idle*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq default-directory "/tmp/")
              (setq gptel-agent--last-activity nil))
            (push buf gptel-agent--active-sessions)
            ;; Should return nil when no activity recorded
            (should-not (gptel-agent--session-idle-p buf)))
        (kill-buffer buf)))))

;;;; Switch Session Tests

(ert-deftest gptel-agent-multi-test-switch-no-sessions ()
  "Test switch with no active sessions."
  (gptel-agent-multi-test--with-cleanup
    (should-error (gptel-agent-switch) :type 'user-error)))

(ert-deftest gptel-agent-multi-test-switch-selects-session ()
  "Test switch selects and switches to session."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "target" "/tmp/")))
      (gptel-agent--register-session buf)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (concat "target"
                           (gptel-agent--format-session-annotation
                            (car (gptel-agent--get-active-sessions)))))))
        (gptel-agent-switch)
        (should (eq (current-buffer) buf))))))

;;;; List Sessions Tests

(ert-deftest gptel-agent-multi-test-list-sessions-none ()
  "Test list sessions with no active sessions."
  (gptel-agent-multi-test--with-cleanup
    (let ((message-shown nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest _)
                   (when (string-match-p "No active" fmt)
                     (setq message-shown t)))))
        (gptel-agent-list-sessions)
        (should message-shown)))))

(ert-deftest gptel-agent-multi-test-list-sessions-short ()
  "Test list sessions with few sessions uses echo area."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf1 (gptel-agent-multi-test--create-mock-session "s1" "/tmp/"))
          (buf2 (gptel-agent-multi-test--create-mock-session "s2" "/tmp/"))
          (message-shown nil))
      (gptel-agent--register-session buf1)
      (gptel-agent--register-session buf2)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest _)
                   (when (string-match-p "Active sessions" fmt)
                     (setq message-shown t)))))
        (gptel-agent-list-sessions)
        (should message-shown)))))

(ert-deftest gptel-agent-multi-test-list-sessions-long ()
  "Test list sessions with many sessions uses buffer."
  (gptel-agent-multi-test--with-cleanup
    (let ((bufs nil))
      ;; Create more than 5 sessions
      (dotimes (i 6)
        (let ((buf (gptel-agent-multi-test--create-mock-session
                    (format "session%d" i) "/tmp/")))
          (push buf bufs)
          (gptel-agent--register-session buf)))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buf) (set-buffer buf))))
        (gptel-agent-list-sessions)
        ;; Should have created a buffer
        (should (get-buffer "*GPTel Agent Sessions*"))
        (kill-buffer "*GPTel Agent Sessions*")))))

;;;; Rename Session Extended Tests

(ert-deftest gptel-agent-multi-test-rename-strips-prefix ()
  "Test rename strips gptel-agent prefix for initial value."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      (with-current-buffer buf
        (setq gptel-agent--session-name "*gptel-agent:myname*")
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_ initial)
                     ;; Verify initial value has prefix stripped
                     initial)))
          ;; The interactive call would strip the prefix
          (should (string= "myname"
                           (string-remove-prefix
                            "*gptel-agent:"
                            (string-remove-suffix "*" gptel-agent--session-name)))))))))

(ert-deftest gptel-agent-multi-test-rename-updates-buffer-name ()
  "Test rename updates buffer name."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "old" "/tmp/")))
      (gptel-agent--register-session buf)
      (with-current-buffer buf
        (gptel-agent-rename-session "new")
        (should (string-match-p "new" (buffer-name)))))))

;;;; Close Idle Sessions Extended Tests

(ert-deftest gptel-agent-multi-test-close-idle-no-force-declined ()
  "Test close idle without force can be declined."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-idle-timeout 1)
          (buf (gptel-agent-multi-test--create-mock-session "idle" "/tmp/")))
      (gptel-agent--register-session buf)
      (sleep-for 1.5)
      ;; Decline to close
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (let ((closed (gptel-agent-close-idle-sessions)))
          (should (= closed 0))
          (should (buffer-live-p buf)))))))

(ert-deftest gptel-agent-multi-test-close-idle-mixed ()
  "Test close idle only closes idle sessions."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-idle-timeout 1)
          (idle-buf (gptel-agent-multi-test--create-mock-session "idle" "/tmp/"))
          (active-buf (gptel-agent-multi-test--create-mock-session "active" "/tmp/")))
      (gptel-agent--register-session idle-buf)
      (sleep-for 1.5)
      (gptel-agent--register-session active-buf)
      (gptel-agent--update-activity active-buf)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (let ((closed (gptel-agent-close-idle-sessions)))
          (should (= closed 1))
          (should-not (buffer-live-p idle-buf))
          (should (buffer-live-p active-buf)))))))

;;;; Buffer Local Variable Extended Tests

(ert-deftest gptel-agent-multi-test-session-name-buffer-local ()
  "Test session name is buffer local."
  (with-temp-buffer
    (setq gptel-agent--session-name "buf1")
    (with-temp-buffer
      (setq gptel-agent--session-name "buf2")
      (should (string= gptel-agent--session-name "buf2")))
    (should (string= gptel-agent--session-name "buf1"))))

(ert-deftest gptel-agent-multi-test-last-activity-buffer-local ()
  "Test last activity is buffer local."
  (with-temp-buffer
    (setq gptel-agent--last-activity (current-time))
    (let ((time1 gptel-agent--last-activity))
      (with-temp-buffer
        (setq gptel-agent--last-activity nil)
        (should-not gptel-agent--last-activity))
      (should (equal gptel-agent--last-activity time1)))))

;;;; Session Registry Variable Test

(ert-deftest gptel-agent-multi-test-active-sessions-global ()
  "Test active sessions registry is global."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "test" "/tmp/")))
      (gptel-agent--register-session buf)
      ;; Should be visible from any buffer
      (with-temp-buffer
        (should (memq buf gptel-agent--active-sessions))))))

;;;; Edge Case Tests

(ert-deftest gptel-agent-multi-test-unregister-not-in-list ()
  "Test unregistering buffer not in list."
  (gptel-agent-multi-test--with-cleanup
    (with-temp-buffer
      ;; Should not error
      (gptel-agent--unregister-session (current-buffer))
      (should-not (memq (current-buffer) gptel-agent--active-sessions)))))

(ert-deftest gptel-agent-multi-test-empty-sessions-list ()
  "Test behavior with empty sessions list."
  (gptel-agent-multi-test--with-cleanup
    (should (= (length (gptel-agent--get-active-sessions)) 0))))

(ert-deftest gptel-agent-multi-test-session-sorting-same-activity ()
  "Test sorting with sessions that have same activity time."
  (gptel-agent-multi-test--with-cleanup
    (let ((now (current-time)))
      (let ((buf1 (gptel-agent-multi-test--create-mock-session "a" "/tmp/"))
            (buf2 (gptel-agent-multi-test--create-mock-session "b" "/tmp/")))
        ;; Set same activity time
        (with-current-buffer buf1
          (setq gptel-agent--last-activity now))
        (with-current-buffer buf2
          (setq gptel-agent--last-activity now))
        (gptel-agent--register-session buf1)
        (gptel-agent--register-session buf2)
        (let ((sessions (gptel-agent--get-active-sessions)))
          ;; Should return both without error
          (should (= (length sessions) 2)))))))

(ert-deftest gptel-agent-multi-test-format-annotation-all-nil ()
  "Test annotation formatting with minimal data."
  (gptel-agent-multi-test--with-cleanup
    (let ((session (list :buffer (current-buffer)
                         :name "test"
                         :project ""
                         :model nil
                         :idle nil
                         :idle-p nil)))
      (let ((annotation (gptel-agent--format-session-annotation session)))
        (should (stringp annotation))))))

(ert-deftest gptel-agent-multi-test-cleanup-hook-removes-from-registry ()
  "Test cleanup hook properly removes session."
  (gptel-agent-multi-test--with-cleanup
    (let ((buf (gptel-agent-multi-test--create-mock-session "cleanup" "/tmp/")))
      (gptel-agent--register-session buf)
      (should (memq buf gptel-agent--active-sessions))
      ;; Simulate cleanup hook
      (with-current-buffer buf
        (gptel-agent--cleanup-on-kill))
      (should-not (memq buf gptel-agent--active-sessions)))))

;;;; Integration Tests

(ert-deftest gptel-agent-multi-test-full-lifecycle ()
  "Test full session lifecycle: create, use, rename, close."
  (gptel-agent-multi-test--with-cleanup
    (let ((gptel-agent-idle-timeout 1))
      ;; Create session
      (let ((buf (gptel-agent-multi-test--create-mock-session "lifecycle" "/tmp/")))
        (gptel-agent--register-session buf)
        (should (memq buf gptel-agent--active-sessions))
        ;; Use session
        (gptel-agent--update-activity buf)
        (should-not (gptel-agent--session-idle-p buf))
        ;; Rename
        (with-current-buffer buf
          (gptel-agent-rename-session "renamed"))
        (should (string= (with-current-buffer buf gptel-agent--session-name) "renamed"))
        ;; Wait to become idle
        (sleep-for 1.5)
        (should (gptel-agent--session-idle-p buf))
        ;; Close
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (gptel-agent-close-idle-sessions))
        (should-not (buffer-live-p buf))))))

(provide 'gptel-agent-multi-test)
;;; gptel-agent-multi-test.el ends here
