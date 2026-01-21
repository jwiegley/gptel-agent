;;; gptel-agent-stats-test.el --- Tests for gptel-agent-stats -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Keywords: tests
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Comprehensive tests for gptel-agent-stats.el token tracking and cost
;; calculation.
;;
;; Test coverage:
;; - Token estimation
;; - Cost calculation with model pricing
;; - Session statistics tracking
;; - Formatting helpers
;; - Budget monitoring
;; - Data export

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'gptel-agent-stats)

;;;; Test Helpers

(defun gptel-agent-stats-test--reset ()
  "Reset all statistics state."
  (setq gptel-agent--session-stats nil)
  (setq gptel-agent--cumulative-stats nil)
  (setq gptel-agent--budget-spent nil))

(defmacro gptel-agent-stats-test--with-clean-state (&rest body)
  "Execute BODY with clean statistics state."
  `(progn
     (gptel-agent-stats-test--reset)
     (unwind-protect
         (progn ,@body)
       (gptel-agent-stats-test--reset))))

;;;; Customization Tests

(ert-deftest gptel-agent-stats-test-model-pricing-default ()
  "Test default model pricing is defined."
  (should (listp gptel-agent-model-pricing))
  (should (> (length gptel-agent-model-pricing) 0))
  ;; Check some common models are included
  (should (assoc "claude-3-5-sonnet" gptel-agent-model-pricing))
  (should (assoc "gpt-4o" gptel-agent-model-pricing)))

(ert-deftest gptel-agent-stats-test-budget-period-default ()
  "Test default budget period."
  (should (eq (default-value 'gptel-agent-budget-period) 'session)))

(ert-deftest gptel-agent-stats-test-budget-action-default ()
  "Test default budget action."
  (should (eq (default-value 'gptel-agent-budget-action) 'warn)))

;;;; Token Estimation Tests

(ert-deftest gptel-agent-stats-test-estimate-tokens-string ()
  "Test token estimation for strings."
  (should (= (gptel-agent-stats--estimate-tokens "1234567890123456") 4))
  (should (= (gptel-agent-stats--estimate-tokens "") 0)))

(ert-deftest gptel-agent-stats-test-estimate-tokens-nil ()
  "Test token estimation for nil."
  (should (= (gptel-agent-stats--estimate-tokens nil) 0)))

(ert-deftest gptel-agent-stats-test-estimate-tokens-non-string ()
  "Test token estimation for non-string."
  (should (= (gptel-agent-stats--estimate-tokens 12345) 0))
  (should (= (gptel-agent-stats--estimate-tokens '(a b c)) 0)))

;;;; Cost Calculation Tests

(ert-deftest gptel-agent-stats-test-get-model-pricing-exact ()
  "Test pricing lookup for exact model match."
  (let ((pricing (gptel-agent-stats--get-model-pricing "gpt-4o")))
    (should pricing)
    (should (plist-get pricing :input))
    (should (plist-get pricing :output))))

(ert-deftest gptel-agent-stats-test-get-model-pricing-prefix ()
  "Test pricing lookup with prefix matching."
  (let ((pricing (gptel-agent-stats--get-model-pricing "claude-3-5-sonnet-20241022")))
    (should pricing)
    (should (= (plist-get pricing :input) 3.00))))

(ert-deftest gptel-agent-stats-test-get-model-pricing-unknown ()
  "Test pricing lookup for unknown model."
  (should-not (gptel-agent-stats--get-model-pricing "unknown-model-xyz")))

(ert-deftest gptel-agent-stats-test-calculate-cost ()
  "Test cost calculation."
  (let ((cost (gptel-agent-stats--calculate-cost 1000000 500000 "gpt-4o")))
    (should cost)
    ;; gpt-4o: $2.50/M input, $10.00/M output
    ;; 1M input = $2.50, 0.5M output = $5.00 -> total $7.50
    (should (= cost 7.50))))

(ert-deftest gptel-agent-stats-test-calculate-cost-small ()
  "Test cost calculation with small token counts."
  (let ((cost (gptel-agent-stats--calculate-cost 1000 500 "gpt-4o")))
    (should cost)
    ;; 1k input = $0.0025, 0.5k output = $0.005 -> total $0.0075
    (should (< (abs (- cost 0.0075)) 0.0001))))

(ert-deftest gptel-agent-stats-test-calculate-cost-unknown-model ()
  "Test cost calculation with unknown model returns nil."
  (should-not (gptel-agent-stats--calculate-cost 1000 500 "unknown-model")))

;;;; Formatting Tests

(ert-deftest gptel-agent-stats-test-format-tokens-small ()
  "Test token formatting for small numbers."
  (should (string= (gptel-agent-stats--format-tokens 500) "500"))
  (should (string= (gptel-agent-stats--format-tokens 999) "999")))

(ert-deftest gptel-agent-stats-test-format-tokens-thousands ()
  "Test token formatting for thousands."
  (should (string= (gptel-agent-stats--format-tokens 1500) "1.5k"))
  (should (string= (gptel-agent-stats--format-tokens 10000) "10.0k")))

(ert-deftest gptel-agent-stats-test-format-tokens-millions ()
  "Test token formatting for millions."
  (should (string= (gptel-agent-stats--format-tokens 1500000) "1.5M"))
  (should (string= (gptel-agent-stats--format-tokens 10000000) "10.0M")))

(ert-deftest gptel-agent-stats-test-format-cost-small ()
  "Test cost formatting for small amounts."
  (should (string= (gptel-agent-stats--format-cost 0.005) "$0.00"))
  (should (string= (gptel-agent-stats--format-cost 0.01) "$0.01")))

(ert-deftest gptel-agent-stats-test-format-cost-normal ()
  "Test cost formatting for normal amounts."
  (should (string= (gptel-agent-stats--format-cost 1.23) "$1.23"))
  (should (string= (gptel-agent-stats--format-cost 10.5) "$10.50")))

;;;; Session Statistics Tests

(ert-deftest gptel-agent-stats-test-init-session ()
  "Test session initialization."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (should gptel-agent--session-stats)
    (should (= (plist-get gptel-agent--session-stats :input-tokens) 0))
    (should (= (plist-get gptel-agent--session-stats :output-tokens) 0))
    (should (= (plist-get gptel-agent--session-stats :session-cost) 0.0))
    (should (null (plist-get gptel-agent--session-stats :responses)))
    (should (plist-get gptel-agent--session-stats :start-time))))

(ert-deftest gptel-agent-stats-test-track-response ()
  "Test response tracking."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (let ((gptel-model "gpt-4o"))
      (gptel-agent-stats--track-response
       '(:prompt "Hello, how are you?"
         :response "I'm doing well, thank you for asking! How can I help you?")))
    (should (> (plist-get gptel-agent--session-stats :input-tokens) 0))
    (should (> (plist-get gptel-agent--session-stats :output-tokens) 0))
    (should (= (length (plist-get gptel-agent--session-stats :responses)) 1))))

(ert-deftest gptel-agent-stats-test-cumulative-update ()
  "Test cumulative statistics update."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--update-cumulative 100 200 0.05)
    (should gptel-agent--cumulative-stats)
    (should (= (plist-get gptel-agent--cumulative-stats :total-input-tokens) 100))
    (should (= (plist-get gptel-agent--cumulative-stats :total-output-tokens) 200))
    (should (= (plist-get gptel-agent--cumulative-stats :total-cost) 0.05))
    (should (= (plist-get gptel-agent--cumulative-stats :total-responses) 1))))

(ert-deftest gptel-agent-stats-test-cumulative-accumulates ()
  "Test cumulative statistics accumulate across calls."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--update-cumulative 100 200 0.05)
    (gptel-agent-stats--update-cumulative 150 250 0.07)
    (should (= (plist-get gptel-agent--cumulative-stats :total-input-tokens) 250))
    (should (= (plist-get gptel-agent--cumulative-stats :total-output-tokens) 450))
    (should (< (abs (- (plist-get gptel-agent--cumulative-stats :total-cost) 0.12)) 0.001))
    (should (= (plist-get gptel-agent--cumulative-stats :total-responses) 2))))

;;;; Header Line Tests

(ert-deftest gptel-agent-stats-test-header-string-no-session ()
  "Test header string with no session."
  (gptel-agent-stats-test--with-clean-state
    (should (string= (gptel-agent-stats--header-string) ""))))

(ert-deftest gptel-agent-stats-test-header-string-with-session ()
  "Test header string with active session."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (plist-put gptel-agent--session-stats :input-tokens 1500)
    (plist-put gptel-agent--session-stats :output-tokens 3500)
    (plist-put gptel-agent--session-stats :session-cost 0.02)
    (let ((str (gptel-agent-stats--header-string)))
      (should (stringp str))
      (should (string-match-p "1\\.5k" str))
      (should (string-match-p "3\\.5k" str))
      (should (string-match-p "\\$0\\.02" str)))))

(ert-deftest gptel-agent-stats-test-header-string-disabled ()
  "Test header string when display disabled."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (let ((gptel-agent-stats-show-in-header nil))
      (should (string= (gptel-agent-stats--header-string) "")))))

;;;; Budget Tests

(ert-deftest gptel-agent-stats-test-init-budget-session ()
  "Test session budget initialization."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-period 'session))
      (gptel-agent-stats--init-budget-period)
      (should gptel-agent--budget-spent)
      (should (= (plist-get gptel-agent--budget-spent :amount) 0.0))
      (should (eq (plist-get gptel-agent--budget-spent :period) 'session)))))

(ert-deftest gptel-agent-stats-test-budget-period-expired-session ()
  "Test session budget never expires."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-period 'session))
      (gptel-agent-stats--init-budget-period)
      ;; Session budgets never expire on their own
      (should-not (gptel-agent-stats--budget-period-expired-p)))))

(ert-deftest gptel-agent-stats-test-budget-check-under-limit ()
  "Test budget check under limit."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 10.00)
          (gptel-agent-budget-action 'warn))
      (gptel-agent-stats--init-budget-period)
      ;; Should not error
      (gptel-agent-stats--check-budget 1.00)
      (should (= (plist-get gptel-agent--budget-spent :amount) 1.00)))))

(ert-deftest gptel-agent-stats-test-budget-check-accumulates ()
  "Test budget checks accumulate spending."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 10.00)
          (gptel-agent-budget-action 'warn))
      (gptel-agent-stats--init-budget-period)
      (gptel-agent-stats--check-budget 1.00)
      (gptel-agent-stats--check-budget 2.00)
      (should (= (plist-get gptel-agent--budget-spent :amount) 3.00)))))

(ert-deftest gptel-agent-stats-test-budget-stop-action ()
  "Test budget stop action."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 1.00)
          (gptel-agent-budget-action 'stop))
      (gptel-agent-stats--init-budget-period)
      (should-error (gptel-agent-stats--check-budget 1.50)))))

;;;; Export Tests

(ert-deftest gptel-agent-stats-test-export-csv-format ()
  "Test CSV export format."
  (let* ((now (current-time))
         (responses (list (list :timestamp now
                               :model "gpt-4o"
                               :input-tokens 100
                               :output-tokens 200
                               :cost 0.05)))
         (csv (gptel-agent-stats--export-csv responses)))
    (should (stringp csv))
    (should (string-match-p "timestamp,model,input_tokens,output_tokens,cost" csv))
    (should (string-match-p "gpt-4o" csv))
    (should (string-match-p "100" csv))
    (should (string-match-p "200" csv))))

(ert-deftest gptel-agent-stats-test-export-json-format ()
  "Test JSON export format."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (plist-put gptel-agent--session-stats :input-tokens 100)
    (plist-put gptel-agent--session-stats :output-tokens 200)
    (plist-put gptel-agent--session-stats :session-cost 0.05)
    (let ((json (gptel-agent-stats--export-json gptel-agent--session-stats)))
      (should (stringp json))
      (should (string-match-p "session" json))
      (should (string-match-p "responses" json)))))

;;;; Minor Mode Tests

(ert-deftest gptel-agent-stats-test-minor-mode-defined ()
  "Test minor mode is defined."
  (should (fboundp 'gptel-agent-stats-mode)))

(ert-deftest gptel-agent-stats-test-minor-mode-enable ()
  "Test minor mode enable."
  (with-temp-buffer
    (gptel-agent-stats-test--reset)
    (gptel-agent-stats-mode 1)
    (should gptel-agent-stats-mode)
    (should gptel-agent--session-stats)
    (gptel-agent-stats-mode -1)))

(ert-deftest gptel-agent-stats-test-minor-mode-disable ()
  "Test minor mode disable."
  (with-temp-buffer
    (gptel-agent-stats-test--reset)
    (gptel-agent-stats-mode 1)
    (gptel-agent-stats-mode -1)
    (should-not gptel-agent-stats-mode)))

;;;; Interactive Command Tests

(ert-deftest gptel-agent-stats-test-stats-command ()
  "Test gptel-agent-stats command."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (gptel-agent-stats)
    (should (get-buffer "*GPTel Agent Stats*"))
    (kill-buffer "*GPTel Agent Stats*")))

(ert-deftest gptel-agent-stats-test-reset-command ()
  "Test reset command."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (plist-put gptel-agent--session-stats :input-tokens 1000)
    (gptel-agent-reset-session-stats)
    (should (= (plist-get gptel-agent--session-stats :input-tokens) 0))))

;;;; Utility Function Tests

(ert-deftest gptel-agent-stats-test-stats-current ()
  "Test stats-current function."
  (gptel-agent-stats-test--with-clean-state
    (should-not (gptel-agent-stats-current))
    (gptel-agent-stats--init-session)
    (should (gptel-agent-stats-current))))

(ert-deftest gptel-agent-stats-test-stats-cumulative ()
  "Test stats-cumulative function."
  (gptel-agent-stats-test--with-clean-state
    (should-not (gptel-agent-stats-cumulative))
    (gptel-agent-stats--update-cumulative 100 200 0.05)
    (should (gptel-agent-stats-cumulative))))

;;;; Face Tests

(ert-deftest gptel-agent-stats-test-faces-defined ()
  "Test faces are defined."
  (should (facep 'gptel-agent-stats-input-face))
  (should (facep 'gptel-agent-stats-output-face))
  (should (facep 'gptel-agent-stats-cost-face))
  (should (facep 'gptel-agent-stats-warning-face)))

;;;; Time Period Helpers Tests

(ert-deftest gptel-agent-stats-test-day-start ()
  "Test day start calculation."
  ;; Create a time that's mid-day
  (let* ((test-time (encode-time (list 30 15 14 10 6 2025)))  ; 2025-06-10 14:15:30
         (day-start (gptel-agent-stats--day-start test-time))
         (decoded (decode-time day-start)))
    ;; Should be midnight of same day
    (should (= (nth 0 decoded) 0))   ; seconds
    (should (= (nth 1 decoded) 0))   ; minutes
    (should (= (nth 2 decoded) 0))   ; hours
    (should (= (nth 3 decoded) 10))  ; day
    (should (= (nth 4 decoded) 6))   ; month
    (should (= (nth 5 decoded) 2025)))) ; year

(ert-deftest gptel-agent-stats-test-month-start ()
  "Test month start calculation."
  ;; Create a time that's mid-month
  (let* ((test-time (encode-time (list 30 15 14 15 6 2025)))  ; 2025-06-15 14:15:30
         (month-start (gptel-agent-stats--month-start test-time))
         (decoded (decode-time month-start)))
    ;; Should be first of month at midnight
    (should (= (nth 0 decoded) 0))   ; seconds
    (should (= (nth 1 decoded) 0))   ; minutes
    (should (= (nth 2 decoded) 0))   ; hours
    (should (= (nth 3 decoded) 1))   ; day (first)
    (should (= (nth 4 decoded) 6))   ; month
    (should (= (nth 5 decoded) 2025)))) ; year

;;;; Budget Period Expiration Tests

(ert-deftest gptel-agent-stats-test-budget-period-expired-daily ()
  "Test daily budget period expiration."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-period 'daily))
      (gptel-agent-stats--init-budget-period)
      ;; Same day - not expired
      (should-not (gptel-agent-stats--budget-period-expired-p))
      ;; Set start to yesterday
      (let ((yesterday (time-subtract (current-time) (* 24 60 60))))
        (plist-put gptel-agent--budget-spent :start yesterday)
        ;; Should be expired
        (should (gptel-agent-stats--budget-period-expired-p))))))

(ert-deftest gptel-agent-stats-test-budget-period-expired-monthly ()
  "Test monthly budget period expiration."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-period 'monthly))
      (gptel-agent-stats--init-budget-period)
      ;; Same month - not expired
      (should-not (gptel-agent-stats--budget-period-expired-p))
      ;; Set start to last month (subtract 32 days to be safe)
      (let ((last-month (time-subtract (current-time) (* 32 24 60 60))))
        (plist-put gptel-agent--budget-spent :start last-month)
        ;; Should be expired (unless we're in early January)
        (let ((decoded (decode-time (current-time))))
          (when (> (nth 3 decoded) 1)  ; Skip test on 1st of month
            (should (gptel-agent-stats--budget-period-expired-p))))))))

(ert-deftest gptel-agent-stats-test-budget-period-expired-nil ()
  "Test budget period expiration when no budget set."
  (gptel-agent-stats-test--with-clean-state
    (should-not (gptel-agent-stats--budget-period-expired-p))))

;;;; Budget Check Edge Cases

(ert-deftest gptel-agent-stats-test-budget-check-80-percent ()
  "Test budget warning at 80% threshold."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 10.00)
          (gptel-agent-budget-action 'warn)
          (message-called nil))
      (gptel-agent-stats--init-budget-period)
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (gptel-agent-stats--check-budget 8.00)  ; 80%
        (should message-called)))))

(ert-deftest gptel-agent-stats-test-budget-check-90-percent ()
  "Test budget warning at 90% threshold."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 10.00)
          (gptel-agent-budget-action 'warn)
          (message-called nil))
      (gptel-agent-stats--init-budget-period)
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (gptel-agent-stats--check-budget 9.00)  ; 90%
        (should message-called)))))

(ert-deftest gptel-agent-stats-test-budget-check-warn-exceeded ()
  "Test budget warning when exceeded."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 10.00)
          (gptel-agent-budget-action 'warn)
          (message-called nil))
      (gptel-agent-stats--init-budget-period)
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) (setq message-called t))))
        (gptel-agent-stats--check-budget 15.00)  ; 150%
        (should message-called)))))

(ert-deftest gptel-agent-stats-test-budget-check-confirm-accepted ()
  "Test budget confirm action when accepted."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 10.00)
          (gptel-agent-budget-action 'confirm))
      (gptel-agent-stats--init-budget-period)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) t)))  ; Accept
        ;; Should not error
        (gptel-agent-stats--check-budget 15.00)
        (should (= (plist-get gptel-agent--budget-spent :amount) 15.00))))))

(ert-deftest gptel-agent-stats-test-budget-check-confirm-rejected ()
  "Test budget confirm action when rejected."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 10.00)
          (gptel-agent-budget-action 'confirm))
      (gptel-agent-stats--init-budget-period)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) nil)))  ; Reject
        (should-error (gptel-agent-stats--check-budget 15.00)
                      :type 'user-error)))))

(ert-deftest gptel-agent-stats-test-budget-check-no-limit ()
  "Test budget check when no limit set."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit nil))
      ;; Should not error
      (gptel-agent-stats--check-budget 1000.00))))

(ert-deftest gptel-agent-stats-test-budget-resets-on-period-expiry ()
  "Test budget resets when period expires."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 10.00)
          (gptel-agent-budget-period 'daily)
          (gptel-agent-budget-action 'warn))
      (gptel-agent-stats--init-budget-period)
      (plist-put gptel-agent--budget-spent :amount 5.00)
      ;; Set start to yesterday
      (let ((yesterday (time-subtract (current-time) (* 24 60 60))))
        (plist-put gptel-agent--budget-spent :start yesterday))
      ;; Should reinitialize with new check
      (gptel-agent-stats--check-budget 1.00)
      ;; Amount should be 1.00, not 6.00
      (should (= (plist-get gptel-agent--budget-spent :amount) 1.00)))))

;;;; Session Persistence Tests

(ert-deftest gptel-agent-stats-test-save-with-session ()
  "Test statistics save for session persistence."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-stats-persist t))
      (gptel-agent-stats--init-session)
      (plist-put gptel-agent--session-stats :input-tokens 100)
      (plist-put gptel-agent--session-stats :output-tokens 200)
      (plist-put gptel-agent--session-stats :session-cost 0.05)
      (push '(:timestamp nil :input-tokens 50 :output-tokens 75 :cost 0.02)
            (plist-get gptel-agent--session-stats :responses))
      (let ((saved (gptel-agent-stats--save-with-session)))
        (should saved)
        (should (= (plist-get saved :input-tokens) 100))
        (should (= (plist-get saved :output-tokens) 200))
        (should (= (plist-get saved :session-cost) 0.05))
        (should (= (plist-get saved :response-count) 1))))))

(ert-deftest gptel-agent-stats-test-save-with-session-persist-disabled ()
  "Test statistics save when persistence disabled."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-stats-persist nil))
      (gptel-agent-stats--init-session)
      (should (null (gptel-agent-stats--save-with-session))))))

(ert-deftest gptel-agent-stats-test-save-with-session-no-stats ()
  "Test statistics save with no stats."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-stats-persist t))
      (should (null (gptel-agent-stats--save-with-session))))))

(ert-deftest gptel-agent-stats-test-load-from-session ()
  "Test loading statistics from session."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-stats-persist t)
          (saved-stats (list :input-tokens 500
                            :output-tokens 1000
                            :session-cost 0.25)))
      (gptel-agent-stats--load-from-session saved-stats)
      (should gptel-agent--session-stats)
      (should (= (plist-get gptel-agent--session-stats :input-tokens) 500))
      (should (= (plist-get gptel-agent--session-stats :output-tokens) 1000))
      (should (= (plist-get gptel-agent--session-stats :session-cost) 0.25)))))

(ert-deftest gptel-agent-stats-test-load-from-session-persist-disabled ()
  "Test loading statistics when persistence disabled."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-stats-persist nil)
          (saved-stats (list :input-tokens 500)))
      (gptel-agent-stats--load-from-session saved-stats)
      (should-not gptel-agent--session-stats))))

(ert-deftest gptel-agent-stats-test-load-from-session-nil-stats ()
  "Test loading nil statistics."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-stats-persist t))
      (gptel-agent-stats--load-from-session nil)
      (should-not gptel-agent--session-stats))))

;;;; Header String Edge Cases

(ert-deftest gptel-agent-stats-test-header-string-nil-cost ()
  "Test header string with nil cost."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (plist-put gptel-agent--session-stats :input-tokens 1000)
    (plist-put gptel-agent--session-stats :output-tokens 2000)
    (plist-put gptel-agent--session-stats :session-cost nil)
    (let ((str (gptel-agent-stats--header-string)))
      (should (stringp str))
      (should (string-match-p "1\\.0k" str))
      (should (string-match-p "2\\.0k" str))
      ;; Cost portion should not be included
      (should-not (string-match-p "\\$" str)))))

(ert-deftest gptel-agent-stats-test-header-string-zero-tokens ()
  "Test header string with zero tokens."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (let ((str (gptel-agent-stats--header-string)))
      (should (stringp str))
      (should (string-match-p "\\^0" str))
      (should (string-match-p "v0" str)))))

;;;; Model Pricing Edge Cases

(ert-deftest gptel-agent-stats-test-get-model-pricing-symbol ()
  "Test pricing lookup with symbol model name."
  (let ((pricing (gptel-agent-stats--get-model-pricing 'gpt-4o)))
    (should pricing)
    (should (plist-get pricing :input))))

(ert-deftest gptel-agent-stats-test-get-model-pricing-nil ()
  "Test pricing lookup with nil model."
  (should-not (gptel-agent-stats--get-model-pricing nil)))

(ert-deftest gptel-agent-stats-test-calculate-cost-nil-pricing ()
  "Test cost calculation when model not found."
  (should (null (gptel-agent-stats--calculate-cost 1000 500 "totally-unknown-model"))))

;;;; Track Response Edge Cases

(ert-deftest gptel-agent-stats-test-track-response-no-session ()
  "Test track response with no session stats."
  (gptel-agent-stats-test--with-clean-state
    ;; Should not error when session stats is nil
    (gptel-agent-stats--track-response
     '(:prompt "test" :response "response"))
    (should-not gptel-agent--session-stats)))

(ert-deftest gptel-agent-stats-test-track-response-nil-content ()
  "Test track response with nil content."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (let ((gptel-model "gpt-4o"))
      (gptel-agent-stats--track-response
       '(:prompt nil :response nil)))
    (should (= (plist-get gptel-agent--session-stats :input-tokens) 0))
    (should (= (plist-get gptel-agent--session-stats :output-tokens) 0))))

(ert-deftest gptel-agent-stats-test-track-response-with-model-in-info ()
  "Test track response with model in info plist."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (gptel-agent-stats--track-response
     '(:prompt "test" :response "response" :model "claude-3-5-sonnet"))
    (let ((responses (plist-get gptel-agent--session-stats :responses)))
      (should (= (length responses) 1))
      (should (string= (plist-get (car responses) :model) "claude-3-5-sonnet")))))

;;;; Export Edge Cases

(ert-deftest gptel-agent-stats-test-export-csv-empty ()
  "Test CSV export with no responses."
  (let ((csv (gptel-agent-stats--export-csv nil)))
    (should (stringp csv))
    ;; Should still have header
    (should (string-match-p "timestamp,model" csv))))

(ert-deftest gptel-agent-stats-test-export-csv-nil-model ()
  "Test CSV export with nil model."
  (let* ((now (current-time))
         (responses (list (list :timestamp now
                               :model nil
                               :input-tokens 100
                               :output-tokens 200
                               :cost 0.05)))
         (csv (gptel-agent-stats--export-csv responses)))
    (should (stringp csv))
    ;; Should handle nil model gracefully
    (should (string-match-p "100" csv))))

(ert-deftest gptel-agent-stats-test-export-json-nil-session ()
  "Test JSON export with nil values."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (plist-put gptel-agent--session-stats :model nil)
    (let ((json (gptel-agent-stats--export-json gptel-agent--session-stats)))
      (should (stringp json)))))

;;;; Buffer Local Variable Tests

(ert-deftest gptel-agent-stats-test-session-stats-buffer-local ()
  "Test session stats is buffer-local."
  (with-temp-buffer
    (gptel-agent-stats--init-session)
    (plist-put gptel-agent--session-stats :input-tokens 500)
    (with-temp-buffer
      (should (null gptel-agent--session-stats)))
    (should (= (plist-get gptel-agent--session-stats :input-tokens) 500))))

;;;; Customization Group Tests

(ert-deftest gptel-agent-stats-test-custom-group-exists ()
  "Test customization group is defined."
  (should (get 'gptel-agent-stats 'custom-group)))

(ert-deftest gptel-agent-stats-test-show-in-header-default ()
  "Test default value for show-in-header."
  (should (default-value 'gptel-agent-stats-show-in-header)))

(ert-deftest gptel-agent-stats-test-persist-default ()
  "Test default value for persist."
  (should (default-value 'gptel-agent-stats-persist)))

(ert-deftest gptel-agent-stats-test-budget-limit-default ()
  "Test default value for budget-limit."
  (should (null (default-value 'gptel-agent-budget-limit))))

;;;; Minor Mode Hook Tests

(ert-deftest gptel-agent-stats-test-mode-adds-hook ()
  "Test minor mode adds response hook."
  (with-temp-buffer
    (gptel-agent-stats-test--reset)
    (gptel-agent-stats-mode 1)
    (should (memq #'gptel-agent-stats--track-response
                  gptel-post-response-functions))
    (gptel-agent-stats-mode -1)))

(ert-deftest gptel-agent-stats-test-mode-removes-hook ()
  "Test minor mode removes response hook."
  (with-temp-buffer
    (gptel-agent-stats-test--reset)
    (gptel-agent-stats-mode 1)
    (gptel-agent-stats-mode -1)
    (should-not (memq #'gptel-agent-stats--track-response
                      gptel-post-response-functions))))

;;;; Formatting Edge Cases

(ert-deftest gptel-agent-stats-test-format-tokens-zero ()
  "Test token formatting for zero."
  (should (string= (gptel-agent-stats--format-tokens 0) "0")))

(ert-deftest gptel-agent-stats-test-format-tokens-boundary-999 ()
  "Test token formatting at k boundary."
  (should (string= (gptel-agent-stats--format-tokens 1000) "1.0k")))

(ert-deftest gptel-agent-stats-test-format-tokens-boundary-999999 ()
  "Test token formatting at M boundary."
  (should (string= (gptel-agent-stats--format-tokens 1000000) "1.0M")))

(ert-deftest gptel-agent-stats-test-format-cost-zero ()
  "Test cost formatting for zero."
  (should (string= (gptel-agent-stats--format-cost 0.0) "$0.00")))

(ert-deftest gptel-agent-stats-test-format-cost-large ()
  "Test cost formatting for large amounts."
  (should (string= (gptel-agent-stats--format-cost 123.456) "$123.46")))

;;;; Stats Display Tests

(ert-deftest gptel-agent-stats-test-stats-display-no-session ()
  "Test stats display with no session."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats)
    (with-current-buffer "*GPTel Agent Stats*"
      (should (string-match-p "No active session" (buffer-string))))
    (kill-buffer "*GPTel Agent Stats*")))

(ert-deftest gptel-agent-stats-test-stats-display-no-cumulative ()
  "Test stats display with no cumulative data."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (gptel-agent-stats)
    (with-current-buffer "*GPTel Agent Stats*"
      (should (string-match-p "No cumulative data" (buffer-string))))
    (kill-buffer "*GPTel Agent Stats*")))

(ert-deftest gptel-agent-stats-test-stats-display-with-budget ()
  "Test stats display with budget set."
  (gptel-agent-stats-test--with-clean-state
    (let ((gptel-agent-budget-limit 10.00))
      (gptel-agent-stats--init-session)
      (gptel-agent-stats--init-budget-period)
      (gptel-agent-stats)
      (with-current-buffer "*GPTel Agent Stats*"
        (should (string-match-p "Budget Status" (buffer-string))))
      (kill-buffer "*GPTel Agent Stats*"))))

(ert-deftest gptel-agent-stats-test-stats-display-response-breakdown ()
  "Test stats display with response breakdown."
  (gptel-agent-stats-test--with-clean-state
    (gptel-agent-stats--init-session)
    (push (list :timestamp (current-time)
               :input-tokens 100
               :output-tokens 200
               :cost 0.05
               :model "gpt-4o")
          (plist-get gptel-agent--session-stats :responses))
    (gptel-agent-stats)
    (with-current-buffer "*GPTel Agent Stats*"
      (should (string-match-p "Response Breakdown" (buffer-string))))
    (kill-buffer "*GPTel Agent Stats*")))

(provide 'gptel-agent-stats-test)
;;; gptel-agent-stats-test.el ends here
