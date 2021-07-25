;; -*- lexical-binding: t -*-

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'dash)

(defun omnisharp-unit-test-at-point ()
  "Runs test case under point, if any."
  (interactive)
  (omnisharp--cs-element-stack-at-point
   (lambda (stack)
     (let* ((element-on-point (car (last stack)))
            (test-method (omnisharp--cs-unit-test-method-p element-on-point))
            (test-method-name (car test-method))
            (test-method-framework (car (cdr test-method))))
       (omnisharp--unit-test-start test-method-framework (list test-method-name))))))

(defun omnisharp-unit-test-buffer ()
  "Runs all test cases defined in the current buffer."
  (interactive)
  (omnisharp--cs-inspect-buffer
   (lambda (elements)
     (let* ((test-methods (omnisharp--cs-filter-resursively
                            'omnisharp--cs-unit-test-method-p
                            elements))
            (test-method-framework (car (cdr (omnisharp--cs-unit-test-method-p (car test-methods)))))
            (test-method-names (mapcar (lambda (method)
                                         (car (omnisharp--cs-unit-test-method-p method)))
                                       test-methods)))
       (omnisharp--unit-test-start test-method-framework test-method-names)))))

(defun omnisharp-unit-test-last ()
  "Re-runs the last unit test run (if any)."
  (interactive)
  (let ((last-unit-test (cdr (assoc :last-unit-test omnisharp--server-info))))
    (apply 'omnisharp--unit-test-start (or last-unit-test (list nil nil)))))

(defun omnisharp--unit-test-start (test-method-framework test-method-names)
  "Runs tests specified by test method name"
  (if (and test-method-framework test-method-names)
      (let ((request-message (-concat
                              (omnisharp--get-request-object)
                              `((TestFrameworkName . ,test-method-framework)
                                (MethodNames . ,test-method-names)))))
        (setcdr (assoc :last-unit-test omnisharp--server-info)
                (list test-method-framework test-method-names))
        (omnisharp--unit-test-reset-test-results-buffer t)
        (omnisharp--send-command-to-server
         "/v2/runtestsinclass"
         request-message
         (-lambda ((&alist 'Results results 'Pass passed))
           (omnisharp--unit-test-emit-results passed results))))
    (omnisharp--message "omnisharp: No Test Methods to run")))

(defun omnisharp--unit-test-emit-results (passed results)
  "Emits unit test results as returned by the server to the unit test result buffer.
PASSED is t if all of the results have passed. RESULTS is a vector of status data for
each of the unit tests ran."
  ; we want to clean output buffer for result if things have passed otherwise
  ; compilation & test run output is to be cleared and results shown only for brevity

  (omnisharp--unit-test-message "")

  (seq-doseq (result results)
    (-let* (((&alist 'MethodName method-name
                     'Outcome outcome
                     'ErrorMessage error-message
                     'ErrorStackTrace error-stack-trace
                     'StandardOutput stdout
                     'StanderError stderr) result)
            (outcome-is-passed (string-equal "passed" outcome)))

      (omnisharp--unit-test-message
       (format "[%s] %s "
               (propertize
                (upcase outcome)
                'font-lock-face (if outcome-is-passed
                                    '(:foreground "green" :weight bold)
                                  '(:foreground "red" :weight bold)))
               (omnisharp--truncate-symbol-name method-name 76)))

      (unless outcome-is-passed
        (omnisharp--unit-test-message error-message)

        (if error-stack-trace
            (omnisharp--unit-test-message error-stack-trace))

        (unless (= (seq-length stdout) 0)
            (omnisharp--unit-test-message "Standard output:")
            (seq-doseq (stdout-line stdout)
              (omnisharp--unit-test-message stdout-line)))

        (unless (= (seq-length stderr) 0)
            (omnisharp--unit-test-message "Standard error:")
            (seq-doseq (stderr-line stderr)
              (omnisharp--unit-test-message stderr-line)))
        )))

  (omnisharp--unit-test-message "")

  (if (eq passed :json-false)
      (omnisharp--unit-test-message
       (propertize "*** UNIT TEST RUN HAS FAILED ***"
                   'font-lock-face '(:foreground "red" :weight bold)))
    (omnisharp--unit-test-message
     (propertize "*** UNIT TEST RUN HAS SUCCEEDED ***"
                 'font-lock-face '(:foreground "green" :weight bold)))
    )
  nil)

(defun omnisharp--unit-test-message (message)
  (let ((existing-buffer (get-buffer omnisharp--unit-test-results-buffer-name)))
    (if existing-buffer
        (with-current-buffer existing-buffer
          (setq buffer-read-only nil)
          (goto-char (point-max))
          (insert message)
          (insert "\n")
          (setq buffer-read-only t)))))

(defun omnisharp--handle-test-message-event (message)
  "This is hooked into omnisharp 'TestMessage event and when handling an
event will emit any test action output to unit test output buffer."
  (-let* (
           ((&alist 'Body body) message)
           ((&alist 'Message log-message) body))
    (omnisharp--unit-test-message log-message)))

(defun omnisharp--unit-test-reset-test-results-buffer (present-buffer)
  "Creates new or reuses existing unit test result output buffer."
  (let ((existing-buffer (get-buffer omnisharp--unit-test-results-buffer-name))
        (solution-root-dir (cdr (assoc :project-root omnisharp--server-info))))
    (if existing-buffer
        (progn
          (with-current-buffer existing-buffer
            (setq buffer-read-only nil)
            (erase-buffer)
            (setq buffer-read-only t)
            (setq default-directory solution-root-dir))
          existing-buffer)
      (let ((buffer (get-buffer-create omnisharp--unit-test-results-buffer-name)))
        (with-current-buffer buffer
          (setq default-directory solution-root-dir)
          (compilation-mode)
          buffer))))

  (if present-buffer
      (display-buffer omnisharp--unit-test-results-buffer-name)))

(provide 'omnisharp-unit-test-actions)
