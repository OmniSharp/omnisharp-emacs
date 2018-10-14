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

(defun omnisharp-unit-test-buffer ()
  "Runs all test cases defined in the current buffer."
  (interactive)
  (omnisharp--current-buffer-project
    (lambda (project-framework)
      (omnisharp--current-file-members-as-tree
        (lambda (file-members)
          (if
            (or
              (equal nil file-members)
              (equal 0 (length file-members)))
            (omnisharp--message "omnisharp: No Test Methods to run")
            (let* (
                    (raw-test-type (alist-get 'Name (-first-item file-members)))
                    (test-method-names (-map (lambda (d) (alist-get 'Data d)) file-members))
                    (test-type (cdr (assoc raw-test-type '(
                                                       ("XunitTestMethod" . "xunit")
                                                       ("MSTestMethod" . "mstest")
                                                       ("NUnitTestMethod" . "nunit")
                                                       ))))
                    (request-message (-concat
                                       (omnisharp--get-request-object)
                                       `((TestFrameworkName . ,test-type)
                                          (TargetFrameworkVersion . ,project-framework)
                                          (MethodNames . ,(vconcat test-method-names))
                                          )
                                     )
                      ))
              (omnisharp--unit-test-reset-test-results-buffer t)
              (omnisharp--register-server-event-handler "TestMessage" 'omnisharp--handle-test-message-event)
              (omnisharp--send-command-to-server "/v2/runtestsinclass"
                request-message
                (lambda (resp)
                  (omnisharp--unregister-server-event-handler "TestMessage")
                  (-let (((&alist 'Results results
                                  'Pass passed) resp))
                    (omnisharp--unit-test-emit-results passed results))
                  ))
              )
            ))))))

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
          buffer)
        )
      )
    )

  (if present-buffer
      (display-buffer omnisharp--unit-test-results-buffer-name)))

(defun omnisharp--get-class-declarations-from-response (response)
  (-filter (lambda (x)
             (equal "ClassDeclaration" (alist-get 'Kind x)))
           (omnisharp--vector-to-list (alist-get 'TopLevelTypeDefinitions response))))

(defun omnisharp--get-test-info-for-class (classes)
  (omnisharp--vector-to-list
    (-mapcat
      (lambda (x3) (omnisharp--vector-to-list (alist-get 'Features x3)))
      (-filter 'omnisharp--is-test-method?
        (-mapcat
          (lambda (x1) (omnisharp--vector-to-list (alist-get 'ChildNodes x1)))
          classes)
        )))
  )

(defun omnisharp--is-test-method? (node)
  (and (equal "MethodDeclaration" (alist-get 'Kind node))
    (< 0 (length (alist-get 'Features node))))
  )

(defun omnisharp--current-file-members-as-tree (callback)
  (omnisharp--send-command-to-server-sync
    "/currentfilemembersastree"
    (omnisharp--get-request-object)
    (-lambda (response)
      (funcall callback (omnisharp--get-test-info-for-class
                          (omnisharp--get-class-declarations-from-response
                            response))))))

(defun omnisharp--current-buffer-project (callback)
  (omnisharp--send-command-to-server
    "/project"
    (omnisharp--get-request-object)
    (lambda (response)
      (funcall callback (omnisharp--get-project-framework response)
        ))))

(defun omnisharp--get-project-framework (response)
  (alist-get 'TargetFramework (alist-get 'MsBuildProject response))
  )


(provide 'omnisharp-unit-test-actions)
