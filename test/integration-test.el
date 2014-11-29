;; You can run tests with M-x ert but remember to evaluate them before
;; running if you changed something!

;; For these tests, an OmniSharpServer process needs to be running.


;; Test helpers
(defmacro with-working-server-configuration (code)
  "Run some code with the current configuration set to some
sensible default values."
  `(let ((omnisharp-host "http://localhost:2000/"))
     ,code))

(defmacro with-broken-server-configuration (code)
  "Run some code with the current configuration set to a
purposefully invalid configuration to test connection errors."
  `(let ((omnisharp-host "http://localhost:22/"))
     ,code))

(defmacro reports-communication-error-with-broken-server-configuration (code)
  "Asserts the given code reports a human-readable error
communication error message when there is no working connection
to the OmniSharpServer instance."
  `(with-broken-server-configuration
    (should (equal "Error communicating to the OmniSharpServer instance"
                   ,code))))


(ert-deftest json-requests-report-communication-errors-nicely ()
  "Human-readable error messages should be returned when doing
calls to the OmniSharpServer and there is no working
connection.

This is to ensure the user sees something other than the cryptic
default message 'json-readtable-error'"
  (reports-communication-error-with-broken-server-configuration
   (omnisharp-post-message-curl-as-json "any-url"))

  ;; returns a process when successful, and crashes when failing
  (with-broken-server-configuration
   (omnisharp-post-message-curl-as-json-async
    "any-url"
    nil
    (lambda (response-json-string)
      (should (equal "Error communicating to the OmniSharpServer instance"
                     response-json-string))))))

(ert-deftest check-alive-status-worker-should-return-server-result ()
  (with-working-server-configuration
   (should (equal t (omnisharp--check-alive-status-worker))))

  (with-broken-server-configuration
   (should (equal nil
                  (omnisharp--check-alive-status-worker)))))

(ert-deftest check-ready-worker-should-return-server-result ()
  (with-working-server-configuration
   (should (equal t (omnisharp--check-ready-status-worker))))

  (with-broken-server-configuration
   (should (equal nil
                  (omnisharp--check-ready-status-worker)))))

;; had some issues with refactoring and this function
(ert-deftest-async omnisharp-post-message-curl-async-doesnt-crash (done-function)
  (with-working-server-configuration
   (omnisharp-post-message-curl-async
     (omnisharp--get-api-url "checkreadystatus")
     nil ; no params needed
     (lambda (_) (funcall done-function)))))
