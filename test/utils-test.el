
(ert-deftest omnisharp--get-api-url ()
  (let ((omnisharp-host "host"))
    (should (string= "host/api"
                     (omnisharp--get-api-url "api")))))
