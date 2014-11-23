(ert-deftest omnisharp-reload-solution ()
  (let ((omnisharp-host "host/"))
    (with-mock
      (mock (omnisharp-post-message-curl-async
              "host/reloadsolution"
              nil
              *))

      (omnisharp-reload-solution))))
