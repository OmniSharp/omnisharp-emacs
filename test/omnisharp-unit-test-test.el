(ert-deftest omnisharp-unit-test-worker ()
  ;; ignore calls, just return the same value
  (noflet ((omnisharp--fix-build-command-if-on-windows (command)
                                                       command))
    (with-mock
      (stub omnisharp-get-build-command => 'build-command)
      (stub omnisharp--get-common-params)
      (stub omnisharp-get-host => "http://localhost:2000/")
      (stub yes-or-no-p => nil)
      (stub compile)
      (mock (omnisharp-post-message-curl-as-json
             "http://localhost:2000/gettestcontext"
             '(("Type" . "all"))))

      (mock (compile 'build-command))
      (omnisharp-unit-test-worker "all"))))
