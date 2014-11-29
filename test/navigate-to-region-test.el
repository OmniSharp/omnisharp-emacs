(ert-deftest omnisharp-navigate-to-region ()
  (with-mock
    (stub omnisharp--get-common-params)
    (stub omnisharp-get-host => "http://omnisharp-host/")

    (mock (omnisharp-post-message-curl-as-json "http://omnisharp-host/gotoregion" *)
          => '((QuickFixes . (1 2 3))))
    (mock (omnisharp--choose-and-go-to-quickfix-ido '(1 2 3) *))

    (omnisharp-navigate-to-region)))
