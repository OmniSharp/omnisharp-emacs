(ert-deftest omnisharp--find-usages-show-response-doesnt-show-zero-quickfixes ()
  (with-mock
    (stub omnisharp--write-quickfixes-to-compilation-buffer =>
          (error "should not show usages when there are none"))
    (omnisharp--find-usages-show-response nil)))
