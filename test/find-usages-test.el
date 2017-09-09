;; License: GNU General Public License version 3, or (at your option) any later version

(ert-deftest omnisharp--find-usages-show-response-doesnt-show-zero-quickfixes ()
  (with-mock
    (mock (omnisharp--message-at-point "No usages found.") => "")
    (stub omnisharp--write-quickfixes-to-compilation-buffer =>
          (error "should not show usages when there are none"))
    (omnisharp--find-usages-show-response nil)))
