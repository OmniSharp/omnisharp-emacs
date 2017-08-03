;; License: GNU General Public License version 3, or (at your option) any later version
;;
;; You can run tests with M-x ert but remember to evaluate them before
;; running if you changed something!

;; For these tests, an OmniSharpServer process needs to be running.

(require 'ert-async)

(ert-deftest server-running-stdio-doesnt-crash-test ()
  (omnisharp-check-ready-status))
