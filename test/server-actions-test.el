;; License: GNU General Public License version 3, or (at your option) any later version

(ert-deftest check-ready-status-test ()
  (with-server-returning "checkreadystatus" t
                         (should (equal "Server is ready"
                                        (omnisharp-check-ready-status))))

  (with-server-returning "checkreadystatus" nil
                         (should (equal "Server is not ready yet"
                                        (omnisharp-check-ready-status)))))

(ert-deftest check-available-test ()
  (with-server-returning "checkalivestatus" t
                         (should (equal "Server is alive and well. Happy coding!"
                                        (omnisharp-check-alive-status))))

  (with-server-returning "checkalivestatus" nil
                         (should (equal "Server is not alive"
                                        (omnisharp-check-alive-status)))))
