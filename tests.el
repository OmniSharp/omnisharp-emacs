(require 'ert)

;; You can run tests with M-x ert but remember to evaluate them before
;; running if you changed something!

(ert-deftest omnisharp--get-omnisharp-server-executable-command ()
  "The correct server path must be returned on windows and unix systems"

  (let ((omnisharp-server-executable-path "OmniSharp.exe"))
    ;; Windows
    (should
     (equal "OmniSharp.exe -s some solution.sln > NUL"
            (let ((system-type 'windows-nt))
              (omnisharp--get-omnisharp-server-executable-command
               "some solution.sln"))))

    ;; osx
    (let ((system-type 'darwin))
      (should
       (equal "mono OmniSharp.exe -s some solution.sln > /dev/null"
              (omnisharp--get-omnisharp-server-executable-command
               "some solution.sln"))))

    ;; linux
    (let ((system-type 'gnu/linux))
      (should
       (equal "mono OmniSharp.exe -s some solution.sln > /dev/null"
              (omnisharp--get-omnisharp-server-executable-command
               "some solution.sln")))

      ;; Should also support an optional parameter
      (should
       (equal "mono /another/path/to/OmniSharp.exe -s some solution.sln > /dev/null"
              (omnisharp--get-omnisharp-server-executable-command
               "some solution.sln"
               "/another/path/to/OmniSharp.exe"))))))
