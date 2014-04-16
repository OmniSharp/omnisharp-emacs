(require 'ert)
(require 'el-mock)

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

(ert-deftest flycheck-error-parser-raw-json-can-convert-syntax-errors-to-flycheck-errors ()
  "The output from a /syntaxerrors call must be a valid input for the
omnisharp--flycheck-error-parser-raw-json error parser"
  (let ((quickfix-in-json-format
         "{\"Errors\":[{\"Message\":\"Unexpected symbol `string\\u0027\\u0027 in class, struct, or interface member declaration\",
                        \"Line\":22,
                        \"Column\":20,
                        \"FileName\":\"/foo/OmniSharpServer/OmniSharp/Common/QuickFix.cs\"}]}"))
    (omnisharp--flycheck-error-parser-raw-json
     quickfix-in-json-format
     'checker
     (get-buffer-create "some.cs"))))

(ert-deftest flycheck-error-parser-raw-json-can-convert-code-issues-to-flycheck-errors ()
  "The output from a /getcodeissues call must be a valid input for the
omnisharp--flycheck-error-parser-raw-json error parser"
  (let ((quickfix-in-json-format
         "{\"QuickFixes\":[{\"Text\":\"Parameter can be demoted to base class\",
                            \"Line\":22,
                            \"Column\":20,
                            \"FileName\":\"/foo/OmniSharpServer/OmniSharp/Common/QuickFix.cs\"}]}"))
    (omnisharp--flycheck-error-parser-raw-json
     quickfix-in-json-format
     'checker
     (get-buffer-create "some.cs")
     ;; This is the level that csharp-omnisharp-curl-code-issues uses
     'info)))


