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

(defmacro with-active-region-in-buffer (buffer-contents
                                        code-to-run-in-buffer)
  "Run CODE-TO-RUN-IN-BUFFER in a temp bufer with BUFFER-CONTENTS, and
the region active between the markers region-starts-here and
region-ends-here."
  `(with-current-buffer (get-buffer-create "omnisharp-test-buffer")
     (switch-to-buffer (get-buffer-create "omnisharp-test-buffer"))
     (delete-region (point-min) (point-max))
     (--map (insert (concat it "\n")) ,buffer-contents)
     (beginning-of-buffer)
     (re-search-forward "(region-starts-here)")
     (replace-match "")
     (setq region-start (point))
     (re-search-forward "(region-ends-here)")
     (replace-match "")
     ;; select the text between the current position and the last one
     (push-mark region-start)
     (activate-mark)

     ,code-to-run-in-buffer))

;; Region line and column helper tests.
;; Could be one test but on the other hand this way we know if only
;; one of them fails.
(ert-deftest omnisharp--region-start-line-reports-correct-line ()
  (with-active-region-in-buffer
   ;; These are multiple lines because my emacs started to mess up the
   ;; syntax highlighting if they were one long multiline string. And
   ;; it's difficult to reason about columns when all lines have
   ;; leading whitespace
   '("line 1"
     "lin(region-starts-here)e 2"
     "line 3"
     "line 4"
     "(region-ends-here)line 5")
   (should (equal 2
                  (omnisharp--region-start-line)))))

(ert-deftest omnisharp--region-end-line-reports-correct-line ()
  (with-active-region-in-buffer
   '("line 1"
     "lin(region-starts-here)e 2"
     "line 3"
     "line 4"
     "(region-ends-here)line 5")
   (should (equal 5
                  (omnisharp--region-end-line)))))

(ert-deftest omnisharp--region-start-column-reports-correct-column ()
  (with-active-region-in-buffer
   '("line 1"
     "lin(region-starts-here)e 2"
     "line 3"
     "line 4"
     "(region-ends-here)line 5")
   (should (equal 3
                  (omnisharp--region-start-column)))))

(ert-deftest omnisharp--region-end-column-reports-correct-column ()
  (with-active-region-in-buffer
   '("line 1"
     "lin(region-starts-here)e 2"
     "line 3"
     "line 4"
     "(region-ends-here)line 5")
   ;; the minimum required column is 1 on the server side
   (should (equal 1
                  (omnisharp--region-end-column)))))

(ert-deftest omnisharp--region-start-column-with-evil-mode-line-selection ()
  "Using evil-mode the user may select a line with V. This must report
the correct start and end columns: the start being the start of the
line (0) and end being the length of the line."

  (with-current-buffer (get-buffer-create "omnisharp-test-buffer")
    (delete-region (point-min) (point-max))
    (insert "This is a line with a length of 34\n") ; this is tested
    (insert "Another line.\n")
    (insert "There is a bug that doesn't occur with just one line.\n")
    (goto-line 1)
    (evil-visual-line)

    (should (equal 1
                   (omnisharp--region-start-column)))
    (should (equal 34
                   (omnisharp--region-end-column)))))


(ert-deftest omnisharp--get-code-actions-from-api-works-with-a-region ()
  "It should not crash on the emacs side of things when composing the
request. This test doesn't care what the server thinks is correct
data."
  (with-active-region-in-buffer
   '("line 1"
     "lin(region-starts-here)e 2"
     "line 3"
     "line 4"
     "(region-ends-here)line 5")
   (with-stub
     (stub omnisharp-post-message-curl-as-json)
     (omnisharp--get-code-actions-from-api))))
