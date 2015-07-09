;; You can run tests with M-x ert but remember to evaluate them before
;; running if you changed something!

(require 'el-mock)
(require 'noflet)

(ert-deftest omnisharp--get-omnisharp-server-executable-command ()
  "The correct server path must be returned on windows and unix systems"

  ;; Windows
  ;;
  ;; ignore expand-file-name calls. just return the original to keep
  ;; things maintainable
  (noflet ((expand-file-name (file-name &rest _args)
                             file-name))
    (with-mock
      (setq omnisharp-server-executable-path "OmniSharp.exe")
      (stub w32-shell-dos-semantics)
      (should
       (equal '("OmniSharp.exe" "-s" "some solution.sln")
              (let ((system-type 'windows-nt))
                (omnisharp--get-omnisharp-server-executable-command
                 "some solution.sln")))))

    ;; osx
    (let ((system-type 'darwin))
      (should
       (equal '("mono" "OmniSharp.exe" "-s" "some solution.sln")
              (omnisharp--get-omnisharp-server-executable-command
               "some solution.sln"))))

    ;; linux
    (let ((system-type 'gnu/linux))
      (should
       (equal '("mono" "OmniSharp.exe" "-s" "some solution.sln")
              (omnisharp--get-omnisharp-server-executable-command
               "some solution.sln")))

      ;; Should also support an optional parameter
      (should
       (equal '("mono" "/another/path/to/OmniSharp.exe" "-s" "some solution.sln")
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
                            \"LogLevel\":\"Info\",
                            \"Column\":20,
                            \"FileName\":\"/foo/OmniSharpServer/OmniSharp/Common/QuickFix.cs\"}]}"))
    (omnisharp--flycheck-error-parser-raw-json
     quickfix-in-json-format
     'checker
     (get-buffer-create "some.cs"))))

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

(defun get-line-text (&optional line-number)
  "Returns the text on the current line or another line with the
number given"
  (when (equal nil line-number)
    (setq line-number (line-number-at-pos)))
  (goto-line line-number)
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(ert-deftest omnisharp--insert-namespace-import ()
  (let* ((new-import "System.IO"))
    (with-temp-buffer
      (-each '("using System;\n"
               "\n"
               "public class Awesome {}")
        'insert)
      (omnisharp--insert-namespace-import new-import)

      (should (equal (concat "using " new-import ";")
                     (get-line-text 0))))))

(ert-deftest activating-omnisharp-mode-should-not-start-server-if-running ()
  "When server is already running, a new server should not be started"
  (with-mock
    (stub omnisharp--check-alive-status-worker => t)
    (stub omnisharp-start-omnisharp-server)
    (not-called start-process)
    (not-called omnisharp--find-solution-file)
    (omnisharp-mode)))

(ert-deftest activating-omnisharp-mode-should-start-server ()
  "Activating omnisharp-mode should start an OmniSharpServer"
  (with-mock
   (let ((path "/solution/directory/")
         (solution-name "first-solution.sln"))
     (mock (omnisharp--find-solution-files) => `(,path ,solution-name))
     (stub omnisharp--check-alive-status-worker => nil)
     (mock (omnisharp-start-omnisharp-server "/solution/directory/first-solution.sln"))
     (omnisharp-mode))))

(ert-deftest omnisharp--write-quickfixes-to-compilation-buffer--has-expected-contents ()
  "Writing QuickFixes to the compilation buffer should have the
expected output in that buffer"
  (save-excursion
    (let ((buffer-name "test-buffer-name")
          (quickfixes-to-write
           '(((Text . "public class MyClass")
              (EndColumn . 0)
              (EndLine . 0)
              (Column . 18)
              (Line . 5)
              (FileName . "/project/MyClass.cs")
              (LogLevel . nil)))))

      (omnisharp--write-quickfixes-to-compilation-buffer
       quickfixes-to-write
       buffer-name
       "test-buffer-header\n\n")
      (switch-to-buffer buffer-name)

      (let ((contents (buffer-string)))
        (should (s-contains? "test-buffer-header" contents))
        (should (s-contains? "/project/MyClass.cs:5:18:" contents))
        (should (s-contains? "public class MyClass" contents))))))


(ert-deftest
    omnisharp--write-quickfixes-to-compilation-buffer-doesnt-mess-with-find-tag-marker-ring ()

  (with-mock
    (stub ring-insert => (error "must not be called"))
    (save-excursion
      (omnisharp--write-quickfixes-to-compilation-buffer
       '()
       "buffer-name"
       "test-buffer-header\n\n"
       ;; don't save old position to find-tag-marker-ring
       t))))

(ert-deftest omnisharp-stop-server-calls-correct-api ()
  (with-mock
    (let ((omnisharp-host "host/"))
      (mock (omnisharp-post-message-curl-async "host/stopserver" * *))
      (omnisharp-stop-server))))

(ert-deftest omnisharp--convert-auto-complete-json-to-popup-format-shows-correct-data ()
  (let* ((description "Verbosity Verbose; - description")
         (completion-text "Verbose - completion text")
         (snippet-text "Verbose$0")
         (auto-completions
          `[((Snippet . ,snippet-text)
             (ReturnType . "OmniSharp.Verbosity")
             (MethodHeader . nil)
             (RequiredNamespaceImport . nil)
             (DisplayText . "Verbosity Verbose - display text")
             (Description . ,description)
             (CompletionText . ,completion-text))])
         (converted-popup-item
          (nth 0
               (omnisharp--convert-auto-complete-json-to-popup-format
                auto-completions))))

    (should (equal description (popup-item-document converted-popup-item)))
    (should (equal completion-text (popup-item-value converted-popup-item)))
    (should (equal snippet-text (get-text-property 0 'Snippet (popup-item-value converted-popup-item))))
    ;; TODO figure out how to verify popup item DisplayText.
    ;; An item looked like this:
    ;; #("Verbosity Verbose - display text" 0 32 (document "Verbosity Verbose; - description" value "Verbose - completion text"))
    ))
