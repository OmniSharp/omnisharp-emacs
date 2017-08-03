
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;
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

(defmacro with-test-buffer-contents (buffer-contents
                                     code-to-run-in-buffer)
  `(with-current-buffer (get-buffer-create "omnisharp-test-buffer")
     (switch-to-buffer (get-buffer-create "omnisharp-test-buffer"))
     (delete-region (point-min) (point-max))
     (--map (insert (concat it "\n")) ,buffer-contents)
     (beginning-of-buffer)

     ,code-to-run-in-buffer))

(defmacro with-active-region-in-buffer (buffer-contents
                                        code-to-run-in-buffer)
  "Run CODE-TO-RUN-IN-BUFFER in a temp bufer with BUFFER-CONTENTS, and
the region active between the markers region-starts-here and
region-ends-here."
  `(with-test-buffer-contents
    ,buffer-contents

    (progn
      (evil-exit-visual-state)
      ;; remove region-starts-here markers
      (re-search-forward "(region-starts-here)")
      (replace-match "")
      (setq region-start (point))
      (re-search-forward "(region-ends-here)")
      (replace-match "")

      ;; select the text between the current position and the last one
      (push-mark region-start)
      (activate-mark)

      ,code-to-run-in-buffer)))

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
   (should (equal 4
                  (omnisharp--region-start-column)))))

(ert-deftest omnisharp--region-end-column-reports-correct-column ()
  (with-active-region-in-buffer
   '("line 1"
     "lin(region-starts-here)e 2"
     "line 3"
     "line 4"
     "(region-ends-here)line 5")
   (should (equal 1 (omnisharp--region-end-column)))))

(ert-deftest omnisharp--region-start-column-with-evil-mode-line-selection ()
  (with-current-buffer (get-buffer-create "omnisharp-test-buffer")
    (erase-buffer)
    (insert "This is a line with a length of 34\n") ; this is tested
    (insert "Another line.\n")
    (insert "There is a bug that doesn't occur with just one line.\n")
    (evil-visual-line
     (progn (goto-line 1) (point))
     (progn (goto-line 2) (point)))

    (should (equal 1 (omnisharp--region-start-line)))
    (should (equal 2 (omnisharp--region-end-line)))
    (should (equal 1 (omnisharp--region-start-column)))
    (should (equal 14 (omnisharp--region-end-column)))))

(ert-deftest omnisharp--go-to-end-of-region-with-evil-selection-test ()
  (with-current-buffer (get-buffer-create "omnisharp-test-buffer")
    (evil-exit-visual-state)
    (erase-buffer)
    (insert "This is a line with a length of 34\n")
    (insert "Another line")
    (goto-line 1)

    (evil-visual-select 0 5)

    (omnisharp--goto-end-of-region)

    (should (equal 1 (omnisharp--region-start-line)))
    (should (equal 1 (omnisharp--region-end-line)))
    (should (equal 1 (omnisharp--region-start-column)))
    (should (equal 5 (omnisharp--region-end-column)))))

(ert-deftest omnisharp--go-to-end-of-region-with-evil-line-selection-test ()
  (with-current-buffer (get-buffer-create "omnisharp-test-buffer")
    (evil-exit-visual-state)
    (erase-buffer)
    (insert "This is a line with a length of 34\n")
    (insert "Another line")
    (goto-line 1)
    (evil-visual-line)

    (omnisharp--goto-end-of-region)

    (should (equal 1 (omnisharp--region-start-line)))
    (should (equal 1 (omnisharp--region-end-line)))
    (should (equal 1 (omnisharp--region-start-column)))
    (should (equal 35 (omnisharp--region-end-column)))))

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

(ert-deftest omnisharp--convert-auto-complete-result-to-popup-format-shows-correct-data ()
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
             (CompletionText . ,completion-text)
             (Kind . "Verbose"))])
         (converted-popup-item
          (nth 0
               (omnisharp--convert-auto-complete-result-to-popup-format
                auto-completions))))

    (should (equal description (popup-item-document converted-popup-item)))
    (should (equal completion-text (popup-item-value converted-popup-item)))
    (should (equal snippet-text (get-text-property 0 'Snippet (popup-item-value converted-popup-item))))
    ;; TODO figure out how to verify popup item DisplayText.
    ;; An item looked like this:
    ;; #("Verbosity Verbose - display text" 0 32 (document "Verbosity Verbose; - description" value "Verbose - completion text"))
    ))

(ert-deftest omnisharp--apply-text-change-to-buffer-text ()
  (with-test-buffer-contents
   ["namespace testing {"
    "    public class WillBeRenamed {}"
    "}"]
   (should (equal (progn
                    (omnisharp--apply-text-change-to-buffer
                     `((NewText . "NewClassName")
                       (StartLine . 2) (EndLine . 2)
                       (StartColumn . 18) (EndColumn . 31)))
                    (omnisharp--get-current-buffer-contents))
                  (s-join "\n"
                          ["namespace testing {"
                           "    public class NewClassName {}"
                           "}"
                           ;; there is a trailing newline in the test
                           ;; buffer too
                           ""])))))
