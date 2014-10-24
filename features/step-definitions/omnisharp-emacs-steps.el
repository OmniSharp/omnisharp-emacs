;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.
;;
;; Find some included steps at
;; https://github.com/ecukes/espuds/blob/master/espuds.el#L130

(Then "^I should see, ignoring line endings\\(?: \"\\(.+\\)\"\\|:\\)$"
      "Asserts that the current buffer includes some text. Ignores
line endings, so windows CRLF is considered the same as Unix LF."
      (lambda (expected)
        (let ((actual (s-replace (string ?\C-m) (string ?\C-j) (buffer-string)))
              (message "Expected '%s' to be part of '%s', but was not."))
          (cl-assert (s-contains? expected actual) nil message expected actual))))

(And "^I evaluate the command \"\\([^\"]+\\)\"$"
  (lambda (command-to-execute)
    (eval (read command-to-execute))))

(When "^I switch to the existing buffer \"\\([^\"]+\\)\"$"
  "Asserts the buffer with the given name exists and switches to that buffer."
  (lambda (existing-buffer-name)
    (let ((buffer (get-buffer existing-buffer-name))
          (message "Expected the buffer %s to exist but it did not."))
      (cl-assert (not (eq nil buffer)) nil message existing-buffer-name)
      (switch-to-buffer buffer))))

(And "^I wait \"\\([^\"]+\\)\" seconds$"
  (lambda (seconds)
    (sit-for (read seconds))))

(When "^My buffer contents are, and my point is at $:$"
  "Test setup. Only works reliably if there is one $ character"
  (lambda (buffer-contents-to-insert)
    (erase-buffer)
    (insert buffer-contents-to-insert)
    (beginning-of-buffer)
    (search-forward "$")
    (delete-backward-char 1)))

(Then "^point should be on line number \"\\([^\"]+\\)\"$"
      (lambda (expected-line-number)
        (let ((current-line-number (line-number-at-pos))
              (expected-line-number (string-to-number expected-line-number)))
          (cl-assert (= expected-line-number
                        current-line-number)
                     nil
                     (concat
                      "Expected point to be on line number '%s'"
                      " but found it on '%s', the buffer containing:\n'%s'")
                     expected-line-number
                     current-line-number
                     (buffer-string)))))

(And "^I save the buffer$"
     'save-buffer)

(When "^I open the Omnisharp server source file \"\\([^\"]+\\)\"$"
      (lambda (file-path-to-open)
        (find-file (f-join omnisharp-minimal-test-solution-path
                           file-path-to-open))))

(Then "^point should be on a line containing \"\\([^\"]+\\)\"$"
  "The expected-line-contents must be contained in the current line"
  (lambda (expected-line-contents)
    (let ((current-line (substring-no-properties (thing-at-point 'line))))
      (cl-assert (s-contains? expected-line-contents current-line)
                 nil
                 (concat "Expected the current line (number '%d') to contain"
                         " '%s'. The current buffer contains:"
                         "\n"
                         "%s"
                         "\n"
                         "The current line contains: '%s'")
                 (line-number-at-pos)
                 expected-line-contents
                 (buffer-string)
                 current-line))))

(Then "^there should be a window editing the file \"\\([^\"]+\\)\"$"
  "The file path must be the name of the file only, not including its path"
  (lambda (file-name)
    (let ((full-path (buffer-file-name
                      (window-buffer
                       (get-buffer-window file-name)))))
      (cl-assert (when full-path
                   (f-filename
                    full-path))
                 nil
                 (concat
                  "No visible window is editing the file '%s'."
                  " Visible windows: '%s'")
                 file-name
                 (window-list)))))

(When "^I switch to the window in the buffer \"\\([^\"]+\\)\"$"
      (lambda (file-name)
        (select-window (get-buffer-window file-name))))

