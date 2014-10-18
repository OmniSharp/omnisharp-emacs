;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.
;;
;; Find some included steps at
;; https://github.com/ecukes/espuds/blob/master/espuds.el#L130

(Given "^I bind key \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
       (lambda (key fn-name)
         (global-set-key (kbd key) (intern fn-name))))

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
  (lambda (buffer-contents-to-insert)
    (insert buffer-contents-to-insert)
    (beginning-of-buffer)
    (re-search-forward "\\$")
    (delete-backward-char 1)))
