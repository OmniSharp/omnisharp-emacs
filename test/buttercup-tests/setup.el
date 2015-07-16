;;; This file is a common place for buttercup testing related
;;; utilities and initialization

;;; These are originally ported from old integration tests that used
;;; ecukes, the emacs cucumber test runner. It aims for very readable
;;; step definitions so that style is encouraged here too.

(require 'f)
(require 's)
(require 'shut-up)

;;; These are displayed in the test output when a test opens a .cs
;;; file. Work around that by loading them in advance.
(require 'csharp-mode)
(require 'vc-git)

(defvar omnisharp-emacs-root-path
  (-> (f-this-file)
    f-parent
    f-parent
    f-parent))

(defvar omnisharp-minimal-test-solution-path
  (f-join omnisharp-emacs-root-path
          "test/MinimalSolution/minimal/"))

(print omnisharp-minimal-test-solution-path)
(add-to-list 'load-path omnisharp-emacs-root-path)

;;; the load-path has to contain omnisharp-emacs-root-path
(require 'omnisharp)
(require 'omnisharp-server-actions)
(require 'omnisharp-utils)

;;; I grew tired of the omnisharp-- prefix so now I use ot--, standing
;;; for omnisharp test
(defun ot--buffer-should-contain (expected)
  (let ((actual (s-replace (string ?\C-m) (string ?\C-j) (buffer-string)))
        (message "Expected '%s' to be part of '%s', but was not."))
    (cl-assert (s-contains? expected actual) nil message expected actual)))

(defun ot--evaluate (command-to-execute)
  (eval (read command-to-execute)))

(defun ot--evaluate-and-wait-for-server-response (command-to-execute)
  "NB: Will crash when calling a command that doesn't respond with a
request id."
  (omnisharp--wait-until-request-completed
   (eval (read command-to-execute))))

(defun ot--switch-to-buffer (existing-buffer-name)
  (let ((buffer (get-buffer existing-buffer-name))
        (message "Expected the buffer %s to exist but it did not."))
    (cl-assert (not (eq nil buffer)) nil message existing-buffer-name)
    (switch-to-buffer buffer)))

(defun ot--wait-for-seconds (seconds)
  (sit-for seconds))

(defun ot--buffer-contents-and-point-at-$ (buffer-contents-to-insert)
  "Test setup. Only works reliably if there is one $ character"
  (erase-buffer)
  (insert buffer-contents-to-insert)
  (beginning-of-buffer)
  (search-forward "$")
  (delete-backward-char 1)
  ;; will block
  (omnisharp--update-buffer))

(defun ot--point-should-be-on-line-number (expected-line-number)
  (let ((current-line-number (line-number-at-pos)))
    (cl-assert (= expected-line-number current-line-number)
               nil
               (concat
                "Expected point to be on line number '%s'"
                " but found it on '%s', the buffer containing:\n'%s'")
               expected-line-number
               current-line-number
               (buffer-string))))

(defun ot--open-the-minimal-solution-source-file (file-path-to-open)
  (find-file-literally (f-join omnisharp-minimal-test-solution-path
                               file-path-to-open))
  (setq buffer-read-only nil))

(defun ot--point-should-be-on-a-line-containing (expected-line-contents)
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
               current-line)))

(defun ot--there-should-be-a-window-editing-the-file (file-name)
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
               (window-list))))

(defun ot--switch-to-the-window-in-the-buffer (file-name)
  (select-window (get-buffer-window file-name)))

(defun ot--i-should-be-in-buffer-name (expected-buffer-name)
  (cl-assert (equal (buffer-name)
                    expected-buffer-name)
             nil
             (concat
              "Expected to be in buffer %s "
              "but was in buffer %s")
             expected-buffer-name
             (buffer-name)))

(defun ot--i-should-see (text)
  (cl-assert (s-contains? text
                          (buffer-string))
             nil
             (concat "Expected the buffer to contain '%s' but it did not. "
                     "The buffer contains '%s'")
             text
             (buffer-string)))

;;; Test suite setup. Start a test server process that can be used by
;;; all tests
(omnisharp--create-ecukes-test-server)
;; wait that the server is alive and ready before starting the test run
(with-timeout (2 ; seconds
               (omnisharp--log "Server did not start in time"))
  (while (not (equal t (cdr (assoc :started? omnisharp--server-info))))
    (accept-process-output)))

(print "buttercup test setup file loaded.")

;;; when reading the test output, make it easier to spot when test
;;; setup noise ends and test results start
(dotimes (i 5) (print "\n"))
