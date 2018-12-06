;; -*- lexical-binding: t -*-
;; License: GNU General Public License version 3, or (at your option) any later version

;;; This file is a common place for buttercup testing related
;;; utilities and initialization

;;; These are originally ported from old integration tests from legacy branch.
;;; It aims for very readable step definitions so that style is encouraged here too.

(require 'f)
(require 's)

;;; Work around for emacs bug#18845
(when (and (= emacs-major-version 24) (>= emacs-minor-version 4))
  (require 'cl))

;;; These are displayed in the test output when a test opens a .cs
;;; file. Work around that by loading them in advance.
(require 'csharp-mode)
(require 'vc-git)
(require 'el-mock)

(defvar omnisharp-emacs-root-path
  (-> (f-this-file)
      f-parent
      f-parent
      f-parent))

(defvar omnisharp-minimal-test-project-path
  (f-join omnisharp-emacs-root-path
          "test/MinimalProject"))

(setq omnisharp-debug t)
(print omnisharp-minimal-test-project-path)
(add-to-list 'load-path omnisharp-emacs-root-path)

;;; the load-path has to contain omnisharp-emacs-root-path
(--each (f-files omnisharp-emacs-root-path
                 (lambda (file)
                   (equal "el" (f-ext file))))
  (load-file it))

(require 'omnisharp)
(require 'buttercup)

;;; I grew tired of the omnisharp-- prefix so now I use ot--, standing
;;; for "omnisharp test"
(defun ot--buffer-should-contain (&rest expected)
  (let ((expected (s-join "\n" expected))
        (actual (s-replace (string ?\C-m) (string ?\C-j)
                           (substring-no-properties (buffer-string))))
        (message "Expected '%s' to be part of '%s', but was not."))
    (cl-assert (s-contains? expected actual) nil (format message expected actual))))

(defun ot--evaluate (command-to-execute)
  (eval (read command-to-execute)))

(defun ot--evaluate-and-wait-for-server-response (command-to-execute)
  "NB: Will crash when calling a command that doesn't respond with a
request id."
  (omnisharp--wait-until-request-completed
   (eval (read command-to-execute))))

(defun ot--wait-for (predicate &optional timeout-seconds)
  (setq timeout-seconds (or timeout-seconds 2))

  (let ((start-time (current-time)))
    (while (not (funcall predicate))
      (when (> (cadr (time-subtract (current-time) start-time))
               timeout-seconds)
        (progn
          (let ((msg (format "Did not complete in %s seconds: %s"
                             timeout-seconds
                             (prin1-to-string predicate))))
            (error msg))))
      (accept-process-output nil 0.01))))

(defun ot--switch-to-buffer (existing-buffer-name)
  (let ((buffer (get-buffer existing-buffer-name))
        (message "Expected the buffer %s to exist but it did not."))
    (cl-assert (not (eq nil buffer)) nil message existing-buffer-name)
    (switch-to-buffer buffer)))

(defun ot--wait-for-seconds (seconds)
  (sit-for seconds))

(defun ot--buffer-contents-and-point-at-$ (&rest buffer-contents-to-insert)
  "Test setup. Only works reliably if there is one $ character"
  (erase-buffer)
  (deactivate-mark)
  (--map (insert it "\n") buffer-contents-to-insert)
  (beginning-of-buffer)
  (search-forward "$")
  (delete-backward-char 1)
  ;; will block
  (omnisharp--update-buffer)
  (when (fboundp 'evil-insert)
    (evil-insert 1)))

(defun ot--buffer-contents-and-region (&rest lines)
  "Notice: LINES have to contain $"

  ;; todo deactivate existing region
  (apply #'ot--buffer-contents-and-point-at-$ lines)

  (beginning-of-buffer)

  ;; remove region-starts-here markers
  (re-search-forward "(region-starts-here)")
  (replace-match "")
  (setq region-start (point))
  (re-search-forward "(region-ends-here)")
  (replace-match "")

  (omnisharp--update-buffer)
  ;; select the text between the current position and the last one
  (push-mark region-start)
  (activate-mark))

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

(defun ot--open-the-minimal-project-source-file (file-path-to-open)
  (when (get-buffer file-path-to-open)
    (kill-buffer file-path-to-open))
  (find-file (f-join omnisharp-minimal-test-project-path
                     file-path-to-open))
  (setq buffer-read-only nil))

(defun ot--delete-the-minimal-project-source-file (file-name)
  (-when-let (buffer (get-buffer file-name))
    (kill-buffer buffer))
  (let ((file-path (f-join omnisharp-minimal-test-project-path file-name)))
    (when (f-exists? file-path)
      (f-delete file-path))))

(defun ot--point-should-be-on-a-line-containing (expected-line-contents)
  (let ((current-line (substring-no-properties (or (thing-at-point 'line) ""))))
    (cl-assert (s-contains? expected-line-contents current-line)
               nil
               (format
                (concat "Expected the current line (%d) to contain '%s'.\n"
                        "The current buffer contains:\n%s\n"
                        "The current line contains: '%s'")
                (line-number-at-pos)
                expected-line-contents
                (buffer-string)
                current-line))))

(defun ot--there-should-be-a-window-editing-the-file (file-name)
  (cl-assert (get-buffer-window file-name)
             nil
             (concat
              "No visible window is editing the file '%s'."
              " Visible windows: '%s'")
             file-name
             (window-list)))

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

(defun ot--i-should-see (&rest lines)
  (cl-assert (s-contains? (s-join "\n" lines)
                          (buffer-string))
             nil
             (concat "Expected the buffer to contain '%s' but it did not. "
                     "The buffer contains '%s'")
             lines
             (buffer-string)))

;; this is a poor man's version of action chains
(defun ot--keyboard-input (&rest text-vectors)
  "Simulates typing. Can be used to do interactive input, but
detecting situations in the middle of input is impossible."
  (condition-case error
      (execute-kbd-macro (cl-reduce 'vconcat text-vectors))
    (error (print (format "ot--keyboard-input error: %s" error)))))

(defun ot--meta-x-command (command)
  (vconcat
   (ot--press-key "M-x")
   (ot--type command)
   (ot--press-key "RET")))

(defun ot--type (text)
  (string-to-vector text))

(defun ot--press-key (key-or-chord)
  (edmacro-parse-keys key-or-chord))

(defun ot--get-completions ()
  (let* ((get-candidates-result (omnisharp--get-company-candidates ""))
         (fetcher (cdr get-candidates-result)))

    ;; omnisharp--get-company-candidates returns an :async callback,
    ;;; -- we need to invoke async machinery to get to the value of
    ;; omnisharp--last-buffer-specific-auto-complete-result
    (omnisharp--wait-until-request-completed (funcall fetcher (lambda (result) nil)))

    (-map (lambda(completion)
            (cdr (assoc 'DisplayText completion)))
          omnisharp--last-buffer-specific-auto-complete-result)
    ))

(defmacro ot--set (symbol value)
  `(setq symbol ,value))

(defmacro ot--answer-omnisharp--completing-read-with (answer-function)
  "Automatically select the first candidate given to
omnisharp--completing-read. This could be done by controlling
ido with the keyboard like in other tests, but ido is not easy to
control programmatically.

ANSWER-FUNCTION should receive a list of choices (strings) and respond
with one."
  `(spy-on 'omnisharp--completing-read :and-call-fake
           (lambda (_prompt _quickfixes)
             (funcall ,answer-function _quickfixes))))

(defun ot--wait-until-all-requests-completed (&optional timeout-seconds)
  (setq timeout-seconds (or timeout-seconds 2))

  (let ((start-time (current-time))
        (process (cdr (assoc :process omnisharp--server-info))))
    (while (cdr (assoc :response-handlers omnisharp--server-info))
      (when (> (cadr (time-subtract (current-time) start-time))
               timeout-seconds)
        (progn
          (let ((msg (format "All requests did not complete in %s seconds"
                             timeout-seconds)))
            (omnisharp--log msg)
            (error msg))))
      (accept-process-output process 0.1))))

;; Test suite setup. Start a test server process that can be used by
;; all tests

(print "trying to download and install omnisharp-roslyn server...")
(omnisharp--install-server nil t)

(print "trying to launch the server...")
(omnisharp--do-server-start (s-concat omnisharp-emacs-root-path
                                      "/test/MinimalProject"))

;; wait that the server is alive and ready before starting the test run
(with-timeout (2 ; seconds
               (omnisharp--log "Server did not start in time"))
  (while (not (equal t (cdr (assoc :started? omnisharp--server-info))))
    (accept-process-output)))

;; still sleep a bit because even with the input received the server
;; might still not be able to response to requests in-time for the
;; first test to run properly
(print "waiting for the server to spin up (5 secs)..")
(print (current-time-string))

;; sleep-for doesn't work in some versions of emacs.  Using current-time-string
;; to ensure from output that we are actually waiting for the server to started
;; and using a hack to force wait which was from this link
;; https://stackoverflow.com/questions/14698081/elisp-sleep-for-doesnt-block-when-running-a-test-in-ert
;;(sleep-for 10)
(let ((now (float-time))
       (process-connection-type nil))
  (start-process "tmp" "*tmp*" "bash" "-c" "sleep 1; echo hi")
  (while (< (- (float-time) now) 5)
    (sleep-for 1))
  )
(print (current-time-string))

(setq create-lockfiles nil)

(print "buttercup test setup file loaded.")

;;; when reading the test output, make it easier to spot when test
;;; setup noise ends and test results start
(print "\n\n\n\n\n\n\n\n\n\n\n")



;; todo this needs to be taken into use
;; (Teardown
;;  ;; After when everything has been run

;;  (omnisharp--log "TEST: shutting down test server in integration test Teardown hook")
;;  (with-current-buffer "OmniServer"
;;    (let ((filename "omnisharp-server-output.txt"))
;;      (write-file filename)
;;      (print (format "OmniServer buffer contents (available in %s):\n"
;;                     filename))
;;      (print (buffer-string))
;;      (kill-process "OmniServer")))

;;  (with-current-buffer "*omnisharp-debug*"
;;    (let ((filename "omnisharp-debug-output.txt"))
;;      (write-file filename)
;;      (print (format "Debug buffer contents (available in %s):\n"
;;                     filename))
;;      (print (buffer-string))))

;;  (print "Server info:\n")
;;  (print (prin1-to-string omnisharp--server-info))
;;  (print "\n"))
