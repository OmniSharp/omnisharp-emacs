;; -*- lexical-binding: t -*-

;;; This file is a common place for buttercup testing related
;;; utilities and initialization

;;; These are originally ported from old integration tests that used
;;; ecukes, the emacs cucumber test runner. It aims for very readable
;;; step definitions so that style is encouraged here too.

(require 'f)
(require 's)

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

(defvar omnisharp-emacs-src-path
  (f-join omnisharp-emacs-root-path "src"))

(defvar omnisharp-minimal-test-solution-path
  (f-join omnisharp-emacs-root-path
          "test/MinimalSolution/minimal/"))

(print omnisharp-minimal-test-solution-path)
(add-to-list 'load-path omnisharp-emacs-root-path)

;;; the load-path has to contain omnisharp-emacs-root-path
(--each (f-files omnisharp-emacs-src-path
                 (lambda (file)
                   (equal "el" (f-ext file)))
                 ;; recursive
                 t)
  (load-file it))

(require 'omnisharp)
(require 'buttercup)

(setq omnisharp-debug 't)
(defun buffer-whole-string (buffer)
  (with-current-buffer buffer
    (save-excursion
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max))))))

;;; I grew tired of the omnisharp-- prefix so now I use ot--, standing
;;; for omnisharp test
(defun ot--buffer-should-contain (&rest expected)
  (let ((expected (s-join "\n" expected))
        (actual (s-replace (string ?\C-m) (string ?\C-j)
                           (substring-no-properties (buffer-string))))
        (message "Expected '%s' to be part of '%s', but was not."))
    (cl-assert (s-contains? expected actual) nil message expected actual)))

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

(defun omnisharp--update-buffer (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (omnisharp--wait-until-request-completed
   (omnisharp--send-command-to-server
    "updatebuffer"
    (cons `(Buffer . ,(omnisharp--get-current-buffer-contents))
          (omnisharp--get-request-object)))))

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

(defun ot--open-the-minimal-solution-source-file (file-path-to-open)
  (when (get-buffer file-path-to-open)
    (kill-buffer file-path-to-open))
  (find-file (f-join omnisharp-minimal-test-solution-path
                     file-path-to-open))
  (setq buffer-read-only nil))

(defun ot--delete-the-minimal-solution-source-file (file-name)
  (-when-let (buffer (get-buffer file-name))
    (kill-buffer buffer))
  (let ((file-path (f-join omnisharp-minimal-test-solution-path file-name)))
    (when (f-exists? file-path)
      (f-delete file-path))))

(defun ot--point-should-be-on-a-line-containing (expected-line-contents)
  (let ((current-line (substring-no-properties (or (thing-at-point 'line) ""))))
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

;; this is a poor man's version of action chains in ecukes
(defun ot--keyboard-input (&rest text-vectors)
  "Simulates typing. Can be used to do interactive input, but
detecting situations in the middle of input is impossible."
  (condition-case error
      (execute-kbd-macro (reduce 'vconcat text-vectors))
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
    (omnisharp--get-company-candidates "")
    (-map (lambda(completion)
            (cdr (assoc 'DisplayText completion)))
          omnisharp--last-buffer-specific-auto-complete-result))

(defmacro ot--set (symbol value)
  `(setq symbol ,value))

(defmacro ot--answer-omnisharp--ido-completing-read-with (answer-function)
  "Automatically select the first candidate given to
omnisharp--ido-completing-read. This could be done by controlling
ido with the keyboard like in other tests, but ido is not easy to
control programmatically.

ANSWER-FUNCTION should receive a list of choices (strings) and respond
with one."
  `(spy-on 'omnisharp--ido-completing-read :and-call-fake
           (lambda (_prompt _quickfixes)
             (funcall ,answer-function _quickfixes))))

;;; Test suite setup. Start a test server process that can be used by
;;; all tests
(let ((omnisharp-server-executable-path (concat omnisharp-emacs-root-path
                                                "/omnisharp-roslyn/omnisharp")))
  (omnisharp--create-ecukes-test-server omnisharp-emacs-root-path))
;; wait that the server is alive and ready before starting the test run
(with-timeout (2 ; seconds
               (omnisharp--log "Server did not start in time"))
  (while (not (equal t (cdr (assoc :started? omnisharp--server-info))))
    (accept-process-output)))

(setq create-lockfiles nil)

(print "buttercup test setup file loaded.")

;;; when reading the test output, make it easier to spot when test
;;; setup noise ends and test results start
(dotimes (i 5) (print "\n"))

