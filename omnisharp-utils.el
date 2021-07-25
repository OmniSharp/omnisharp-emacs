;; -*- lexical-binding: t; -*-

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

(require 'dash)

(defun omnisharp--path-to-server (path)
  (if (and path (eq system-type 'cygwin))
      (cygwin-convert-file-name-to-windows path)
    path))

(defun omnisharp--path-from-server (path)
  (if (and path (eq system-type 'cygwin))
      (cygwin-convert-file-name-from-windows path)
    path))

(defun omnisharp--get-filename (item)
  (omnisharp--path-from-server (cdr (assoc 'FileName item))))

(defun omnisharp--to-filename (path)
  `(FileName . ,(omnisharp--path-to-server path)))

(defun omnisharp--write-quickfixes-to-compilation-buffer
  (quickfixes
   buffer-name
   buffer-header
   &optional dont-save-old-pos)
  "Takes a list of QuickFix objects and writes them to the
compilation buffer with HEADER as its header. Shows the buffer
when finished.

If DONT-SAVE-OLD-POS is specified, will not save current position to
find-tag-marker-ring. This is so this function may be used without
messing with the ring."
  (let ((output-in-compilation-mode-format
         (mapcar
          'omnisharp--find-usages-output-to-compilation-output
          quickfixes)))

    (omnisharp--write-lines-to-compilation-buffer
     output-in-compilation-mode-format
     (get-buffer-create buffer-name)
     buffer-header)
    (unless dont-save-old-pos
      (ring-insert find-tag-marker-ring (point-marker))
      (omnisharp--show-last-buffer-position-saved-message
       (buffer-file-name)))))

(defun omnisharp--write-lines-to-compilation-buffer
  (lines-to-write buffer-to-write-to &optional header)
  "Writes the given lines to the given buffer, and sets
compilation-mode on. The contents of the buffer are erased. The
buffer is marked read-only after inserting all lines.

LINES-TO-WRITE are the lines to write, as-is.

If HEADER is given, that is written to the top of the buffer.

Expects the lines to be in a format that compilation-mode
recognizes, so that the user may jump to the results."
  (with-current-buffer buffer-to-write-to
    (let ((inhibit-read-only t))
      ;; read-only-mode new in Emacs 24.3
      (if (fboundp 'read-only-mode)
          (read-only-mode nil)
        (setq buffer-read-only nil))
      (erase-buffer)

      (when (not (null header))
        (insert header))

      (mapc (lambda (element)
              (insert element)
              (insert "\n"))
            lines-to-write)
      (compilation-mode)
      (if (fboundp 'read-only-mode)
          (read-only-mode t)
        (setq buffer-read-only t))
      (display-buffer buffer-to-write-to))))

(defun omnisharp--find-usages-output-to-compilation-output
  (json-result-single-element)
  "Converts a single element of a /findusages JSON response to a
format that the compilation major mode understands and lets the user
follow results to the locations in the actual files."
  (let ((filename (omnisharp--get-filename json-result-single-element))
        (line (cdr (assoc 'Line json-result-single-element)))
        (column (cdr (assoc 'Column json-result-single-element)))
        (text (cdr (assoc 'Text json-result-single-element))))
    (concat filename
            ":"
            (prin1-to-string line)
            ":"
            (prin1-to-string column)
            ": \n"
            text
            "\n")))

(defun omnisharp--set-buffer-contents-to (filename-for-buffer
                                          new-buffer-contents
                                          &optional
                                          result-point-line
                                          result-point-column)
  "Sets the buffer contents to new-buffer-contents for the buffer
visiting filename-for-buffer. If no buffer is visiting that file, does
nothing. Afterwards moves point to the coordinates RESULT-POINT-LINE
and RESULT-POINT-COLUMN.

If RESULT-POINT-LINE and RESULT-POINT-COLUMN are not given, and a
buffer exists for FILENAME-FOR-BUFFER, its current positions are
used. If a buffer does not exist, the file is visited and the default
point position is used."
  (save-window-excursion
    (omnisharp--find-file-possibly-in-other-window
     filename-for-buffer nil) ; not in other-window

    ;; Default values are the ones in the buffer that is visiting
    ;; filename-for-buffer.
    (setq result-point-line
          (or result-point-line (line-number-at-pos)))
    (setq result-point-column
          (or result-point-column (omnisharp--current-column)))
    (erase-buffer)
    (insert new-buffer-contents)

    ;; Hack. Puts point where it belongs.
    (omnisharp-go-to-file-line-and-column-worker
     result-point-line result-point-column filename-for-buffer)))

(defun omnisharp--current-column ()
  "Returns the current column, converting tab characters in a way that
the OmniSharp server understands."
  (let ((tab-width 1))
    (1+ (current-column))))

(defun omnisharp--buffer-exists-for-file-name (file-name)
  (let ((all-open-buffers-list (-non-nil (buffer-list))))
    (--first (string-equal file-name (buffer-file-name it))
             all-open-buffers-list)))

(defun omnisharp--get-current-buffer-contents ()
  (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)))

(defun omnisharp--log-reset ()
  "Kills the *omnisharp-log* buffer"
  (let ((log-buffer (get-buffer "*omnisharp-log*")))
    (if log-buffer
        (kill-buffer log-buffer))))

(defun omnisharp--log (single-or-multiline-log-string)
  "Writes message to the log."
  (when omnisharp-debug
    (message (concat "*omnisharp-log*: "
                     (format-time-string "[%H:%M:%S] ")
                     single-or-multiline-log-string)))

  (let ((log-buffer (get-buffer-create "*omnisharp-log*")))
    (save-window-excursion
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (insert (format-time-string "[%H:%M:%S] "))
        (insert single-or-multiline-log-string)
        (insert "\n")))))

(defun omnisharp--json-read-from-string (json-string
                                         &optional error-message)
  "Deserialize the given JSON-STRING to a lisp object. If
something goes wrong, return a pseudo-packet alist with keys
ServerMessageParseError and Message."
  (condition-case possible-error
      (json-read-from-string json-string)
    (error
     (when omnisharp-debug
       (omnisharp--log (format "omnisharp--json-read-from-string error: %s reading input %s"
                               possible-error
                               json-string)))
     (list (cons 'ServerMessageParseError
                 (or error-message "Error communicating to the OmniSharpServer instance"))
           (cons 'Message
                 json-string)))))

(defun omnisharp--replace-symbol-in-buffer-with (symbol-to-replace
                                                 replacement-string)
  "In the current buffer, replaces the given SYMBOL-TO-REPLACE
\(a string\) with REPLACEMENT-STRING."
  (search-backward symbol-to-replace)
  (replace-match replacement-string t t))

(defun omnisharp--insert-namespace-import (full-import-text-to-insert)
  "Inserts the given text at the top of the current file without
moving point."
  (save-excursion
    (beginning-of-buffer)
    (insert "using " full-import-text-to-insert ";")
    (newline)))

(defun omnisharp--current-word-or-empty-string ()
  (or (thing-at-point 'symbol)
      ""))

(defun omnisharp--t-or-json-false (val)
  (if val
      t
    :json-false))

(defun omnisharp--get-omnisharp-server-executable-command
  (solution-file-path &optional server-exe-file-path)
  (let* ((server-exe-file-path-arg (expand-file-name 
            (if (eq nil server-exe-file-path)
          omnisharp-server-executable-path
              server-exe-file-path)))
   (solution-file-path-arg (expand-file-name solution-file-path))
   (args (list server-exe-file-path-arg
         "-s"
         solution-file-path-arg)))
    (cond
     ((or (equal system-type 'cygwin) ;; No mono needed on cygwin or if using omnisharp-roslyn
          (equal system-type 'windows-nt)
          (not (s-ends-with? ".exe" server-exe-file-path-arg)))
      args)
     (t ; some kind of unix: linux or osx
      (cons "mono" args)))))

(defun omnisharp--update-buffer (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (omnisharp--wait-until-request-completed
   (omnisharp--send-command-to-server
    "updatebuffer"
    (omnisharp--get-request-object))))

(defun omnisharp--update-files-with-text-changes (file-name text-changes)
  (let ((file (find-file (omnisharp--convert-backslashes-to-forward-slashes
                          file-name))))
    (with-current-buffer file
      (-map 'omnisharp--apply-text-change-to-buffer text-changes))))

(defun omnisharp--apply-text-change-to-buffer (text-change
                                               &optional buffer)
  "Takes a LinePositionSpanTextChange and applies it to the
current buffer.

If this is used as a response handler, the call to the server
must be blocking (synchronous) so the user doesn't have time to
switch the buffer to some other buffer. That would cause the
changes to be applied to that buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (-let* (((&alist 'NewText new-text
                       'StartLine start-line
                       'StartColumn start-column
                       'EndLine end-line
                       'EndColumn end-column) text-change)
              ;; In emacs, the first column is 0. On the server, it's
              ;; 1. In emacs we always handle the first column as 0.
              (start-point (progn
                             (omnisharp--go-to-line-and-column
                              start-line
                              (- start-column 1))
                             (point)))
              (end-point (progn
                           (omnisharp--go-to-line-and-column
                            end-line
                            (- end-column 1))
                           (point))))

        (delete-region start-point end-point)
        (goto-char start-point)
        (insert (s-replace (kbd "RET") "" new-text))))))

(defun omnisharp--handler-exists-for-request (request-id)
  (--any? (= request-id (car it))
          (cdr (assoc :response-handlers omnisharp--server-info))))

(defun omnisharp--wait-until-request-completed (request-id
                                                &optional timeout-seconds)
  (setq timeout-seconds (or timeout-seconds 30))

  (let ((start-time (current-time))
        (process (cdr (assoc :process omnisharp--server-info))))
    (while (omnisharp--handler-exists-for-request request-id)
      (when (> (cadr (time-subtract (current-time) start-time))
               timeout-seconds)
        (progn
          (let ((msg (format "Request %s did not complete in %s seconds"
                             request-id timeout-seconds)))
            (omnisharp--log msg)
            (error msg))))
      (accept-process-output process 0.1)))
  request-id)

(defun omnisharp-builtin-completing-read (&rest args)
  "Default completing read. See `omnisharp-completing-read-function'"
  ;; e.g. ivy and helm don't need a case here, because they set
  ;; `completing-read-function' in their mode
  (let ((completing-read-variant (cond ((bound-and-true-p ido-mode) 'ido-completing-read)
                                       (t 'completing-read))))
    (apply completing-read-variant args)))

(defun omnisharp--completing-read (&rest args)
  "Mockable wrapper for completing-read.
The problem with mocking completing-read directly is that
sometimes the mocks are not removed when an error occurs. This renders
the developer's emacs unusable."
    (apply omnisharp-completing-read-function args))

(defun omnisharp--read-string (&rest args)
  "Mockable wrapper for read-string, see
`omnisharp--completing-read' for the explanation."
  (apply 'read-string args))

(defun omnisharp--mkdirp (dir)
  "Makes a directory recursively, similarly to a 'mkdir -p'."
  (let* ((absolute-dir (expand-file-name dir))
         (components (f-split absolute-dir)))
    (omnisharp--mkdirp-item (f-join (apply #'concat (-take 1 components))) (-drop 1 components))
    absolute-dir))

(defun omnisharp--mkdirp-item (dir remaining)
  "Makes a directory if not exists,
 and tries to do the same with the remaining components, recursively."
  (unless (f-directory-p dir)
    (f-mkdir dir))
  (unless (not remaining)
    (omnisharp--mkdirp-item (f-join dir (car (-take 1 remaining)))
                            (-drop 1 remaining))))

(defun omnisharp--project-root ()
  "Tries to resolve project root for current buffer. nil if no project root directory
was found. Uses projectile for the job, falling back to project.el."
  ;; use project root as a candidate (if we have projectile available)
  (cond ((require 'projectile nil 'noerror)
         (condition-case nil
             (projectile-project-root)
           (error nil)))
        ;; otherwise fall back to `project', introduced in Emacs 25.1 core
        ((require 'project nil 'noerror)
         (condition-case nil
             (let ((proj (cdr (project-current))))
               (when proj
                 (expand-file-name proj)))
           (error nil)))))

(defun omnisharp--buffer-contains-metadata()
  "Returns t if buffer is omnisharp metadata buffer."
  (or (boundp 'omnisharp--metadata-source)
      (s-starts-with-p "*omnisharp-metadata:" (buffer-name))))

(defun omnisharp--message (format-string &rest args)
  "Displays passed text using message function."
  (apply 'message (cons format-string args)))

(defun omnisharp--message-at-point (format-string &rest args)
  "Displays passed text at point using popup-tip function."
  (popup-tip (apply 'format (cons format-string args))))

(defun omnisharp--truncate-symbol-name (name trunc-length)
  "This attempts to truncate a fully-qualified dotnet symbol name to given length.
Basically, in case NAME is longer than TRUNC-LENGTH it will replace text in the middle
with ellipsis (...) so the result would fit into TRUNC-LENGTH.

It assumes the tail of NAME is more important than the beginning as that usually
has namespaces and parent class name."

  (if (< (length name) trunc-length)
      name
    (let* ((trunc-length (- trunc-length 3)) ; take ellipsis into account
           (trunc-1/4th (/ trunc-length 4))
           (head-len (max 0 (- trunc-length (* trunc-1/4th 3))))
           (tail-len (max 0 (- trunc-length head-len)))
           (head (substring name 0 head-len))
           (tail (substring name (- (length name) tail-len))))
      (concat head "..." tail))))

(provide 'omnisharp-utils)
