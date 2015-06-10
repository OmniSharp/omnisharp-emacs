
(defun omnisharp-get-host ()
  "Makes sure omnisharp-host is ended by / "
  (if (string= (substring omnisharp-host -1 ) "/")
      omnisharp-host
    (concat omnisharp-host "/")))

(defun omnisharp--get-api-url (api-name)
  (concat (omnisharp-get-host) api-name))

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
  (let ((filename (cdr (assoc 'FileName json-result-single-element)))
        (text (cdr (assoc 'Text json-result-single-element)))
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
  (omnisharp--find-file-possibly-in-other-window
   filename-for-buffer nil) ; not in other-window

  ;; Default values are the ones in the buffer that is visiting
  ;; filename-for-buffer.
  (setq result-point-line
        (or result-point-line (line-number-at-pos)))
  (setq result-point-column
        (or result-point-column (omnisharp--current-column)))

  (save-buffer)
  (erase-buffer)
  (insert new-buffer-contents)

  ;; Hack. Puts point where it belongs.
  (omnisharp-go-to-file-line-and-column-worker
   result-point-line result-point-column filename-for-buffer))

(defun omnisharp--current-column ()
  "Returns the current column, converting tab characters in a way that
the OmniSharp server understands."
  (let ((tab-width 1))
    (current-column)))

(defun omnisharp--buffer-exists-for-file-name (file-name)
  (let ((all-open-buffers-list
         (-map 'buffer-file-name (-non-nil (buffer-list)))))
    (--any? (string-equal file-name it)
            all-open-buffers-list)))

(defun omnisharp--get-current-buffer-contents ()
  (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)))

(defun omnisharp-post-message-curl (url &optional params)
  "Post json stuff to url with --data set to given params. Return
result."
  (let ((curl-command-plist
         (omnisharp--get-curl-command url params)))
    (with-temp-buffer
      (apply 'call-process
             (plist-get curl-command-plist :command)
             nil ;; infile
             (buffer-name);; destination
             nil ;; display (no specialities needed)
             ;; these are just args
             (plist-get curl-command-plist :arguments))
      (buffer-string))))

(defun omnisharp--get-curl-command (url params)
  "Returns a command that may be used to communicate with the API via
the curl program. Depends on the operating system."
  (let ((curl-command
         (if (equal system-type 'windows-nt)
             (omnisharp--get-curl-command-windows-with-tmp-file url params)
           (omnisharp--get-curl-command-unix url params))))
    (when omnisharp-debug
      (omnisharp--log-curl-command curl-command))
    curl-command))

(defun omnisharp--log-curl-command (curl-command)
  (omnisharp--log (prin1-to-string curl-command)))

(defun omnisharp--log (single-or-multiline-log-string)
  (let* ((log-buffer (get-buffer-create "*omnisharp-debug*"))
         (iso-format-string "%Y-%m-%dT%T%z")
         (timestamp-and-log-string
          (format-time-string iso-format-string (current-time))))
    (with-current-buffer log-buffer
      (end-of-buffer)
      (insert "\n\n\n")
      (insert (concat timestamp-and-log-string
                      "\n"
                      single-or-multiline-log-string))
      (insert "\n"))))

(defun omnisharp--get-curl-command-arguments-string-for-api-name
  (params api-name)
  "Returns the full command to call curl with PARAMS for the api API-NAME.
Example: when called with \"getcodeactions\", returns
\"curl (stuff) http://localhost:2000/getcodeactions (stuff)\"
with \"stuff\" set to sensible values."
  (let ((command-plist
         (omnisharp--get-curl-command
          (concat (omnisharp-get-host) api-name)
          params)))
    (plist-get command-plist :arguments)))

(defun omnisharp--get-curl-command-unix (url params)
  "Returns a command using plain curl that can be executed to
communicate with the API."
  `(:command
    ,omnisharp--curl-executable-path
    :arguments
    ("--ipv4" "--silent" "-H" "Content-type: application/json"
     "--data"
     ,(json-encode params)
     ,url)))

(defun omnisharp--get-curl-command-windows-with-tmp-file (url params)
  "Basically: put PARAMS to file, then create a curl command to the
api at URL using that file as the parameters."
  ;; TODO could optimise: short buffers need not be written to tmp
  ;; files.
  (omnisharp--write-json-params-to-tmp-file
   omnisharp--windows-curl-tmp-file-path
   (json-encode params))
  (let ((path-with-curl-prefix
         (concat "@"
                 omnisharp--windows-curl-tmp-file-path
                 )))
    `(:command ,omnisharp--curl-executable-path
               :arguments
               ("--noproxy" "localhost"
                "--silent" "-H" "Content-type: application/json"
                "--data-binary"
                ;; @ specifies a file path to curl
                ,path-with-curl-prefix
                ,url))))

(defun omnisharp--write-json-params-to-tmp-file
  (target-path stuff-to-write-to-file)
  "Deletes the file when done."
  (with-temp-file target-path
    (insert stuff-to-write-to-file)))

(defun omnisharp--json-read-from-string (json-string
                                         &optional error-message)
  "Deserialize the given JSON-STRING to a lisp object. If
something goes wrong, return a human-readable warning."
  (condition-case nil
      (json-read-from-string json-string)
    (error
     (when omnisharp-debug
       (omnisharp--log (concat "omnisharp--json-read-from-string error: "
                               (prin1-to-string json-string))))
     (or error-message
         "Error communicating to the OmniSharpServer instance"))))

(defun omnisharp-post-message-curl-as-json (url &optional params)
  (omnisharp--json-read-from-string
   (omnisharp-post-message-curl url params)))

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

(defun omnisharp--server-process-sentinel (process event)
  (if (string-match "^exited abnormally" event)
      (error (concat "OmniSharp server process " event))))

(defun omnisharp--valid-solution-path-p (path-to-solution)
  (or (string= (file-name-extension path-to-solution) "sln")
      (file-directory-p path-to-solution)))

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


(provide 'omnisharp-utils)
