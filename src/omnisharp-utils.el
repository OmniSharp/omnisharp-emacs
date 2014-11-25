
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

(provide 'omnisharp-utils)
