;; -- lexical-binding: t; --
(defun omnisharp--line-column-from-pos(pos)
  (save-excursion (goto-char pos)
                  (cons (line-number-at-pos pos) (+ 1 (current-column)))))

(defun omnisharp--before-change-function (begin end)
  "Function attached to before-change-functions hook.
It saves the position of the text to be changed
These information will be used by omnisharp--after-changed-function."
  (setq omnisharp--before-change-begin (omnisharp--line-column-from-pos begin))
  (setq omnisharp--before-change-end (omnisharp--line-column-from-pos end)))

(defun omnisharp--after-change-function (begin end leng-before)
  "Function attached to after-change-functions hook"

  "If the change is too large to send via the pipe, send it via disk instead."
  (if (>= (- end begin) 4000)
      (omnisharp--save-and-update-server)

    "else, send the change via the pipe"
    (omnisharp--send-command-to-server-sync
     "changebuffer"
     (let* ((filename-tmp (or buffer-file-name ""))
            (text (buffer-substring-no-properties begin end))
            (params `((StartLine   . ,(car omnisharp--before-change-begin))
                      (EndLine     . ,(car omnisharp--before-change-end))
                      (StartColumn . ,(cdr omnisharp--before-change-begin))
                      (EndColumn   . ,(cdr omnisharp--before-change-end))
                      (NewText     . ,text))))

       (if (/= 0 (length filename-tmp))
           (cons `(FileName . ,filename-tmp)
                 params)
         params)))))

(defun omnisharp--save-and-update-server ()
  (save-buffer)
  (omnisharp--update-from-disk))

(defun omnisharp--update-from-disk ()
  (omnisharp--send-command-to-server
   "updatebuffer"
   (cons `(FromDisk . ,t) (omnisharp--get-request-object))))

(provide 'omnisharp-update-buffer-actions)
