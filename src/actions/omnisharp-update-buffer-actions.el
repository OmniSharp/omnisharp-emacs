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
  (when (omnisharp--buffer-is-csharp-file)
    ;; If the change is too large to send via the pipe, send it via disk instead.
    (if (>= (- end begin) 4000)
        (omnisharp--save-and-update-server)

      ;; else, send the change via the pipe
      (let* ((text (buffer-substring-no-properties begin end)))
        (omnisharp--send-change-buffer-request
         omnisharp--before-change-begin omnisharp--before-change-end text)))))

(defun omnisharp--send-change-buffer-request (begin end text)
  (omnisharp--send-command-to-server-sync
   "changebuffer"
   (let* ((filename-tmp (or buffer-file-name ""))
          (params `((StartLine   . ,(car begin))
                    (EndLine     . ,(car end))
                    (StartColumn . ,(cdr begin))
                    (EndColumn   . ,(cdr end))
                    (NewText     . ,text))))

     (if (/= 0 (length filename-tmp))
         (cons `(FileName . ,filename-tmp)
               params)
       params))))

(defun omnisharp--yas-skip-and-clear (orig-fun &rest args)
  (let* ((field (car args))
         (begin (yas--field-start field))
         (end (yas--field-end field))
         (begin-line-col (omnisharp--line-column-from-pos begin))
         (end-line-col (omnisharp--line-column-from-pos end)))
    (omnisharp--send-change-buffer-request begin-line-col end-line-col ""))
  (apply orig-fun args))


(advice-add 'yas--skip-and-clear :around #'omnisharp--yas-skip-and-clear)

(defun omnisharp--save-and-update-server ()
  (save-buffer)
  (omnisharp--update-from-disk))

(defun omnisharp--update-from-disk ()
  (when (omnisharp--buffer-is-csharp-file)
    (omnisharp--send-command-to-server
     "updatebuffer"
     (cons `(FromDisk . ,t) (omnisharp--get-request-object)))))

(defun omnisharp--buffer-is-csharp-file ()
  "Returns true if the current buffer has a .cs extension"
  (string= "cs"
           (file-name-extension
            (buffer-file-name))))

(defadvice delete-region (before omnisharp--send-delete activate)
  (setq inhibit-modification-hooks nil))

(provide 'omnisharp-update-buffer-actions)
