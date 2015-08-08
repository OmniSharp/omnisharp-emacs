;; -- lexical-binding: t; --
(setq inhibit-modification-hooks nil)

(defun omnisharp--column-from-pos(pos)
  (+ 1 (save-excursion (goto-char pos) (current-column))))

(defun omnisharp--before-change-function (begin end)
  "Function attached to before-change-functions hook.
It saves the position of the text to be changed
These information will be used by omnisharp--after-changed-function."
  (setq omnisharp-before-change-begin begin)
  (setq omnisharp-before-change-end end))

(defun omnisharp--after-change-function (begin end leng-before)
  "Function attached to after-change-functions hook"
  (omnisharp--send-command-to-server-sync
   "changebuffer"
   (let* ((filename-tmp (or buffer-file-name ""))
          (text (buffer-substring-no-properties begin end))
          (params `((StartLine   . ,(line-number-at-pos omnisharp-before-change-begin))
                    (EndLine     . ,(line-number-at-pos omnisharp-before-change-end))
                    (StartColumn . ,(omnisharp--column-from-pos omnisharp-before-change-begin))
                    (EndColumn   . ,(omnisharp--column-from-pos omnisharp-before-change-end))
                    (NewText     . ,text))))

     (if (/= 0 (length filename-tmp))
         (cons `(FileName . ,filename-tmp)
               params)
       params))))

(provide 'omnisharp-update-buffer-actions)
