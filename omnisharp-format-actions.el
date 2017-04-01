;; -*- lexical-binding: t -*-

(defun omnisharp-code-format-entire-file ()
  "Format the code in the current file. Replaces the file contents
with the formatted result."
  (interactive)
  (omnisharp--send-command-to-server-sync
   "codeformat"
   (omnisharp--get-request-object)
   (let ((current-file (buffer-file-name)))
     (-lambda ((&alist 'Buffer new-buffer-contents))
              (omnisharp--set-buffer-contents-to
               current-file
               new-buffer-contents
               (line-number-at-pos)
               (omnisharp--current-column))))))

(defun omnisharp-code-format-region ()
  "Format the code in the current region."
  (interactive)
  (let ((request (-concat (omnisharp--get-request-object)
                          `((EndLine . ,(omnisharp--region-end-line))
                            (EndColumn . ,(omnisharp--region-end-column)))))
        (buffer (current-buffer)))
    ;; The server refers to the start Line and Column in this
    ;; case. Replace the ones that refer to point
    (setcdr (assoc 'Line request) (omnisharp--region-start-line))
    (setcdr (assoc 'Column request) (omnisharp--region-start-column))

    (if (not mark-active)
        (message "Need to select something before trying to format the region")
      (omnisharp--send-command-to-server-sync
       "formatRange"
       request
       (-lambda ((&alist 'Changes text-changes))
                (--map (omnisharp--apply-text-change-to-buffer it buffer)
                       text-changes))))))

(defun omnisharp-format-on-keystroke (char)
  "Formats the current block as you type `;` or `}`.
 <return> support to come soon (via server fix))."
  (interactive)
  (insert char)

  (-let ((request (-concat (omnisharp--get-request-object)
                           `((Character . ,char))))
         (buffer (current-buffer)))

    (omnisharp--send-command-to-server-sync
     "formatAfterKeystroke"
     request
     (-lambda ((&alist 'Changes text-changes))
       (--map (omnisharp--apply-text-change-to-buffer it buffer)
              text-changes)))))

(provide 'omnisharp-format-actions)
