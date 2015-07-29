(defun omnisharp-format-on-keystroke (char)
  "Formats the current block as you type `;` or `}`.
 <return> support to come soon."
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
