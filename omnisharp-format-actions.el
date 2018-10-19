;; -*- lexical-binding: t -*-

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


(defun omnisharp-fix-usings ()
  "Find usages for the symbol under point."
  (interactive)
  (let*((fixusings-request
         (->> (omnisharp--get-request-object)
              (cons `(WantsTextChanges . true))))
        (buffer (current-buffer)))

    (omnisharp--send-command-to-server-sync
     "fixusings"
     fixusings-request
     (lambda (fixusings-response) (omnisharp--fixusings-worker
                                   fixusings-response
                                   buffer)))))

(defun omnisharp--fixusings-worker (fixusings-response
                         buffer)
  (-if-let (error-message (cdr (assoc 'ErrorMessage fixusings-response)))
      (omnisharp--message error-message)
    (-let (((&alist 'AmbiguousResults quickfixes) fixusings-response))
      (if (> (length quickfixes) 0)
          (omnisharp--write-quickfixes-to-compilation-buffer
           quickfixes
           omnisharp--ambiguous-symbols-buffer-name
           omnisharp-ambiguous-results-header)))
    (-let (((&alist 'Changes text-changes) fixusings-response))
      (--map (omnisharp--apply-text-change-to-buffer it buffer)
             text-changes))))

(provide 'omnisharp-format-actions)
