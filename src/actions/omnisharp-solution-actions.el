;; -*- lexical-binding: t -*-

;; TODO create omnisharp-add-to-solution that lets user choose which
;; file to add.
(defun omnisharp-add-to-solution-current-file ()
  (interactive)
  (let ((params (omnisharp--get-request-object)))
    (omnisharp-add-to-solution-worker params)
    (message "Added %s to the solution."
             (omnisharp--get-filename params))))

(defun omnisharp-add-to-solution-dired-selected-files ()
  "Add the files currently selected in dired to the current solution."
  (interactive)
  (let ((selected-files (dired-get-marked-files)))
    (--each selected-files
      (let ((params
             (cons (omnisharp--to-filename it)
                   (omnisharp--get-request-object))))
        (omnisharp-add-to-solution-worker params))
      (message "Added %s files to the solution."
               (prin1-to-string (length selected-files))))))

(defun omnisharp-run-code-action-refactoring ()
  "Gets a list of refactoring code actions for the current editor
position and file from the server. Asks the user what kind of
refactoring they want to run. Then runs the action."
  (interactive)
  (let ((code-actions-request (omnisharp--get-code-actions-request)))
    (omnisharp--send-command-to-server
     "v2/getcodeactions"
     code-actions-request
     (-lambda ((&alist 'CodeActions code-actions))
              (let* ((code-actions (omnisharp--vector-to-list code-actions))
                     (action-names (--map (cdr (assoc 'Name it))
                                          code-actions)))
                (if (<= (length action-names) 0)
                    (message "No refactorings available at this position.")

                  (let* ((chosen-action-name (omnisharp--ido-completing-read
                                              "Run code action: "
                                              action-names))
                         (chosen-action
                          (--first (equal (cdr (assoc 'Name it))
                                          chosen-action-name)
                                   code-actions)))

                    (omnisharp-run-code-action-refactoring-worker
                     (cdr (assoc 'Identifier chosen-action))
                     code-actions-request))))))))

(defun omnisharp-run-code-action-refactoring-worker (chosen-action-identifier
                                                     code-actions-request)
  (let* ((run-code-action-request
          (-concat code-actions-request
                   `((Identifier . ,chosen-action-identifier)
                     (WantsTextChanges . t)))))
    (omnisharp--send-command-to-server-sync
     "v2/runcodeaction"
     run-code-action-request
     (-lambda ((&alist 'Changes modified-file-responses))
              (-map #'omnisharp--apply-text-changes
                    modified-file-responses)))))

(defun omnisharp--get-code-actions-request ()
  "Returns an ICodeActionRequest for the current buffer position"
  (if (region-active-p)
      (-concat (omnisharp--get-request-object)
               `((Selection . ((Start . ((Line . ,(omnisharp--region-start-line))
                                         (Column . ,(omnisharp--region-start-column))))
                               (End . ((Line . ,(omnisharp--region-end-line))
                                       (Column . ,(omnisharp--region-end-column))))))))
    (omnisharp--get-request-object)))

(defun omnisharp--convert-backslashes-to-forward-slashes
  (string-to-convert)
  "Converts the given STRING-TO-CONVERT's backslashes to forward
slashes."
  (replace-regexp-in-string "\\\\" "/" string-to-convert))

(provide 'omnisharp-solution-actions)
