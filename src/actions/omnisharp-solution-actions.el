;; -*- lexical-binding: t -*-

;; TODO create omnisharp-add-to-solution that lets user choose which
;; file to add.
(defun omnisharp-add-to-solution-current-file ()
  (interactive)
  (let ((params (omnisharp--get-request-object)))
    (omnisharp-add-to-solution-worker params)
    (message "Added %s to the solution."
             (cdr (assoc 'FileName params)))))

(defun omnisharp-add-to-solution-dired-selected-files ()
  "Add the files currently selected in dired to the current solution."
  (interactive)
  (let ((selected-files (dired-get-marked-files)))
    (--each selected-files
      (let ((params
             (cons `(FileName . ,it)
                   (omnisharp--get-request-object))))
        (omnisharp-add-to-solution-worker params))
      (message "Added %s files to the solution."
               (prin1-to-string (length selected-files))))))

(defun omnisharp-add-to-solution-worker (params)
  "TODO"
  ;; TODO report results somehow
  (omnisharp-post-message-curl
   (concat (omnisharp-get-host) "addtoproject")
   params))

(defun omnisharp-remove-from-project-current-file ()
  (interactive)
  (let ((params (omnisharp--get-request-object)))
    (omnisharp-remove-from-project-current-file-worker params)
    (message "Removed %s from the solution."
             (cdr (assoc 'FileName params)))))

(defun omnisharp-remove-from-project-dired-selected-files ()
  "Remove the files currently selected in dired from the current
solution."
  (interactive)
  (let ((selected-files (dired-get-marked-files)))
    (--each selected-files
      (let ((params
             (cons `(FileName . ,it)
                   (omnisharp--get-request-object))))
        (omnisharp-remove-from-project-current-file-worker params))
      (message "Removed %s files from the project."
               (prin1-to-string (length selected-files))))))

(defun omnisharp-remove-from-project-current-file-worker (params)
  (omnisharp-post-message-curl
   (concat (omnisharp-get-host) "removefromproject")
   params))

(defun omnisharp-add-reference ()
  (interactive)
  (let* ((path-to-ref-file-to-add
          (ido-read-file-name "Add reference to (dll / project): "
                              nil ;; start in current dir
                              nil ;; no default filename
                              t ;; only allow existing files

                              ;; TODO use a predicate for filtering
                              ;; dll and csproj files
                              ))
         (tmp-params (omnisharp--get-request-object))
         (params (cl-pushnew `(Reference . ,path-to-ref-file-to-add)
			     tmp-params)))
    (omnisharp-add-reference-worker params)))

(defun omnisharp-add-reference-worker (params)
  (omnisharp-post-message-curl-as-json
   (concat (omnisharp-get-host) "addreference")
   params))

(defun omnisharp-build-in-emacs ()
  "Build the current solution in a non-blocking fashion inside emacs.
Uses the standard compilation interface (compile)."
  (interactive)
  ;; Build command contains backslashes on Windows systems. Work
  ;; around this by using double backslashes. Other systems are not
  ;; affected.
  (let ((build-command (omnisharp--fix-build-command-if-on-windows
                        (omnisharp-get-build-command))))
    (omnisharp--recognize-mono-compilation-error-format)
    (compile build-command)
    (add-to-list 'compile-history build-command)))

(defun omnisharp-run-code-action-refactoring ()
  "Gets a list of refactoring code actions for the current editor
position and file from the server. Asks the user what kind of
refactoring they want to run. Then runs the action.

Saves the current file before doing anything else. This is so that the
user is less likely to lose data."
  (interactive)
  (save-buffer)
  (let ((get-code-actions-request (omnisharp--get-code-actions-request)))
    (omnisharp--send-command-to-server
     "v2/getcodeactions"
     get-code-actions-request
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
                     get-code-actions-request))))))))

(defun omnisharp-run-code-action-refactoring-worker (chosen-action-identifier
                                                     get-code-actions-request)
  (let* ((run-code-action-request
          (-concat get-code-actions-request
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

;; TODO
;;;###autoload
(defun omnisharp-fix-code-issue-at-point ()
  (interactive)
  ;; TTODOO lololo
  )

(defun omnisharp--fix-code-issue-at-point-worker (get-code-actions-response)

  ;; The api uses a RunCodeActionRequest but currently ignores the
  ;; CodeAction property in that class
  (let ((run-code-action-request
         (cons `(CodeAction . 0) get-code-actions-response)))
    (omnisharp-post-message-curl-as-json
     (concat (omnisharp-get-host)
             "fixcodeissue")
     run-code-action-request)))

(defun omnisharp-get-build-command ()
  "Retrieve the shell command to build the current solution."
  (omnisharp-post-message-curl
   (concat (omnisharp-get-host) "buildcommand")
   nil))

(defun omnisharp--recognize-mono-compilation-error-format ()
  "Makes Emacs recognize the mono compiler errors as clickable
compilation buffer elements."
  (add-to-list 'compilation-error-regexp-alist
               '(" in \\(.+\\):\\([0-9]+\\)" 1 2)))

(defun omnisharp--fix-build-command-if-on-windows (command)
  "Fixes the build command gotten via omnisharp-get-build-command.
See function definition for an example.

If not on windows, returns COMMAND unchanged."

  ;; Example input without fix:
  ;; C:\Windows\Microsoft.NET\Framework64\v4.0.30319\Msbuild.exe /m /nologo
  ;;     /v:q /property:GenerateFullPaths=true
  ;;     \"c:/Projects/foo/foo.sln\"

  ;; This changes that to this:
  ;; C:/Windows/Microsoft.NET/Framework64/v4.0.30319/Msbuild.exe
  ;;     //m //nologo //v:q //property:GenerateFullPaths=true
  ;;     \"c://Projects//bowsville_freelancer//src//Bowsville.Freelancer.sln\"
  ;;
  ;; ^ this works :)


  (if (equal system-type 'windows-nt)
      ;; Compiler path fix. C:\Path is interpreted as C:Path
      (omnisharp--convert-backslashes-to-forward-slashes
       command)

    ;; Not on windows. Do not change.
    command))

(defun omnisharp--convert-backslashes-to-forward-slashes
  (string-to-convert)
  "Converts the given STRING-TO-CONVERT's backslashes to forward
slashes."
  (replace-regexp-in-string "\\\\" "/" string-to-convert))

(defun omnisharp--convert-slashes-to-double-slashes (command)
  (replace-regexp-in-string "/" "//" command))

(provide 'omnisharp-solution-actions)
