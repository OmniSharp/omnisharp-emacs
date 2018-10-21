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


(defun omnisharp--prepare-solution-errors-buffer ()
  "Makes a new *omnisharp-solution-errors* buffer or creates a new one
and enabled compilation-mode on the buffer."
  (let ((existing-buffer (get-buffer "*omnisharp-solution-errors*"))
        (solution-root-dir (cdr (assoc :project-root omnisharp--server-info))))
    (if existing-buffer
        (progn
          (with-current-buffer existing-buffer
            (setq buffer-read-only nil)
            (erase-buffer)
            (setq buffer-read-only t)
            (setq default-directory solution-root-dir))
          existing-buffer)
      (let ((buffer (get-buffer-create "*omnisharp-solution-errors*")))
        (with-current-buffer buffer
          (setq default-directory solution-root-dir)
          (compilation-mode)
          buffer)))))

(defun omnisharp-solution-errors (&optional errors-only)
  "Opens a new buffer *omnisharp-solution-errors* (or updates existing one)
with solution errors. This is the same error list as emitted by flycheck only
for the whole solution."
  (interactive "P")
  (if omnisharp--server-info
      ;; we want to show solutions error buffer early
      (let ((buffer (omnisharp--prepare-solution-errors-buffer))
            (time-started (current-time)))
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (insert "omnisharp-solution-errors: waiting for omnisharp server ...")
          (setq buffer-read-only t))
        (display-buffer buffer)

        ;; actually invoke the request
        (omnisharp--send-command-to-server
         "codecheck"
         `((FileName . nil))
         (lambda (response)
           (let ((buffer (omnisharp--prepare-solution-errors-buffer))
                 (error-list (omnisharp--vector-to-list (cdr (assoc 'QuickFixes response))))
                 (time-elapsed-seconds (time-to-seconds (time-subtract (current-time) time-started))))
             (display-buffer buffer)
             (save-window-excursion
               (with-current-buffer buffer
                 (setq buffer-read-only nil)
                 (dolist (item error-list)
                   (let ((log-level (if (string= (cdr (assoc 'LogLevel item)) "Error") "error" "warning"))
                         (filename (string-remove-prefix (concat default-directory "/") (cdr (assoc 'FileName item))))
                         (line (cdr (assoc 'Line item)))
                         (col (cdr (assoc 'Column item)))
                         (text (cdr (assoc 'Text item)))
                         (id (or (cdr (assoc 'Id item)) "CS0000")))
                     (if (or (not errors-only)
                             (string= log-level "error"))
                         (insert (concat filename
                                         "(" (number-to-string line) "," (number-to-string col) "): "
                                         log-level " " id ": "
                                         text
                                         "\n")))))
                 (insert (concat "\nomnisharp-solution-errors: finished, "
                                 "took " (number-to-string time-elapsed-seconds) " seconds to complete.\n"))
                 (setq buffer-read-only t)))))))))

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

                  (let* ((chosen-action-name (omnisharp--completing-read
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
