;; -*- lexical-binding: t; -*-

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


(defun omnisharp--resolve-omnisharp-server-executable-path ()
    "Attempts to resolve a path to local executable for the omnisharp-roslyn server.
Will return `omnisharp-server-executable-path` override if set, otherwise will attempt
to use server installed via `omnisharp-install-server`.

 Failing all that an error message will be shown and nil returned."
    (if omnisharp-server-executable-path
        omnisharp-server-executable-path
      (let ((server-installation-path (omnisharp--server-installation-path)))
        (if server-installation-path
            server-installation-path
          (progn
            (message "omnisharp: No omnisharp server could be found.")
            (message (concat "omnisharp: Please use M-x 'omnisharp-install-server' or download server manually"
                             " as detailed in https://github.com/OmniSharp/omnisharp-emacs/blob/master/doc/server-installation.md"))
            nil)))))

(defun omnisharp--do-server-start (path-to-project server-executable-path)
  (message (format "omnisharp: Starting OmniSharpServer using project folder/solution file: %s" path-to-project))
  (message "omnisharp: using the server at: %s" server-executable-path)

  ;; Save all csharp buffers to ensure the server is in sync"
  (save-some-buffers t (lambda () (string-equal (file-name-extension (buffer-file-name)) "cs")))

  (setq omnisharp--last-project-path path-to-project)

  ;; this can be set by omnisharp-reload-solution to t
  (setq omnisharp--restart-server-on-stop nil)

  (setq omnisharp--server-info
        (make-omnisharp--server-info
         ;; use a pipe for the connection instead of a pty
         (omnisharp--setq-and-restore
          process-connection-type nil

          (-doto (start-process
                  "OmniServer" ; process name
                  "OmniServer" ; buffer name
                  server-executable-path
                  "--stdio" "-s" (omnisharp--path-to-server (expand-file-name path-to-project)))
            (lambda (process) (buffer-disable-undo (process-buffer process)))
            (set-process-filter 'omnisharp--handle-server-message)
            (set-process-sentinel (lambda (process event)
                                    (when (memq (process-status process) '(exit signal))
                                      (message "omnisharp: OmniSharp server terminated")
                                      (setq omnisharp--server-info nil)
                                      (if omnisharp--restart-server-on-stop
                                          (omnisharp--do-server-start
                                           omnisharp--last-project-path
                                           server-executable-path))))))))))

(defun omnisharp--start-omnisharp-server (path-to-project)
  "Actual implementation for autoloaded omnisharp-start-omnisharp-server"
  (let ((server-executable-path (omnisharp--resolve-omnisharp-server-executable-path)))
    (if server-executable-path
        (if (or (file-directory-p path-to-project)
                (file-exists-p path-to-project))
            (omnisharp--do-server-start path-to-project server-executable-path)
          (error (format "Path does not lead to a valid project folder or solution file path: %s" path-to-project))))))

(defun omnisharp--stop-server ()
  "Actual implementation for autoloaded omnisharp-stop-server"
  (unless (equal nil omnisharp--server-info)
    (kill-process (cdr (assoc :process omnisharp--server-info)))))

(defun omnisharp--reload-solution ()
  "Actual implementation for autoloaded omnisharp-reload-solution"
  (if (and (not (equal nil omnisharp--last-project-path))
           (not (equal nil omnisharp--server-info)))
      (progn 
        (setq omnisharp--restart-server-on-stop t)
        (kill-process (cdr (assoc :process omnisharp--server-info))))
    (message "Cannot reload project in Omnisharp - no project previously loaded")))

(defun omnisharp--check-alive-status ()
  "Actual implementation for autoloaded omnisharp-check-alive-status"
  (omnisharp--send-command-to-server
   "checkalivestatus"
   nil
   #'omnisharp--check-alive-status-worker))

(defun omnisharp--check-alive-status-worker (alive?)
  (if alive?
      (message "Server is alive and well. Happy coding!")
    (message "Server is not alive")))

(defun omnisharp--check-ready-status ()
  "Actual implementation for autoloaded omnisharp--check-ready-status"
  (omnisharp--send-command-to-server
   "checkreadystatus"
   nil
   (lambda (ready?)
     (if ready?
         (message "Server is ready")
       (message "Server is not ready yet")))))

(provide 'omnisharp-server-actions)
