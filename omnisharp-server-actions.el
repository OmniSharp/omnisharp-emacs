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


(defun omnisharp--start-omnisharp-server (no-autodetect)
  "Actual implementation for autoloaded omnisharp-start-omnisharp-server.

Will query user for a path to project/solution file to start the server with."
  (let ((server-executable-path (omnisharp--resolve-omnisharp-server-executable-path))
        (project-root (omnisharp--project-root)))
    (if server-executable-path
        (if (and (not no-autodetect)
                 project-root
                 (file-directory-p project-root))
            (omnisharp--do-server-start project-root)
          (let ((project-root (read-directory-name "Project root to use with OmniSharp: ")))
            (if (file-directory-p project-root)
                (omnisharp--do-server-start project-root)
              (error (format "Path does not lead to a directory: %s" project-root))))))))

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
