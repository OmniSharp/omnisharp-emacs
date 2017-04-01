(defcustom omnisharp-server-executable-path (executable-find "OmniSharp")  ; omnisharp-roslyn
  "Path to OmniSharp. If its value is nil, search for the server in the exec-path"
  :type '(choice (const :tag "Not Set" nil) string))

(defun omnisharp--do-server-start (path-to-project)
  (message (format "omnisharp: Starting OmniSharpServer using project folder/solution file: %s" path-to-project))
  (message "omnisharp: using the server at: %s" omnisharp-server-executable-path)

  ;; Save all csharp buffers to ensure the server is in sync"
  (save-some-buffers t (lambda () (string-equal (file-name-extension (buffer-file-name)) "cs")))

  (setq omnisharp--last-project-path path-to-project)

  ;; this can be set by omnisharp-reload-solution to t
  (setq omnisharp--restart-server-on-stop nil)

  (setq omnisharp--server-info
        (make-omnisharp--server-info
         ;; use a pipe for the connection instead of a pty
         (let ((process-connection-type nil))
           (-doto (start-process
                   "OmniServer" ; process name
                   "OmniServer" ; buffer name
                   omnisharp-server-executable-path
                   "--stdio" "-s" (omnisharp--path-to-server (expand-file-name path-to-project)))
             (set-process-filter 'omnisharp--handle-server-message)
             (set-process-sentinel (lambda (process event)
                                     (when (memq (process-status process) '(exit signal))
                                       (message "omnisharp: OmniSharp server terminated")
                                       (setq omnisharp--server-info nil)
                                       (if omnisharp--restart-server-on-stop
                                           (omnisharp--do-server-start omnisharp--last-project-path))))))))))

;;;###autoload
(defun omnisharp-start-omnisharp-server (path-to-project)
  "Starts an OmniSharp server for a given path to a project file or a directory"
  (interactive "GStart OmniSharp for project folder or solution file: ")
  (unless (bound-and-true-p omnisharp-server-executable-path)
    (error "Could not find the OmniSharp executable. Please set the variable omnisharp-server-executable-path to a valid path"))

  (if (or (file-directory-p path-to-project)
          (file-exists-p path-to-project))
      (omnisharp--do-server-start path-to-project)
    (error (format "Path does not lead to a valid project folder or solution file path: %s" path-to-project))))

;;;###autoload
(defun omnisharp-stop-server ()
  "Stops Omnisharp server if running."
  (interactive)
  (unless (equal nil omnisharp--server-info)
    (kill-process (cdr (assoc :process omnisharp--server-info)))))

;;;###autoload
(defun omnisharp-reload-solution ()
  "Restarts omnisharp server on solution last loaded"
  (interactive)

  (if (and (not (equal nil omnisharp--last-project-path))
           (not (equal nil omnisharp--server-info)))
      (progn 
        (setq omnisharp--restart-server-on-stop t)
        (kill-process (cdr (assoc :process omnisharp--server-info))))
    (message "Cannot reload project in Omnisharp - no project previously loaded")))

;;;###autoload
(defun omnisharp-check-alive-status ()
  "Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration is
alive.
\"Alive\" means it is running and not stuck. It also means the connection
to the server is functional - I.e. The user has the correct host and
port specified."
  (interactive)
  (omnisharp--send-command-to-server
   "checkalivestatus"
   nil
   #'omnisharp--check-alive-status-worker))

(defun omnisharp--check-alive-status-worker (alive?)
  (if alive?
      (message "Server is alive and well. Happy coding!")
    (message "Server is not alive")))

;;;###autoload
(defun omnisharp-check-ready-status ()
  "Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration has
finished loading the solution."
  (interactive)
  (omnisharp--send-command-to-server
   "checkreadystatus"
   nil
   (lambda (ready?)
     (if ready?
         (message "Server is ready")
       (message "Server is not ready yet")))))

(provide 'omnisharp-server-actions)
