
(defcustom omnisharp-server-executable-path (executable-find "omnisharp")  ; omnisharp-roslyn
  "Path to omnisharp-server. If its value is nil, search for the server in the exec-path"
  :type '(choice (const :tag "Not Set" nil) string))

(defun omnisharp--find-solution-files ()
  "Find solution files in parent directories. Returns a list
containing the directory and matching filenames, or nil if no
solution files were found."
  (let ((solutions nil))
    (when buffer-file-name
      (locate-dominating-file
       (file-name-directory buffer-file-name)
       (lambda (file)
         (-when-let (dir-files (directory-files file nil "\\.sln$"))
           (setq solutions (cons (file-name-as-directory file)
                                 dir-files))))))
    solutions))

(defun omnisharp--start-omnisharp-server-for-solution-in-parent-directory ()
  (unless (omnisharp--check-alive-status-worker)
    (-let [(directory file . rest) (omnisharp--find-solution-files)]
      (when directory
        (omnisharp-start-omnisharp-server
         (if (null rest) ; only one solution found
             (concat directory file)
           (read-file-name "Select solution for current file: "
                           directory
                           nil
                           t
                           file)))))))

;;;###autoload
(defun omnisharp-start-omnisharp-server (path-to-solution)
  "Starts an OmniSharpServer for a given path to a solution file or a directory"
  (interactive
   (list
    (-let [(directory filename . rest) (omnisharp--find-solution-files)]
      (read-file-name "Start OmniSharpServer.exe for solution: "
                      directory
                      nil
                      t
                      filename))))
  (setq BufferName "*Omni-Server*")
  (if (equal nil omnisharp-server-executable-path)
      (error "Could not find the OmniSharpServer. Please set the variable omnisharp-server-executable-path to a valid path")
    (if (omnisharp--valid-solution-path-p path-to-solution)
        (progn
          (if (string= (file-name-extension path-to-solution) "sln")
              (message (format "Starting OmniSharpServer for solution file: %s" path-to-solution))
            (message (format "Starting OmniSharpServer using folder mode in: %s" path-to-solution)))

          (when (not (eq nil (get-buffer BufferName)))
            (kill-buffer BufferName))

          (setq omnisharp--server-info
                (make-omnisharp--server-info
                 ;; use a pipe for the connection instead of a pty
                 (let ((process-connection-type nil)
                       (process (start-process
                                 "Omni-Server" ; process name
                                 "Omni-Server" ; buffer name
                                 omnisharp-server-executable-path
                                 ;; remaining arguments
                                 ;; "-v"
                                 "--stdio" "-s" path-to-solution)))
                   (set-process-filter process 'omnisharp--handle-server-message)
                   process))))
      (error (format "Path does not lead to a valid solution path: %s" path-to-solution)))))

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
