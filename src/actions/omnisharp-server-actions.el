
;; Path to the server
(defcustom omnisharp-server-executable-path (executable-find "OmniSharp.exe")
  "Path to OmniSharpServer. If its value is nil, search for the server in the exec-path"
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
      (if directory
          (omnisharp-start-omnisharp-server
           (if (null rest) ; only one solution found
               (concat directory file)
             (read-file-name "Select solution for current file: "
                             directory
                             nil
                             t
                             file)))
        (message "Solution not found")
        (omnisharp-start-omnisharp-server
         (file-name-directory buffer-file-name))))))

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

  (when (not (and omnisharp-server-executable-path (file-executable-p omnisharp-server-executable-path)))
    (error "Could not find the OmniSharpServer. Please set the variable omnisharp-server-executable-path to a valid path"))
  (when (not (omnisharp--valid-solution-path-p path-to-solution))
    (error (format "Path does not lead to a solution file: %s" path-to-solution)))

  (if (string= (file-name-extension path-to-solution) "sln")
      (message (format "Starting OmniSharpServer for solution file: %s" path-to-solution))
    (message (format "Starting OmniSharpServer using folder mode in: %s" path-to-solution)))

  (when (not (eq nil (get-buffer BufferName)))
    (kill-buffer BufferName))
  (let ((process (apply
                  'start-process
                  "Omni-Server"
                  (get-buffer-create BufferName)
                  (omnisharp--get-omnisharp-server-executable-command path-to-solution))))
    (set-process-sentinel process 'omnisharp--server-process-sentinel)
    (unless omnisharp-debug ;; ignore process output if debug flag not set
      (set-process-filter process (lambda (process string))))))

;;;###autoload
(defun omnisharp-check-alive-status ()
  "Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration is
alive.
\"Alive\" means it is running and not stuck. It also means the connection
to the server is functional - I.e. The user has the correct host and
port specified."
  (interactive)
  (if (omnisharp--check-alive-status-worker)
      (message "Server is alive and well. Happy coding!")
    (message "Server is not alive")))

(defun omnisharp--check-alive-status-worker ()
  (let ((result (omnisharp-post-message-curl-as-json
                 (concat (omnisharp-get-host) "checkalivestatus"))))
    (eq result t)))

;;;###autoload
(defun omnisharp-check-ready-status ()
  "Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration has
finished loading the solution."
  (interactive)
  (if (omnisharp--check-ready-status-worker)
      (message "Server is ready")
    (message "Server is not ready yet")))

(defun omnisharp--check-ready-status-worker ()
  (let ((result (omnisharp-post-message-curl-as-json
                 (concat (omnisharp-get-host) "checkreadystatus"))))
    (eq result t)))

(defun omnisharp-reload-solution ()
  "Reload the current solution."
  (interactive)
  (message (concat "Reloading the server. Calls to the server will not"
                   " work until the server has reloaded."))
  (omnisharp-post-message-curl-async
   (omnisharp--get-api-url "reloadsolution")
   nil ; no params needed
   (lambda (_)
     (message "OmniSharpServer solution reloaded"))))

(defun omnisharp-stop-server ()
  "Stop the current omnisharp instance."
  (interactive)
  (omnisharp-post-message-curl-async
   (concat (omnisharp-get-host) "stopserver")
   nil
   (lambda (_)
     (message "OmniSharpServer stopped."))))

(provide 'omnisharp-server-actions)
