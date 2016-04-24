
(defun omnisharp-post-message-curl (url &optional params)
  "Post json stuff to url with --data set to given params. Return
result."
  (let ((curl-command-plist
         (omnisharp--get-curl-command (omnisharp--get-api-url url) params)))
    (with-temp-buffer
      (apply 'call-process
             (plist-get curl-command-plist :command)
             nil ;; infile
             (buffer-name);; destination
             nil ;; display (no specialities needed)
             ;; these are just args
             (plist-get curl-command-plist :arguments))
      (buffer-string))))

(defun omnisharp--get-curl-command (url params)
  "Returns a command that may be used to communicate with the API via
the curl program. Depends on the operating system."
  (let ((curl-command
         (if (equal system-type 'windows-nt)
             (omnisharp--get-curl-command-windows-with-tmp-file url params)
           (omnisharp--get-curl-command-unix url params))))
    (when omnisharp-debug
      (omnisharp--log-curl-command curl-command))
    curl-command))

(defun omnisharp--log-curl-command (curl-command)
  (omnisharp--log (prin1-to-string curl-command)))

(defun omnisharp--get-curl-command-arguments-string-for-api-name
  (params api-name)
  "Returns the full command to call curl with PARAMS for the api API-NAME.
Example: when called with \"getcodeactions\", returns
\"curl (stuff) http://localhost:2000/getcodeactions (stuff)\"
with \"stuff\" set to sensible values."
  (let ((command-plist
         (omnisharp--get-curl-command
          (concat (omnisharp-get-host) api-name)
          params)))
    (plist-get command-plist :arguments)))

(defun omnisharp--get-curl-command-unix (url params)
  "Returns a command using plain curl that can be executed to
communicate with the API."
  `(:command
    ,omnisharp--curl-executable-path
    :arguments
    ("--ipv4" "--silent" "-H" "Content-type: application/json"
     "--data"
     ,(json-encode params)
     ,url)))

(defun omnisharp--get-curl-command-windows-with-tmp-file (url params)
  "Basically: put PARAMS to file, then create a curl command to the
api at URL using that file as the parameters."
  ;; TODO could optimise: short buffers need not be written to tmp
  ;; files.
  (omnisharp--write-json-params-to-tmp-file
   omnisharp--windows-curl-tmp-file-path
   (json-encode params))
  (let ((path-with-curl-prefix
         (concat "@"
                 omnisharp--windows-curl-tmp-file-path
                 )))
    `(:command ,omnisharp--curl-executable-path
               :arguments
               ("--noproxy" "localhost"
                "--silent" "-H" "Content-type: application/json"
                "--data-binary"
                ;; @ specifies a file path to curl
                ,path-with-curl-prefix
                ,url))))

(defun omnisharp--write-json-params-to-tmp-file
  (target-path stuff-to-write-to-file)
  "Deletes the file when done."
  (with-temp-file target-path
    (insert stuff-to-write-to-file)))

(defun omnisharp-post-message-curl-as-json (url &optional params)
  (omnisharp--json-read-from-string
   (omnisharp-post-message-curl url params)))


(defun omnisharp--server-process-sentinel (process event)
  (if (string-match "^exited abnormally" event)
      (error (concat "OmniSharp server process " event))))


(defun omnisharp-post-message-curl-as-json-async (url params callback)
  "Posts message to curl at URL with PARAMS asynchronously.
On completion, the curl output is parsed as json and passed into CALLBACK."
  (omnisharp-post-message-curl-async url params
                                     (lambda (str)
                                       (funcall callback (omnisharp--json-read-from-string str)))))

(defun omnisharp-post-message-curl-async (url params callback)
  "Post json stuff to url asynchronously with --data set to given params.
On completion, CALLBACK is run with the result as it's only parameter.

Returns the curl process"
  (let* ((curl-command-plist
          (omnisharp--get-curl-command url params))
         (process-name (concat "* Omnisharp curl : " url "*"))
         (process-buffer (generate-new-buffer process-name))
         (process (apply 'start-process
                         process-name
                         process-buffer
                         (plist-get curl-command-plist :command)
                         (plist-get curl-command-plist :arguments))))
    (set-process-sentinel
     process
     (lambda (proc status)
       (unless (process-live-p proc)
         (funcall callback
                  (progn (let ((output (with-current-buffer process-buffer (buffer-string))))
                           (kill-buffer process-buffer)
                           output))))))
    process))

(provide 'omnisharp-http-utils)
