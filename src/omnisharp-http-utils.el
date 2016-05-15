
(defun omnisharp--get-host ()
  "Makes sure omnisharp-host is ended by / "
  (if (string= (substring omnisharp-host -1 ) "/")
      omnisharp-host
    (concat omnisharp-host "/")))

(defun omnisharp--get-api-url (api-name)
  (concat (omnisharp--get-host) api-name))

;;;###autoload
(defun omnisharp-post-http-message (url &optional params)
  "Post http request to server. Return result."
  (let* ((url (omnisharp--get-api-url url))
         (response (omnisharp--submit-request url params))
         (data (elt response 3)))
    (unless data
      (message "Error when sending request %s" url))
    (when omnisharp-debug
      (message "Response:\n%s" (prin1-to-string data)))
    data))

(defun omnisharp--submit-request (url &optional params)
  (require 'request)
  (request url
           :type "POST"
           :parser 'json-read
           :sync t
           :data (json-encode params)
           :error
           (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                          (message "Got error: %S" error-thrown)))
           :complete (lambda (&rest _) (when omnisharp-debug (message "Request completed")))
           :success (cl-function
                     (lambda (&key data &allow-other-keys) (when omnisharp-debug (message "Request succeeded"))
                       ))))

(defun omnisharp--server-process-sentinel (process event)
  (if (string-match "^exited abnormally" event)
      (error (concat "OmniSharp server process " event))))

(provide 'omnisharp-http-utils)
