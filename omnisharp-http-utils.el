
(defun omnisharp--get-host ()
  "Makes sure omnisharp-host is ended by / "
  (if (string= (substring omnisharp-host -1 ) "/")
      omnisharp-host
    (concat omnisharp-host "/")))

(defun omnisharp--get-api-url (api-name)
  (concat (omnisharp--get-host) api-name))

;;;###autoload
(defun omnisharp-post-http-message (url callback &optional params async)
  "Post http request to server. Return result."
  (omnisharp--submit-request (omnisharp--get-api-url url) callback params async))

(defun omnisharp--submit-request (url callback &optional params async)
  (if (require 'request nil 'noerror)
      (lexical-let* ((c callback))
        (request url
                 :type "POST"
                 :parser 'json-read
                 :sync (not async)
                 :data (json-encode params)
                 :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                       (message "Error from %s : %S" url error-thrown)))
                 :complete
                 (lambda (&rest _)
                   (when omnisharp-debug
                     (message "Request completed")))
                 :success (cl-function (lambda (&key data &allow-other-keys)
                                         (progn
                                           (when c
                                             (funcall c data))
                                           (when omnisharp-debug
                                             (message "Request succeeded"))
                                           )))
                 :status-code '((404 . (lambda (&rest _) (message (format "Endpoint %s does not exist." url))))
                                (500 . (lambda (&rest _) (message (format "Error from  %s." url))))
                                )))
    (message "ERROR: You must install 'request-deferred' package")))

(provide 'omnisharp-http-utils)
