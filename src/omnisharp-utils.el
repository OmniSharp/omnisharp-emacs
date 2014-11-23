
(defun omnisharp-get-host ()
  "Makes sure omnisharp-host is ended by / "
  (if (string= (substring omnisharp-host -1 ) "/")
      omnisharp-host
    (concat omnisharp-host "/")))

(defun omnisharp--get-api-url (api-name)
  (concat (omnisharp-get-host) api-name))

(provide 'omnisharp-utils)
