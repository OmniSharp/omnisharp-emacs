;; Work in progress!
;; Requires http://edward.oconnor.cx/2006/03/json.el
(require 'json)
(require 'cl)

(defvar omnisharp-host
  "http://localhost:2000/"
  "Currently expected to end with a / character")

(defvar omnisharp-timeout
  1
  "Timeout, in seconds, after which to abort stalling queries to the
  OmniSharp server.")

(defun omnisharp--get-response
  (api-path &optional additional-parameters timeout)
  "TODO

   api-path : the path to the desired OmniSharp API. Example:
              /autocomplete

   additional-parameters : TODO currently not used
   timeout               : override global omnisharp-timeout"
  (let* ((params
          '(:line (line-number-at-pos)
            :column (current-column)
            :buffer (line-number-at-pos)
            :filename (omnisharp--convert-slashes-to-double-backslashes
                       buffer-file-name)
            :timeout (or timeout omnisharp-timeout))))
    (omnisharp--plist-merge params additional-parameters))
  (omnisharp--get-response-worker api-path params))

(defun omnisharp--get-response-worker (api-path params)
  "Takes a plist and makes an API query with them. Targets the given
api-path. TODO"
  ;; json.el URL encodes params automatically.
  )

(defun omnisharp--convert-slashes-to-double-backslashes (str)
  "This might be useful. A direct port from OmniSharp.py."
  (replace-regexp-in-string "/" "\\\\" str))

(defun omnisharp--plist-merge (plist-a plist-b)
  "This is a very naive implementation. Duplicates will prevail,
but are likely to work due to plist-get specific behaviour. A better
implementation is strongly desired."
  (append plist-a plist-b))

