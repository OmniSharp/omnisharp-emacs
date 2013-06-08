;; Work in progress!
;; Requires http://edward.oconnor.cx/2006/03/json.el
(require 'json)
(require 'cl)
;; TODO this does not seem to be able to return anything.

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

(defun omnisharp-get-current-buffer-contents ()
  (buffer-substring-no-properties 1 (buffer-size)))

(defun omnisharp-autocomplete-test ()
  (let* ((line-number (number-to-string (line-number-at-pos)))
         (column-number (number-to-string (+ 1 (current-column))))
         (buffer-contents (omnisharp-get-current-buffer-contents))
         (filename-tmp (omnisharp--convert-slashes-to-double-backslashes
                        buffer-file-name))
         (timeout-tmp omnisharp-timeout)
         (params `((line . ,line-number)
                   (column . ,column-number)
                   (buffer . ,buffer-contents)
                   (filename . ,filename-tmp))))
    (request
     (concat omnisharp-host "autocomplete")
     :type "POST"
     :data params
     :parser 'json-read
     :sync nil

     ;; Something seems to hang when sending the request. For the
     ;; request to actually start, we need to kill a buffer called
     ;; "localhost: something".
     ;; 'request' will crash after this timeout, causing the request
     ;; to start.
     :timeout 5
     ;; TODO timeout to a var with a sensible value
     :success (function*
                (lambda (&key data &allow-other-keys)
                  (message "I sent: %S" (prin1-to-string data))))))
  "done")

(defun omnisharp-display-autocomplete-suggestions
  (data)
  "TODO describe expected data:
((DisplayText . \"Gender\")
 (Description . \"int Gender { get; set; }\")
 (CompletionText . \"Gender\"))"
  (when (not (null data))
    (setq result (popup-menu* data
                              :width 20
                              :keymap popup-menu-keymap
                              :margin-left 1
                              :margin-right 1
                              :scroll-bar t
                              ;; TODO make this into a variable
                              :isearch t))
    (let ((end (point)))
      (backward-kill-word 1)
      (insert result))))

(defun test () (omnisharp-autocomplete-test))

;;            ;; TODO timeout to a var with a sensible value
;;            :success (function*
;;            (lambda (&key data &allow-other-keys)
;;              (omnisharp-display-autocomplete-suggestions data)
;;              ;;(message "I sent: %S" (prin1-to-string data))
;;              ))))))
;;   result)

;; (defun omnisharp-display-autocomplete-suggestions
;;   (data)
;;   "TODO describe expected data:
;; ((DisplayText . \"Gender\")
;;  (Description . \"int Gender { get; set; }\")
;;  (CompletionText . \"Gender\"))"
;;   (when (not (null data))
;;     (setq result (popup-menu* data
;;                               :width 20
;;                               :keymap popup-menu-keymap
;;                               :margin-left 1
;;                               :margin-right 1
;;                               :scroll-bar t
;;                               ;; TODO make this into a variable
;;                               :isearch t))
;;     (let ((end (point)))
;;       (backward-kill-word 1)
;;       (insert result))))

;; (defun test () (omnisharp-autocomplete-test))
