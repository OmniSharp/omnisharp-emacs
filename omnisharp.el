;; Requires http://edward.oconnor.cx/2006/03/json.el
;; Work in progress! Judge gently!
(require 'json)
;;(require 'http-post-simple)
;;(require 'request)

(defvar omnisharp-host
  "http://localhost:2000/"
  "Currently expected to end with a / character")

(defvar omnisharp-timeout
  1
  "Timeout, in seconds, after which to abort stalling queries to the
  OmniSharp server.")

(defun omnisharp-auto-complete
  (&optional additional-parameters)
  "TODO

   api-path : the path to the desired OmniSharp API. Example:
              /autocomplete

   additional-parameters : TODO currently not used
   timeout               : override global omnisharp-timeout"
  (let* ((line-number (number-to-string (line-number-at-pos)))
         (column-number (number-to-string (+ 1 (current-column))))
         (buffer-contents (omnisharp--get-current-buffer-contents))
         (filename-tmp (omnisharp--convert-slashes-to-double-backslashes
                        buffer-file-name))
         (params `((line . ,line-number)
                   (column . ,column-number)
                   (buffer . ,buffer-contents)
                   (filename . ,filename-tmp))))

    (omnisharp-auto-complete-worker params)))

(defun omnisharp-auto-complete-worker (params)
  "Takes a plist and makes an autocomplete query with them. Targets
the given api-path. TODO"
  ;; json.el URL encodes params automatically.
  (let* ((raw-result
          (omnisharp-post-message-curl
           (concat omnisharp-host "autocomplete")
           params))
         (json-result (json-read-from-string raw-result)))
    (omnisharp--display-autocomplete-suggestions json-result)))

(defun omnisharp--convert-slashes-to-double-backslashes (str)
  "This might be useful. A direct port from OmniSharp.py."
  (replace-regexp-in-string "/" "\\\\" str))

(defun omnisharp--plist-merge (plist-a plist-b)
  "This is a very naive implementation. Duplicates will prevail,
but are likely to work due to plist-get specific behaviour. A better
implementation is strongly desired."
  (append plist-a plist-b))

(defun omnisharp--get-current-buffer-contents ()
  (buffer-substring-no-properties 1 (buffer-size)))

(defun omnisharp-escape-single-quote (string-to-quote)
  (replace-regexp-in-string "'" "\\\\'" string-to-quote))

(defun omnisharp-post-message-curl (url params)
  "Post json stuff to url with --data set to given params. Return
result."
  (with-temp-buffer
    (call-process
     "curl"
     nil ;; infile
     (buffer-name);; destination
     nil ;; display (no specialities needed)
     ;; these are just args
     "--silent" "-H" "Content-type: application/json"
     "--data"
     (json-encode params)
     url)
    (buffer-string)))

(define-key evil-insert-state-map
  (kbd "<f5>")
  (lambda () (interactive)
    (omnisharp-auto-complete)))

(defun omnisharp--display-autocomplete-suggestions
  (json-result-alist)
  "TODO describe expected data:
 (((DisplayText    . \"Gender\")
   (Description    . \"int Gender { get; set; }\")
   (CompletionText . \"Gender\")))"
  (when (not (null json-result-alist))
    (let* ((display-list
            ;; TODO refactor outside
            ;; Use a plist. This is ridiculous.
            (mapcar (lambda (element)
                      (popup-make-item
                       ;; TODO get item from json-result-alist
                       (cdr (assoc 'DisplayText element))
                       :value (cdr (assoc 'CompletionText element))
                       :document (cdr (assoc 'Description element))))
                    json-result-alist)))
      (setq result (popup-menu* display-list
                                ;; TODO set width to a sensible value
                                :width 70
                                :keymap popup-menu-keymap
                                :margin-left 1
                                :margin-right 1
                                :scroll-bar t
                                ;; TODO make this into a variable
                                :isearch t
                                :help-delay 1)))
    (insert result)))
