;; Requires http://edward.oconnor.cx/2006/03/json.el
;; Work in progress! Judge gently!
(require 'json)
(require 'cl)

(defvar omnisharp-host "http://localhost:2000/"
  "Currently expected to end with a / character")

(defvar omnisharp-timeout 1
  "Timeout, in seconds, after which to abort stalling queries to the
  OmniSharp server.")

(defvar omnisharp-auto-complete-popup-want-isearch t
  "Whether to automatically start isearch when auto-completing.")

(defun omnisharp-reload-solution ()
  "Reload the current solution."
  (interactive)
  (omnisharp-post-message-curl
   (concat omnisharp-host "reloadsolution")
   ;; no params needed
   nil))

(defun omnisharp-go-to-definition ()
  "TODO"
  (interactive)
  (let ((result (omnisharp-post-message-curl ; TODO create JSON version
                 (concat omnisharp-host "gotodefinition")
                 (omnisharp--get-common-params))))
    ;; TODO open file :FileName at :Line and :Column
    (find-file (cdr (assoc 'FileName result)))
    (message result)))

(defun omnisharp-stop-server ()
  "Stop the current omnisharp instance."
  (interactive)
  (omnisharp-post-message-curl
   (concat omnisharp-host "stopserver")
   nil))

;; TODO create omnisharp-add-to-solution that lets user choose which
;; file to add.
(defun omnisharp-add-to-solution-current-file ()
  (interactive)
  (let ((params (omnisharp--get-common-params)))
    (omnisharp-add-to-solution-worker params)))

(defun omnisharp-add-to-solution-worker (params)
  "TODO"
  ;; TODO report results somehow
  (omnisharp-post-message-curl
   (concat omnisharp-host "addtoproject")
   params))

(defun omnisharp-auto-complete
  (&optional)
  "TODO

   api-path : the path to the desired OmniSharp API. Example:
              /autocomplete

   timeout               : override global omnisharp-timeout"
  (let ((params (omnisharp--get-common-params)))

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

(defun omnisharp--get-current-buffer-contents ()
  (buffer-substring-no-properties 1 (buffer-size)))

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

(defun omnisharp--display-autocomplete-suggestions
  (json-result-alist)
  "Gets an association list such as this:
 (((DisplayText    . \"Gender\")
   (Description    . \"int Gender { get; set; }\")
   (CompletionText . \"Gender\")))

Displays a popup.el popup menu, and inserts the chosen element in the
current buffer."
  (if (equalp 0 (length json-result-alist))
      (progn (message "No completions.")
             nil)

    (let* ((display-list
            (omnisharp--convert-auto-complete-json-to-popup-format
             json-result-alist))

           (completion-texts
            (mapcar 'omnisharp--completion-result-item-get-display-text
                    json-result-alist))

           (max-width (omnisharp--get-max-item-length
                       completion-texts)))
      (setq result (popup-menu* display-list
                                :width max-width
                                :keymap popup-menu-keymap
                                :margin-left 1
                                :margin-right 1
                                :scroll-bar t
                                :isearch omnisharp-auto-complete-popup-want-isearch
                                :help-delay 1)))
    (insert result)))

;; TODO Use a plist. This is ridiculous.
(defun omnisharp--convert-auto-complete-json-to-popup-format
  (json-result-alist)
  (mapcar
   (lambda (element)
     (popup-make-item
      ;; TODO get item from json-result-alist
      ;;
      ;; TODO these are already calculated in
      ;; omnisharp--display-autocomplete-suggestions, stored as
      ;; completion-texts
      (cdr (assoc 'DisplayText element))
      :value (omnisharp--completion-result-item-get-completion-text
              element)
      :document (cdr (assoc 'Description element))))
   json-result-alist))

(defun omnisharp--completion-result-item-get-completion-text (item)
  (cdr (assoc 'CompletionText item)))

(defun omnisharp--completion-result-item-get-display-text (item)
  (cdr (assoc 'DisplayText item)))

(defun omnisharp--get-max-item-length (completions)
  "Returns the length of the longest completion in 'completions'."
  (if (null completions)
      0
    (reduce 'max (mapcar 'length completions))))

(defun omnisharp--get-common-params ()
  "Get common parameters used in the base request class Request."
  (let* ((line-number (number-to-string (line-number-at-pos)))
         (column-number (number-to-string (+ 1 (current-column))))
         (buffer-contents (omnisharp--get-current-buffer-contents))
         (filename-tmp (omnisharp--convert-slashes-to-double-backslashes
                        buffer-file-name))
         (params `((line     . ,line-number)
                   (column   . ,column-number)
                   (buffer   . ,buffer-contents)
                   (filename . ,filename-tmp))))
    params))
