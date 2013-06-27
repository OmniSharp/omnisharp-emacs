;; Requires http://edward.oconnor.cx/2006/03/json.el
;; Work in progress! Judge gently!
(require 'json)
(require 'cl)
(require 'files)

(defvar omnisharp-host "http://localhost:2000/"
  "Currently expected to end with a / character")

(defvar omnisharp-timeout 1
  "Timeout, in seconds, after which to abort stalling queries to the
  OmniSharp server.")

(defvar omnisharp-auto-complete-popup-want-isearch t
  "Whether to automatically start isearch when auto-completing.")

(defvar omnisharp--find-usages-buffer-name "* OmniSharp : Usages *"
  "The name of the temporary buffer that is used to display the
results of a 'find usages' call.")

(defvar omnisharp-auto-complete-popup-help-delay nil
  "The timeout after which the auto-complete popup will show its help
  popup. Disabled by default because the help is often scrambled and
  looks bad.")

(defvar omnisharp-auto-complete-popup-persist-help t
  "Whether to keep the help window (accessed by pressing f1 while the
popup window is active) open after any other key is
pressed. Defaults to true.")

(defvar omnisharp-find-usages-header
  (concat "Usages in the current solution:"
          "\n\n")
  "This is shown at the top of the result buffer when
omnisharp-find-usages is called.")

(defun omnisharp-reload-solution ()
  "Reload the current solution."
  (interactive)
  (omnisharp-post-message-curl
   (concat omnisharp-host "reloadsolution")
   ;; no params needed
   nil))

(defun omnisharp-go-to-definition ()
  "Jump to the definition of the symbol under point."
  (interactive)
  (let* ((json-result (omnisharp-post-message-curl-as-json
                       (concat omnisharp-host "gotodefinition")
                       (omnisharp--get-common-params)))
         (filename (cdr (assoc 'FileName json-result))))
    (if (null filename)
        (message
         "Cannot go to definition as none was returned by the API.")
      (progn
        ;; open file :FileName at :Line and :Column
        (find-file filename)
        (goto-line (cdr (assoc 'Line json-result)))
        (move-to-column (- (cdr (assoc 'Column json-result))
                           1))))))

(defun omnisharp-find-usages ()
  "Find usages for the symbol under point"
  (interactive)
  (omnisharp-find-usages-worker (omnisharp--get-common-params)))

(defun omnisharp-find-usages-worker (params)
  ;; TODO make this asyncronic like all other compilation processes!
  (let* ((json-result (omnisharp-post-message-curl-as-json
                       (concat omnisharp-host "findusages")
                       params))
         (output-buffer (get-buffer-create
                         omnisharp--find-usages-buffer-name))
         (output-in-compilation-mode-format
          ;; Loop over a vector such as:
          ;; [((Text . "public static AstNode GetDefinition(this
          ;; AstNode node)") (Column . 25) (Line . 39) (FileName
          ;; . "/foo")) ((Text ...)]
          (mapcar
           'omnisharp--find-usages-output-to-compilation-output
           (cdr (assoc 'Usages json-result)))))

    (omnisharp--write-lines-to-compilation-buffer
     output-in-compilation-mode-format
     output-buffer
     omnisharp-find-usages-header)))

(defun omnisharp--write-lines-to-compilation-buffer
  (lines-to-write buffer-to-write-to &optional header)
  "Writes the given lines to the given buffer, and sets
compilation-mode on. The contents of the buffer are erased. The
buffer is marked read-only after inserting all lines.

If HEADER is given, that is written to the top of the buffer.

Expects the lines to be in a format that compilation-mode
recognizes, so that the user may jump to the results."
  (with-current-buffer buffer-to-write-to
    (let ((inhibit-read-only t))
      (read-only-mode nil)
      (erase-buffer)

      (when (not (null header))
        (insert header))

      (mapcar (lambda (element)
                (insert element)
                (insert "\n"))
              lines-to-write)
      (compilation-mode)
      (read-only-mode t)
      (display-buffer buffer-to-write-to))))

(defun omnisharp--find-usages-output-to-compilation-output
  (json-result-single-element)
  "Converts a single element of a /findusages JSON response to a
format that the compilation major mode understands and lets the user
follow results to the locations in the actual files."
  (let ((filename (cdr (assoc 'FileName json-result-single-element)))
        (line (cdr (assoc 'Line json-result-single-element)))
        (text (cdr (assoc 'Text json-result-single-element))))
    (concat filename
            ":"
            (prin1-to-string line)
            ": "
            text)))

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

(defun omnisharp-add-reference ()
  (interactive)
  (let* ((path-to-ref-file-to-add
          (ido-read-file-name "Add reference to (dll / project): "
                              nil ;; start in current dir
                              nil ;; no default filename
                              t ;; only allow existing files

                              ;; TODO use a predicate for filtering
                              ;; dll and csproj files
                              ))
         (tmp-params (omnisharp--get-common-params))
         (params (add-to-list 'tmp-params
                              `(Reference . ,path-to-ref-file-to-add))))
    (omnisharp-add-reference-worker params)))

(defun omnisharp-add-reference-worker (params)
  (omnisharp-post-message-curl-as-json
   (concat omnisharp-host "addreference")
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

(defun omnisharp-post-message-curl-as-json (url params)
  (json-read-from-string
   (omnisharp-post-message-curl url params)))

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
      (setq result
            (popup-menu* display-list
                         :width max-width
                         :keymap popup-menu-keymap
                         :margin-left 1
                         :margin-right 1
                         :scroll-bar t
                         :isearch
                         omnisharp-auto-complete-popup-want-isearch
                         :help-delay
                         omnisharp-auto-complete-popup-help-delay)))
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
