;;; omnisharp.el --- Omnicompletion (intellisense) and more for C#
;; Copyright (C) 2013 Mika Vilpas (GPLv3)
;; Author: Mika Vilpas
;; Version: 0.1
;; Url: https://github.com/sp3ctum/omnisharp-emacs
;; Package-Requires: ((json "1.2") (dash "1.8.0") (popup "0.5") (auto-complete "1.4") (flycheck "0.13"))
;; Keywords: csharp c# IDE auto-complete intellisense

;;; Commentary:
;; omnisharp-emacs is a port of the awesome OmniSharp server to the
;; Emacs text editor. It provides IDE-like features for editing files
;; in C# solutions in Emacs, provided by an OmniSharp server instance
;; that works in the background.
;;
;; See the project home page for more information.

;; Work in progress! Judge gently!
(require 'json)
(with-no-warnings
  (require 'cl))
(require 'files)
(require 'ido)
(require 'thingatpt)
(require 'dash)
(require 'compile)
(require 'dired)
(require 'popup)
(require 'etags)
(require 'flycheck)
(require 'auto-complete)

;;; Code:
(defvar omnisharp-host "http://localhost:2000/"
  "Currently expected to end with a / character.")

(defvar omnisharp-timeout 1
  "Timeout, in seconds, after which to abort stalling queries to the
OmniSharp server.")

(defvar omnisharp-auto-complete-popup-want-isearch t
  "Whether to automatically start isearch when auto-completing.")

(defvar omnisharp--find-usages-buffer-name "* OmniSharp : Usages *"
  "The name of the temporary buffer that is used to display the
results of a 'find usages' call.")

(defvar omnisharp--find-implementations-buffer-name "* OmniSharp : Implementations *"
  "The name of the temporary buffer that is used to display the
results of a 'find implementations' call.")

(defvar omnisharp-auto-complete-popup-help-delay nil
  "The timeout after which the auto-complete popup will show its help
  popup. Disabled by default because the help is often scrambled and
  looks bad.")

(defvar omnisharp-auto-complete-popup-persist-help t
  "Whether to keep the help window (accessed by pressing f1 while the
popup window is active) open after any other key is
pressed. Defaults to true.")

(defvar-local
  omnisharp--last-buffer-specific-auto-complete-result
  nil
  "Contains the last result of an autocomplete query.")

(defvar omnisharp-auto-complete-want-documentation t
  "Whether to include auto-complete documentation for each and every
response. This may be set to nil to get a speed boost for
completions.")

(defvar omnisharp-auto-complete-popup-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap popup-menu-keymap)

    (define-key keymap (kbd "<f2>") 'omnisharp--popup-to-ido)
    keymap)
  "The keymap used when displaying an autocomplete result in a popup
menu.")

(defvar omnisharp-find-usages-header
  (concat "Usages in the current solution:"
          "\n\n")
  "This is shown at the top of the result buffer when
omnisharp-find-usages is called.")

(defvar omnisharp-find-implementations-header
  (concat "Implementations of the current interface / class:"
          "\n\n")
  "This is shown at the top of the result buffer when
omnisharp-find-implementations is called.")

(defvar omnisharp--auto-complete-display-backend
  'popup
  "Defines what auto-complete result displaying backend to use when
showing autocomplete results to the user. Valid values are found in
omnisharp--auto-complete-display-backends-alist.")

(defvar omnisharp--auto-complete-display-backends-alist
  '((popup . omnisharp--auto-complete-display-function-popup)
    (ido . omnisharp--auto-complete-display-function-ido))
  "Holds an alist of all available auto-complete display backends.
See the documentation for the variable
omnisharp--auto-complete-display-backend for more information.")

(defvar omnisharp-code-format-expand-tab t
  "Whether to expand tabs to spaces in code format requests.")

;;;###autoload
(defun omnisharp-reload-solution ()
  "Reload the current solution."
  (interactive)
  (omnisharp-post-message-curl
   (concat omnisharp-host "reloadsolution")
   ;; no params needed
   nil))

;;;###autoload
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
      (omnisharp-go-to-file-line-and-column json-result))))

;;;###autoload
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

;;;###autoload
(defun omnisharp-find-implementations ()
  "Show a buffer containing all implementations of the interface under
point, or classes derived from the class under point. Allow the user
to select one (or more) to jump to."
  (interactive)
  (omnisharp-find-implementations-worker (omnisharp--get-common-params)))

(defun omnisharp-find-implementations-worker (params)
  (let* ((json-result (omnisharp-post-message-curl-as-json
                       (concat omnisharp-host "findimplementations")
                       params))
         (output-in-compilation-mode-format
          (mapcar
           'omnisharp--find-usages-output-to-compilation-output
           (cdr (assoc 'Locations json-result)))))

    (omnisharp--write-lines-to-compilation-buffer
     output-in-compilation-mode-format
     (get-buffer-create omnisharp--find-implementations-buffer-name)
     omnisharp-find-implementations-header)))

;;;###autoload
(defun omnisharp-rename ()
  "Rename the current symbol to a new name. Lets the user choose what
name to rename to, defaulting to the current name of the symbol."
  (interactive)
  (let* ((current-word (thing-at-point 'symbol))
         (rename-to (read-string "Rename to: " current-word))
         (rename-request
          (cons `(RenameTo . ,rename-to)
                (omnisharp--get-common-params))))

    (omnisharp-rename-worker rename-request)
    (message "Rename complete")))

(defun omnisharp-rename-worker (rename-request)
  (let* ((rename-responses
          (omnisharp-post-message-curl-as-json
           (concat omnisharp-host "rename")
           rename-request))
         (modified-files (omnisharp--vector-to-list
                          (cdr (assoc 'Changes rename-responses)))))
    (save-excursion
      (mapc (lambda (modified-file-response)
              (omnisharp--set-buffer-contents-to
               (cdr (assoc 'FileName modified-file-response))
               (cdr (assoc 'Buffer modified-file-response))
               1
               1))
            modified-files))))

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

      (mapc (lambda (element)
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
;;;###autoload
(defun omnisharp-add-to-solution-current-file ()
  (interactive)
  (let ((params (omnisharp--get-common-params)))
    (omnisharp-add-to-solution-worker params)
    (message "Added %s to the solution."
             (cdr (assoc 'FileName params)))))

;;;###autoload
(defun omnisharp-add-to-solution-dired-selected-files ()
  "Add the files currently selected in dired to the current solution."
  (interactive)
  (let ((selected-files (dired-get-marked-files)))
    (--each selected-files
      (let ((params
             (cons `(FileName . ,it)
                   (omnisharp--get-common-params))))
        (omnisharp-add-to-solution-worker params))
      (message "Added %s files to the solution."
               (prin1-to-string (length selected-files))))))

(defun omnisharp-add-to-solution-worker (params)
  "TODO"
  ;; TODO report results somehow
  (omnisharp-post-message-curl
   (concat omnisharp-host "addtoproject")
   params))

;;;###autoload
(defun omnisharp-remove-from-project-current-file ()
  (interactive)
  (let ((params (omnisharp--get-common-params)))
    (omnisharp-remove-from-project-current-file-worker params)
    (message "Removed %s from the solution."
             (cdr (assoc 'FileName params)))))

;;;###autoload
(defun omnisharp-remove-from-project-dired-selected-files ()
  "Remove the files currently selected in dired from the current
solution."
  (interactive)
  (let ((selected-files (dired-get-marked-files)))
    (--each selected-files
      (let ((params
             (cons `(FileName . ,it)
                   (omnisharp--get-common-params))))
        (omnisharp-remove-from-project-current-file-worker params))
      (message "Removed %s files from the project."
               (prin1-to-string (length selected-files))))))

(defun omnisharp-remove-from-project-current-file-worker (params)
  (omnisharp-post-message-curl
   (concat omnisharp-host "removefromproject")
   params))

;;;###autoload
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

;;;###autoload
(defun omnisharp-auto-complete ()
  (interactive)
  (let* ((json-false :json-false)
         ;; json-false helps distinguish between null and false in
         ;; json. This is an emacs limitation.

         (params
          (omnisharp--get-auto-complete-params))

         (display-function
          (omnisharp--get-auto-complete-display-function))

         (json-result-auto-complete-response
          (omnisharp-auto-complete-worker params)))

    (funcall display-function json-result-auto-complete-response)))

;;;###autoload
(defun omnisharp-add-dot-and-auto-complete ()
  "Adds a . character and calls omnisharp-auto-complete. Meant to be
bound to the dot key so pressing dot will automatically insert a dot
and complete members."
  (interactive)
  (insert ".")
  (omnisharp-auto-complete))

(defun omnisharp--get-auto-complete-params ()
  "Return an AutoCompleteRequest for the current buffer state."
  (let* ((request (omnisharp--get-common-params))
         (want-doc (if (equal
                        omnisharp-auto-complete-want-documentation
                        nil)
                       :json-false
                     omnisharp-auto-complete-want-documentation))
         (request-with-doc-option
          (cons
           `(WantDocumentationForEveryCompletionResult
             . ,want-doc)
           (omnisharp--get-common-params)))
         (final-request
          ;; Add WordToComplete to params
          (cons `(WordToComplete . ,(thing-at-point 'symbol))
                request-with-doc-option)))
    final-request))

;; Use this source in your csharp editing mode hook like so:
;; (add-to-list 'ac-sources 'ac-source-omnisharp)
;;
;; Unfortunately there seems to be a limit in the auto-complete
;; library that disallows camel case completions and such fancy
;; completions useless.

;; The library only seems to accept completions that have the same
;; leading characters as results. Oh well.
(ac-define-source omnisharp
  '((candidates . omnisharp--get-auto-complete-result-in-popup-format)))

(defun omnisharp--get-auto-complete-result-in-popup-format ()
  "Returns /autocomplete API results \(autocompletions\) as popup
items."
  (let* ((json-result-auto-complete-response
          (omnisharp-auto-complete-worker
           (omnisharp--get-auto-complete-params)))
         (completions-in-popup-format
          (omnisharp--convert-auto-complete-json-to-popup-format
           json-result-auto-complete-response)))
    completions-in-popup-format))

(defun omnisharp--get-auto-complete-display-function ()
  "Returns a function that can be fed the output from
omnisharp-auto-complete-worker - the AutoCompleteResponse JSON output
from the omnisharp /autocomplete API.

This function must know how to convert the raw JSON into a format that
the user can choose one completion out of.  Then that function must
handle inserting that result in the way it sees fit (e.g. in the
current buffer)."
  (cdr (assoc omnisharp--auto-complete-display-backend
              omnisharp--auto-complete-display-backends-alist)))

(defun omnisharp-auto-complete-worker (params)
  "Takes a plist and makes an autocomplete query with them. Targets
the given api-path.

Returns the raw JSON result. Also caches that result as
omnisharp--last-buffer-specific-auto-complete-result."


  ;; json.el URL encodes params automatically.
  (let ((json-result
         (omnisharp-post-message-curl-as-json
          (concat omnisharp-host "autocomplete")
          params)))
    ;; Cache result so it may be juggled in different contexts easily
    (setq omnisharp--last-buffer-specific-auto-complete-result
          json-result)
    ))

;;;###autoload
(defun omnisharp-auto-complete-overrides ()
  (interactive)
  (let ((params (omnisharp--get-common-params)))
    (omnisharp-auto-complete-overrides-worker params)))

(defun omnisharp-auto-complete-overrides-worker (params)
  (let* ((json-result
          (omnisharp--vector-to-list
           (omnisharp-post-message-curl-as-json
            (concat omnisharp-host "getoverridetargets")
            params)))
         (target-names
          (mapcar (lambda (a)
                    (cdr (assoc 'OverrideTargetName a)))
                  json-result))
         (chosen-override (ido-completing-read
                           "Override: "
                           target-names
                           t)))
    (omnisharp-auto-complete-overrides-run-override
     chosen-override)))

(defun omnisharp-auto-complete-overrides-run-override (override-name)
  (omnisharp-auto-complete-overrides-run-override-worker
   (cons `(OverrideTargetName . ,override-name)
         (omnisharp--get-common-params))))

(defun omnisharp-auto-complete-overrides-run-override-worker (params)
  (let ((json-result (omnisharp-post-message-curl-as-json
                      (concat omnisharp-host "runoverridetarget")
                      params)))
    (omnisharp--set-buffer-contents-to
     (cdr (assoc 'FileName json-result))
     (cdr (assoc 'Buffer   json-result))
     (cdr (assoc 'Line     json-result))
     (cdr (assoc 'Column   json-result)))))

;;;###autoload
(defun omnisharp-run-code-action-refactoring ()
  "Gets a list of refactoring code actions for the current editor
position and file from the API. Asks the user what kind of refactoring
they want to run. Then runs the action.

Saves the current file before doing anything else. This is so that the
user is less likely to lose data."

  (interactive)
  (save-buffer)
  (let* ((actions-vector (omnisharp--get-code-actions-from-api))
         ;; CodeActions is a vector. Need to convert it to a list.
         (actions-list
          (omnisharp--vector-to-list
           (cdr (assoc 'CodeActions actions-vector))))
         (chosen-action (ido-completing-read
                         "Run code action: "
                         actions-list
                         t))
         (chosen-action-index
          (position chosen-action actions-list)))
    (when (not (= 0 (length chosen-action)))
      (omnisharp-run-code-action-refactoring-worker
       chosen-action-index))))

(defun omnisharp--get-code-actions-from-api ()
  "Fetches and returns a GetCodeActionsResponse: the runnable
refactoring code actions for the current file and position."
  (omnisharp-post-message-curl-as-json
   (concat omnisharp-host "getcodeactions")
   (omnisharp--get-common-params)))

(defun omnisharp-run-code-action-refactoring-worker
  (chosen-action-index)

  (let* ((run-action-params
          (cons `(CodeAction . ,chosen-action-index)
                (omnisharp--get-common-params)))
         (json-run-action-result ; RunCodeActionsResponse
          (omnisharp-post-message-curl-as-json
           (concat omnisharp-host "runcodeaction")
           run-action-params)))

    (omnisharp-run-code-action-worker run-action-params
                                      json-run-action-result)))

(defun omnisharp-run-code-action-worker (run-action-params
                                         json-run-action-result)
  "Gets new file contents with the chosen refactoring
applied. Attempts to keep point still.

run-action-params: original parameters sent to /runcodeaction API."
  (omnisharp--set-buffer-contents-to
   (buffer-file-name)
   (cdr (assoc 'Text json-run-action-result))
   (line-number-at-pos)
   (omnisharp--current-column)))

(defun omnisharp--set-buffer-contents-to (filename-for-buffer
                                          new-buffer-contents
                                          result-point-line
                                          result-point-column)
  "Sets the buffer contents to new-buffer-contents for the buffer
visiting filename-for-buffer. Afterwards moves point to the
coordinates result-point-line and result-point-column."
  (omnisharp-go-to-file-line-and-column-worker
   result-point-line result-point-column filename-for-buffer)
  (save-buffer)

  (erase-buffer)
  (insert new-buffer-contents)

  ;; Hack. Puts point where it belongs.
  (omnisharp-go-to-file-line-and-column-worker
   result-point-line result-point-column filename-for-buffer))

(defun omnisharp--current-column ()
  "Returns the current column, converting tab characters in a way that
the OmniSharp server understands."
  (let ((tab-width 1))
    (current-column)))

(defun omnisharp--buffer-exists-for-file-name (file-name)
  (cl-some (lambda (a)
             (equalp (buffer-file-name)
                     file-name))
           (buffer-list)))

(defun omnisharp--convert-slashes-to-double-backslashes (str)
  "This might be useful. A direct port from OmniSharp.py."
  (replace-regexp-in-string "/" "\\\\" str))

(defun omnisharp--get-current-buffer-contents ()
  (buffer-substring-no-properties 1 (buffer-size)))

(defun omnisharp-post-message-curl (url params)
  "Post json stuff to url with --data set to given params. Return
result."
  (let ((curl-command-plist
         (omnisharp--get-curl-command url params)))
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
  `(:command "curl"
    :arguments
    ("--silent" "-H" "Content-type: application/json"
     "--data"
     ,(json-encode params)
     ,url)))

(defun omnisharp-post-message-curl-as-json (url params)
  (json-read-from-string
   (omnisharp-post-message-curl url params)))

(defun omnisharp--auto-complete-display-function-popup
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
                       completion-texts))
           (result (popup-menu* display-list
                                :width max-width
                                :keymap omnisharp-auto-complete-popup-keymap
                                :margin-left 1
                                :margin-right 1
                                :scroll-bar t
                                :isearch
                                omnisharp-auto-complete-popup-want-isearch
                                :help-delay
                                omnisharp-auto-complete-popup-help-delay)))
      (omnisharp--replace-symbol-in-buffer-with
       (omnisharp--current-word-or-empty-string)
       result))))

(defun omnisharp--replace-symbol-in-buffer-with (symbol-to-replace
                                                 replacement-string)
  "In the current buffer, replaces the given SYMBOL-TO-REPLACE
\(a string\) with REPLACEMENT-STRING."
  (search-backward symbol-to-replace)
  (replace-match replacement-string t t))

(defun omnisharp--auto-complete-display-function-ido
  (json-result-alist)
  "Use ido style completion matching with autocomplete candidates. Ido
is a more sophisticated matching framework than what popup.el offers."

  (if (equalp 0 (length json-result-alist))
      (progn (message "No completions.")
             nil)

    (let* ((candidates (omnisharp--vector-to-list json-result-alist))

           (display-texts
            (mapcar 'omnisharp--completion-result-item-get-display-text
                    candidates))

           ;; This is only the display text. The text to be inserted
           ;; in the buffer will be fetched with this
           ;;
           ;; TODO does ido-completing-read allow a custom format that
           ;; could store these, as with popup-make-item ?
           (user-chosen-display-text
            (ido-completing-read
             "Complete: "
             display-texts))

           ;; Get the chosen candidate by getting the index of the
           ;; chosen DisplayText. The candidate with the same index is
           ;; the one we want.
           (json-result-element-index-with-user-chosen-text
            (position-if (lambda (element)
                           (equal element
                                  user-chosen-display-text))
                         display-texts))
           (chosen-candidate
            (nth json-result-element-index-with-user-chosen-text
                 candidates))

           (completion-text-to-insert
            (cdr (assoc 'CompletionText
                        chosen-candidate))))
      (omnisharp--replace-symbol-in-buffer-with
       (omnisharp--current-word-or-empty-string)
       completion-text-to-insert))))

(defun omnisharp--current-word-or-empty-string ()
  (or (thing-at-point 'symbol)
      ""))

;; TODO Use a plist. This is ridiculous.
(defun omnisharp--convert-auto-complete-json-to-popup-format
  (json-result-alist)
  (mapcar
   (lambda (element)
     (popup-make-item
      ;; TODO get item from json-result-alist
      ;;
      ;; TODO these are already calculated in
      ;; omnisharp--auto-complete-display-function-popup, stored as
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
         (column-number (number-to-string (+ 1 (omnisharp--current-column))))
         (buffer-contents (omnisharp--get-current-buffer-contents))
         (filename-tmp (omnisharp--convert-slashes-to-double-backslashes
                        (or buffer-file-name "")))
         (params `((Line     . ,line-number)
                   (Column   . ,column-number)
                   (Buffer   . ,buffer-contents))))
    (if (/= 0 (length filename-tmp))
        (cons `(FileName . ,filename-tmp)
              params)
      params)))

(defun omnisharp-go-to-file-line-and-column (json-result)
  "Open file :FileName at :Line and :Column. If filename is not given,
defaults to the current file. This function works for a
GotoDefinitionResponse line json-result."
  (omnisharp-go-to-file-line-and-column-worker
   (cdr (assoc 'Line json-result))
   (- (cdr (assoc 'Column json-result)) 1)
   (cdr (assoc 'FileName json-result))))

(defun omnisharp-go-to-file-line-and-column-worker (line
                                                    column
                                                    &optional filename)
  "Open file filename at line and column. If filename is not given,
defaults to the current file. Saves the current location into the tag
ring so that the user may return with (pop-tag-mark)."
  (ring-insert find-tag-marker-ring (point-marker))
  (when (not (equal filename nil))
    (find-file filename))

  ;; calling goto-line directly results in a compiler warning.
  (let ((current-prefix-arg line))
    (call-interactively 'goto-line line))

  (move-to-column column))

(defun omnisharp--vector-to-list (vector)
  (append vector nil))

;;;###autoload
(defun omnisharp--popup-to-ido ()
  "When in a popup menu with autocomplete suggestions, calling this
function will close the popup and open an ido prompt instead.

Note that currently this will leave the popup menu active even when
the user selects a completion and the completion is inserted."

  (interactive) ; required. Otherwise call to this is silently ignored

  ;; TODO how to check if popup is active?
  (omnisharp--auto-complete-display-function-ido
   omnisharp--last-buffer-specific-auto-complete-result))

;;;###autoload
(defun omnisharp-current-type-information ()
  (interactive)
  (omnisharp-current-type-information-worker
   (omnisharp--get-common-params)))

(defun omnisharp-current-type-information-worker (params)
  (let ((json-result
         (omnisharp-post-message-curl-as-json
          (concat omnisharp-host "typelookup")
          params)))
    (message (cdr (assoc 'Type json-result)))))

(defun omnisharp-get-build-command ()
  "Retrieve the shell command to build the current solution."
  (omnisharp-post-message-curl
   (concat omnisharp-host "buildcommand")
   nil))

;;;###autoload
(defun omnisharp-build-in-emacs ()
  "Build the current solution in a non-blocking fashion inside emacs.
Uses the standard compilation interface (compile)."
  (interactive)
  (let ((build-command (omnisharp-get-build-command)))
    (omnisharp--recognize-mono-compilation-error-format)
    (compile
     ;; Build command contains backslashes on Windows systems. Work
     ;; around this by using double backslashes. Other systems are not
     ;; affected.
     (omnisharp--fix-build-command-if-on-windows
      build-command))))

(defun omnisharp--recognize-mono-compilation-error-format ()
  "Makes Emacs recognize the mono compiler errors as clickable
compilation buffer elements."
  (add-to-list 'compilation-error-regexp-alist
               '(" in \\(.+\\):\\([0-9]+\\)" 1 2)))

(defun omnisharp--fix-build-command-if-on-windows (command)
  "Fixes the build command gotten via omnisharp-get-build-command.
See function definition for an example.

If not on windows, returns COMMAND unchanged."

  ;; Example input without fix:
  ;; C:\Windows\Microsoft.NET\Framework64\v4.0.30319\Msbuild.exe /m /nologo
  ;;     /v:q /property:GenerateFullPaths=true
  ;;     \"c:/Projects/foo/foo.sln\"

  ;; This changes that to this:
  ;; C:/Windows/Microsoft.NET/Framework64/v4.0.30319/Msbuild.exe
  ;;     //m //nologo //v:q //property:GenerateFullPaths=true
  ;;     \"c://Projects//bowsville_freelancer//src//Bowsville.Freelancer.sln\"
  ;;
  ;; ^ this works :)


  (if (equal system-type 'windows-nt)
      ;; Compiler path fix. C:\Path is interpreted as C:Path
      (omnisharp--convert-backslashes-to-forward-slashes
       ;; Compiler parameter fix. Emacs thinks "/m" refers to the path
       ;; /m - that is, (root)/m
       (omnisharp--convert-slashes-to-double-slashes
        command))

    ;; Not on windows. Do not change.
    command))

(defun omnisharp--convert-backslashes-to-forward-slashes
  (string-to-convert)
  "Converts the given STRING-TO-CONVERT's backslashes to forward
slashes."
  (replace-regexp-in-string "\\\\" "/" string-to-convert))

(defun omnisharp--convert-slashes-to-double-slashes (command)
  (replace-regexp-in-string "/" "//" command))

;;;###autoload
(defun omnisharp-code-format ()
  "Format the code in the current file. Replaces the file contents
with the formatted result. Saves the file before starting."
  (interactive)
  (save-buffer)
  (omnisharp-code-format-worker
   ;; Add omnisharp-code-format-expand-tab to params
   (cons `(ExpandTab . ,omnisharp-code-format-expand-tab)
         (omnisharp--get-common-params))
   (buffer-file-name)
   (line-number-at-pos)
   (omnisharp--current-column)))

(defun omnisharp-code-format-worker (code-format-request
                                     filename
                                     current-line
                                     current-column)
  (let ((json-result
         (omnisharp-post-message-curl-as-json
          (concat omnisharp-host "codeformat")
          code-format-request)))
    (omnisharp--set-buffer-contents-to
     filename
     (cdr (assoc 'Buffer json-result))
     current-line
     current-column)))

;; This currently has no UI, so there only exists the
;; worker. Originally the plan was to be able to run manual syntax
;; checks but I couldn't figure out how to call them with flycheck.
(defun omnisharp-syntax-check-worker (params)
  "Takes a Request and returns a SyntaxErrorsResponse."
  (omnisharp-post-message-curl-as-json
   (concat omnisharp-host "syntaxerrors")
   params))

(flycheck-define-checker csharp-omnisharp-curl
  "A csharp source syntax checker using curl to call an OmniSharp
server process running in the background. Only checks the syntax - not
type errors."
  ;; This must be an external process. Currently flycheck does not
  ;; support using elisp functions as checkers.
  :command ((eval
             (let ((command-plist
                    (omnisharp--get-curl-command
                     (concat omnisharp-host "syntaxerrors")
                     (omnisharp--get-common-params))))
               (cons
                (plist-get command-plist :command)
                (plist-get command-plist :arguments)))))

  :error-patterns ((error line-start
                          (file-name) ":"
                          line ":"
                          column
                          " "
                          (message (one-or-more not-newline))))
  ;; TODO this should be moved out, but I can't get it to compile that
  ;; way.
  :error-parser (lambda (output checker buffer)
                  (let* ((json-result
                          (json-read-from-string output))
                         (errors (omnisharp--vector-to-list
                                  (cdr (assoc 'Errors json-result)))))
                    (when (not (equal (length errors) 0))
                      (mapcar (lambda (it)
                                (flycheck-error-new
                                 :buffer buffer
                                 :checker checker
                                 :filename (cdr (assoc 'FileName it))
                                 :line (cdr (assoc 'Line it))
                                 :column (cdr (assoc 'Column it))
                                 :message (cdr (assoc 'Message it))
                                 :level 'error))
                              errors))))
  ;; TODO use only is csharp files - but there are a few different
  ;; extensions available for these!
  :predicate (lambda () t))

;;;###autoload
(defun omnisharp-navigate-to-current-type-member ()
  (interactive)
  (omnisharp-navigate-to-current-type-member-worker
   (omnisharp--get-common-params)))

(defun omnisharp-navigate-to-current-type-member-worker (request)
  (let ((quickfixes (omnisharp-post-message-curl-as-json
                     (concat omnisharp-host "currentfilemembersasflat")
                     request)))
    (omnisharp--choose-and-go-to-quickfix-ido
     quickfixes)))

(defun omnisharp--choose-and-go-to-quickfix-ido (quickfixes)
  "Given a list of QuickFixes in list format (not JSON), displays them
in an ido-completing-read prompt and jumps to the chosen one's
Location."
  (let ((chosen-quickfix
         (omnisharp--choose-quickfix-ido
          (omnisharp--vector-to-list quickfixes))))
    (omnisharp-go-to-file-line-and-column chosen-quickfix)))

(defun omnisharp--choose-quickfix-ido (quickfixes)
  "Given a list of QuickFixes, lets the user choose one using
ido-completing-read. Returns the chosen element."
  (let* ((quickfix-choices (--map
                            (cdr (assoc 'Text it))
                            quickfixes))
         (chosen-quickfix-text
          (ido-completing-read
           "Go to: "
           ;; TODO use a hashmap if too slow.
           ;; This algorithm is two iterations in the worst case
           ;; scenario.
           quickfix-choices))
         (chosen-quickfix-index
          (position-if (lambda (quickfix-text)
                         (equal quickfix-text chosen-quickfix-text))
                       quickfix-choices)))
    (nth chosen-quickfix-index quickfixes)))

;;;###autoload
(defun omnisharp-navigate-to-type-in-current-file ()
  (interactive)
  (omnisharp-navigate-to-type-in-current-file-worker
   (omnisharp--get-common-params)))

(defun omnisharp-navigate-to-type-in-current-file-worker (request)
  (let ((quickfixes
         (omnisharp-post-message-curl-as-json
          (concat omnisharp-host "currentfiletopleveltypes")
          request)))
    (omnisharp--choose-and-go-to-quickfix-ido
     quickfixes)))

;; No need for a worker pattern since findsymbols takes no arguments
(defun omnisharp-nagivate-to-solution-member ()
  (interactive)
  (let ((quickfix-response
         (omnisharp-post-message-curl-as-json
          (concat omnisharp-host "findsymbols")
          nil)))
    (omnisharp--choose-and-go-to-quickfix-ido
     (omnisharp--vector-to-list
      (cdr (assoc 'QuickFixes quickfix-response))))))

;;;###autoload
(defun omnisharp-start-flycheck ()
  "Selects and starts the csharp-omnisharp-curl syntax checker for the
current buffer. Use this in your csharp-mode hook."
  (interactive)
  (flycheck-mode)
  (flycheck-select-checker 'csharp-omnisharp-curl)
  (flycheck-start-checker  'csharp-omnisharp-curl))

(provide 'omnisharp)

;;; omnisharp.el ends here
