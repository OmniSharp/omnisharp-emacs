;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;;; omnisharp.el --- Omnicompletion (intellisense) and more for C#
;; Copyright (C) 2013 Mika Vilpas (GPLv3)
;; Author: Mika Vilpas
;; Version: 2.4
;; Url: https://github.com/sp3ctum/omnisharp-emacs
;; Package-Requires: ((json "1.2") (dash "1.8.0") (popup "0.5") (auto-complete "1.4") (flycheck "0.19") (csharp-mode "0.8.6"))
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

(defgroup omnisharp ()
  "Omnisharp-emacs is a port of the awesome OmniSharp server to
the Emacs text editor. It provides IDE-like features for editing
files in C# solutions in Emacs, provided by an OmniSharp server
instance that works in the background."
  :group 'external
  :group 'csharp)

;;; Code:
(defcustom omnisharp-host "http://localhost:2000/"
  "Currently expected to end with a / character."
  :group 'omnisharp
  :type 'string)

(defcustom omnisharp-timeout 1
  "Timeout, in seconds, after which to abort stalling queries to the
OmniSharp server."
  :group 'omnisharp
  :type 'integer)

(defvar omnisharp-auto-complete-popup-want-isearch t
  "Whether to automatically start isearch when auto-completing.")

(defvar omnisharp--find-usages-buffer-name "* OmniSharp : Usages *"
  "The name of the temporary buffer that is used to display the
results of a 'find usages' call.")

(defvar omnisharp--find-implementations-buffer-name "* OmniSharp : Implementations *"
  "The name of the temporary buffer that is used to display the
results of a 'find implementations' call.")

(defvar omnisharp--last-auto-complete-result-buffer-name
  "* OmniSharp : Last auto-complete result *"
  "The name of the temporary buffer that is used to display the
results of an auto-complete call.")

(defvar omnisharp--last-auto-complete-result-buffer-header
  (concat
   "Last auto-complete result:"
   "\n\n")
  "The header for the temporary buffer that is used to display the
results of an auto-complete call.")

(defcustom omnisharp-auto-complete-popup-help-delay nil
  "The timeout after which the auto-complete popup will show its help
  popup. Disabled by default because the help is often scrambled and
  looks bad."
  :group 'omnisharp
  :type '(choice (const :tag "disabled" nil)
                 integer))

(defcustom omnisharp-auto-complete-popup-persist-help t
  "Whether to keep the help window (accessed by pressing f1 while the
popup window is active) open after any other key is
pressed. Defaults to true."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defvar-local
  omnisharp--last-buffer-specific-auto-complete-result
  nil
  "Contains the last result of an autocomplete query.")

(defcustom omnisharp-auto-complete-want-documentation t
  "Whether to include auto-complete documentation for each and every
response. This may be set to nil to get a speed boost for
completions."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)))

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

(defvar omnisharp--show-last-auto-complete-result-frontend
  'plain-buffer
  "Defines the function that is used for displaying the last
auto-complete result with various functions. Valid values are found in
omnisharp--auto-complete-display-backends-alist.")

(defvar omnisharp--show-last-auto-complete-result-frontends-alist
  '((plain-buffer . omnisharp--show-last-auto-complete-result-in-plain-buffer))
  "Holds an alist of all available frontends for displaying the last
auto-complete result.  See the documentation for the variable
omnisharp--show-last-auto-complete-result-frontend for more
information.")

(defcustom omnisharp-code-format-expand-tab t
  "Whether to expand tabs to spaces in code format requests."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)))

(defvar omnisharp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO add good default keys here
    ;;(define-key map (kbd "C-c f") 'insert-foo)
    map)
  "Keymap for omnisharp-mode.")

;; Note that emacs seems to internally expect windows paths to have
;; forward slashes.
(defcustom omnisharp--windows-curl-tmp-file-path
  "C:/omnisharp-tmp-file.cs"
  "The full file path where to save temporary stuff that gets sent to
the OmniSharp API. Only used on Windows.
Must be writable by the current user."
  :group 'omnisharp
  :type 'file)

(defcustom omnisharp--curl-executable-path
  "curl"
  "The absolute or relative path to the curl executable.")

;;;###autoload
(define-minor-mode omnisharp-mode
  "Omnicompletion (intellisense) and more for C# using an OmniSharp
server backend."
  :lighter " omnisharp"
  :global nil
  :keymap omnisharp-mode-map
  (when omnisharp-imenu-support
    (if omnisharp-mode
        (progn
          (setq imenu-create-index-function 'omnisharp-imenu-create-index)
          (imenu-add-menubar-index))
      (setq imenu-create-index-function 'imenu-default-create-index-function)))
  (when omnisharp-eldoc-support
    (when omnisharp-mode
      (make-local-variable 'eldoc-documentation-function)
      (setq eldoc-documentation-function 'omnisharp-eldoc-function)))

  ;; These are selected automatically when flycheck is enabled
  (add-to-list 'flycheck-checkers
               'csharp-omnisharp-curl)
  (add-to-list 'flycheck-checkers
               'csharp-omnisharp-curl-code-issues))

(easy-menu-define omnisharp-mode-menu omnisharp-mode-map
  "Menu for omnisharp-mode"
  '("OmniSharp"
    ("Auto-complete"
     ["at point" omnisharp-auto-complete]
     ["Add . and complete members" omnisharp-add-dot-and-auto-complete]
     ["Override superclass member" omnisharp-auto-complete-overrides]
     ["Show last result" omnisharp-show-last-auto-complete-result]
     ["Show overloads at point" omnisharp-show-overloads-at-point])

    ("Navigate to.."
     ["Definition at point" omnisharp-go-to-definition]
     ["Current file member" omnisharp-navigate-to-current-file-member]
     ["Type in current file" omnisharp-navigate-to-type-in-current-file]
     ["Solution member" omnisharp-navigate-to-solution-member]
     ["File in solution" omnisharp-navigate-to-solution-file]
     ["Region in current file" omnisharp-navigate-to-region])

    ("OmniSharp server"
     ["Start OmniSharp server with solution (.sln) file" omnisharp-start-omnisharp-server]
     ["Reload solution" omnisharp-reload-solution]
     ["Stop OmniSharp server" omnisharp-stop-server]
     ["Check alive status" omnisharp-check-alive-status]
     ["Check ready status" omnisharp-check-ready-status])

    ("Current symbol"
     ["Show type" omnisharp-current-type-information]
     ["Show type and add it to kill ring" omnisharp-current-type-information-to-kill-ring]
     ["Find usages" omnisharp-find-usages]
     ["Find implementations" omnisharp-find-implementations]
     ["Rename" omnisharp-rename]
     ["Rename interactively" omnisharp-rename-interactively])

    ("Solution actions"
     ["Add current file to solution" omnisharp-add-to-solution-current-file]
     ["Remove current file from solution" omnisharp-remove-from-project-current-file]
     ["Add marked files in dired to solution" omnisharp-add-to-solution-dired-selected-files]
     ["Remove marked files in dired from solution" omnisharp-remove-from-project-current-file]
     ["Add reference to dll or project" omnisharp-add-reference]
     ["Build solution in emacs" omnisharp-build-in-emacs]
     ["Start syntax check" flycheck-mode]
     ["Fix code issue at point" omnisharp-fix-code-issue-at-point]
     )

    ["Run contextual code action / refactoring at point" omnisharp-run-code-action-refactoring]
    ["Run code format on current buffer" omnisharp-code-format]
    ))

(defun omnisharp-get-host ()
  "Makes sure omnisharp-host is ended by / "
  (if (string= (substring omnisharp-host -1 ) "/")
      omnisharp-host
    (concat omnisharp-host "/")))

(defun omnisharp-reload-solution ()
  "Reload the current solution."
  (interactive)
  (omnisharp-post-message-curl
   (concat (omnisharp-get-host) "reloadsolution")
   ;; no params needed
   nil))

(defun omnisharp-go-to-definition (&optional other-window)
  "Jump to the definition of the symbol under point. With prefix
argument, use another window."
  (interactive "P")
  (let* ((json-result (omnisharp-post-message-curl-as-json
                       (concat (omnisharp-get-host) "gotodefinition")
                       (omnisharp--get-common-params)))
         (filename (cdr (assoc 'FileName json-result))))
    (if (null filename)
        (message
         "Cannot go to definition as none was returned by the API.")
      (omnisharp-go-to-file-line-and-column json-result other-window))))

(defun omnisharp-go-to-definition-other-window ()
  "Do `omnisharp-go-to-definition' displaying the result in a different window."
  (interactive)
  (omnisharp-go-to-definition t))

(defun omnisharp-find-usages ()
  "Find usages for the symbol under point"
  (interactive)
  (message "Finding usages...")
  (omnisharp-find-usages-worker
   (omnisharp--get-common-params)
   (lambda (quickfixes)
     (if (equal 0 (length quickfixes))
         (message "No usages found."))
     (omnisharp--write-quickfixes-to-compilation-buffer
      quickfixes
      omnisharp--find-usages-buffer-name
      omnisharp-find-usages-header))))

(defun omnisharp-find-usages-worker (request callback)
  "Gets a list of QuickFix lisp objects from a findusages api call
asynchronously. On completions, CALLBACK is run with the quickfixes as its only argument."
  (declare (indent defun))
  (omnisharp-post-message-curl-as-json-async
   (concat (omnisharp-get-host) "findusages")
   request
   (lambda (quickfix-response)
     (apply callback (list (omnisharp--vector-to-list
                            (cdr (assoc 'QuickFixes quickfix-response))))))))

(defun omnisharp-find-implementations ()
  "Show a buffer containing all implementations of the interface under
point, or classes derived from the class under point. Allow the user
to select one (or more) to jump to."
  (interactive)
  (message "Finding implementations...")
  (omnisharp-find-implementations-worker
   (omnisharp--get-common-params)
   (lambda (quickfixes)
     (if (equal 0 (length quickfixes))
         (message "No implementations found."))
     (omnisharp--write-quickfixes-to-compilation-buffer
      quickfixes
      omnisharp--find-implementations-buffer-name
      omnisharp-find-implementations-header))))

(defun omnisharp-find-implementations-worker (request callback)
  "Gets a list of QuickFix lisp objects from a findimplementations api call
asynchronously. On completions, CALLBACK is run with the quickfixes as its only argument."
  (declare (indent defun))
  (omnisharp-post-message-curl-as-json-async
   (concat (omnisharp-get-host) "findimplementations")
   request
   (lambda (quickfix-response)
     (apply callback (list (omnisharp--vector-to-list
                            (cdr (assoc 'QuickFixes quickfix-response))))))))

(defun omnisharp-navigate-to-region
  (&optional other-window)
  "Navigate to region in current file. If OTHER-WINDOW is given and t,
use another window."
  (interactive "P")
  (let ((quickfix-response
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "gotoregion")
          (omnisharp--get-common-params))))
    (omnisharp--choose-and-go-to-quickfix-ido
     (cdr (assoc 'QuickFixes quickfix-response))
     other-window)))

(defun omnisharp-rename ()
  "Rename the current symbol to a new name. Lets the user choose what
name to rename to, defaulting to the current name of the symbol."
  (interactive)
  (let* ((current-word (thing-at-point 'symbol))
         (rename-to (read-string "Rename to: " current-word))
         (rename-request
          (cons `(RenameTo . ,rename-to)
                (omnisharp--get-common-params)))

         (modified-file-responses
          (omnisharp-rename-worker rename-request))
         (location-before-rename
          (omnisharp--get-common-params-for-emacs-side-use)))

    ;; save-excursion does not work here for some reason.

    ;; Set all modified files' contents to what the server thinks they
    ;; now contain. Doing this will make the user see the results of
    ;; the rename.
    (--each modified-file-responses
      (omnisharp--set-buffer-contents-to
       (cdr (assoc 'FileName it))
       (cdr (assoc 'Buffer it))))

    ;; Keep point in the buffer that initialized the rename so that
    ;; the user deos not feel disoriented
    (omnisharp-go-to-file-line-and-column location-before-rename)

    (message "Rename complete in files: %s"
             (--map (cdr (assoc 'FileName it))
                    modified-file-responses))))

(defun omnisharp-rename-worker (rename-request)
  "Given a RenameRequest, returns a list of ModifiedFileResponse
objects."
  (let* ((rename-responses
          (omnisharp-post-message-curl-as-json
           (concat (omnisharp-get-host) "rename")
           rename-request))
         (modified-files (omnisharp--vector-to-list
                          (cdr (assoc 'Changes rename-responses)))))
    modified-files))

(defun omnisharp-rename-interactively ()
  "Rename the current symbol to a new name. Lets the user choose what
name to rename to, defaulting to the current name of the symbol. Any
renames require interactive confirmation from the user."
  (interactive)
  (let* ((current-word (thing-at-point 'symbol))
         (rename-to (read-string "Rename to: " current-word))
         (delimited
          (y-or-n-p "Only rename full words?"))
         (all-solution-files
          (omnisharp--get-solution-files-list-of-strings))
         (location-before-rename
          (omnisharp--get-common-params-for-emacs-side-use)))
    (tags-query-replace current-word
                        rename-to
                        delimited
                        ;; This is expected to be a form that will be
                        ;; evaluated to get the list of all files to
                        ;; process.
                        'all-solution-files)
    ;; Keep point in the buffer that initialized the rename so that
    ;; the user deos not feel disoriented
    (omnisharp-go-to-file-line-and-column location-before-rename)))

(defun omnisharp--write-quickfixes-to-compilation-buffer
  (quickfixes
   buffer-name
   buffer-header
   &optional dont-save-old-pos)
  "Takes a list of QuickFix objects and writes them to the
compilation buffer with HEADER as its header. Shows the buffer
when finished.

If DONT-SAVE-OLD-POS is specified, will not save current position to
find-tag-marker-ring. This is so this function may be used without
messing with the ring."
  (let ((output-in-compilation-mode-format
         (mapcar
          'omnisharp--find-usages-output-to-compilation-output
          quickfixes)))

    (omnisharp--write-lines-to-compilation-buffer
     output-in-compilation-mode-format
     (get-buffer-create buffer-name)
     buffer-header)
    (unless dont-save-old-pos
      (ring-insert find-tag-marker-ring (point-marker))
      (omnisharp--show-last-buffer-position-saved-message
       (buffer-file-name)))))

(defun omnisharp--write-lines-to-compilation-buffer
  (lines-to-write buffer-to-write-to &optional header)
  "Writes the given lines to the given buffer, and sets
compilation-mode on. The contents of the buffer are erased. The
buffer is marked read-only after inserting all lines.

LINES-TO-WRITE are the lines to write, as-is.

If HEADER is given, that is written to the top of the buffer.

Expects the lines to be in a format that compilation-mode
recognizes, so that the user may jump to the results."
  (with-current-buffer buffer-to-write-to
    (let ((inhibit-read-only t))
      ;; read-only-mode new in Emacs 24.3
      (if (fboundp 'read-only-mode)
          (read-only-mode nil)
        (setq buffer-read-only nil))
      (erase-buffer)

      (when (not (null header))
        (insert header))

      (mapc (lambda (element)
              (insert element)
              (insert "\n"))
            lines-to-write)
      (compilation-mode)
      (if (fboundp 'read-only-mode)
          (read-only-mode t)
        (setq buffer-read-only t))
      (display-buffer buffer-to-write-to))))

(defun omnisharp--find-usages-output-to-compilation-output
  (json-result-single-element)
  "Converts a single element of a /findusages JSON response to a
format that the compilation major mode understands and lets the user
follow results to the locations in the actual files."
  (let ((filename (cdr (assoc 'FileName json-result-single-element)))
        (text (cdr (assoc 'Text json-result-single-element)))
        (line (cdr (assoc 'Line json-result-single-element)))
        (column (cdr (assoc 'Column json-result-single-element)))
        (text (cdr (assoc 'Text json-result-single-element))))
    (concat filename
            ":"
            (prin1-to-string line)
            ":"
            (prin1-to-string column)
            ": \n"
            text
            "\n")))

(defun omnisharp-stop-server ()
  "Stop the current omnisharp instance."
  (interactive)
  (omnisharp-post-message-curl
   (concat (omnisharp-get-host) "stopserver")
   nil))

;; TODO create omnisharp-add-to-solution that lets user choose which
;; file to add.
(defun omnisharp-add-to-solution-current-file ()
  (interactive)
  (let ((params (omnisharp--get-common-params)))
    (omnisharp-add-to-solution-worker params)
    (message "Added %s to the solution."
             (cdr (assoc 'FileName params)))))

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
   (concat (omnisharp-get-host) "addtoproject")
   params))

(defun omnisharp-remove-from-project-current-file ()
  (interactive)
  (let ((params (omnisharp--get-common-params)))
    (omnisharp-remove-from-project-current-file-worker params)
    (message "Removed %s from the solution."
             (cdr (assoc 'FileName params)))))

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
   (concat (omnisharp-get-host) "removefromproject")
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
         (params (cl-pushnew `(Reference . ,path-to-ref-file-to-add)
			     tmp-params)))
    (omnisharp-add-reference-worker params)))

(defun omnisharp-add-reference-worker (params)
  (omnisharp-post-message-curl-as-json
   (concat (omnisharp-get-host) "addreference")
   params))

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
(defvar ac-source-omnisharp
  '((candidates . omnisharp--get-auto-complete-result-in-popup-format)))

(defun ac-complete-omnisharp nil
  (interactive)
  (auto-complete '(ac-source-omnisharp)))

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


;; company-mode integration
(defvar omnisharp-company-do-template-completion nil
  "Set to t if you want in-line parameter completion, nil
  otherwise. CURRENTLY UNSUPPORTED.")

(defvar omnisharp-company-type-separator " : "
  "The string used to visually seperate functions/variables from
  their types")

(defvar omnisharp-company-ignore-case t
  "If t, case is ignored in completion matches.")

(defvar omnisharp-company-begin-after-member-access t
  "If t, begin completion when pressing '.' after a class, object
  or namespace")

(defvar omnisharp-imenu-support nil
"If t, activate imenu integration. Defaults to nil.")

(defvar omnisharp-eldoc-support t
"If t, activate eldoc integration - eldoc-mode must also be enabled for
 this to work. Defaults to t.")

(defvar omnisharp--eldoc-fontification-buffer-name " * OmniSharp : Eldoc Fontification *"
  "The name of the buffer that is used to fontify eldoc strings.")

;; Path to the server
(defcustom omnisharp-server-executable-path nil
"Path to OmniSharpServer. If its value is nil, search for the server in the exec-path")

(defun omnisharp-company--prefix ()
  "Returns the symbol to complete. Also, if point is on a dot,
triggers a completion immediately"
  (let ((symbol (company-grab-symbol)))
    (if symbol
        (if (and omnisharp-company-begin-after-member-access
                 (save-excursion
                   (forward-char (- (length symbol)))
                   (looking-back "\\." (- (point) 2))))
            (cons symbol t)
          symbol)
      'stop)))

(defun company-omnisharp (command &optional arg &rest ignored)
  "`company-mode' completion back-end using OmniSharp."
  (case command
    (prefix (and omnisharp-mode
                 (not (company-in-string-or-comment))
                 (omnisharp-company--prefix)))

    (candidates (omnisharp--get-company-candidates arg))

    ;; because "" doesn't return everything
    (no-cache (equal arg ""))

    (annotation (omnisharp--company-annotation arg))

    (meta (omnisharp--get-company-candidate-data arg 'DisplayText))

    (doc-buffer (let ((doc-buffer (company-doc-buffer
                                   (omnisharp--get-company-candidate-data arg 'Description))))
                  (with-current-buffer doc-buffer
                    (visual-line-mode))
                  doc-buffer))

    (ignore-case omnisharp-company-ignore-case)

    (post-completion (let ((ann (omnisharp--company-annotation arg)))
                       (when (and omnisharp-company-do-template-completion
                                  ann (string-match-p "([^)]" ann))
                         ;; This was a function match, do templating.
                         (insert ann)
                         (company-template-c-like-templatify ann))))))

(defun omnisharp--make-company-completion (item)
  "`company-mode' expects the beginning of the candidate to be
the same as the characters being completed.  This method converts
a function description of 'void SomeMethod(int parameter)' to
string 'SomeMethod' propertized with annotation 'void
SomeMethod(int parameter)' and the original value ITEM."
  (let* ((case-fold-search nil)
         (completion (omnisharp--completion-result-item-get-completion-text item))
         (display (omnisharp--completion-result-item-get-display-text item))
         output
         annotation)
    
    ;; Remove any trailing brackets from the completion string
    (setq output (car (split-string completion "(")))
    (setq annotation (concat omnisharp-company-type-separator display))
    (add-text-properties 0 (length output)
                         (list 'omnisharp-item item 'omnisharp-ann annotation)
                         output)
    output))

(defun omnisharp--get-company-candidates (pre)
  "Returns completion results in company format.  Company-mode
doesn't make any distinction between the text to be inserted and
the text to be displayed.  As a result, since we want to see
parameters and things, we need to munge 'DisplayText so it's
company-mode-friendly"
  (let* ((json-false :json-false)
         ;; json-false helps distinguish between null and false in
         ;; json. This is an emacs limitation.
         (params
          (omnisharp--get-auto-complete-params))
         (json-result-auto-complete-response
          (omnisharp-auto-complete-worker params)))
    (all-completions pre (mapcar #'omnisharp--make-company-completion
                                 json-result-auto-complete-response))))

(defun omnisharp--company-annotation (candidate)
  (get-text-property 0 'omnisharp-ann candidate))

(defun omnisharp--get-company-candidate-data (candidate datatype)
  "Return the DATATYPE request (e.g. 'DisplayText) for CANDIDATE."
  (let ((item (get-text-property 0 'omnisharp-item candidate)))
    (cdr (assoc datatype item))))

;;Add this completion backend to company-mode
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-omnisharp))



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

(defun omnisharp--get-last-auto-complete-result-display-function ()
  "Returns a function that can be fed the output from
omnisharp-auto-complete-worker (an AutoCompleteResponse). The function
must take a single argument, the auto-complete result texts to show."
  (cdr (assoc omnisharp--show-last-auto-complete-result-frontend
              omnisharp--show-last-auto-complete-result-frontends-alist)))

(defun omnisharp-auto-complete-worker (auto-complete-request)
  "Takes an AutoCompleteRequest and makes an autocomplete query with
them.

Returns the raw JSON result. Also caches that result as
omnisharp--last-buffer-specific-auto-complete-result."
  (let ((json-result
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "autocomplete")
          auto-complete-request)))
    ;; Cache result so it may be juggled in different contexts easily
    (setq omnisharp--last-buffer-specific-auto-complete-result
          json-result)))

(defun omnisharp-auto-complete-overrides ()
  (interactive)
  (let ((params (omnisharp--get-common-params)))
    (omnisharp-auto-complete-overrides-worker params)))

(defun omnisharp-auto-complete-overrides-worker (params)
  (let* ((json-result
          (omnisharp--vector-to-list
           (omnisharp-post-message-curl-as-json
            (concat (omnisharp-get-host) "getoverridetargets")
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
                      (concat (omnisharp-get-host) "runoverridetarget")
                      params)))
    (omnisharp--set-buffer-contents-to
     (cdr (assoc 'FileName json-result))
     (cdr (assoc 'Buffer   json-result))
     (cdr (assoc 'Line     json-result))
     (cdr (assoc 'Column   json-result)))))

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
           (cdr (assoc 'CodeActions actions-vector)))))
    ;; Exit early if no refactorings are provided by the API.
    (if (<= (length actions-list) 0)
        (message "No refactorings available at this position.")

      (progn
        (let* ((chosen-action (ido-completing-read
                               "Run code action: "
                               actions-list
                               t))
               (chosen-action-index
                (position chosen-action actions-list)))

          (omnisharp-run-code-action-refactoring-worker
           chosen-action-index))))))

(defun omnisharp--get-code-actions-from-api ()
  "Fetches and returns a GetCodeActionsResponse: the runnable
refactoring code actions for the current file and position."
  (omnisharp-post-message-curl-as-json
   (concat (omnisharp-get-host) "getcodeactions")
   (->> (omnisharp--get-common-params)
     (cons `(SelectionStartColumn . ,(omnisharp--region-start-column)))
     (cons `(SelectionStartLine   . ,(omnisharp--region-start-line)))
     (cons `(SelectionEndColumn   . ,(omnisharp--region-end-column)))
     (cons `(SelectionEndLine     . ,(omnisharp--region-end-line))))))

(defun omnisharp--with-minimum-value (min-number actual-number)
  "If ACTUAL-NUMBER is less than MIN-NUMBER, return MIN-NUMBER.
Otherwise return ACTUAL-NUMBER."
  (if (< actual-number min-number)
      min-number
    actual-number))

(defun omnisharp--region-start-line ()
  (when (region-active-p)
    (save-excursion
      (goto-char (region-beginning))
      (line-number-at-pos))))

(defun omnisharp--region-end-line ()
  (when (region-active-p)
    (save-excursion
      (goto-char (region-end))
      (line-number-at-pos))))

(defun omnisharp--region-start-column ()
  (when (region-active-p)
    (save-excursion
      (goto-char (region-beginning))
      (omnisharp--with-minimum-value 1
                                     (omnisharp--current-column)))))

(defun omnisharp--region-end-column ()
  (when (region-active-p)
    (save-excursion
      ;; evil-mode has its own Vim-like concept of the region. A
      ;; visual line selection in evil-mode reports the end column to
      ;; be 0 in some cases. Work around this.
      (if (and (boundp 'evil-visual-end) evil-visual-end)
          (progn
            (goto-char evil-visual-end)
            ;; Point moves to the next line for some reason. So move
            ;; it back
            (backward-char))
        (goto-char (region-end)))
      (omnisharp--with-minimum-value 1
                                     (omnisharp--current-column)))))

(defun omnisharp-run-code-action-refactoring-worker
  (chosen-action-index)

  (let* ((run-action-params
          (->> (omnisharp--get-common-params)
            (cons `(CodeAction . ,chosen-action-index))
            (cons `(SelectionStartColumn . ,(omnisharp--region-start-column)))
            (cons `(SelectionStartLine   . ,(omnisharp--region-start-line)))
            (cons `(SelectionEndColumn   . ,(omnisharp--region-end-column)))
            (cons `(SelectionEndLine     . ,(omnisharp--region-end-line)))))
         (json-run-action-result ; RunCodeActionsResponse
          (omnisharp-post-message-curl-as-json
           (concat (omnisharp-get-host) "runcodeaction")
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
                                          &optional
                                          result-point-line
                                          result-point-column)
  "Sets the buffer contents to new-buffer-contents for the buffer
visiting filename-for-buffer. If no buffer is visiting that file, does
nothing. Afterwards moves point to the coordinates RESULT-POINT-LINE
and RESULT-POINT-COLUMN.

If RESULT-POINT-LINE and RESULT-POINT-COLUMN are not given, and a
buffer exists for FILENAME-FOR-BUFFER, its current positions are
used. If a buffer does not exist, the file is visited and the default
point position is used."
  (omnisharp--find-file-possibly-in-other-window
   filename-for-buffer nil) ; not in other-window

  ;; Default values are the ones in the buffer that is visiting
  ;; filename-for-buffer.
  (setq result-point-line
        (or result-point-line (line-number-at-pos)))
  (setq result-point-column
        (or result-point-column (omnisharp--current-column)))

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
  (let ((all-open-buffers-list
         (-map 'buffer-file-name (buffer-list))))
    (--any? (string-equal file-name it)
           all-open-buffers-list)))

(defun omnisharp--get-current-buffer-contents ()
  (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)))

(defun omnisharp-post-message-curl (url &optional params)
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

(defun omnisharp-post-message-curl-async (url params callback)
  "Post json stuff to url asynchronously with --data set to given params.
On completion, CALLBACK is run with the result as it's only parameter.

Returns the curl process"
  (declare (indent defun))
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
         (apply callback
                (list (progn (let ((output (with-current-buffer process-buffer (buffer-string))))
                               (kill-buffer process-buffer)
                               output)))))))
    process))

(defun omnisharp--get-curl-command (url params)
  "Returns a command that may be used to communicate with the API via
the curl program. Depends on the operating system."
  (if (equal system-type 'windows-nt)
      (omnisharp--get-curl-command-windows-with-tmp-file url params)
    (omnisharp--get-curl-command-unix url params)))

(defun omnisharp--get-curl-command-executable-string-for-api-name
  (params api-name)
  "Returns the full command to call curl with PARAMS for the api API-NAME.
Example: when called with \"getcodeactions\", returns
\"curl (stuff) http://localhost:2000/getcodeactions (stuff)\"
with \"stuff\" set to sensible values."
  (let ((command-plist
         (omnisharp--get-curl-command
          (concat (omnisharp-get-host) api-name)
          params)))
    (cons
     (plist-get command-plist :command)
     (plist-get command-plist :arguments))))

(defun omnisharp--get-curl-command-unix (url params)
  "Returns a command using plain curl that can be executed to
communicate with the API."
  `(:command ,omnisharp--curl-executable-path
             :arguments
             ("--silent" "-H" "Content-type: application/json"
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
                 omnisharp--windows-curl-tmp-file-path)))
    `(:command ,omnisharp--curl-executable-path
               :arguments
               ("--silent" "-H" "Content-type: application/json"
                "--data-binary"
                ;; @ specifies a file path to curl
                ,path-with-curl-prefix
                ,url))))

(defun omnisharp--write-json-params-to-tmp-file
  (target-path stuff-to-write-to-file)
  "Deletes the file when done."
  (with-temp-file target-path
    (insert stuff-to-write-to-file)))

(defun omnisharp--json-read-from-string (json-string
                                         &optional error-message)
  "Deserialize the given JSON-STRING to a lisp object. If
something goes wrong, return a human-readable warning."
  (condition-case nil
      (json-read-from-string json-string)
    (error
     (or error-message
         "Error communicating to the OmniSharpServer instance"))))

(defun omnisharp-post-message-curl-as-json (url &optional params)
  (omnisharp--json-read-from-string
   (omnisharp-post-message-curl url params)))

(defun omnisharp-post-message-curl-as-json-async (url params callback)
  "Posts message to curl at URL with PARAMS asynchronously.
On completion, the curl output is parsed as json and passed into CALLBACK."
  (omnisharp-post-message-curl-async url params
    (lambda (str)
      (apply callback (list (omnisharp--json-read-from-string str))))))

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
         (filename-tmp (or buffer-file-name ""))
         (params `((Line     . ,line-number)
                   (Column   . ,column-number)
                   (Buffer   . ,buffer-contents))))
    (if (/= 0 (length filename-tmp))
        (cons `(FileName . ,filename-tmp)
              params)
      params)))

(defun omnisharp--get-common-params-for-emacs-side-use ()
  "Gets a Request class that can be only handled safely inside
Emacs. This should not be transferred to the server backend - it might
not work on all platforms."
  (let* ((line-number (line-number-at-pos))
         (column-number (omnisharp--current-column))
         (buffer-contents (omnisharp--get-current-buffer-contents))
         (filename-tmp (or buffer-file-name ""))
         (params `((Line     . ,line-number)
                   (Column   . ,column-number)
                   (Buffer   . ,buffer-contents))))
    (if (/= 0 (length filename-tmp))
        (cons `(FileName . ,filename-tmp)
              params)
      params)))

(defun omnisharp-go-to-file-line-and-column (json-result
                                             &optional other-window)
  "Open file :FileName at :Line and :Column. If filename is not given,
defaults to the current file. This function works for a
QuickFix class json result."
  (omnisharp-go-to-file-line-and-column-worker
   (cdr (assoc 'Line json-result))
   (- (cdr (assoc 'Column json-result)) 1)
   (cdr (assoc 'FileName json-result))
   other-window))

(defun omnisharp-go-to-file-line-and-column-worker (line
                                                    column
                                                    &optional filename
                                                    other-window
                                                    dont-save-old-pos)
  "Open file filename at line and column. If filename is not given,
defaults to the current file. Saves the current location into the tag
ring so that the user may return with (pop-tag-mark).

If DONT-SAVE-OLD-POS is specified, will not save current position to
find-tag-marker-ring. This is so this function may be used without
messing with the ring."

  (let ((position-before-jumping (point-marker)))
    (when filename
      (omnisharp--find-file-possibly-in-other-window filename
                                                     other-window))

    ;; calling goto-line directly results in a compiler warning.
    (with-no-warnings
      (goto-line line))

    (move-to-column column)

    (unless dont-save-old-pos
      (omnisharp--save-position-to-find-tag-marker-ring
       position-before-jumping)
      (omnisharp--show-last-buffer-position-saved-message
       (buffer-file-name
        (marker-buffer position-before-jumping))))))

(defun omnisharp--show-last-buffer-position-saved-message
  (&optional file-name)
  "Notifies the user that the previous buffer position has been saved
with a message in the minibuffer. If FILE-NAME is given, shows that as
the file. Otherwise uses the current file name."
  (message "Previous position in %s saved. Go back with (pop-tag-mark)."
           (or file-name
               (buffer-file-name))))

(defun omnisharp--save-position-to-find-tag-marker-ring
  (&optional marker)
  "Record position in find-tag-marker-ring. If MARKER is non-nil,
record that position. Otherwise record the current position."
  (setq marker (or marker (point-marker)))
  (ring-insert find-tag-marker-ring marker))

(defun omnisharp--find-file-possibly-in-other-window
  (filename &optional other-window)
  "Open a buffer editing FILENAME. If no buffer for that filename
exists, a new one is created.
If the optional argument OTHER-WINDOW is non-nil, uses another
window."

  (cond
   ((omnisharp--buffer-exists-for-file-name filename)
    (let ((target-buffer-to-switch-to
           (--first (string= (buffer-file-name it)
                             filename)
                    (buffer-list))))
      (if other-window
          (pop-to-buffer target-buffer-to-switch-to)
        (pop-to-buffer-same-window target-buffer-to-switch-to))))

   (t ; no buffer for this file exists yet
    (funcall (if other-window
               'find-file-other-window
             'find-file)
           filename))))

(defun omnisharp--vector-to-list (vector)
  (append vector nil))

(defun omnisharp--popup-to-ido ()
  "When in a popup menu with autocomplete suggestions, calling this
function will close the popup and open an ido prompt instead.

Note that currently this will leave the popup menu active even when
the user selects a completion and the completion is inserted."

  (interactive) ; required. Otherwise call to this is silently ignored

  ;; TODO how to check if popup is active?
  (omnisharp--auto-complete-display-function-ido
   omnisharp--last-buffer-specific-auto-complete-result))

(defun omnisharp-current-type-information (&optional add-to-kill-ring)
  "Display information of the current type under point. With prefix
argument, add the displayed result to the kill ring. This can be used
to insert the result in code, for example."
  (interactive "P")
  (let ((current-type-information
         (omnisharp-current-type-information-worker 'Type
          (omnisharp--get-common-params))))

    (message current-type-information)
    (when add-to-kill-ring
      (kill-new current-type-information))))

(defun omnisharp-current-type-documentation (&optional add-to-kill-ring)
  "Display documentation of the current type under point. With prefix
argument, add the displayed result to the kill ring. This can be used
to insert the result in code, for example."
  (interactive "P")
  (let ((current-type-information
         (omnisharp-current-type-information-worker 'Documentation
          (omnisharp--get-common-params))))

    (message current-type-information)
    (when add-to-kill-ring
      (kill-new current-type-information))))

(defun omnisharp-current-type-information-worker (datatype params)
  "Returns information about the type under the cursor in the given
PARAMS as a single human-readable string."
  (let ((json-result
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "typelookup")
          params)))
    (cdr (assoc datatype json-result))))


(defun omnisharp-current-type-information-to-kill-ring ()
  "Shows the information of the current type and adds it to the kill
ring."
  (interactive)
  (omnisharp-current-type-information t))

(defun omnisharp-get-build-command ()
  "Retrieve the shell command to build the current solution."
  (omnisharp-post-message-curl
   (concat (omnisharp-get-host) "buildcommand")
   nil))

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
      build-command))
    (add-to-list 'compile-history build-command)))

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
          (concat (omnisharp-get-host) "codeformat")
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
   (concat (omnisharp-get-host) "syntaxerrors")
   params))

;; Flycheck interns this variable and uses it as the program to get
;; the errors with.
(setq flycheck-csharp-omnisharp-curl-executable
      omnisharp--curl-executable-path)

(flycheck-define-checker csharp-omnisharp-curl
  "A csharp source syntax checker using curl to call an OmniSharp
server process running in the background. Only checks the syntax - not
type errors."
  ;; This must be an external process. Currently flycheck does not
  ;; support using elisp functions as checkers.
  :command ("curl" ; this is overridden by
                   ; flycheck-csharp-omnisharp-curl-executable if it
                   ; is set
            (eval
             (omnisharp--get-curl-command-executable-string-for-api-name
              (omnisharp--get-common-params)
              "syntaxerrors")))

  :error-patterns ((error line-start
                          (file-name) ":"
                          line ":"
                          column
                          " "
                          (message (one-or-more not-newline))))
  :error-parser (lambda (output checker buffer)
                  (omnisharp--flycheck-error-parser-raw-json
                   output checker buffer))

  :predicate (lambda () omnisharp-mode)
  :next-checkers ((no-errors . csharp-omnisharp-curl-code-issues)))

(flycheck-define-checker csharp-omnisharp-curl-code-issues
  "Reports code issues (refactoring suggestions) that the user can
then accept and have fixed automatically."
  :command ("curl"
            (eval
             (omnisharp--get-curl-command-executable-string-for-api-name
              (omnisharp--get-common-params)
              "getcodeissues")))

  :error-patterns ((warning line-start
                            (file-name) ":"
                            line ":"
                            column
                            " "
                            (message (one-or-more not-newline))))
  :error-parser (lambda (output checker buffer)
                  (omnisharp--flycheck-error-parser-raw-json
                   output checker buffer 'info))
  :predicate (lambda () omnisharp-mode))

(defun omnisharp--flycheck-error-parser-raw-json
  (output checker buffer &optional error-level)
  "Takes either a QuickFixResponse or a SyntaxErrorsResponse as a
json string. Returns flycheck errors created based on the locations in
the json."
  (let* ((json-result
          (omnisharp--json-read-from-string output))
         (errors (omnisharp--vector-to-list
                  ;; Support both a SyntaxErrorsResponse and a
                  ;; QuickFixResponse. they are essentially the same,
                  ;; but have the quickfixes (buffer locations) under
                  ;; different property names.
                  (cdr (or (assoc 'QuickFixes json-result)
                           (assoc 'Errors json-result))))))
    (when (not (equal (length errors) 0))
      (mapcar (lambda (it)
                (flycheck-error-new
                 :buffer buffer
                 :checker checker
                 :filename (cdr (assoc 'FileName it))
                 :line (cdr (assoc 'Line it))
                 :column (cdr (assoc 'Column it))
                 ;; A CodeIssues response has Text instead of Message
                 :message (cdr (or (assoc 'Message it)
                                   (assoc 'Text it)))
                 :level (or error-level 'error)))
              errors))))

(defun omnisharp--imenu-make-marker (element)
  "Takes a QuickCheck element and returns the position of the
cursor at that location"
  (let* ((element-line (cdr (assoc 'Line quickfix-alist)))
         (element-column (cdr (assoc 'Column quickfix-alist)))
         (element-filename (cdr (assoc 'Filename quickfix-alist)))
         (use-buffer (current-buffer)))
    (save-excursion
        (omnisharp-go-to-file-line-and-column-worker
         element-line
         element-column
         element-filename
         nil ; other-window
         ;; dont-save-old-pos
         t)
        (point-marker))))

(defun omnisharp-imenu-create-index ()
  "Imenu callback function - returns an alist of ((member-name . position))"
  (interactive)
  (condition-case nil
      (let* ((quickfixes (omnisharp-post-message-curl-as-json
                          (concat (omnisharp-get-host) "currentfilemembersasflat")
                          (omnisharp--get-common-params)))
             (list-quickfixes (omnisharp--vector-to-list quickfixes))
             (imenu-list (mapcar (lambda (quickfix-alist)
                                   (cons (cdr (assoc 'Text quickfix-alist))
                                         (omnisharp--imenu-make-marker quickfix-alist)))
                                 list-quickfixes)))
        imenu-list)
    (error nil)))


(defun omnisharp-navigate-to-current-file-member
  (&optional other-window)
  "Show a list of all members in the current file, and jump to the
selected member. With prefix argument, use another window."
  (interactive "P")
  (omnisharp-navigate-to-current-file-member-worker
   (omnisharp--get-common-params)
   other-window))

(defun omnisharp-navigate-to-current-file-member-other-window ()
  (interactive)
  (omnisharp-navigate-to-current-file-member t))

(defun omnisharp-navigate-to-current-file-member-worker
  (request &optional other-window)
  (let ((quickfixes (omnisharp-post-message-curl-as-json
                     (concat (omnisharp-get-host) "currentfilemembersasflat")
                     request)))
    (omnisharp--choose-and-go-to-quickfix-ido
     quickfixes
     other-window)))

(defun omnisharp--choose-and-go-to-quickfix-ido
  (quickfixes &optional other-window)
  "Given a list of QuickFixes in list format (not JSON), displays them
in an ido-completing-read prompt and jumps to the chosen one's
Location.

If OTHER-WINDOW is given, will jump to the result in another window."
  (let ((chosen-quickfix
         (omnisharp--choose-quickfix-ido
          (omnisharp--vector-to-list quickfixes))))
    (omnisharp-go-to-file-line-and-column chosen-quickfix
                                          other-window)))

(defun omnisharp--choose-quickfix-ido (quickfixes)
  "Given a list of QuickFixes, lets the user choose one using
ido-completing-read. Returns the chosen element."
  ;; Ido cannot navigate non-unique items reliably. It either gets
  ;; stuck, or results in that we cannot reliably determine the index
  ;; of the item. Work around this by prepending the index of all items
  ;; to their end. This makes them unique.
  (let* ((quickfix-choices
          (--map-indexed
           (let ((this-quickfix-text (cdr (assoc 'Text it))))
             (concat "#"
                     (number-to-string it-index)
                     "\t"
                     this-quickfix-text))

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

(defun omnisharp-navigate-to-type-in-current-file ()
  (interactive)
  (omnisharp-navigate-to-type-in-current-file-worker
   (omnisharp--get-common-params)))

(defun omnisharp-navigate-to-type-in-current-file-worker (request)
  (let ((quickfixes
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "currentfiletopleveltypes")
          request)))
    (omnisharp--choose-and-go-to-quickfix-ido
     quickfixes)))

;; No need for a worker pattern since findsymbols takes no arguments
(defun omnisharp-navigate-to-solution-member
  (&optional other-window)
  (interactive "P")
  (let ((quickfix-response
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "findsymbols")
          nil)))
    (omnisharp--choose-and-go-to-quickfix-ido
     (omnisharp--vector-to-list
      (cdr (assoc 'QuickFixes quickfix-response)))
     other-window)))

(defun omnisharp-navigate-to-solution-member-other-window ()
  (omnisharp-navigate-to-solution-member t))

(defun omnisharp-navigate-to-solution-file
  (&optional other-window)
  (interactive "P")
  (let ((quickfix-response
         (omnisharp--get-solution-files-quickfix-response)))
    (omnisharp--choose-and-go-to-quickfix-ido
     (omnisharp--vector-to-list
      (cdr (assoc 'QuickFixes quickfix-response)))
     other-window)))

(defun omnisharp--get-solution-files-quickfix-response ()
  "Return a QuickFixResponse containing a list of all locations of
files in the current solution."
  (omnisharp-post-message-curl-as-json
   (concat (omnisharp-get-host) "gotofile")
   nil))

(defun omnisharp--get-solution-files-list-of-strings ()
  "Returns all files in the current solution as a list of strings."
  ;; This is just mapping functions one after another. Read from top
  ;; to bottom.
  (->> (omnisharp--get-solution-files-quickfix-response)
    (assoc 'QuickFixes)
    (cdr)
    (omnisharp--vector-to-list)
    (--map (cdr (assoc 'FileName it)))))

(defun omnisharp-navigate-to-solution-file-then-file-member
  (&optional other-window)
  "Navigates to a file in the solution first, then to a member in that
file. With prefix argument uses another window."
  (interactive "P")
  (omnisharp-navigate-to-solution-file other-window)
  ;; Do not set other-window here. No need to use two different
  ;; windows.
  (omnisharp-navigate-to-current-file-member))

(defun omnisharp-navigate-to-solution-file-then-file-member-other-window
  (&optional other-window)
  (omnisharp-navigate-to-solution-file-then-file-member t))

(defun omnisharp-navigate-to-region-other-window ()
  (interactive) (omnisharp-navigate-to-region t))

(defun omnisharp-show-last-auto-complete-result ()
  (interactive)
  (let ((auto-complete-result-in-human-readable-form
         (--map (cdr (assoc 'DisplayText it))
                omnisharp--last-buffer-specific-auto-complete-result)))
    (funcall (omnisharp--get-last-auto-complete-result-display-function)
             auto-complete-result-in-human-readable-form)))

(defun omnisharp--show-last-auto-complete-result-in-plain-buffer
  (auto-complete-result-in-human-readable-form-list)
  "Display function for omnisharp-show-last-auto-complete-result using
a simple 'compilation' like buffer to display the last auto-complete
result."
  (let ((buffer
         (get-buffer-create
          omnisharp--last-auto-complete-result-buffer-name)))
    (omnisharp--write-lines-to-compilation-buffer
     auto-complete-result-in-human-readable-form-list
     buffer
     omnisharp--last-auto-complete-result-buffer-header)))

(defun omnisharp-show-overloads-at-point ()
  (interactive)
  ;; Request completions from API but only cache them - don't show the
  ;; results to the user
  (save-excursion
    (end-of-thing 'symbol)
    (omnisharp-auto-complete-worker
     (omnisharp--get-auto-complete-params))
    (omnisharp-show-last-auto-complete-result)))

(defun omnisharp--get-eldoc-fontification-buffer ()
  (let ((buffer (get-buffer omnisharp--eldoc-fontification-buffer-name)))
    (if (buffer-live-p buffer)
        buffer
      (with-current-buffer (generate-new-buffer omnisharp--eldoc-fontification-buffer-name)
        (ignore-errors
          (let ((csharp-mode-hook nil))
            (csharp-mode)))
        (current-buffer)))))

(defun omnisharp--eldoc-fontify-string (str)
  (with-current-buffer (omnisharp--get-eldoc-fontification-buffer)
    (delete-region (point-min) (point-max))
    (font-lock-fontify-region (point) (progn (insert str ";") (point)))
    (buffer-substring (point-min) (1- (point-max)))))

(defun omnisharp-eldoc-function ()
  "Returns a doc string appropriate for the current context, or nil."
  (condition-case nil
      (let ((current-type-information
             (omnisharp-current-type-information-worker
              'Type
              (omnisharp--get-common-params))))
        (omnisharp--eldoc-fontify-string current-type-information))
    (error nil)))

;; define a method to nicely start the server
;;;###autoload
(defun omnisharp-start-omnisharp-server (solution)
  "Starts an OmniSharpServer for a given solution"
  (interactive "fStart OmniSharpServer.exe for solution: ")
  (setq BufferName "*Omni-Server*")
  (omnisharp--find-and-cache-omnisharp-server-executable-path)
  (if (equal nil omnisharp-server-executable-path)
      (error "Could not find the OmniSharpServer. Please set the variable omnisharp-server-executable-path to a valid path")
    (if (string= (file-name-extension solution) "sln")
        (progn
          (message (format "Starting OmniSharpServer for solution file: %s" solution))
          (if (not (eq nil (get-buffer BufferName)))
              (kill-buffer BufferName))
          (start-process-shell-command
           "Omni-Server"
           (get-buffer-create BufferName)
           (omnisharp--get-omnisharp-server-executable-command solution)))

      (error (format "Path does not lead to a solution file: %s" solution)))))

(defun omnisharp--find-and-cache-omnisharp-server-executable-path ()
  "Tries to find OmniSharpServer in exec-path, if omnisharp-server-executable-path is not set"
  (when (equal nil omnisharp-server-executable-path)
    (setq omnisharp-server-executable-path (executable-find "OmniSharp"))))

(defun omnisharp--get-omnisharp-server-executable-command
  (solution-file-path &optional server-exe-file-path)
  (when (eq nil server-exe-file-path)
    (setq server-exe-file-path
          omnisharp-server-executable-path))
  (setq server-exe-file-path (shell-quote-argument
                              (expand-file-name server-exe-file-path)))
  (setq solution-file-path (shell-quote-argument
                              (expand-file-name solution-file-path)))
  (cond
   ((equal system-type 'windows-nt)
    (concat server-exe-file-path " -s " solution-file-path " > NUL"))

   (t ; some kind of unix: linux or osx
    (concat "mono " server-exe-file-path
            " -s " solution-file-path
            " > /dev/null"))))

;;;###autoload
(defun omnisharp-check-alive-status ()
  "Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration is
alive.
\"Alive\" means it is running and not stuck. It also means the connection
to the server is functional - I.e. The user has the correct host and
port specified."
  (interactive)
  (if (omnisharp--check-alive-status-worker)
      (message "Server is alive and well. Happy coding!")
    (message "Server is not alive")))

(defun omnisharp--check-alive-status-worker ()
  (let ((result (omnisharp-post-message-curl-as-json
		 (concat (omnisharp-get-host) "checkalivestatus"))))
    (eq result t)))

;;;###autoload
(defun omnisharp-check-ready-status ()
  "Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration has
finished loading the solution."
  (interactive)
  (if (omnisharp--check-ready-status-worker)
      (message "Server is ready")
    (message "Server is not ready yet")))

(defun omnisharp--check-ready-status-worker ()
  (let ((result (omnisharp-post-message-curl-as-json
		 (concat (omnisharp-get-host) "checkreadystatus"))))
    (eq result t)))

;;;###autoload
(defun omnisharp-fix-code-issue-at-point ()
  (interactive)
  (let ((run-code-action-result
         (omnisharp--fix-code-issue-at-point-worker
          (omnisharp--get-common-params))))
    (omnisharp--set-buffer-contents-to
     (buffer-name)
     (cdr (assoc 'Text run-code-action-result))
     (line-number-at-pos)
     (omnisharp--current-column))))

(defun omnisharp--fix-code-issue-at-point-worker (request)
  "Takes a Request in lisp format. Calls the api and returns a
RunCodeIssuesResponse that contains Text - the new buffer
contents with the issue at point fixed."
  ;; The api uses a RunCodeActionRequest but currently ignores the
  ;; CodeAction property in that class
  (let ((run-code-action-request
         (cons `(CodeAction . 0) request)))
    (omnisharp-post-message-curl-as-json
     (concat (omnisharp-get-host)
             "fixcodeissue")
     run-code-action-request)))

(provide 'omnisharp)

;;; omnisharp.el ends here

