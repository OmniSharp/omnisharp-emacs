;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;;; omnisharp.el --- Omnicompletion (intellisense) and more for C#
;; Copyright (C) 2013 Mika Vilpas (GPLv3)
;; Author: Mika Vilpas
;; Version: 3.4
;; Url: https://github.com/sp3ctum/omnisharp-emacs
;; Package-Requires: ((json "1.2") (flycheck "0.25.1") (dash "20141201.2206") (auto-complete "1.4") (popup "0.5.1") (csharp-mode "0.8.7") (cl-lib "0.5") (s "1.9.0"))
;; Keywords: csharp c# IDE auto-complete intellisense

;;; Commentary:
;; omnisharp-emacs is a port of the awesome OmniSharp server to the
;; Emacs text editor. It provides IDE-like features for editing files
;; in C# solutions in Emacs, provided by an OmniSharp server instance
;; that works in the background.
;;
;; See the project home page for more information.
(require 'json)
(require 'cl-lib)
(require 'files)
(require 'ido)
(require 'thingatpt)
(require 'dash)
(require 'compile)
(require 'dired)
(require 'popup)
(require 'etags)
(require 'flycheck)
(require 's)

(add-to-list 'load-path (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) "/src/")))
(add-to-list 'load-path (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) "/src/actions")))

(require 'omnisharp-utils)
(require 'omnisharp-server-actions)
(require 'omnisharp-auto-complete-actions)
(require 'omnisharp-settings)

;;; Code:
;;;###autoload
(define-minor-mode omnisharp-mode
  "Omnicompletion (intellisense) and more for C# using an OmniSharp
server backend."
  :lighter " omnisharp"
  :global nil
  :keymap omnisharp-mode-map
  (omnisharp--init-imenu-support)
  (omnisharp--init-eldoc-support)
  (omnisharp--start-omnisharp-server-for-solution-in-parent-directory)

  ;; These are selected automatically when flycheck is enabled
  (add-to-list 'flycheck-checkers 'csharp-omnisharp-codecheck))

(defun omnisharp--init-imenu-support ()
  (when omnisharp-imenu-support
    (if omnisharp-mode
        (progn
          (setq imenu-create-index-function 'omnisharp-imenu-create-index)
          (imenu-add-menubar-index))
      (setq imenu-create-index-function 'imenu-default-create-index-function))))

(defun omnisharp--init-eldoc-support ()
  (when omnisharp-eldoc-support
    (when omnisharp-mode
      (make-local-variable 'eldoc-documentation-function)
      (setq eldoc-documentation-function 'omnisharp-eldoc-function))))

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
     ["Show documentation" omnisharp-current-type-documentation]
     ["Show type and add it to kill ring" omnisharp-current-type-information-to-kill-ring]
     ["Find usages" omnisharp-find-usages]
     ["Find usages with ido" omnisharp-find-usages-with-ido]
     ["Find implementations" omnisharp-find-implementations]
     ["Find implementations with ido" omnisharp-find-implementations-with-ido]
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
     ["Fix code issue at point" omnisharp-fix-code-issue-at-point])

    ("Unit tests"
     ["Run test at point" omnisharp-unit-test-single]
     ["Run test fixture" omnisharp-unit-test-fixture]
     ["Run all tests in project" omnisharp-unit-test-all])

    ["Run contextual code action / refactoring at point" omnisharp-run-code-action-refactoring]
    ["Run code format on current buffer" omnisharp-code-format]
    ["Fix using statements" omnisharp-fix-usings]))

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
    (lambda (quickfixes) (omnisharp--find-usages-show-response quickfixes))))

(defun omnisharp-find-usages-worker (request callback)
  "Gets a list of QuickFix lisp objects from a findusages api call
asynchronously. On completions, CALLBACK is run with the quickfixes as
its only argument."
  (declare (indent defun))
  (omnisharp-post-message-curl-as-json-async
   (concat (omnisharp-get-host) "findusages")
   request
   (-lambda ((&alist 'QuickFixes quickfixes))
            (apply callback (list (omnisharp--vector-to-list quickfixes))))))

(defun omnisharp--find-usages-show-response (quickfixes)
  (if (equal 0 (length quickfixes))
      (message "No usages found.")
    (omnisharp--write-quickfixes-to-compilation-buffer
     quickfixes
     omnisharp--find-usages-buffer-name
     omnisharp-find-usages-header)))

(defun omnisharp-find-implementations ()
  "Show a buffer containing all implementations of the interface under
point, or classes derived from the class under point. Allow the user
to select one (or more) to jump to."
  (interactive)
  (message "Finding implementations...")
  (omnisharp-find-implementations-worker
    (omnisharp--get-common-params)
    (lambda (quickfixes)
      (cond ((equal 0 (length quickfixes))
             (message "No implementations found."))

            ;; Go directly to the implementation if there only is one
            ((equal 1 (length quickfixes))
             (omnisharp-go-to-file-line-and-column (car quickfixes)))

            (t
             (omnisharp--write-quickfixes-to-compilation-buffer
              quickfixes
              omnisharp--find-implementations-buffer-name
              omnisharp-find-implementations-header))))))

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

(defun omnisharp-find-implementations-popup ()
  "Show a popup containing all implementations of the interface under
point, or classes derived from the class under point. Allow the user
to select one (or more) to jump to."
  (interactive)
  (message "Finding implementations...")
  (omnisharp-find-implementations-worker
    (omnisharp--get-common-params)
    (lambda (quickfixes)
      (cond ((equal 0 (length quickfixes))
             (message "No implementations found."))

            ;; Go directly to the implementation if there only is one
            ((equal 1 (length quickfixes))
             (omnisharp-go-to-file-line-and-column (car quickfixes)))

            (t
             (omnisharp-navigate-to-implementations-popup quickfixes))))))

(defun omnisharp-get-implementation-title (item)
  "Get the human-readable class-name declaration from an alist with
information about implementations found in omnisharp-find-implementations-popup."
  (let* ((text (cdr (assoc 'Text item))))
    (if (or (string-match-p " class " text)
            (string-match-p " interface " text))
	text
      (concat
       (file-name-nondirectory (cdr (assoc 'FileName item)))
       ":"
       (number-to-string (cdr (assoc 'Line item))))
      )))

(defun omnisharp-get-implementation-by-name (items title)
  "Return the implementation-object which matches the provided title."
  (--first (string= title (omnisharp-get-implementation-title it))
	   items))

(defun omnisharp-navigate-to-implementations-popup (items)
  "Creates a navigate-to-implementation popup with the provided items
and navigates to the selected one."
  (let* ((chosen-title (popup-menu* (mapcar 'omnisharp-get-implementation-title items)))
	 (chosen-item  (omnisharp-get-implementation-by-name items chosen-title)))
    (omnisharp-go-to-file-line-and-column chosen-item)))

(defun omnisharp-fix-usings ()
  "Sorts usings, removes unused using statements and
adds any missing usings. If there are any ambiguous unresolved symbols, they are
shown in a compilation buffer."
  (interactive)
  (save-buffer)
  (message "Fixing using directives for the current buffer. Hold on...")
  (-if-let (ambiguous-results
            (omnisharp-fix-usings-worker
             (buffer-file-name)
             (line-number-at-pos)
             (omnisharp--current-column)))
      (omnisharp--write-quickfixes-to-compilation-buffer
       ambiguous-results
       omnisharp--ambiguous-symbols-buffer-name
       omnisharp-ambiguous-results-header)

    ;; Otherwise destroy any previous ambiguous result so the user
    ;; clearly sees the compilation buffer contents have changed
    (-if-let (ambiguous-results-buffer
              (get-buffer omnisharp--ambiguous-symbols-buffer-name))
        (kill-buffer ambiguous-results-buffer))))

(defun omnisharp-fix-usings-worker (filename
				    current-line
				    current-column)
  "Sets the current buffer contents to a buffer with fixed up usings
or if necessary, returns any ambiguous results so the user may fix
them manually."
  (let ((json-result
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "fixusings")
          (omnisharp--get-common-params))))

    (omnisharp--set-buffer-contents-to
     filename
     (cdr (assoc 'Buffer json-result))
     current-line
     current-column)
    (omnisharp--vector-to-list
     (cdr (assoc 'AmbiguousResults json-result)))))

(defun omnisharp-navigate-to-region
  (&optional other-window)
  "Navigate to region in current file. If OTHER-WINDOW is given and t,
use another window."
  (interactive "P")
  (-let [(&alist 'QuickFixes qfs) (omnisharp-post-message-curl-as-json
                                   (concat (omnisharp-get-host) "gotoregion")
                                   (omnisharp--get-common-params))]
    (omnisharp--choose-and-go-to-quickfix-ido qfs other-window)))

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

    (message "Rename complete in files: \n%s"
             (-interpose "\n" (--map (cdr (assoc 'FileName it))
                                     modified-file-responses)))))

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

    (setq omnisharp--current-solution-files all-solution-files)
    (tags-query-replace current-word
                        rename-to
                        delimited
                        ;; This is expected to be a form that will be
                        ;; evaluated to get the list of all files to
                        ;; process.
                        'omnisharp--current-solution-files)
    ;; Keep point in the buffer that initialized the rename so that
    ;; the user deos not feel disoriented
    (omnisharp-go-to-file-line-and-column location-before-rename)))

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

(defvar omnisharp--eldoc-fontification-buffer-name " * OmniSharp : Eldoc Fontification *"
  "The name of the buffer that is used to fontify eldoc strings.")

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
                (cl-position chosen-action actions-list)))

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

(defun omnisharp-post-message-curl-as-json-async (url params callback)
  "Posts message to curl at URL with PARAMS asynchronously.
On completion, the curl output is parsed as json and passed into CALLBACK."
  (omnisharp-post-message-curl-async url params
                                     (lambda (str)
                                       (funcall callback (omnisharp--json-read-from-string str)))))

(defun omnisharp-post-message-curl-async (url params callback)
  "Post json stuff to url asynchronously with --data set to given params.
On completion, CALLBACK is run with the result as it's only parameter.

Returns the curl process"
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
         (funcall callback
                  (progn (let ((output (with-current-buffer process-buffer (buffer-string))))
                           (kill-buffer process-buffer)
                           output))))))
    process))


(defun omnisharp--completion-result-item-get-completion-text (item)
  (cdr (assoc 'CompletionText item)))

(defun omnisharp--completion-result-item-get-display-text (item)
  (cdr (assoc 'DisplayText item)))

(defun omnisharp--completion-result-item-get-method-header (item)
  (cdr (assoc 'MethodHeader item)))

(defun omnisharp--completion-result-item-get-method-snippet (item)
  (cdr (assoc 'Snippet item)))

(defun omnisharp--completion-result-get-item (json-alist type)
  (cdr (assoc type json-alist)))

(defun omnisharp--get-max-item-length (completions)
  "Returns the length of the longest completion in 'completions'."
  (if (null completions)
      0
    (cl-reduce 'max (mapcar 'length completions))))

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
  (omnisharp-current-type-information-worker 'Type))

(defun omnisharp-current-type-documentation (&optional add-to-kill-ring)
  "Display documentation of the current type under point. With prefix
argument, add the displayed result to the kill ring. This can be used
to insert the result in code, for example."
  (interactive "P")
  (omnisharp-current-type-information-worker 'Documentation))

(defun omnisharp-current-type-information-worker (type-property-name
                                                  &optional add-to-kill-ring)
  "Get type info from the API and display a part of the response as a
message. TYPE-PROPERTY-NAME is a symbol in the type lookup response
from the server side, i.e. 'Type or 'Documentation that will be
displayed to the user."
  (omnisharp-post-message-curl-as-json-async
   (concat (omnisharp-get-host) "typelookup")
   (omnisharp--get-common-params)
   (lambda (response)
     (let ((stuff-to-display (cdr (assoc type-property-name
                                         response))))
       (message stuff-to-display)
       (when add-to-kill-ring
         (kill-new stuff-to-display))))))

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
  ;; Build command contains backslashes on Windows systems. Work
  ;; around this by using double backslashes. Other systems are not
  ;; affected.
  (let ((build-command (omnisharp--fix-build-command-if-on-windows
                        (omnisharp-get-build-command))))
    (omnisharp--recognize-mono-compilation-error-format)
    (compile build-command)
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
       command)

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

(flycheck-define-checker csharp-omnisharp-codecheck
  "A csharp source syntax checker using curl to call an OmniSharp
server process running in the background."
  ;; This must be an external process. Currently flycheck does not
  ;; support using elisp functions as checkers.
  :command ("curl" ; this is overridden by
                                        ; flycheck-csharp-omnisharp-curl-executable if it
                                        ; is set
            (eval
             (omnisharp--get-curl-command-arguments-string-for-api-name
              (omnisharp--get-common-params)
              "codecheck")))

  :error-patterns ((error line-start
                          (file-name) ":"
                          line ":"
                          column
                          " "
                          (message (one-or-more not-newline))))
  :error-parser (lambda (output checker buffer)
                  (omnisharp--flycheck-error-parser-raw-json
                   output checker buffer))

  :predicate (lambda () omnisharp-mode))

(defun omnisharp--flycheck-error-parser-raw-json (output checker buffer)
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
                 :level (if (equal (cdr (assoc 'LogLevel it)) "Warning")
                            'warning
                          'error)))
              errors))))

(defun omnisharp--imenu-make-marker (element)
  "Takes a QuickCheck element and returns the position of the
cursor at that location"
  (let* ((element-line (cdr (assoc 'Line element)))
         (element-column (cdr (assoc 'Column element)))
         (element-filename (cdr (assoc 'Filename element)))
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

(defun omnisharp-format-find-output-to-ido (item)
  (let ((filename (cdr (assoc 'FileName item))))
    (cons
     (cons
      (car (car item))
      (concat (car (last (split-string filename "/"))) ": " (s-trim (cdr (car item)))))
     (cdr item))))

(defun omnisharp-find-implementations-with-ido (&optional other-window)
  (interactive "P")
  (let ((quickfixes (omnisharp--vector-to-list
                     (cdr (assoc 'QuickFixes (omnisharp-post-message-curl-as-json
                                              (concat (omnisharp-get-host) "findimplementations")
                                              (omnisharp--get-common-params)))))))
    (cond ((equal 0 (length quickfixes))
           (message "No implementations found."))
          ((equal 1 (length quickfixes))
           (omnisharp-go-to-file-line-and-column (car quickfixes) other-window))
          (t
           (omnisharp--choose-and-go-to-quickfix-ido
            (mapcar 'omnisharp-format-find-output-to-ido quickfixes)
            other-window)))))

(defun omnisharp-find-usages-with-ido (&optional other-window)
  (interactive "P")
  (let ((quickfixes (omnisharp--vector-to-list
                     (cdr (assoc 'QuickFixes (omnisharp-post-message-curl-as-json
                                              (concat (omnisharp-get-host) "findusages")
                                              (omnisharp--get-common-params)))))))
    (cond ((equal 0 (length quickfixes))
           (message "No usages found."))
          ((equal 1 (length quickfixes))
           (omnisharp-go-to-file-line-and-column (car quickfixes) other-window))
          (t
           (omnisharp--choose-and-go-to-quickfix-ido
            (mapcar 'omnisharp-format-find-output-to-ido quickfixes)
            other-window)))))

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
          (cl-position-if (lambda (quickfix-text)
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
    (omnisharp--choose-and-go-to-quickfix-ido quickfixes)))

(defun omnisharp-format-symbol (item) 
  (cons
   (cons
    (car (car item))
    (mapconcat
     'identity
     (reverse (delete "in" (split-string (cdr (car item)) "[\t\n ()]" t))) "."))
   (cdr item)))

;; No need for a worker pattern since findsymbols takes no arguments
(defun omnisharp-navigate-to-solution-member
  (&optional other-window)
  (interactive "P")
  (let ((quickfix-response
         (omnisharp-post-message-curl-as-json
          (concat (omnisharp-get-host) "findsymbols")
          nil)))
    (omnisharp--choose-and-go-to-quickfix-ido
     (mapcar 'omnisharp-format-symbol
	     (omnisharp--vector-to-list
	      (cdr (assoc 'QuickFixes quickfix-response))))
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

(defun omnisharp--jump-to-enclosing-func ()
  "Jumps to the closing brace of the current function definition"
  (interactive)
  (let ((start-point (point))
        (found-point (point))
        (found-start nil))
    (save-excursion
      (let ((test-point (point)))
        (while (not found-start)
          (search-backward-regexp "(\\|;\\|{")
          (cond ((eq (point) test-point)
                 (setq found-start t))

                ((looking-at-p "(")
                 (setq test-point (point))

                 ;; forward-sexp will throw an error if the sexp is unbalanced
                 (condition-case nil
                     (forward-sexp)
                   (error nil))
                 
                 (when (> (point) start-point)
                   (setq found-point test-point)
                   (setq found-start t))
                 (goto-char test-point))

                (t (setq found-start t))))))
    (goto-char found-point)))

(defun omnisharp--eldoc-default ()
  "Tries to find completion information about the method before point"
  (save-excursion
    (omnisharp--jump-to-enclosing-func)
    (search-backward-regexp "\\sw")
    (let* ((json-result (get-text-property (point) 'omnisharp-result))
           (type-info (omnisharp--completion-result-get-item json-result 'DisplayText)))

      (if (and type-info (not (string= "" type-info)))
          (omnisharp--eldoc-fontify-string type-info)
        nil))))


(defun omnisharp--eldoc-worker ()
  "Gets type information from omnisharp server about the symbol at point"
  (omnisharp--completion-result-get-item 
   (omnisharp-post-message-curl-as-json
    (concat (omnisharp-get-host) "typelookup")
    (omnisharp--get-common-params))
   'Type))

(defun omnisharp-eldoc-function ()
  "Returns a doc string appropriate for the current context.
   If point is on an empty char, it looks for data on any previous completions.
   Otherwise, returns nil."
  (condition-case nil
      (if (looking-at-p " ")
          (omnisharp--eldoc-default)
        (let ((current-type-information
               (omnisharp--eldoc-worker)))
          (if (and current-type-information (not (string= "" current-type-information)))
              (progn
                (omnisharp--eldoc-fontify-string current-type-information))
            (omnisharp--eldoc-default))))
    (error nil
           (omnisharp--eldoc-default))))

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

(add-to-list 'compilation-error-regexp-alist
             '(" in \\(.+\\):\\([1-9][0-9]+\\)" 1 2))

;; nunit-console.exe on windows uses this format
(add-to-list 'compilation-error-regexp-alist
             '(" in \\(.+\\):line \\([0-9]+\\)" 1 2))

(defun omnisharp-unit-test-single ()
  (interactive)
  (omnisharp-unit-test-worker "single"))

(defun omnisharp-unit-test-fixture ()
  (interactive)
  (omnisharp-unit-test-worker "fixture"))

(defun omnisharp-unit-test-all ()
  (interactive)
  (omnisharp-unit-test-worker "all"))

(defun omnisharp-unit-test-worker (mode)
  "Run tests after building the solution. Mode should be one of 'single', 'fixture' or 'all'" 
  (let ((build-command
         (omnisharp--fix-build-command-if-on-windows
          (omnisharp-get-build-command)))

        (test-command
         (omnisharp--fix-build-command-if-on-windows
          (cdr (assoc 'TestCommand
                      (omnisharp-post-message-curl-as-json
                       (concat (omnisharp-get-host) "gettestcontext") 
                       (cons `("Type" . ,mode)
                             (omnisharp--get-common-params))))))))

    (compile build-command)
    ;; User can answer yes straight away if they don't want to
    ;; recompile. But they have to be very fast!
    (when (yes-or-no-p "Compilation started. Answer yes when you want to run tests.")
      (compile test-command))))

;;; Some Helm integration
(when (require 'helm-grep nil 'noerror)
  ;;; Helm usages
  (defvar omnisharp-helm-usage-candidates nil)

  (defun omnisharp--helm-usage-transform-candidate (candidate)
    "Convert a quickfix entry into helm output"
    (cons
     (format "%s(%s): %s"
             (propertize (file-name-nondirectory
                          (cdr (assoc 'FileName candidate)))
                         'face 'helm-grep-file)
             (propertize (number-to-string (cdr (assoc 'Line candidate)))
                         'face 'helm-grep-lineno)
             (cdr (assoc 'Text candidate)))
     candidate))
  
  (defun omnisharp--helm-got-usages (quickfixes)
    (setq omnisharp-helm-usage-candidates (mapcar 'omnisharp--helm-usage-transform-candidate quickfixes))
    (helm :sources (helm-make-source "Omnisharp - Symbol Usages" 'helm-source-sync
                                     :candidates omnisharp-helm-usage-candidates
                                     :action 'omnisharp--helm-jump-to-candidate)
          :truncate-lines t
          :buffer omnisharp--find-usages-buffer-name))

  (defun omnisharp-helm-find-usages ()
    "Find usages for the symbol under point using Helm"
    (interactive)
    (message "Helm Finding usages...")
    (omnisharp-find-usages-worker
      (omnisharp--get-common-params)
      'omnisharp--helm-got-usages))

  (defun omnisharp--helm-jump-to-candidate (json-result)
    (omnisharp-go-to-file-line-and-column json-result)
    (helm-highlight-current-line nil nil nil nil t))


  ;;; Helm find symbols
  (defun omnisharp-helm-find-symbols ()
    (interactive)
    (helm :sources (helm-make-source "Omnisharp - Find Symbols" 'helm-source-sync
                                     :action 'omnisharp--helm-jump-to-candidate
                                     :matchplugin nil
                                     :match '((lambda (candidate) (string-match-p
                                                                   helm-pattern
                                                                   (nth 1 (split-string
                                                                           candidate ":" t)))))
                                     :candidates 'omnisharp--helm-find-symbols-candidates)
          :buffer "*Omnisharp Symbols*"
          :truncate-lines t))

  (defun omnisharp--helm-find-symbols-candidates ()
    (let ((quickfix-response
           (omnisharp-post-message-curl-as-json
            (concat (omnisharp-get-host) "findsymbols")
            nil)))
      (mapcar 'omnisharp--helm-find-symbols-transform-candidate
              (omnisharp--vector-to-list
               (cdr (assoc 'QuickFixes quickfix-response))))))

  (defun omnisharp--helm-find-symbols-transform-candidate (candidate)
    "Convert a quickfix entry into helm output"
    (cons
     (format "%s : %s"
             (propertize (cdr (assoc 'FileName candidate))
                         'face 'helm-grep-file)
             (nth 0 (split-string (cdr (assoc 'Text candidate)) "(")))
     candidate)))

(provide 'omnisharp)

;;; omnisharp.el ends here

