;;; omnisharp.el --- Omnicompletion (intellisense) and more for C# -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2018 Mika Vilpas and others (GPLv3)
;; Author: Mika Vilpas and others
;; Version: 4.2
;; License: GNU General Public License version 3, or (at your option) any later version
;; Url: https://github.com/Omnisharp/omnisharp-emacs
;; Package-Requires: ((emacs "24.4") (flycheck "30") (dash "2.12.0") (auto-complete "1.4") (popup "0.5.1") (csharp-mode "0.8.7") (cl-lib "0.5") (s "1.10.0") (f "0.19.0"))
;; Keywords: languages csharp c# IDE auto-complete intellisense

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; omnisharp-emacs is a port of the awesome OmniSharp server to the
;; Emacs text editor. It provides IDE-like features for editing files
;; in C# solutions in Emacs, provided by an OmniSharp server instance
;; that works in the background.
;;
;; See the project home page for more information.


(require 'cl)
(require 'cl-lib)
(require 'csharp-mode)
(require 'json)
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
(require 'f)

(require 'omnisharp-settings)
(require 'omnisharp-server-management)
(require 'omnisharp-utils)
(require 'omnisharp-http-utils)
(require 'omnisharp-server-actions)
(require 'omnisharp-auto-complete-actions)
(require 'omnisharp-current-symbol-actions)
(require 'omnisharp-navigation-actions)
(require 'omnisharp-helm-integration)
(require 'omnisharp-solution-actions)
(require 'omnisharp-format-actions)
(require 'omnisharp-server-installation)
(require 'omnisharp-code-structure)
(require 'omnisharp-unit-test-actions)

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
  (omnisharp--attempt-to-start-server-for-buffer)

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

;;
;; below are all the interactive functions that are to be available via autoload
;;
;; (autoloaded f-ns should go in omnisharp.el in order to make
;; internal file dependencies easier to manage when autoloading)
;;

;;;###autoload
(defun omnisharp-start-omnisharp-server (&optional no-autodetect)
  "Starts an OmniSharp server for a given path to a project or solution file"
  (interactive "P")
  (omnisharp--start-omnisharp-server no-autodetect))

;;;###autoload
(defun omnisharp-stop-server ()
  "Stops Omnisharp server if running."
  (interactive)
  (omnisharp--stop-server))

;;;###autoload
(defun omnisharp-reload-solution ()
  "Restarts omnisharp server on solution last loaded"
  (interactive)
  (omnisharp--reload-solution))

;;;###autoload
(defun omnisharp-check-alive-status ()
  "Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration is
alive.
\"Alive\" means it is running and not stuck. It also means the connection
to the server is functional - I.e. The user has the correct host and
port specified."
  (interactive)
  (omnisharp--check-alive-status))

;;;###autoload
(defun omnisharp-check-ready-status ()
  "Shows a message to the user describing whether the
OmniSharpServer process specified in the current configuration has
finished loading the solution."
  (interactive)
  (omnisharp--check-ready-status))

;;;###autoload
(defun omnisharp-install-server (reinstall)
  "Installs OmniSharp server locally into ~/.emacs/cache/omnisharp/server/$(version)"
  (interactive "P")
  (omnisharp--install-server reinstall))

;;;###autoload
(defun company-omnisharp (command &optional arg &rest ignored)
  (interactive '(interactive))
  "`company-mode' completion back-end using OmniSharp."

  (cl-case command
    (interactive (company-begin-backend 'company-omnisharp))
    (prefix (when (and (bound-and-true-p omnisharp-mode)
                       (not (company-in-string-or-comment)))
              (omnisharp-company--prefix)))

    (candidates (omnisharp--get-company-candidates arg))

    ;; because "" doesn't return everything, and we don't cache if we're handling the filtering
    (no-cache (or (equal arg "")
                  (not (eq omnisharp-company-match-type 'company-match-simple))))

    (match (if (eq omnisharp-company-match-type 'company-match-simple)
               nil
             0))

    (annotation (omnisharp--company-annotation arg))

    (meta (omnisharp--get-company-candidate-data arg 'DisplayText))

    (require-match 'never)

    (doc-buffer (let ((doc-buffer (company-doc-buffer
                                   (omnisharp--get-company-candidate-data
                                    arg 'Description))))
                  (with-current-buffer doc-buffer
                    (visual-line-mode))
                  doc-buffer))

    (ignore-case omnisharp-company-ignore-case)

    (sorted (if (eq omnisharp-company-match-type 'company-match-simple)
                (not omnisharp-company-sort-results)
              t))

    ;; Check to see if we need to do any templating
    (post-completion (let* ((json-result (get-text-property 0 'omnisharp-item arg))
                            (allow-templating (get-text-property 0 'omnisharp-allow-templating arg)))

                       (omnisharp--tag-text-with-completion-info arg json-result)
                       (when allow-templating
                         ;; Do yasnippet completion
                         (if (and omnisharp-company-template-use-yasnippet (boundp 'yas-minor-mode) yas-minor-mode)
                             (-when-let (method-snippet (omnisharp--completion-result-item-get-method-snippet
							 json-result))
			       (omnisharp--snippet-templatify arg method-snippet json-result))
                           ;; Fallback on company completion but make sure company-template is loaded.
                           ;; Do it here because company-mode is optional
                           (require 'company-template)
                           (let ((method-base (omnisharp--get-method-base json-result)))
                             (when (and method-base
                                        (string-match-p "([^)]" method-base))
                               (company-template-c-like-templatify method-base)))))))))

;;
;; easymenu
;;
(easy-menu-define omnisharp-mode-menu omnisharp-mode-map
  "Menu for omnisharp-mode"
  '("OmniSharp"
    ("Auto-complete"
     ["at point" omnisharp-auto-complete]
     ["Add . and complete members" omnisharp-add-dot-and-auto-complete]
     ["Show last result" omnisharp-show-last-auto-complete-result]
     ["Show overloads at point" omnisharp-show-overloads-at-point])

    ("Navigate to.."
     ["Definition at point" omnisharp-go-to-definition]
     ["Current file member" omnisharp-navigate-to-current-file-member]
     ["Solution member" omnisharp-navigate-to-solution-member]
     ["File in solution" omnisharp-navigate-to-solution-file]
     ["Region in current file" omnisharp-navigate-to-region])

    ("OmniSharp server"
     ["Start OmniSharp server" omnisharp-start-omnisharp-server]
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
     ["Rename" omnisharp-rename])

    ("Solution actions"
     ["Start syntax check" flycheck-mode]
     ["Fix code issue at point" omnisharp-fix-code-issue-at-point]
     ["Run code format on current buffer" omnisharp-code-format-entire-file])

    ["Run contextual code action / refactoring at point" omnisharp-run-code-action-refactoring]))

(defvar omnisharp--eldoc-fontification-buffer-name " * OmniSharp : Eldoc Fontification *"
  "The name of the buffer that is used to fontify eldoc strings.")

(defun omnisharp--region-start-line ()
  (when mark-active
    (save-excursion
      (goto-char (region-beginning))
      (line-number-at-pos))))

(defun omnisharp--goto-end-of-region ()
  "evil-mode has its own Vim-like concept of the region. A visual
line selection in evil-mode reports the end column to be 0 in
some cases. Work around this."
  (when mark-active
    (if (and (boundp 'evil-visual-end)
             evil-visual-end
             ;; at this point we know the user is using evil-mode.
             ;; It's possible to select vanilla emacs regions even
             ;; when using evil-mode, so make sure the user has
             ;; selected the region using evil-visual-state
             (fboundp 'evil-visual-state-p)
             (evil-visual-state-p))
        (-let (((_start end _selection-type) (evil-visual-range)))
          ;; Point moves to the next line when it's at the very last
          ;; character of the line, for some reason. So move it back.
          (goto-char end)
          (backward-char))
      (goto-char (region-end)))))

(defun omnisharp--region-end-line ()
  (when mark-active
    (save-excursion (omnisharp--goto-end-of-region)
                    (line-number-at-pos))))

(defun omnisharp--region-start-column ()
  (when mark-active
    (save-excursion
      (goto-char (region-beginning))
      (omnisharp--current-column))))

(defun omnisharp--region-end-column ()
  (when mark-active
    (save-excursion
      (omnisharp--goto-end-of-region)
      (omnisharp--current-column))))

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

(defun omnisharp--get-request-object ()
  "Construct a Request object based on the current buffer contents."
  (let* ((line-number (number-to-string (line-number-at-pos)))
         (column-number (number-to-string (omnisharp--current-column)))
         (buffer-contents (if (boundp 'omnisharp--metadata-source)
                              nil
                            (omnisharp--get-current-buffer-contents)))
         (filename-tmp (or buffer-file-name ""))
         (params `((Line     . ,line-number)
                   (Column   . ,column-number)
                   (Buffer   . ,buffer-contents))))
    (cond
     ((boundp 'omnisharp--metadata-source)
      (cons `(FileName . ,omnisharp--metadata-source)
            params))
     ((/= 0 (length filename-tmp))
      (cons (omnisharp--to-filename filename-tmp)
            params))
     (t
      params))))

(defun omnisharp--get-typelookup-request-object ()
  "Construct a Request object for typelookup endpoint based on the current buffer contents."
  (append
   '((IncludeDocumentation . t))
   (omnisharp--get-request-object)))

(defun omnisharp--get-request-object-for-emacs-side-use ()
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
        (cons (omnisharp--to-filename filename-tmp)
              params)
      params)))

(defun omnisharp-go-to-file-line-and-column (json-result
                                             &optional other-window
                                             buffer)
  "Open file :FileName at :Line and :Column. If filename is not given,
defaults to the current file. This function works for a
QuickFix class json result.

Switches to BUFFER instead of :FileName when buffer is set."
  (omnisharp-go-to-file-line-and-column-worker
   (cdr (assoc 'Line json-result))
   (- (cdr (assoc 'Column json-result)) 1)
   (omnisharp--get-filename json-result)
   other-window
   nil
   buffer))

(defun omnisharp--go-to-line-and-column (line column)
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char (max 0 column)))

(defun omnisharp-go-to-file-line-and-column-worker (line
                                                    column
                                                    &optional filename
                                                    other-window
                                                    dont-save-old-pos
                                                    buffer)
  "Open filename at line and column. Switches to BUFFER if provided,
otherwise defaults to the current file if filename is not given.
Saves the current location into the tag ring so that the user may
return with (pop-tag-mark).

If DONT-SAVE-OLD-POS is specified, will not save current position to
find-tag-marker-ring. This is so this function may be used without
messing with the ring."

  (let ((position-before-jumping (point-marker)))
    (when (or buffer filename)
      (omnisharp--find-file-possibly-in-other-window (or buffer filename)
                                                     other-window))

    ;; calling goto-line directly results in a compiler warning.
    (omnisharp--go-to-line-and-column line column)

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
  (file &optional other-window)
  "Open a buffer editing FILE. If no buffer for that filename
exists, a new one is created.
If the optional argument OTHER-WINDOW is non-nil, uses another
window.

FILE can be a buffer in which case that buffer is selected."

  (cond
   ((or (bufferp file)
        (omnisharp--buffer-exists-for-file-name file))
    (let ((target-buffer-to-switch-to
           (if (bufferp file)
               file
             (--first (string= (buffer-file-name it)
                               file)
                      (buffer-list)))))
      (if other-window
          (pop-to-buffer target-buffer-to-switch-to)
        (pop-to-buffer-same-window target-buffer-to-switch-to))))

   (t ; no buffer for this file exists yet
    (funcall (if other-window
                 'find-file-other-window
               'find-file)
             file))))

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

(defun omnisharp--flycheck-start (checker callback)
  "Start an OmniSharp syntax check with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  ;; Put the current buffer into the closure environment so that we have access
  ;; to it later.
  (let ((buffer (current-buffer)))
    (omnisharp--send-command-to-server
     "codecheck"
     (omnisharp--get-request-object)
     (lambda (response)
       (let ((errors (omnisharp--flycheck-error-parser response checker buffer)))
         (funcall callback 'finished (delq nil errors)))))))

(flycheck-define-generic-checker 'csharp-omnisharp-codecheck
                                 "A csharp source syntax checker using the OmniSharp server process
   running in the background"
                                 :start #'omnisharp--flycheck-start
                                 :modes '(csharp-mode)
                                 :predicate (lambda () (and omnisharp-mode
                                                            omnisharp--server-info
                                                            (not (boundp 'omnisharp--metadata-source)))))

(defun omnisharp--flycheck-error-parser (response checker buffer)
  "Takes a QuickFixResponse result. Returns flycheck errors created based on the
locations in the json."
  (->> (omnisharp--vector-to-list
        (cdr (assoc 'QuickFixes response)))
       (mapcar (lambda (it)
                 (flycheck-error-new
                  :buffer buffer
                  :checker checker
                  :filename (omnisharp--get-filename it)
                  :line (cdr (assoc 'Line it))
                  :column (cdr (assoc 'Column it))
                  :message (cdr (assoc 'Text it))
                  :level (pcase (cdr (assoc 'LogLevel it))
                           ("Warning" 'warning)
                           ("Hidden" 'info)
                           (_ 'error)))))))

(defun omnisharp--imenu-make-marker (element)
  "Takes a QuickCheck element and returns the position of the
cursor at that location"
  (let* ((element-line (cdr (assoc 'Line element)))
         (element-column (cdr (assoc 'Column element)))
         (element-filename (omnisharp--get-filename element)))
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
      (let (result)
        (omnisharp--send-command-to-server-sync
         "currentfilemembersasflat"
         (omnisharp--get-request-object)
         (lambda (quickfixes)
           (setq result
                 (-map (lambda (quickfix-alist)
                         (cons (cdr (assoc 'Text quickfix-alist))
                               (omnisharp--imenu-make-marker quickfix-alist)))
                       quickfixes))))
        result)
    (error nil)))

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
  "Returns a doc string appropriate for the current context.
   If point is on an empty char, it looks for data on any previous completions.
   Otherwise, returns nil."
  (unless (equal nil omnisharp--server-info)
    (condition-case nil
        (if (not (looking-at-p " "))
            (progn
              (condition-case nil
                  (omnisharp--send-command-to-server
                   "typelookup"
                   (omnisharp--get-typelookup-request-object)
                   (lambda (response)
                     (let* ((current-type-information
                             (omnisharp--completion-result-get-item response 'Type))
                            (current-type-documentation
                             (string-trim-right
                              (or (omnisharp--completion-result-get-item response 'Documentation)
                                  "")))
                            (have-type (and current-type-information (not (string= "" current-type-information))))
                            (have-doc (and current-type-documentation (not (string= "" current-type-documentation))))
                            (message-to-show (concat (if current-type-information (omnisharp--eldoc-fontify-string current-type-information))
                                                     (if (and have-type have-doc) "\n\n")
                                                     current-type-documentation)))
                       (if (or have-type have-doc)
                           (eldoc-message message-to-show)))))
                (error nil (ignore)))
              nil))
      (error nil (ignore)))))

(add-to-list 'compilation-error-regexp-alist
             '(" in \\(.+\\):\\([1-9][0-9]+\\)" 1 2))

;; nunit-console.exe on windows uses this format
(add-to-list 'compilation-error-regexp-alist
             '(" in \\(.+\\):line \\([0-9]+\\)" 1 2))

;; dotnet test with xunit project
;; [xUnit.net 00:00:00.6080370]         /TestProject/UnitTest1.cs(15,0): at TestProject.UnitTest1.Test1()
(add-to-list 'compilation-error-regexp-alist '("\\[xUnit.net .*\\] +\\(.*\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\))" 1 2 3))

(provide 'omnisharp)

;;; omnisharp.el ends here
