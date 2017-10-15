;; -*- lexical-binding: t -*-

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

(require 'popup)
(require 'dash)

(defvar omnisharp-auto-complete-popup-want-isearch t
  "Whether to automatically start isearch when auto-completing.")

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

(defvar-local
  omnisharp--last-buffer-specific-auto-complete-result
  nil
  "Contains the last result of an autocomplete query.")

(defvar omnisharp-auto-complete-popup-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap popup-menu-keymap)

    (define-key keymap (kbd "<f2>") 'omnisharp--popup-to-ido)
    keymap)
  "The keymap used when displaying an autocomplete result in a popup
menu.")

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

;; company-mode integration
(defvar omnisharp-company-type-separator " : "
  "The string used to visually separate functions/variables from
  their types")

(defun omnisharp-auto-complete (&optional invert-importable-types-setting)
  "If called with a prefix argument, will complete types that are not
present in the current namespace or imported namespaces, inverting the
default `omnisharp-auto-complete-want-importable-types'
value. Selecting one of these will import the required namespace."
  (interactive "P")
  (let* ((auto-complete-request
          (let ((omnisharp-auto-complete-want-importable-types
                 ;; Invert the user configuration value if requested
                 (if invert-importable-types-setting
                     (not omnisharp-auto-complete-want-importable-types)
                   omnisharp-auto-complete-want-importable-types)))
            (omnisharp--create-auto-complete-request))))

    (omnisharp--wait-until-request-completed
     (omnisharp-auto-complete-worker
      auto-complete-request
      (lambda (auto-complete-response)
        (funcall (omnisharp--get-auto-complete-display-function)
                 auto-complete-response))))))

(defun omnisharp-add-dot-and-auto-complete ()
  "Adds a . character and calls omnisharp-auto-complete. Meant to be
bound to the dot key so pressing dot will automatically insert a dot
and complete members."
  (interactive)
  (insert ".")
  (omnisharp-auto-complete))

(defun omnisharp--create-auto-complete-request ()
  "Return an AutoCompleteRequest for the current buffer state."
  (append `((WantDocumentationForEveryCompletionResult
             . ,(omnisharp--t-or-json-false
                 omnisharp-auto-complete-want-documentation))

            (WantMethodHeader
             . ,(omnisharp--t-or-json-false
                 omnisharp-company-do-template-completion))

            (WantReturnType . t)

            (WantSnippet
             . ,(omnisharp--t-or-json-false
                 (and omnisharp-company-do-template-completion
                      omnisharp-company-template-use-yasnippet)))

            (WantImportableTypes
             . ,(omnisharp--t-or-json-false
                 omnisharp-auto-complete-want-importable-types))

            (WordToComplete . ,(thing-at-point 'symbol))

            (WantKind . t))

          (omnisharp--get-request-object)))

;; Use this source in your csharp editing mode hook like so:
;; (add-to-list 'ac-sources 'ac-source-omnisharp)
;;
;; Unfortunately there seems to be a limit in the auto-complete
;; library that disallows camel case completions and such fancy
;; completions useless.

;; The library only seems to accept completions that have the same
;; leading characters as results. Oh well.
(defvar ac-source-omnisharp
  '((candidates . omnisharp--get-auto-complete-result-in-popup-format)
    (action . omnisharp--ac-expand)))

(defun omnisharp--ac-expand()
  (interactive)
  (let* ((begin-point
          (car ac-last-completion))
         (completion-text
          (cdr ac-last-completion))
         (completion-value
          (get-text-property 0 'value completion-text))
         (completion-snippet
          (get-text-property 0 'Snippet completion-value)))
    (if (and
         completion-snippet
         omnisharp-auto-complete-template-use-yasnippet
         (boundp 'yas-minor-mode)
         yas-minor-mode)
        (yas-expand-snippet completion-snippet begin-point))))

(defun ac-complete-omnisharp nil
  (interactive)
  (auto-complete '(ac-source-omnisharp)))

(defun omnisharp--get-auto-complete-result-in-popup-format ()
  "Returns /autocomplete API results \(autocompletions\) as popup
items."
  (omnisharp--wait-until-request-completed
   (omnisharp-auto-complete-worker
    (omnisharp--create-auto-complete-request)))

  ;; result is stored in this buffer-local-variable
  (omnisharp--convert-auto-complete-result-to-popup-format
   omnisharp--last-buffer-specific-auto-complete-result))

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

(defun omnisharp-company-flx-score-filter-list (query candidates cache)
  (let ((matches nil))
    (dolist (candidate candidates)
      (let* ((completion-text (omnisharp--get-company-candidate-data
                               candidate
                               'CompletionText))
             (flx-val (flx-score completion-text query cache)))
        (when (not (null flx-val))
          (setq matches (cons (cons candidate flx-val) matches)))))

    (if omnisharp-company-match-sort-by-flx-score
        (setq matches (sort matches (lambda (el1 el2) (> (nth 1 el1) (nth 1 el2)))))
      (setq matches (reverse matches)))

    (mapcar 'car matches)))

(defvar omnisharp-company-current-flx-match-list nil)
(defvar omnisharp-company-current-flx-arg-being-matched nil)
(defvar omnisharp-company-checked-for-flex nil)
(defvar omnisharp-company-flx-cache nil)

(defun omnisharp--tag-text-with-completion-info (call json-result)
  "Adds data to the completed text which we then use in ElDoc"
  (add-text-properties (- (point) (length call)) (- (point) 1)
                       (list 'omnisharp-result json-result)))


(defun omnisharp--yasnippet-tag-text-with-completion-info ()
  "This is called after yasnippet has finished expanding a template.
   It adds data to the completed text, which we later use in ElDoc"
  (when omnisharp-snippet-json-result
    (add-text-properties yas-snippet-beg yas-snippet-end
                         (list 'omnisharp-result omnisharp-snippet-json-result))
    (remove-hook 'yas-after-exit-snippet-hook 'omnisharp--yasnippet-tag-text-with-completion-info)
    (setq omnisharp-snippet-json-result nil)))

(defvar omnisharp-snippet-json-result nil
  "Internal, used by snippet completion callback to tag a
  yasnippet completion with data, used by ElDoc.")

(defun omnisharp--snippet-templatify (call snippet json-result)
  "Does a snippet expansion of the completed text.
   Also sets up a hook which will eventually add data for ElDoc"
  (when (not omnisharp-snippet-json-result)
    (setq omnisharp-snippet-json-result json-result)
    (add-hook 'yas-after-exit-snippet-hook 'omnisharp--yasnippet-tag-text-with-completion-info))

  (delete-region (- (point) (length call)) (point))
  (yas-expand-snippet snippet))


(defun omnisharp--get-method-base (json-result)
  "If function templating is turned on, and the method is not a
   generic, return the 'method base' (basically, the method definition
   minus its return type)"
  (when omnisharp-company-do-template-completion
    (let ((method-base (omnisharp--completion-result-item-get-method-header json-result))
          (display (omnisharp--completion-result-item-get-completion-text
                    json-result)))
      (when (and method-base
                 ;; company doesn't expand < properly, so
                 ;; if we're not using yasnippet, disable templating on methods that contain it
                 (or omnisharp-company-template-use-yasnippet
                     (not (string-match-p "<" display)))
                 (not (string= method-base "")))
        method-base))))

(defun omnisharp--make-company-completion (json-result)
  "`company-mode' expects the beginning of the candidate to be
the same as the characters being completed.  This method converts
a function description of 'void SomeMethod(int parameter)' to
string 'SomeMethod' propertized with annotation 'void
SomeMethod(int parameter)' and the original value ITEM."
  (let* ((case-fold-search nil)
         (completion (omnisharp--completion-result-item-get-completion-text json-result))
         (display (omnisharp--completion-result-item-get-display-text json-result))
         (output completion)
         (method-base (omnisharp--get-method-base json-result))
         (allow-templating omnisharp-company-do-template-completion)
         (annotation (concat omnisharp-company-type-separator
                             (omnisharp--completion-result-get-item
                              json-result 'ReturnType))))

    ;; If we have templating turned on, if there is a method header
    ;; use that for completion.  The templating engine will then pick
    ;; up the completion for you
    ;; If we're looking at a line that already has a < or (, don't
    ;; enable templating, and also strip < and ( from our completions
    (cond ((looking-at-p "\\s-*(\\|<")
           (setq allow-templating nil)
           (setq output (car (split-string output "\\.*(\\|<"))))
          ((and (not omnisharp-company-do-template-completion)
                omnisharp-company-strip-trailing-brackets)
           (setq output (car (split-string completion "(\\|<"))))
          (method-base
           (setq output method-base)))

    ;; When we aren't templating, show the full description of the
    ;; method, rather than just the return type
    (when (not allow-templating)
      (setq annotation (concat omnisharp-company-type-separator
                               display)))

    ;; Embed in completion into the completion text, so we can use it later
    (add-text-properties 0 (length output)
                         (list 'omnisharp-item json-result
                               'omnisharp-ann annotation
                               'omnisharp-allow-templating allow-templating)
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
         (params (omnisharp--create-auto-complete-request))
         (handler (lambda (result)
                    (let* ((completion-list (mapcar #'omnisharp--make-company-completion
                                                    omnisharp--last-buffer-specific-auto-complete-result)))
                      (if (eq omnisharp-company-match-type 'company-match-simple)
                          (all-completions pre completion-list)
                        completion-list)))))

    ;; store auto-complete results
    ;; (omnisharp--wait-until-request-completed (omnisharp-auto-complete-worker params))
    (cons :async (lambda (cb)
                   (omnisharp-auto-complete-worker
                    params
                    (lambda (result)
                      (let ((completion-ignore-case omnisharp-company-ignore-case))
                       (funcall cb (funcall handler result)))))))))

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

(defun omnisharp-auto-complete-worker (auto-complete-request &optional callback)
  "Takes an AutoCompleteRequest and makes an autocomplete query with
them.

Calls the given CALLBACK with the result. Also caches that result
as omnisharp--last-buffer-specific-auto-complete-result.
Returns the request-id for the auto-complete request to the server."
  (omnisharp--send-command-to-server
   "autocomplete"
   auto-complete-request
   (lambda (auto-complete-response)
     ;; Cache result so it may be juggled in different contexts easily
     (setq omnisharp--last-buffer-specific-auto-complete-result
           auto-complete-response)
     (when callback
       (funcall callback auto-complete-response)))))

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
  (let ((buffer (get-buffer-create
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
     (omnisharp--create-auto-complete-request))
    (omnisharp-show-last-auto-complete-result)))

(defun omnisharp--auto-complete-display-function-popup
  (json-result-alist)
  "Gets an association list such as this:
 (((DisplayText    . \"Gender\")
   (Description    . \"int Gender { get; set; }\")
   (CompletionText . \"Gender\")))

Displays a popup.el popup menu, and inserts the chosen element in the
current buffer."
  (if (eql 0 (length json-result-alist))
      (progn (message "No completions.")
             nil)

    (setq json-result-alist
          (omnisharp--vector-to-list json-result-alist))
    (let* ((display-list
            (omnisharp--convert-auto-complete-result-to-popup-format
             json-result-alist))

           (completion-texts
            (mapcar 'omnisharp--completion-result-item-get-display-text
                    json-result-alist))

           (max-width (omnisharp--get-max-item-length
                       completion-texts))
           (result-completion-text
            (popup-menu* display-list
                         :width max-width
                         :keymap omnisharp-auto-complete-popup-keymap
                         :margin-left 1
                         :margin-right 1
                         :scroll-bar t
                         :isearch
                         omnisharp-auto-complete-popup-want-isearch
                         :help-delay
                         omnisharp-auto-complete-popup-help-delay))

           ;; A performance improvement may be gained here by using
           ;; hashtables if this seems too slow.
           ;;
           ;; Get the full item so we can then get the
           ;; RequiredNamespaceImport value from it
           (completion-snippet
            (get-text-property 0 'Snippet result-completion-text))
           (required-namespace-import
            (get-text-property 0 'RequiredNamespaceImport result-completion-text))

           (current-symbol-end-point (point))

           (current-symbol-start-point
            (save-excursion
              (search-backward (omnisharp--current-word-or-empty-string)))))

      (if (and completion-snippet omnisharp-company-template-use-yasnippet (boundp 'yas-minor-mode) yas-minor-mode)
          (yas-expand-snippet
           completion-snippet
           current-symbol-start-point
           current-symbol-end-point)
        (omnisharp--replace-symbol-in-buffer-with
         (omnisharp--current-word-or-empty-string)
         result-completion-text))

      (when required-namespace-import
        (omnisharp--insert-namespace-import required-namespace-import)))))

(defun omnisharp--auto-complete-display-function-ido
  (json-result-alist)
  "Use ido style completion matching with autocomplete candidates. Ido
is a more sophisticated matching framework than what popup.el offers."

  (if (eql 0 (length json-result-alist))
      (progn (message "No completions.")
             nil)

    (let* ((candidates (omnisharp--vector-to-list json-result-alist))

           (display-texts
            (mapcar 'omnisharp--completion-result-item-get-display-text
                    candidates))

           ;; This is only the display text. The text to be inserted
           ;; in the buffer will be fetched with this
           ;;
           ;; TODO does completing-read allow a custom format that
           ;; could store these, as with popup-make-item ?
           (user-chosen-display-text
            (omnisharp--completing-read
             "Complete: "
             display-texts))

           ;; Get the chosen candidate by getting the index of the
           ;; chosen DisplayText. The candidate with the same index is
           ;; the one we want.
           (json-result-element-index-with-user-chosen-text
            (cl-position-if (lambda (element)
                              (equal element
                                     user-chosen-display-text))
                            display-texts))
           (chosen-candidate
            (nth json-result-element-index-with-user-chosen-text
                 candidates))

           (completion-text-to-insert
            (cdr (assoc 'CompletionText
                        chosen-candidate)))
           (required-namespace-import
            (cdr (assoc 'RequiredNamespaceImport
                        chosen-candidate))))

      (omnisharp--replace-symbol-in-buffer-with
       (omnisharp--current-word-or-empty-string)
       completion-text-to-insert)

      (when required-namespace-import
        (omnisharp--insert-namespace-import required-namespace-import)))))

(defun omnisharp--convert-auto-complete-kind-to-popup-symbol-value (kind)
  (pcase kind
    ;; auto-complete's recommended rules
    ;;; Symbol
    ("Keyword" "s")
    ;;; Function, Method
    ("Method" "f")
    ("Function" "f")
    ("Constructor" "f")
    ;;; Variable
    ("Field" "v")
    ("Variable" "v")
    ("Property" "v")
    ;;; Constant
    ;;; Abbreviation
    ("Value" "a")
    ;; original rules
    ("Text" "")
    ("Class" "t")
    ("Interface" "i")
    ("Enum" "e")
    ("Module" "m")
    ("Unit" "u")
    ("Snippet" "")
    ("Color" "")
    ("File" "f")
    ("Reference" "r")))

(defun omnisharp--convert-auto-complete-result-to-popup-format (json-result-alist)
  (mapcar
   (-lambda ((&alist 'DisplayText display-text
                     'CompletionText completion-text
                     'Description description
                     'Snippet snippet
                     'RequiredNamespaceImport require-ns-import
                     'Kind kind))
            (popup-make-item display-text
                             :value (propertize completion-text 'Snippet snippet 'RequiredNamespaceImport require-ns-import)
                             :symbol (omnisharp--convert-auto-complete-kind-to-popup-symbol-value kind)
                             :document description))
   json-result-alist))

(provide 'omnisharp-auto-complete-actions)
