;; this file contains settings that are used throughout the project

(defgroup omnisharp ()
  "Omnisharp-emacs is a port of the awesome OmniSharp server to
the Emacs text editor. It provides IDE-like features for editing
files in C# solutions in Emacs, provided by an OmniSharp server
instance that works in the background."
  :group 'external
  :group 'csharp)

(defcustom omnisharp-host "http://localhost:2000/"
  "Currently expected to end with a / character."
  :group 'omnisharp
  :type 'string)

(defvar omnisharp--find-usages-buffer-name "* OmniSharp : Usages *"
  "The name of the temporary buffer that is used to display the
results of a 'find usages' call.")

(defvar omnisharp-debug nil
  "When non-nil, omnisharp-emacs will write entries a debug log")

(defvar omnisharp--find-implementations-buffer-name "* OmniSharp : Implementations *"
  "The name of the temporary buffer that is used to display the
results of a 'find implementations' call.")

(defvar omnisharp--ambiguous-symbols-buffer-name "* OmniSharp : Ambiguous unresolved symbols *"
  "The name of the temporary buffer that is used to display any 
ambiguous unresolved symbols of a 'fix usings' call.")

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

(defvar omnisharp-ambiguous-results-header
  (concat "These results are ambiguous. You can run
(omnisharp-run-code-action-refactoring) when point is on them to see
options for fixing them."
          "\n\n")
  "This is shown at the top of the result buffer when
there are ambiguous unresolved symbols after running omnisharp-fix-usings")

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
(eval-after-load 'omnisharp
  '(defcustom omnisharp--windows-curl-tmp-file-path
     (omnisharp--convert-backslashes-to-forward-slashes
      (concat (getenv "USERPROFILE")
              "/omnisharp-tmp-file.cs"))
     "The full file path where to save temporary stuff that gets sent to
the OmniSharp API. Only used on Windows.
Must be writable by the current user."
     :group 'omnisharp
     :type 'file))

(defcustom omnisharp--curl-executable-path
  "curl"
  "The absolute or relative path to the curl executable.")


(provide 'omnisharp-settings)
