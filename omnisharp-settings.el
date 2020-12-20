;; -*- lexical-binding: t; -*-

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

(defvar omnisharp--unit-test-results-buffer-name "* Omnisharp : Unit Test Results *"
  "The name of the temporary buffer that is used to display the results
of a 'run tests' call.")

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

(defcustom omnisharp-cache-directory (f-join (locate-user-emacs-file ".cache") "omnisharp")
  "Directory to store files that omnisharp produces."
  :group 'omnisharp
  :type 'directory)

(defcustom omnisharp-server-executable-path nil
  "Path to OmniSharp server override. Should be set to non-nil if server is installed locally.
Otherwise omnisharp request the user to do M-x `omnisharp-install-server` and that server
executable will be used instead."
  :type '(choice (const :tag "Not Set" nil) string))

(defcustom omnisharp-expected-server-version "1.37.5"
  "Version of the omnisharp-roslyn server that this omnisharp-emacs package
is built for. Also used to select version for automatic server installation."
  :group 'omnisharp
  :type 'string)

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

(defcustom omnisharp-auto-complete-want-documentation t
  "Whether to include auto-complete documentation for each and every
response. This may be set to nil to get a speed boost for
completions."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-auto-complete-want-importable-types nil
  "Whether to search for autocompletions in all available
namespaces. If a match is found for a new namespace, the namespace is
automatically imported. This variable may be set to nil to get a speed
boost for completions."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-company-do-template-completion t
  "Set to t if you want in-line parameter completion, nil
  otherwise."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-company-template-use-yasnippet t
  "Set to t if you want completion to happen via yasnippet
  otherwise fall back on company's templating. Requires yasnippet
  to be installed"

  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-company-ignore-case t
  "If t, case is ignored in completion matches."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-company-strip-trailing-brackets nil
  "If t, strips trailing <> and () from completions."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-company-begin-after-member-access t
  "If t, begin completion when pressing '.' after a class, object
  or namespace"
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-company-sort-results t
  "If t, autocompletion results are sorted alphabetically"
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-imenu-support nil
  "If t, activate imenu integration. Defaults to nil."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-eldoc-support t
  "If t, activate eldoc integration - eldoc-mode must also be enabled for
  this to work. Defaults to t."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-company-match-type 'company-match-simple
  "Simple defaults to company's normal prefix matching (fast).
   Server allows the omnisharp-server to do the matching (slow but does fuzzy matching)."
  :group 'omnisharp
  :type '(choice (const :tag "Simple" 'company-match-simple)
                 (const :tag "Server" 'company-match-server)))

;; auto-complete-mode integration
(defcustom omnisharp-auto-complete-template-use-yasnippet t
  "Set to t if you want completion to happen via yasnippet
  otherwise fall back on auto-complete's templating. Requires yasnippet
  to be installed"

  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom omnisharp-completing-read-function 'omnisharp-builtin-completing-read
  "Function to be called when requesting input from the user."
  :group 'omnisharp
  :type '(radio (function-item omnisharp-builtin-completing-read)
                (function-item ido-completing-read)
                (function-item ivy-completing-read)
                (function-item helm--completing-read-default)
                (function :tag "Other function")))

(provide 'omnisharp-settings)
