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


(require 'dash)

(defun omnisharp-go-to-definition (&optional other-window)
  "Jump to the definition of the symbol under point. With prefix
argument, use another window."
  (interactive "P")
  (let ((gotodefinition-request (append
                                 '((WantMetadata . t))
                                 (omnisharp--get-request-object))))
    (omnisharp--send-command-to-server
     "gotodefinition"
     gotodefinition-request
     (lambda (response)
       (omnisharp--prepare-metadata-buffer-if-needed
        (omnisharp--get-filename response)
        (cdr (assoc 'MetadataSource response))
        (lambda (buffer filename)
          (omnisharp-go-to-file-line-and-column response
                                                other-window
                                                buffer)))))))

(defun omnisharp--prepare-metadata-buffer-if-needed (filename
                                                     metadata-source
                                                     callback)
  "Prepares metadata buffer if required (if FILENAME is missing and
METADATA-SOURCE is available) and then invokes CALLBACK with either
buffer or FILENAME of the file containing the definition.

Metadata buffer is made readonly and both omnisharp-mode and csharp-mode's
are enabled on this buffer."
  (cond
   ;; when gotodefinition returns FileName for the same
   ;; metadata buffer as we're in:
   ;;   just return current buffer
   ((and (boundp 'omnisharp--metadata-source)
         (string-equal filename omnisharp--metadata-source))
    (funcall callback (current-buffer) nil))

   ;; when gotodefinition returns an actual filename on the filesystem:
   ;;   navigate to this file
   (filename
    (funcall callback nil filename))

   ;; when gotodefinition returns metadata reference:
   ;;   in this case we need to invoke /metadata endpoint to fetch
   ;;   generated C# source for this type from the server (unless we
   ;;   have it already in an existing buffer)
   (metadata-source
    (let* ((metadata-buffer-name (omnisharp--make-metadata-buffer-name
                                  metadata-source))
           (existing-metadata-buffer (get-buffer metadata-buffer-name)))
      (if existing-metadata-buffer
          ;; ok, we have this buffer for this metadata source loaded already
          (funcall callback existing-metadata-buffer nil)

        ;; otherwise we need to actually retrieve metadata-generated source
        ;; and create a buffer for this type
        (omnisharp--send-command-to-server
         "metadata"
         metadata-source
         (lambda (response)
           (let ((source (cdr (assoc 'Source response)))
                 (source-name (cdr (assoc 'SourceName response)))
                 (new-metadata-buffer (get-buffer-create metadata-buffer-name)))
             (with-current-buffer new-metadata-buffer
               (insert source)
               (csharp-mode)
               (omnisharp-mode)
               (setq-local omnisharp--metadata-source source-name)
               (toggle-read-only 1))
             (funcall callback new-metadata-buffer nil)))))))
   (t
    (message
     "Cannot go to definition as none was returned by the API."))))

(defun omnisharp--make-metadata-buffer-name (metadata-source)
  "Builds unique buffer name for the given MetadataSource object.
This buffer name assumed to be stable and unique."

  (let ((assembly-name (cdr (assoc 'AssemblyName metadata-source)))
        (type-name (cdr (assoc 'TypeName metadata-source)))
        (project-name (cdr (assoc 'ProjectName metadata-source))))
    (concat "*omnisharp-metadata:" project-name ":" assembly-name ":" type-name "*")))

(defun omnisharp-go-to-definition-other-window ()
  "Do `omnisharp-go-to-definition' displaying the result in a different window."
  (interactive)
  (omnisharp-go-to-definition t))

(defun omnisharp-navigate-to-current-file-member
  (&optional other-window)
  "Show a list of all members in the current file, and jump to the
selected member. With prefix argument, use another window."
  (interactive "P")
  (omnisharp--send-command-to-server
   "currentfilemembersasflat"
   (omnisharp--get-request-object)
   (lambda (quickfixes)
     (omnisharp--choose-and-go-to-quickfix-ido
      quickfixes
      other-window))))

(defun omnisharp-navigate-to-current-file-member-other-window ()
  (interactive)
  (omnisharp-navigate-to-current-file-member t))

(defun omnisharp--choose-and-go-to-quickfix-ido
  (quickfixes &optional other-window)
  "Given a list of QuickFixes in list format (not JSON), displays them
in an completing-read prompt and jumps to the chosen one's
Location.

If OTHER-WINDOW is given, will jump to the result in another window."
  (let ((chosen-quickfix
         (omnisharp--choose-quickfix-ido
          (omnisharp--vector-to-list quickfixes))))
    (omnisharp-go-to-file-line-and-column chosen-quickfix
                                          other-window)))

(defun omnisharp--choose-quickfix-ido (quickfixes)
  "Given a list of QuickFixes, lets the user choose one using
completing-read. Returns the chosen element."
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
          (omnisharp--completing-read
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

(defun omnisharp-navigate-to-solution-member (&optional other-window)
  (interactive "P")
  (let ((filter (omnisharp--read-string
                 "Enter the start of the symbol to go to: ")))
    (omnisharp--send-command-to-server
     "findsymbols"
     ;; gets all symbols. could also filter here but ido doesn't play
     ;; well with changing its choices
     `((Filter . ,filter))
     (-lambda ((&alist 'QuickFixes quickfixes))
              (omnisharp--choose-and-go-to-quickfix-ido quickfixes other-window)))))

(defun omnisharp-navigate-to-solution-member-other-window ()
  (omnisharp-navigate-to-solution-member t))

(defun omnisharp-navigate-to-solution-file (&optional other-window)
  (interactive "P")
  (omnisharp--send-command-to-server
   "gotofile"
   nil
   (-lambda ((&alist 'QuickFixes quickfixes))
            (omnisharp--choose-and-go-to-quickfix-ido quickfixes other-window))))

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

(defun omnisharp-navigate-to-region
  (&optional other-window)
  "Navigate to region in current file. If OTHER-WINDOW is given and t,
use another window."
  (interactive "P")
  (omnisharp--send-command-to-server
   "gotoregion"
   (omnisharp--get-request-object)
   (-lambda ((&alist 'QuickFixes quickfixes))
            (omnisharp--choose-and-go-to-quickfix-ido quickfixes other-window))))

(provide 'omnisharp-navigation-actions)
