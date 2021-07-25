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

(defun omnisharp-current-type-information (&optional add-to-kill-ring)
  "Display information of the current type under point. With prefix
argument, add the displayed result to the kill ring. This can be used
to insert the result in code, for example."
  (interactive "P")
  (omnisharp-current-type-information-worker 'Type add-to-kill-ring))

(defun omnisharp-current-type-documentation (&optional add-to-kill-ring)
  "Display documentation of the current type under point. With prefix
argument, add the displayed result to the kill ring. This can be used
to insert the result in code, for example."
  (interactive "P")
  (omnisharp-current-type-information-worker 'Documentation add-to-kill-ring))

(defun omnisharp-current-type-information-worker (type-property-name
                                                  &optional add-to-kill-ring)
  "Get type info from the API and display a part of the response as a
message. TYPE-PROPERTY-NAME is a symbol in the type lookup response
from the server side, i.e. 'Type or 'Documentation that will be
displayed to the user."
  (omnisharp--send-command-to-server
   "typelookup"
   (omnisharp--get-typelookup-request-object)
   (lambda (response)
     (let ((stuff-to-display (cdr (assoc type-property-name
                                         response))))
       (omnisharp--message-at-point stuff-to-display)
       (when add-to-kill-ring
         (kill-new stuff-to-display))))))

(defun omnisharp-current-type-information-to-kill-ring ()
  "Shows the information of the current type and adds it to the kill
ring."
  (interactive)
  (omnisharp-current-type-information t))

(defun omnisharp-find-usages ()
  "Find usages for the symbol under point"
  (interactive)
  (omnisharp--message "Finding usages...")
  (omnisharp--send-command-to-server
   "findusages"
   (omnisharp--get-request-object)
   (-lambda ((&alist 'QuickFixes quickfixes))
            (omnisharp--find-usages-show-response quickfixes))))

(defun omnisharp--find-usages-show-response (quickfixes)
  (if (equal 0 (length quickfixes))
      (omnisharp--message-at-point "No usages found.")
    (omnisharp--write-quickfixes-to-compilation-buffer
     quickfixes
     omnisharp--find-usages-buffer-name
     omnisharp-find-usages-header)))

(defun omnisharp-find-implementations-with-ido (&optional other-window)
  (interactive "P")
  (omnisharp--send-command-to-server-sync
   "findimplementations"
   (omnisharp--get-request-object)
   (lambda (quickfix-response)
     (omnisharp--show-or-navigate-to-quickfixes-with-ido quickfix-response
                                                         other-window))))

(defun omnisharp--show-or-navigate-to-quickfixes-with-ido (quickfix-response
                                                           &optional other-window)
  (-let (((&alist 'QuickFixes quickfixes) quickfix-response))
    (cond ((equal 0 (length quickfixes))
           (omnisharp--message "No implementations found."))
          ((equal 1 (length quickfixes))
           (omnisharp-go-to-file-line-and-column (-first-item (omnisharp--vector-to-list quickfixes))
                                                 other-window))
          (t
           (omnisharp--choose-and-go-to-quickfix-ido quickfixes other-window)))))

(defun omnisharp-find-usages-with-ido (&optional other-window)
  (interactive "P")
  (omnisharp--send-command-to-server
   "findusages"
   (omnisharp--get-request-object)
   (lambda (quickfix-response)
     (omnisharp--show-or-navigate-to-quickfixes-with-ido quickfix-response
                                                         other-window))))

(defun omnisharp-find-implementations ()
  "Show a buffer containing all implementations of the interface under
point, or classes derived from the class under point. Allow the user
to select one (or more) to jump to."
  (interactive)
  (omnisharp--message "Finding implementations...")
  (omnisharp-find-implementations-worker
   (omnisharp--get-request-object)
   (lambda (quickfixes)
     (cond ((equal 0 (length quickfixes))
            (omnisharp--message "No implementations found."))

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
  (omnisharp--send-command-to-server
   "findimplementations"
   request
   (-lambda ((&alist 'QuickFixes quickfixes))
            (apply callback (list (omnisharp--vector-to-list quickfixes))))))

(defun omnisharp-rename ()
  "Rename the current symbol to a new name. Lets the user choose what
name to rename to, defaulting to the current name of the symbol."
  (interactive)
  (let* ((current-word (thing-at-point 'symbol))
         (rename-to (read-string "Rename to: " current-word))
         (rename-request
          (->> (omnisharp--get-request-object)
            (cons `(RenameTo . ,rename-to))
            (cons `(WantsTextChanges . true))))
         (location-before-rename
          (omnisharp--get-request-object-for-emacs-side-use)))
    (omnisharp--send-command-to-server-sync
     "rename"
     rename-request
     (lambda (rename-response) (omnisharp--rename-worker
                                rename-response
                                location-before-rename)))))

(defun omnisharp--rename-worker (rename-response
                                 location-before-rename)
  (-if-let (error-message (cdr (assoc 'ErrorMessage rename-response)))
      (omnisharp--message error-message)
    (-let (((&alist 'Changes modified-file-responses) rename-response))
      ;; The server will possibly update some files that are currently open.
      ;; Save all buffers to avoid conflicts / losing changes
      (save-some-buffers t)

      (-map #'omnisharp--apply-text-changes modified-file-responses)

      ;; Keep point in the buffer that initialized the rename so that
      ;; the user does not feel disoriented
      (omnisharp-go-to-file-line-and-column location-before-rename)

      (omnisharp--message "Rename complete in files: \n%s"
               (-interpose "\n" (--map (omnisharp--get-filename it)
                                       modified-file-responses))))))

(defun omnisharp--apply-text-changes (modified-file-response)
  (-let (((&alist 'Changes changes) modified-file-response))
    (omnisharp--update-files-with-text-changes
     (omnisharp--get-filename modified-file-response)
     (omnisharp--vector-to-list changes))))

(provide 'omnisharp-current-symbol-actions)
