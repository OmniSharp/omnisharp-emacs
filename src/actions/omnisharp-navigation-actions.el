;; -*- lexical-binding: t -*-

(defun omnisharp-go-to-definition (&optional other-window)
  "Jump to the definition of the symbol under point. With prefix
argument, use another window."
  (interactive "P")
  (omnisharp--send-command-to-server
   "gotodefinition"
   (omnisharp--get-request-object)
   (lambda (response)
     (if (null (omnisharp--get-filename response))
         (message
          "Cannot go to definition as none was returned by the API.")
       (omnisharp-go-to-file-line-and-column response other-window)))))

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
          (omnisharp--ido-completing-read
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

(defun omnisharp--get-solution-files-list-of-strings ()
  "Returns all files in the current solution as a list of strings."
  (->> (omnisharp--get-solution-files-quickfix-response)
    (assoc 'QuickFixes)
    (cdr)
    (omnisharp--vector-to-list)
    (--map (omnisharp--get-filename it))))

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
