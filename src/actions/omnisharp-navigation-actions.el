
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

(defun omnisharp-navigate-to-region
  (&optional other-window)
  "Navigate to region in current file. If OTHER-WINDOW is given and t,
use another window."
  (interactive "P")
  (-let [(&alist 'QuickFixes qfs) (omnisharp-post-message-curl-as-json
                                   (concat (omnisharp-get-host) "gotoregion")
                                   (omnisharp--get-common-params))]
    (omnisharp--choose-and-go-to-quickfix-ido qfs other-window)))

(provide 'omnisharp-navigation-actions)
