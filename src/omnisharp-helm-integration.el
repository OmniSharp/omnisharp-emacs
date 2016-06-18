;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-

(when (require 'helm-grep nil 'noerror)
  ;;; Helm usages
  (defvar omnisharp-helm-usage-candidates nil)

  (defun omnisharp--helm-usage-transform-candidate (candidate)
    "Convert a quickfix entry into helm output"
    (cons
     (format "%s(%s): %s"
             (propertize (file-name-nondirectory
                          (omnisharp--get-filename candidate))
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
    (omnisharp--send-command-to-server
     "findusages"
     (omnisharp--get-request-object)
     (-lambda ((&alist 'QuickFixes quickfixes))
              (omnisharp--helm-got-usages quickfixes))))

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
                                     :candidates (omnisharp--helm-find-symbols-candidates))
          :buffer "*Omnisharp Symbols*"
          :truncate-lines t))

  (defun omnisharp--helm-find-symbols-candidates ()
    (let (candidates)
      (omnisharp--send-command-to-server-sync
       "findsymbols"
       '((Filter . ""))
       (-lambda ((&alist 'QuickFixes quickfixes))
                (setq candidates
                      (-map 'omnisharp--helm-find-symbols-transform-candidate
                            quickfixes))))
      candidates))

  (defun omnisharp--helm-find-symbols-transform-candidate (candidate)
    "Convert a quickfix entry into helm output"
    (cons
     (format "%s : %s"
             (propertize (omnisharp--get-filename candidate)
                         'face 'helm-grep-file)
             (nth 0 (split-string (cdr (assoc 'Text candidate)) "(")))
     candidate)))

(provide 'omnisharp-helm-integration)
