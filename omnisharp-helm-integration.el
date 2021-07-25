;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-

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
    (helm-highlight-current-line))

  ;;; Helm find symbols
  (defun omnisharp-helm-find-symbols ()
    (interactive)
    (helm :sources (helm-make-source "Omnisharp - Find Symbols" 'helm-source-sync
                                     :action 'omnisharp--helm-jump-to-candidate
                                     :volatile t
                                     :candidates 'omnisharp--helm-find-symbols-candidates)
          :truncate-lines t))

  (defun omnisharp--helm-find-symbols-candidates ()
    (if (string= helm-pattern "")
        ;; we want to wait for at least one char before we trigger
        ;; server search because listing all the symbols can take a lot
        ;; of time on large projects
        nil

      (let (candidates)
        (omnisharp--send-command-to-server-sync
         "findsymbols"
         `((Filter . ,helm-pattern))
         (-lambda ((&alist 'QuickFixes quickfixes))
           (setq candidates
                 (-map 'omnisharp--helm-find-symbols-transform-candidate
                       quickfixes))))
        candidates)))

  (defun omnisharp--helm-find-symbols-transform-candidate (candidate)
    "Convert a quickfix entry into helm output"
    (cons
     (format "%s : %s(%s)"
             (propertize (nth 0 (split-string (cdr (assoc 'Text candidate)) "("))
                         'face 'helm-grep-match)
             (propertize (omnisharp--get-filename candidate)
                         'face 'helm-grep-file)
             (propertize (number-to-string (cdr (assoc 'Line candidate)))
                         'face 'helm-grep-lineno))
     candidate)))

(provide 'omnisharp-helm-integration)
