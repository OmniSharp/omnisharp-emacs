;; Example evil-mode config

(evil-define-key 'insert omnisharp-mode-map
  (kbd "M-.") 'omnisharp-auto-complete)

(evil-define-key 'normal omnisharp-mode-map
  (kbd "<f12>") 'omnisharp-go-to-definition)

(evil-define-key 'normal omnisharp-mode-map
  (kbd "g u") 'omnisharp-find-usages)

(evil-define-key 'normal omnisharp-mode-map
  (kbd "g o") 'omnisharp-go-to-definition)

(evil-define-key 'normal omnisharp-mode-map
  (kbd "g r") 'omnisharp-run-code-action-refactoring)

(evil-define-key 'normal omnisharp-mode-map
  (kbd ", i") 'omnisharp-current-type-information)

(evil-define-key 'insert omnisharp-mode-map
  (kbd ".") 'omnisharp-add-dot-and-auto-complete)

(evil-define-key 'normal omnisharp-mode-map
  (kbd ", n t") 'omnisharp-navigate-to-current-file-member)

(evil-define-key 'normal omnisharp-mode-map
  (kbd ", n s") 'omnisharp-navigate-to-solution-member)

;; Speed up auto-complete on mono drastically. This comes with the
;; downside that documentation is impossible to fetch.
(setq omnisharp-auto-complete-want-documentation nil)
