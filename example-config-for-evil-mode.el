;; Example evil-mode config

(evil-define-key 'insert omnisharp-mode-map
  (kbd "M-.")
  (lambda () (interactive)
    (omnisharp-auto-complete)))

(evil-define-key 'normal omnisharp-mode-map
  (kbd "<f12>")
  (lambda () (interactive)
    (omnisharp-go-to-definition)))

(evil-define-key 'normal omnisharp-mode-map
  (kbd "g u")
  (lambda () (interactive)
    (omnisharp-find-usages)))

(evil-define-key 'normal omnisharp-mode-map
  (kbd "g o")
  (lambda () (interactive)
    (omnisharp-go-to-definition)))

(evil-define-key 'normal omnisharp-mode-map
  (kbd "g r")
  (lambda () (interactive)
    (omnisharp-run-code-action-refactoring)))

(evil-define-key 'normal omnisharp-mode-map
  (kbd ", i") 'omnisharp-current-type-information)

(evil-define-key 'normal omnisharp-mode-map
  (kbd ", b") 'omnisharp-browse-type)

(evil-define-key 'insert omnisharp-mode-map
  (kbd ".") 'omnisharp-add-dot-and-auto-complete)

;; Speed up auto-complete on mono drastically. This comes with the
;; downside that documentation is impossible to fetch.
(setq omnisharp-auto-complete-want-documentation nil)
