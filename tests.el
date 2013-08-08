;; Example evil-mode config

(define-key evil-insert-state-map
  (kbd "M-.")
  (lambda () (interactive)
    (omnisharp-auto-complete)))

(define-key evil-normal-state-map
  (kbd "<f12>")
  (lambda () (interactive)
    (omnisharp-go-to-definition)))

(define-key evil-normal-state-map
  (kbd "g u")
  (lambda () (interactive)
    (omnisharp-find-usages)))

(define-key evil-normal-state-map
  (kbd "g o")
  (lambda () (interactive)
    (omnisharp-go-to-definition)))

(define-key evil-normal-state-map
  (kbd "g r")
  (lambda () (interactive)
    (omnisharp-run-code-action-refactoring)))

(define-key evil-normal-state-map
  (kbd ", i") 'omnisharp-current-type-information)

(define-key evil-normal-state-map
  (kbd ", b") 'omnisharp-browse-type)

;; Speed up auto-complete on mono drastically. This comes with the
;; downside that documentation is impossible to fetch.
(setq omnisharp-auto-complete-want-documentation nil)
