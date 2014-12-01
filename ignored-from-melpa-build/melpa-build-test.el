(require 'package)
(require 'files)

;; should be run in the repo root directory

(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(let ((file (expand-file-name
             (car (file-expand-wildcards "melpa/packages/omnisharp-*.tar")))))
  (message "installing file %s" file)
  (package-install-file file))


