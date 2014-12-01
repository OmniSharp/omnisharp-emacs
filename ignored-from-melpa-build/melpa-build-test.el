(setq user-emacs-directory "./sandbox")

(require 'package)
(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(package-install-file "~/git/melpa/packages/omnisharp-20141201.2121.tar")
