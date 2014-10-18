;; In this file the test environment is configured. Test packages are
;; required, package state is reset between test runs, etc.
(require 'f)

(defvar omnisharp-emacs-support-path
  (f-dirname (f-this-file)))

(defvar omnisharp-emacs-features-path
  (f-parent omnisharp-emacs-support-path))

(defvar omnisharp-emacs-root-path
  (f-parent omnisharp-emacs-features-path))

(add-to-list 'load-path omnisharp-emacs-root-path)

(require 'omnisharp)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run

 ;; evil's normal mode messes up point locations
 (when (fboundp 'evil-insert)
   (evil-insert 1))
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
