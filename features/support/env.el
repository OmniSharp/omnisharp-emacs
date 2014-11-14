;; In this file the test environment is configured. Test packages are
;; required, package state is reset between test runs, etc.
(require 'f)
(require 's)

(defvar omnisharp-emacs-support-path
  (f-dirname (f-this-file)))

(defvar omnisharp-emacs-features-path
  (f-parent omnisharp-emacs-support-path))

(defvar omnisharp-emacs-root-path
  (f-parent omnisharp-emacs-features-path))

(defvar omnisharp-server-root-path
  (f-join omnisharp-emacs-root-path
          "OmniSharpServer/"))

(defvar omnisharp-minimal-test-solution-path
  (f-join omnisharp-server-root-path
          "OmniSharp.Tests/Solution/minimal/"))

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
