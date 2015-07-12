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
  (f-join omnisharp-emacs-root-path
          "test/MinimalSolution/"))

(add-to-list 'load-path omnisharp-emacs-root-path)

(require 'omnisharp)
(require 'espuds)
(require 'ert)

(defun omnisharp--create-ecukes-test-server ()
  (setq omnisharp--server-info
        (make-omnisharp--server-info
         ;; use a pipe for the connection instead of a pty
         (let ((process-connection-type nil)
               (process (start-process
                         "omnisharp-server"             ; process name
                         "omnisharp-server"             ; buffer name
                         "/home/mika/git/omnisharp-emacs/omnisharp-roslyn/omnisharp"
                         ;; "-v"
                         "-s" "/home/mika/git/omnisharp-emacs/test/MinimalSolution/" "--stdio")))
           (set-process-filter process 'omnisharp--handle-server-message)
           (set-process-sentinel process 'omnisharp--server-process-sentinel)
           (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
           process)
         1)))

(Setup
 ;; Before anything has run
 (omnisharp--create-ecukes-test-server)
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
 (kill-process "omnisharp-server")
 )
