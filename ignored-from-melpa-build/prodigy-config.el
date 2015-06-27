;; If you use the prodigy plugin for emacs, you can quickly adapt this
;; file to set up emacs as an interface to start background processes
;; and run test for this project.
;;
;; To use this file as such do the following:
;; - Configure the file and set the paths right
;; - Load this file's code into emacs somehow, e.g. from your init file do
;;     (load-file "/home/mika/git/omnisharp-emacs/ignored-from-melpa-build/prodigy-example-config.el")
;; - Then run M-x prodigy. You will be shown the services defined in
;;   this file. Start a service with "s" and view its output with "$"
;;   (once it starts generating output). For more info, see the
;;   prodigy site below
;;
;; For prodigy:
;; https://github.com/rejeep/prodigy.el
(require 'prodigy)

;; omnisharp-emacs development hacks
(setenv "PATH" (concat "/opt/monodevelop/bin/:"
                       (getenv "PATH")
                       ":/home/mika/.cask/bin"
                       ":/home/mika/bin/"))

(let ((omni-dir "/home/mika/git/omnisharp-emacs/"))
  (prodigy-define-service
    :name "OmniSharpServer for integration tests"
    :command (concat omni-dir "start-omnisharp-server-for-integration-tests.sh")
    :cwd omni-dir
    :stop-signal 'kill
    :kill-process-buffer-on-stop t
    :truncate-output 200
    :tags '(omnisharp))

  (prodigy-define-service
    :name "omnisharp-emacs integration tests"
    :command (concat omni-dir "run-integration-tests.sh")
    :cwd omni-dir
    :stop-signal 'kill
    :kill-process-buffer-on-stop t
    :truncate-output 200
    :tags '(omnisharp))

  (prodigy-define-service
    :name "omnisharp-emacs unit tests"
    :command (concat omni-dir "run-tests.sh")
    :cwd omni-dir
    :stop-signal 'kill
    :kill-process-buffer-on-stop t
    :truncate-output 200
    :tags '(omnisharp))

  (prodigy-define-service
    :name "omnisharp-emacs installation test"
    :command (concat omni-dir "run-melpa-build-test.sh")
    :cwd omni-dir
    :stop-signal 'kill
    :kill-process-buffer-on-stop t
    :truncate-output 200
    :tags '(omnisharp)))

(add-to-list 'evil-emacs-state-modes 'prodigy-mode)
