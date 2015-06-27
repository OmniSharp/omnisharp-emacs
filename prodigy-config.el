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
