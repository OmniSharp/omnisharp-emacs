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
(setenv "PATH" (concat (getenv "PATH")
                       ":/home/mika/.cask/bin"
                       ":/home/mika/bin/"))

(defmacro def-omnisharp-service (name command)
  (let ((omni-dir "/home/mika/git/omnisharp-emacs/"))
    `(prodigy-define-service
       :name ,name
       :command (concat ,omni-dir ,command)
       :cwd ,omni-dir
       :stop-signal 'kill
       :kill-process-buffer-on-stop t
       :truncate-output 200
       :tags '(omnisharp))))

(def-omnisharp-service "OmniSharpServer for integration tests"
  "start-omnisharp-server-for-integration-tests.sh")

(def-omnisharp-service "omnisharp-emacs integration tests"
  "run-integration-tests.sh")

(def-omnisharp-service "omnisharp-emacs single integration test"
  "run-single-integration-test.sh")

(def-omnisharp-service "omnisharp-emacs unit tests"
  "run-tests.sh")

(def-omnisharp-service "omnisharp-emacs installation test"
  "run-melpa-build-test.sh")

(add-to-list 'evil-emacs-state-modes 'prodigy-mode)
(provide 'prodigy-config)
