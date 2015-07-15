;; In this file the test environment is configured. Test packages are
;; required, package state is reset between test runs, etc.
(require 'f)
(require 's)
(require 'shut-up)

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

(require 'ecukes)
(require 'omnisharp)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 (when (null omnisharp--server-info)
   (omnisharp--log "TEST: starting test server in integration test Setup hook")
   (omnisharp--create-ecukes-test-server))

 ;; wait that the server is alive and ready before starting the test run
 (with-timeout (2 ; seconds
                (omnisharp--log "Server did not start in time"))
   (while (not (equal t (cdr (assoc :started? omnisharp--server-info))))
     (accept-process-output))))

(Before
 ;; Before each scenario is run

 ;; evil's normal mode messes up point locations
 (when (fboundp 'evil-insert)
   (evil-insert 1)))

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run

 (omnisharp--log "TEST: shutting down test server in integration test Teardown hook")
 (with-current-buffer "Omni-Server"
   (let ((filename "omnisharp-server-output.txt"))
     (write-file filename)
     (print (format "Omni-Server buffer contents (available in %s):\n"
                    filename))
     (print (buffer-string))
     (kill-process "Omni-Server")))

 (with-current-buffer "*omnisharp-debug*"
   (let ((filename "omnisharp-debug-output.txt"))
     (write-file filename)
     (print (format "Debug buffer contents (available in %s):\n"
                    filename))
     (print (buffer-string))))

 (print "Server info:\n")
 (print (prin1-to-string omnisharp--server-info))
 (print "\n"))
