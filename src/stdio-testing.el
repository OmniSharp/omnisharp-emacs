;; this is a testing / development file only. don't use this.

(defvar omnisharp--server-process nil)
(defvar omnisharp--server-request-id 20
  "This is incremented for each request. Do not modify it in other places.")

;;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Processes.html
;;; test:
;;; start server
(comment
 (setq omnisharp--server-process
       ;; use a pipe for the connection instead of a pty
       (let ((process-connection-type nil))
         (start-process
          "omnisharp-server"             ; process name
          "omnisharp-server"             ; buffer name
          "/home/mika/git/omnisharp-emacs/omnisharp-roslyn/omnisharp"
          "-v" "-s" "/home/mika/git/omnisharp-emacs/test/MinimalSolution/" "--stdio"))))

(setq omnisharp-debug t)

;;; send input
(defun omnisharp--send-command-to-server (server api-name payload)
  (let ((request (omnisharp--make-request-packet api-name payload)))
    (when omnisharp-debug
      (message "--> %s" (prin1-to-string request)))
    (if (process-running-child-p server)
        (process-send-string server request)
      (message "Error: server process has died."))))

(defun omnisharp--make-request-packet (api-name payload)
  (let ((response (-concat payload `((Command . ,api-name)
                                     (Seq . ,omnisharp--server-request-id)))))
    (setq omnisharp--server-request-id (+ 1 omnisharp--server-request-id))
    (concat (json-encode response) "\n")))

(omnisharp--send-command-to-server omnisharp--server-process
                                   "checkreadystatus"
                                   nil)

;;; read output
;;; read async event?
;;; - how can I set up listeners for different types of messages?

;;; todo stop-process
