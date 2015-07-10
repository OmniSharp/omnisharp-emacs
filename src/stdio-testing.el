;; this is a testing / development file only. don't use this.

(defun make-omnisharp--server-info (process request-id)
  `((:process . ,process)
    ;; This is incremented for each request. Do not modify it in other
    ;; places.
    (:request-id . ,request-id)))

(defvar omnisharp--server-info
  (make-omnisharp--server-info
   ;; use a pipe for the connection instead of a pty
   (let ((process-connection-type nil))
     (start-process
      "omnisharp-server"             ; process name
      "omnisharp-server"             ; buffer name
      "/home/mika/git/omnisharp-emacs/omnisharp-roslyn/omnisharp"
      ;; "-v"
      "-s" "/home/mika/git/omnisharp-emacs/test/MinimalSolution/" "--stdio"))
   1))

(setq omnisharp-debug t)

;;; send input
(defun omnisharp--send-command-to-server (server-info api-name payload)
  (-let* (((&alist :process process
                   :request-id request-id) server-info)
          (request (omnisharp--make-request-packet api-name
                                                   payload
                                                   request-id)))
    (when omnisharp-debug
      (message "--> %s" (prin1-to-string request)))

    (setcdr (assoc :request-id server-info) (+ 1 request-id))

    (if (process-running-child-p process)
        (process-send-string process request)
      (message "Error: server-info process has died."))))

(defun omnisharp--make-request-packet (api-name payload request-id)
  (let ((response (-concat payload `((Command . ,api-name)
                                     (Seq . ,request-id)))))
    (concat (json-encode response) "\n")))

(omnisharp--send-command-to-server omnisharp--server-info
                                   "checkreadystatus"
                                   nil)

;;; read output
;;;
;;; Plan:
;;; make request-id
;;; send request
;;; store response handler associated with the request id
;;;
;;; read output, increment request-id
;;; log events to process-buffer
;;; when a response ("Type": "response") arrives, call function associated with that Request_seq
;;; also remove the response handler

;;;
;;; (set-process-filter omnisharp--server-process omnisharp--server-process-filter)
;;; read async event?
;;; - how can I set up listeners for different types of messages?

;;; todo stop-process
