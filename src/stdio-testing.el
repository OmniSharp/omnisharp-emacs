;; this is a testing / development file only. don't use this.

(defun make-omnisharp--server-info (process request-id)
  `((:process . ,process)
    ;; This is incremented for each request. Do not modify it in other
    ;; places.
    (:request-id . ,request-id)))

(defvar omnisharp--server-info
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
     (set-process-coding-system process (utf-8-unix . utf-8-unix))
     process)
   1))

(setq omnisharp-debug t)

(defun omnisharp--send-command-to-server (server-info api-name payload)
  ;; make RequestPacket with request-id
  ;; send request
  ;; store response handler associated with the request id
  (-let* (((&alist :process process
                   :request-id request-id) server-info)
          (request (omnisharp--make-request-packet api-name
                                                   payload
                                                   request-id)))
    (when omnisharp-debug
      (omnisharp--log (format "--> %s" (prin1-to-string request))))

    (setcdr (assoc :request-id server-info) (+ 1 request-id))
    (process-send-string process request)))

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
;;;
;;; read output, increment request-id
;;; log events to process-buffer
;;; when a response ("Type": "response") arrives, call function associated with that Request_seq
;;; also remove the response handler
(defun omnisharp--handle-server-message (process message-part)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark process))
          (insert (concat "\n" message-part "\n"))
          (set-marker (process-mark process) (point)))
        (if moving (goto-char (process-mark process)))))))

;;; read async event?
;;; - how can I set up listeners for different types of messages?

;;; todo stop-process
