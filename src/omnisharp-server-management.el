;; this is a testing / development file only. don't use this.

(defun make-omnisharp--server-info (process request-id)
  `((:process . ,process)
    ;; This is incremented for each request. Do not modify it in other
    ;; places.
    (:request-id . ,request-id)
    ;; alist of (request-id . response-handler)
    (:response-handlers '(()))))

(setq omnisharp-debug t)

(defun omnisharp--send-command-to-server (api-name payload &optional response-handler)
  ;; make RequestPacket with request-id
  ;; send request
  ;; store response handler associated with the request id
  (-let* ((server-info omnisharp--server-info)
          ((&alist :process process
                   :request-id request-id) server-info)
          (request (omnisharp--make-request-packet api-name
                                                   payload
                                                   request-id)))
    (when omnisharp-debug
      (omnisharp--log (format "--> %s" (prin1-to-string request))))

    ;; update current request-id and associate a response-handler for
    ;; this request
    (setcdr (assoc :request-id server-info) (+ 1 request-id))
    (setcdr (assoc :response-handlers server-info)
            (assoc `(,request-id . response-handler)
                   (cdr (assoc :response-handlers server-info))))
    (process-send-string process request)))

(defun omnisharp--make-request-packet (api-name payload request-id)
  (let ((response (-concat payload `((Command . ,api-name)
                                     (Seq . ,request-id)))))
    (concat (json-encode response) "\n")))

;;; read output
;;;
;;; Plan:
;;;
;;; read output, increment request-id
;;; log events to process-buffer
;;; when a response ("Type": "response") arrives, call function associated with that Request_seq
;;; also remove the response handler
(defun omnisharp--handle-server-message (process message-part)
  (let* ((messages-from-server (omnisharp--read-lines-from-process-output
                                process message-part))
         (error-message (concat
                         "The server sent an unknown json message. "
                         "Inspect the omnisharp-server process buffer "
                         "to view recent messages from the server. "
                         "Set `omnisharp-debug' to t and inspect the "
                         "*omnisharp-debug* buffer to this error specifically."))
         (json-messages (--map (omnisharp--json-read-from-string it error-message)
                               messages-from-server)))
    (-map 'omnisharp--handle-server-event json-messages)))

(defun omnisharp--handle-server-event (packet)
  "Takes an alist representing some kind of Packet, possibly a
ResponsePacket or an EventPacket, and processes it depending on
its type."
  (-let ((server-info omnisharp--server-info)
         ((&alist 'Type type) packet))
    (cond ((equal "event" type)
           (-let [(&alist 'LogLevel log-level
                          'Message message) (cdr (assoc 'Body packet))]
             (omnisharp--log (format "%s: %s" log-level message))))

          ((equal "response" type)
           (omnisharp--handle-server-response-packet packet server-info))

          (t (omnisharp--log (format "Received an unknown server packet: %s"
                                     (prin1-to-string packet)))))))

(defun omnisharp--handle-server-response-packet (packet server-info)
  "Calls the appropriate response callback for the received packet"
  (-let (((&alist 'Success success
                  'Message message
                  'Body body
                  'Command command
                  'Request_seq request-seq) packet)
         ((&alist :response-handlers response-handlers) server-info))

    ;; try to find the matching response-handler
    (-if-let (request-response (--first (= (car it) request-seq)
                                        response-handlers))
        (-let (((request-id . response-handler) request-response))
          (omnisharp--log (format "<-- %s %s: %s"
                                  request-seq
                                  command
                                  body))
          ;; remove handler for this request
          (setcdr (assoc :response-handlers server-info)
                  (--remove (= (car it) request-seq)
                            response-handlers))

          (apply response-handler (list body)))

      (omnisharp--log (format "<-- %s %s: Warning: response could not be handled: %s"
                              request-seq
                              command
                              body)))))

(defun omnisharp--at-full-line? ()
  ;; all platforms use \n as newline in emacs
  (s-ends-with? "\n"
                (substring-no-properties (or (thing-at-point 'line)
                                             ""))))

(defun omnisharp--marker-at-full-line? (position-or-marker)
  (save-excursion
    (goto-char position-or-marker)
    (omnisharp--at-full-line?)))

(defun omnisharp--read-lines-from-process-output (process message-part)
  "Problem: emacs reads output from the omnisharp-roslyn subprocess
not line by line, but by some amount of characters. The way we want
to read the omnisharp-roslyn output is line by line, since each
response seems to be exactly one line long.

This function returns full lines returned from the server process that
have not been returned before."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      ;; previous-text-marker will change if it refers to the marker
      ;; and the marker is changed. Get it as an integer instead to
      ;; avoid mutation
      (let* ((previous-text-marker (save-excursion
                                     (goto-char (process-mark process))
                                     (point))))
        (save-excursion
          (progn
            ;; Insert the text, advancing the process marker.
            (goto-char (buffer-end 1))
            (insert message-part)
            ;; Return previous pending lines only when the latest line
            ;; is complete. Might be slightly slower but easier to
            ;; implement
            (when (omnisharp--marker-at-full-line? (point))
              (progn
                (set-marker (process-mark process) (point))
                ;; get the start of the last inserted line
                (goto-char previous-text-marker)
                (beginning-of-line)
                (--filter (not (s-blank? it))
                          (s-lines (buffer-substring-no-properties
                                    (point)
                                    (process-mark process))))))))))))

(defvar omnisharp--server-info nil)

;;; todo stop-process

(provide 'omnisharp-server-management)
