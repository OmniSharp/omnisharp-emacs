(defun make-omnisharp--server-info (process)
  `((:process . ,process)
    ;; This is incremented for each request. Do not modify it in other
    ;; places.
    (:request-id . 1)
    ;; alist of (request-id . response-handler)
    (:response-handlers . nil)))

(setq omnisharp-debug t)

(defun omnisharp--clear-response-handlers ()
  "For development time cleaning up impossible states of response
handlers in the current omnisharp--server-info."
  (setcdr (assoc :response-handlers omnisharp--server-info)
          nil))

(defmacro comment (&rest body) nil)
(comment (omnisharp--clear-response-handlers))

(defun omnisharp--send-command-to-server (api-name payload &optional response-handler)
  ;; make RequestPacket with request-id
  ;; send request
  ;; store response handler associated with the request id
  (if (equal nil omnisharp--server-info)
      (message (concat "OmniSharp server not running. "
                       "Start it with `omnisharp-start-omnisharp-server' first"))
    (-let* ((server-info omnisharp--server-info)
            ((&alist :process process
                     :request-id request-id) server-info)
            (request (omnisharp--make-request-packet api-name
                                                     payload
                                                     request-id)))
      (when omnisharp-debug
        (omnisharp--log (format "--> %s %s"
                                api-name
                                (prin1-to-string request))))

      ;; update current request-id and associate a response-handler for
      ;; this request
      (setcdr (assoc :request-id server-info) (+ 1 request-id))
      (setcdr (assoc :response-handlers server-info)
              (-concat `((,request-id . ,response-handler))
                       (cdr (assoc :response-handlers server-info))))
      (process-send-string process (concat request "\n")))))

(defun omnisharp--make-request-packet (api-name payload request-id)
  (let ((response (-concat payload `((Command . ,api-name)
                                     (Seq . ,request-id)))))
    (json-encode response)))

(defun omnisharp--handle-server-message (process message-part)
  (condition-case maybe-error-data
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
        (-map 'omnisharp--handle-server-event json-messages))
    (error (progn
             (let ((msg (format "omnisharp--handle-server-message error: %s"
                                (prin1-to-string maybe-error-data))))
               (omnisharp--log msg)
               (message msg))))))

(defun omnisharp--handle-server-event (packet)
  "Takes an alist representing some kind of Packet, possibly a
ResponsePacket or an EventPacket, and processes it depending on
its type."
  (-let ((server-info omnisharp--server-info)
         ((&alist 'Type type
                  'Event event) packet))
    (cond ((and (equal "event" type)
                (equal "log" event))
           (-let [(&alist 'LogLevel log-level
                          'Message message) (cdr (assoc 'Body packet))]
             (omnisharp--log (format "%s: %s" log-level message))))

          ((equal "response" type)
           (omnisharp--handle-server-response-packet packet server-info))

          (t (omnisharp--log (format "Received an unknown server packet: %s"
                                     (prin1-to-string packet)))))))

(defun omnisharp--remove-response-handler (server-info request-id)
  (setcdr (assoc :response-handlers server-info)
          (--remove (= (car it) request-id)
                    (-non-nil response-handlers))))

(defun omnisharp--handle-server-response-packet (packet server-info)
  "Calls the appropriate response callback for the received packet"
  (-let (((&alist 'Success success
                  'Message message
                  'Body body
                  'Command command
                  'Request_seq request-id) packet)
         ((&alist :response-handlers response-handlers) server-info))

    ;; try to find the matching response-handler
    (-if-let (request-response (--first (= (car it) request-id)
                                        response-handlers))
        (-let (((request-id . response-handler) request-response))
          (omnisharp--log (format "<-- %s %s: %s"
                                  request-id
                                  command
                                  body))
          (omnisharp--remove-response-handler server-info request-id)
          (apply response-handler (list body)))

      (omnisharp--log (format "<-- %s %s: Warning: response could not be handled: %s"
                              request-id
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
                                  (process-mark process)))))))))))

(defvar omnisharp--server-info nil)

;;; todo stop-process

(provide 'omnisharp-server-management)
