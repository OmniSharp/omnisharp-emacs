;; this is a testing / development file only. don't use this.

(defun make-omnisharp--server-info (process request-id)
  `((:process . ,process)
    ;; This is incremented for each request. Do not modify it in other
    ;; places.
    (:request-id . ,request-id)))

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

;;; read output
;;;
;;; Plan:
;;;
;;; read output, increment request-id
;;; log events to process-buffer
;;; when a response ("Type": "response") arrives, call function associated with that Request_seq
;;; also remove the response handler
(defun omnisharp--handle-server-message (process message-part)
  (let ((messages-from-server (omnisharp--read-lines-from-process-output
                               process message-part)))
    messages-from-server))

(defun omnisharp--at-full-line? ()
  ;; all platforms use \n as newline in emacs
  (s-ends-with? "\n"
                (substring-no-properties (or (thing-at-point 'line)
                                             ""))))

(defun omnisharp--marker-at-full-line? (marker)
  (save-excursion
    (goto-char marker)
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
                                     (point)))
             (previous-text-was-complete-line?
              (omnisharp--marker-at-full-line? previous-text-marker)))
        (save-excursion
          (progn
            ;; Insert the text, advancing the process marker.
            (goto-char previous-text-marker)
            (insert message-part)
            (set-marker (process-mark process) (point))
            (when (omnisharp--marker-at-full-line? (process-mark process))
              (progn
                ;; get the start of the last inserted line
                (goto-char previous-text-marker)
                (beginning-of-line)
                (s-lines (buffer-substring-no-properties
                          (point) (process-mark process)))))))))))

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
     (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
     process)
   1))

;;; - how can I set up listeners for different types of messages?

;;; todo stop-process
