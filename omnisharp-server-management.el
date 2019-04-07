;; -*- lexical-binding: t; -*-

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;
;; omnisharp--server-info an assoc list is used to track all the metadata
;; about currently running server.
;;
;; NOTE 1: this will go away with multi-server functionality
;; NOTE 2: you shouldn't use this in user code, this is implementation detail
;;
;; keys:
;;  :process           - process of the server
;;  :request-id        - used for and incremented on every outgoing request
;;  :response-handlers - alist of (request-id . response-handler)
;;  :started?          - t if server reported it has started OK and is ready
;;  :project-path      - path to server project .sln, .csproj or directory
;;  :project-root      - project root directory (based on project-path)
;;  :last-unit-test    - a tuple of (test-framework (test-method-names ..))
(defvar omnisharp--server-info nil)

(defvar omnisharp--last-project-path nil)
(defvar omnisharp--restart-server-on-stop nil)
(defvar omnisharp-use-http nil "Set to t to use http instead of stdio.")

(defun make-omnisharp--server-info (process project-path)
  (let ((project-root (if (f-dir-p project-path) project-path
                        (f-dirname project-path))))
    ;; see notes on (defvar omnisharp--server-info)
    `((:process . ,process)
      (:request-id . 1)
      (:response-handlers . nil)
      (:started? . nil)
      (:project-path . ,project-path)
      (:project-root . ,project-root)
      (:last-unit-test . nil))))

(defun omnisharp--resolve-omnisharp-server-executable-path ()
    "Attempts to resolve a path to local executable for the omnisharp-roslyn server.
Will return `omnisharp-server-executable-path` override if set, otherwise will attempt
to use server installed via `omnisharp-install-server`.

 Failing all that an error message will be shown and nil returned."
    (if omnisharp-server-executable-path
        omnisharp-server-executable-path
      (let ((server-installation-path (omnisharp--server-installation-path)))
        (if server-installation-path
            server-installation-path
          (progn
            (omnisharp--message "omnisharp: No omnisharp server could be found.")
            (omnisharp--message (concat "omnisharp: Please use M-x 'omnisharp-install-server' or download server manually"
                                        " as detailed in https://github.com/OmniSharp/omnisharp-emacs/blob/master/doc/server-installation.md"))
            nil)))))

(defun omnisharp--do-server-start (project-root)
  (let ((server-executable-path (omnisharp--resolve-omnisharp-server-executable-path)))
    (message (format "omnisharp: starting server on project root: \"%s\"" project-root))

    (omnisharp--log-reset)
    (omnisharp--log (format "starting server on project root \"%s\"" project-root))
    (omnisharp--log (format "Using server binary on %s" server-executable-path))

    ;; Save all csharp buffers to ensure the server is in sync"
    (save-some-buffers t (lambda () (string-equal (file-name-extension (buffer-file-name)) "cs")))

    (setq omnisharp--last-project-path project-root)

    ;; this can be set by omnisharp-reload-solution to t
    (setq omnisharp--restart-server-on-stop nil)

    (setq omnisharp--server-info
          (make-omnisharp--server-info
           ;; use a pipe for the connection instead of a pty
           (let* ((process-connection-type nil)
                  (default-directory (omnisharp--path-to-server (expand-file-name project-root)))
                  (omnisharp-process (start-process
                                      "OmniServer" ; process name
                                      "OmniServer" ; buffer name
                                      server-executable-path
                                      "--encoding" "utf-8"
                                      "--stdio")))
             (buffer-disable-undo (process-buffer omnisharp-process))
             (set-process-filter omnisharp-process 'omnisharp--handle-server-message)
             (set-process-sentinel omnisharp-process
                                   (lambda (process event)
                                     (when (memq (process-status process) '(exit signal))
                                       (message "omnisharp: server has been terminated")
                                       (setq omnisharp--server-info nil)
                                       (if omnisharp--restart-server-on-stop
                                           (omnisharp--do-server-start omnisharp--last-project-path)))))
             omnisharp-process)
           project-root))))

(defun omnisharp--clear-response-handlers ()
  "For development time cleaning up impossible states of response
handlers in the current omnisharp--server-info."
  (setcdr (assoc :response-handlers omnisharp--server-info)
          nil))

(defmacro comment (&rest body) nil)
(comment (omnisharp--clear-response-handlers))

(defun omnisharp--send-command-to-server (api-name contents &optional response-handler async)
  "Sends the given command to the server.
Depending on omnisharp-use-http it will either send it via http or stdio.
The variable ASYNC has no effect when not using http."

  (if omnisharp-use-http
      (omnisharp--send-command-to-server-http api-name contents response-handler async)
    (omnisharp--send-command-to-server-stdio api-name contents response-handler)))

(defun omnisharp--send-command-to-server-http (api-name contents response-handler &optional async)
  "Sends the given command via curl"
  (omnisharp-post-http-message api-name response-handler contents async))

(defun omnisharp--send-command-to-server-stdio (api-name contents &optional response-handler)
  "Sends the given command to the server and associates a
response-handler for it. The server will respond to this request
later and the response handler will get called then.

Returns the unique request id that the request is given before
sending."
  ;; make RequestPacket with request-id
  ;; send request
  ;; store response handler associated with the request id
  (if (equal nil omnisharp--server-info)
      (message (concat "omnisharp: server is not running. "
                       "Start it with `omnisharp-start-omnisharp-server' first"))
    (if (not (s-starts-with? "/" api-name))
        (setq api-name (concat "/" api-name)))

    (-let* ((server-info omnisharp--server-info)
            ((&alist :process process
                     :request-id request-id) server-info)
            (request (omnisharp--make-request-packet api-name
                                                     contents
                                                     request-id)))
      (when omnisharp-debug
        (omnisharp--log (format "--> %s %s %s"
                                request-id
                                api-name
                                (prin1-to-string request))))

      ;; update current request-id and associate a response-handler for
      ;; this request
      (setcdr (assoc :request-id server-info) (+ 1 request-id))

      ;; requests that don't require handling are still added with a
      ;; dummy handler. This means they are pending. This is required
      ;; so that omnisharp--wait-until-request-completed can know when
      ;; the requests have completed.
      (setcdr (assoc :response-handlers server-info)
              (-concat `((,request-id . ,(or response-handler #'identity)))
                       (cdr (assoc :response-handlers server-info))))

      (process-send-string process (concat (json-encode request) "\n"))
      request-id)))

(defun omnisharp--send-command-to-server-sync (&rest args)
  "Like `omnisharp--send-command-to-server' but will block until the
request responded by the server."
  (omnisharp--wait-until-request-completed
   (apply 'omnisharp--send-command-to-server args)))

(defun omnisharp--make-request-packet (api-name contents request-id)
  (-concat `((Arguments . ,contents))
           `((Command . ,api-name)
             (Seq . ,request-id))))

(defun omnisharp--handle-server-message (process message-part)
  "Parse alists from accumulated json responses in the server's
process buffer, and handle them as server events"
  (condition-case maybe-error-data
      (let* ((messages-from-server (omnisharp--read-lines-from-process-output
                                    process message-part))
             (error-message (concat
                             "The server sent an unknown json message. "
                             "Inspect the omnisharp-server process buffer "
                             "to view recent messages from the server. "
                             "Set `omnisharp-debug' to t and inspect the "
                             "*omnisharp-debug* buffer for this error specifically."))
             (json-messages (--map (omnisharp--json-read-from-string it error-message)
                                   messages-from-server)))
        ;; should use -each here since it's for side effects only, but
        ;; it can't work with vectors. -map can, so use that instead.
        (-map #'omnisharp--handle-server-event json-messages))
    (error (let ((msg (format (concat "omnisharp--handle-server-message error: %s. "
                                      "See the OmniServer process buffer for detailed server output.")
                              (prin1-to-string maybe-error-data))))
             (omnisharp--log msg)
             (message msg)))))

(defun omnisharp--log-packet? (packet)
  (and (equal "event" (cdr (assoc 'Type packet)))
       (equal "log" (cdr (assoc 'Event packet)))))

(defun omnisharp--log-log-packet (packet)
  (-let (((&alist 'LogLevel log-level
                  'Name name
                  'Message message) (cdr (assoc 'Body packet))))
    (omnisharp--log (format "%s: %s, %s" log-level name message))
    (if (string-equal name "OmniSharp.Startup")
        (message (format "omnisharp: %s, %s" name message)))))

(defun omnisharp--event-packet? (packet)
  (and (equal "event" (cdr (assoc 'Type packet)))))

(defun omnisharp--response-packet? (packet)
  (equal "response" (cdr (assoc 'Type packet))))

(defun omnisharp--ignorable-packet? (packet)
  ;; todo what exactly are these? can they be ignored?
  (and (assq 'Arguments packet)
       (assq 'Command packet)))

(defun omnisharp--handle-event-packet (packet server-info)
  (-let (((&alist 'Type packet-type 'Event event-type) packet))
    (cond ((-contains? '("ProjectAdded" "ProjectChanged") event-type)
            (comment ignore these for now.))
          ((equal "TestMessage" event-type)
           (apply 'omnisharp--handle-test-message-event (list packet)))
          ((equal "started" event-type)
           (omnisharp--message "omnisharp: server has been started, check *omnisharp-log* for startup progress messages")
           (setcdr (assoc :started? server-info) t)))))

(defun omnisharp--handle-server-event (packet)
  "Takes an alist representing some kind of Packet, possibly a
ResponsePacket or an EventPacket, and processes it depending on
its type."
  (let ((server-info omnisharp--server-info))
    (cond ((omnisharp--ignorable-packet? packet)
           nil)

          ((omnisharp--response-packet? packet)
           (omnisharp--handle-server-response-packet packet server-info))

          ((omnisharp--log-packet? packet)
           (omnisharp--log-log-packet packet))

          ((omnisharp--event-packet? packet)
           (omnisharp--handle-event-packet packet server-info))

          (t (progn
               (omnisharp--log (format "<-- Received an unknown server packet: %s"
                                       (prin1-to-string packet))))))))

(defun omnisharp--remove-response-handler (server-info request-id)
  (setcdr (assoc :response-handlers server-info)
          (--remove (= (car it) request-id)
                    (-non-nil (cdr (assoc :response-handlers server-info))))))

(defun omnisharp--handle-server-response-packet (packet server-info)
  "Calls the appropriate response callback for the received packet"
  (-let (((&alist 'Message message
                  'Body body
                  'Command command
                  'Success success?
                  'Request_seq request-id) packet)
         ((&alist :response-handlers response-handlers) server-info))
    ;; try to find the matching response-handler
    (-if-let* ((id-and-handler (--first (= (car it) request-id)
                                        response-handlers)))
        (-let (((request-id . response-handler) id-and-handler))
          (condition-case maybe-error-data
              (progn
                (if (equal success? :json-false)
                    (omnisharp--log (format "<-- %s %s: request failed"
                                            request-id
                                            command
                                            (prin1-to-string body))))
                (omnisharp--remove-response-handler server-info request-id)
                (when (equal t success?)
                  (apply response-handler (list body))))
            (error
             (progn
               (let ((msg (format
                           (concat "\n"
                                   "omnisharp--handle-server-response-packet error: \n%s.\n\n"
                                   "Tried to handle this packet: \n%s\n\n"
                                   "This can mean an error in the handler function:\n%s\n\n")
                           (prin1-to-string maybe-error-data)
                           (prin1-to-string packet)
                           (prin1-to-string response-handler))))
                 (omnisharp--log msg)
                 (omnisharp--remove-response-handler server-info request-id)
                 (message msg))))))

      (omnisharp--log (format "<-- %s %s: Warning: internal error - response has no handler: %s"
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
      (let ((previous-text-marker (save-excursion
                                    (goto-char (process-mark process))
                                    (point))))
        ;; Insert the text, advancing the process marker.
        (goto-char (buffer-end 1))
        (insert message-part)
        ;; Return previous pending lines only when the latest line
        ;; is complete. Might be slightly slower but easier to
        ;; implement
        (when (omnisharp--marker-at-full-line? (point))
          (set-marker (process-mark process) (point))
          ;; get the start of the last inserted line
          (goto-char previous-text-marker)
          (beginning-of-line)
          (let ((text (s-lines (buffer-substring-no-properties
                                (point)
                                (process-mark process))))
                (trim-bom (lambda (s) (string-remove-prefix "\ufeff" (string-remove-prefix "\ufeff" s)))))
            ;; don't store messages in the process buffer unless
            ;; debugging, as they can slow emacs down when they pile
            ;; up
            (when (not omnisharp-debug) (erase-buffer))
            (-map trim-bom (--filter (not (s-blank? it)) text))))))))

(defun omnisharp--attempt-to-start-server-for-buffer ()
  "Checks if the server for the project of the buffer is running
and attempts to start it if it is not."

  (unless (or (omnisharp--buffer-contains-metadata)
              (not (buffer-file-name)))
    (let* ((project-root (omnisharp--project-root))
           (server-project-root (if omnisharp--server-info (cdr (assoc :project-root omnisharp--server-info)) nil))
           (filename (buffer-file-name))
           (filename-in-scope (and server-project-root
                                   (f-same-p (f-common-parent (list filename server-project-root))
                                             server-project-root))))
      (cond ((and (not server-project-root) project-root)
             (omnisharp--do-server-start project-root))

            ((and (not server-project-root) (not project-root))
             (message (concat "omnisharp: no project root could be found to start omnisharp server for this buffer automatically"))
             (message "omnisharp: start the server manually with M-x omnisharp-start-omnisharp-server or make sure project root is discoverable by projectile"))

            ((and server-project-root (not filename-in-scope))
             (message (format (concat "omnisharp: buffer will not be managed by omnisharp: "
                                      "%s is outside the root directory of the project loaded on the "
                                      "current OmniSharp server: %s")
                              filename
                              server-project-root)))))))

(provide 'omnisharp-server-management)
