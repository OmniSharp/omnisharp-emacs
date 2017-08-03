
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


(defmacro with-test-omnisharp-roslyn-process (process-symbol &rest test-forms)
  `(unwind-protect
       (progn
         (let ((p (start-process "mock-omnisharp-roslyn-test-process"
                                 "mock-omnisharp-roslyn-test-process" ; buffer name
                                 "cat")))
           (with-current-buffer "mock-omnisharp-roslyn-test-process"
             (erase-buffer))
           ;; allow using the process outside this macro
           (setq ,process-symbol p)
           ,@test-forms))
     (kill-process "mock-omnisharp-roslyn-test-process")))

(ert-deftest omnisharp--read-lines-from-process-output-test ()
  ;; A single message in its entirety. Should return the message.
  (with-test-omnisharp-roslyn-process
   process
   (should (equal '("{Some: Json}")
                  (omnisharp--read-lines-from-process-output
                   process
                   ;; fake json does not confuse emacs syntax
                   ;; highlighting when editing
                   "{Some: Json}\n"))))

  ;; Partial message. Should return nothing. The server will send
  ;; another message, which will include the rest of this message, and
  ;; the next call will return this line and any new lines added in
  ;; that call.
  (with-test-omnisharp-roslyn-process
   process
   (should (equal '()
                  (omnisharp--read-lines-from-process-output
                   process
                   ;; notice no newline at the end! This means the
                   ;; message is divided into more parts
                   "{This message would continue in the next part..."))))

  ;; A response message arriving in two parts. Should return the full
  ;; line and the next call should return the partial line and any new
  ;; lines added in that call. So the messages arriving are:
  ;; 1. Message start
  ;; 2. Message end
  (with-test-omnisharp-roslyn-process
   process
   (progn
     (omnisharp--read-lines-from-process-output
      process
      "{Message start")
     (should (equal
              '("{Message start, and message end}")
              (omnisharp--read-lines-from-process-output
               process
               ", and message end}\n")))))

  ;; Two lines arriving like this:
  ;; 1. Message 1 part a
  ;; 2. Message 1 part b & message 2 and 3
  (with-test-omnisharp-roslyn-process
   process
   (progn
     (omnisharp--read-lines-from-process-output process "{Message start")
     (should (equal
              '("{Message start, message end}"
                "{Second message}"
                "{Third message}")
              (omnisharp--read-lines-from-process-output
               process
               ", message end}\n{Second message}\n{Third message}\n")))))

  ;; 1. Full messages with a partial message
  ;; 2. The rest of the partial message.
  ;; All messages will be returned after the second call.
  (with-test-omnisharp-roslyn-process
   process
   (progn
     (omnisharp--read-lines-from-process-output
      process "{First message}\n{Second message}\n{Third message start")
     (should (equal
              '("{First message}"
                "{Second message}"
                "{Third message start, third message end}")
              (omnisharp--read-lines-from-process-output
               process
               ", third message end}\n")))

     ;; a new message should not repeat any previous ones
     (should (equal
              '("{Fourth message}")
              (omnisharp--read-lines-from-process-output
               process
               "{Fourth message}\n"))))))

(ert-deftest omnisharp--handle-server-response-packet ()
  ;; should call response-handler when a response with a matching
  ;; request id is received
  (let* ((response-body "lalala")
         (response `((Success . t)
                     (Message . "message")
                     (Body . ,response-body)
                     (Command . "getfoodata")
                     (Request_seq . 1)))
         (server-info
          `((:process . nil)
            (:request-id . 1)
            (:response-handlers . ((1 . (lambda (body)
                                          (should (equal body ,response-body)))))))))

    (omnisharp--handle-server-response-packet response server-info)

    ;; should have removed handlers for request-id 1 in server-info
    (should (--none? (= (car it) 1)
                     (cdr (assoc :response-handlers server-info)))))

  ;; should not call handler when a response for another request is
  ;; received (when there is no matching handler)
  (let* ((response `((Success . t)
                     (Message . "message")
                     (Body . "not used")
                     (Command . "getfoodata")
                     (Request_seq . 1)))
         (server-info
          `((:process . nil)
            (:request-id . 1)
            (:response-handlers
             . ((2 . (lambda (body)
                       ;; just fail
                       (should
                        (equal nil
                               "error: should not have been called")))))))))

    (should (equal nil
                   (omnisharp--handle-server-response-packet response server-info)))))
