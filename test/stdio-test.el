(omnisharp--send-command-to-server omnisharp--server-info
                                   "checkreadystatus"
                                   nil)

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
  ;; 2. Message 1 part b & message 2
  (with-test-omnisharp-roslyn-process
   process
   (progn
     (omnisharp--read-lines-from-process-output process "{Message start")
     (should (equal
              '("{Message start, message end}"
                "{Second message}")
              (omnisharp--read-lines-from-process-output
               process
               ", message end}\n{Second message}\n")))))

  ;; 1. Full message with partial message
  ;; 2. The rest of the partial message.
  ;; The first full message will be returned with the first call, and
  ;; the partial message will be returned after the second call.
  (with-test-omnisharp-roslyn-process
   process
   (progn
     (omnisharp--read-lines-from-process-output process "{First message}\n{Second message start")
     (should (equal
              '("{Second message start, second message end}")
              (omnisharp--read-lines-from-process-output
               process
               ", second message end}\n"))))))
