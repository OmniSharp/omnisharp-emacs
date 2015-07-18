(describe "Update buffer"
  (before-each
    (condition-case error-symbol
        (progn
          (ot--open-the-minimal-solution-source-file "MyClass.cs")
          (ot--buffer-contents-and-point-at-$
           "//contents don't matter")
          (progn
            (with-current-buffer "*omnisharp-debug*"
              ;; If there is a problem with the server, it will show in
              ;; this trivial test case. Better view all logs to
              ;; inspect.
              (print "\n\n\n*omnisharp-debug* buffer contents:\n")
              (print (buffer-string)))
            (with-current-buffer "Omni-Server"
              (print "\n\n\nOmni-Server buffer contents:\n")
              (print (buffer-string)))))
      (error (progn
               (with-current-buffer "*omnisharp-debug*"
                 ;; If there is a problem with the server, it will show in
                 ;; this trivial test case. Better view all logs to
                 ;; inspect.
                 (print "\n\n\n*omnisharp-debug* buffer contents:\n")
                 (print (buffer-string)))
               (with-current-buffer "Omni-Server"
                 (print "\n\n\nOmni-Server buffer contents:\n")
                 (print (buffer-string)))))))

  (it "does not crash when called"
    ;; no easy way to verify the server has done its thing
    (omnisharp--update-buffer))

  ;; This is actually a test for omnisharp--remove-response-handler,
  ;; sort of. But sue me! :D
  (it "cleans up its response handler after it's done"
    (let ((request-id (omnisharp--update-buffer)))
      (expect (not (omnisharp--handler-exists-for-request request-id))))))
