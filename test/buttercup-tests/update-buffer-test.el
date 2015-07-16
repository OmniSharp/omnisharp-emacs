(describe "Update buffer"
  (before-each
    (ot--open-the-minimal-solution-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "namespace foo {
        public class $Test {}
      }"))

  ;; no easy way to verify the server has done its thing
  (it "does not crash when called"
    (omnisharp--update-buffer))

  ;; This is actually a test for omnisharp--remove-response-handler,
  ;; sort of. But sue me! :D
  (it "cleans up its response handler after it's done"
    (let ((request-id (omnisharp--update-buffer)))
      (expect (not (omnisharp--handler-exists-for-request request-id))))))
