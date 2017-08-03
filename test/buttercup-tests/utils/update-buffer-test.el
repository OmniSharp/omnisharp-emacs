;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Update buffer"
  (before-each
    (ot--open-the-minimal-project-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "//contents don't matter$"))

  (it "does not crash when called"
    ;; no easy way to verify the server has done its thing
    (omnisharp--update-buffer))

  ;; This is actually a test for omnisharp--remove-response-handler,
  ;; sort of. But sue me! :D
  (it "cleans up its response handler after it's done"
    (let ((request-id (omnisharp--update-buffer)))
      (expect (not (omnisharp--handler-exists-for-request request-id))))))
