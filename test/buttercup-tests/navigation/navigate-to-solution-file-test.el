(describe "Navigate to solution file"
  (it "asks the user for a file to navigate to and goes there"
    (-when-let (buffer (get-buffer "MyClass.cs"))
      (kill-buffer buffer))

    (spy-on 'omnisharp--choose-quickfix-ido :and-call-fake
            (lambda (quickfixes)
              (--first (s-contains? "MyClass.cs" (cdr (assoc 'Text it)))
                       quickfixes)))
    (omnisharp--wait-until-request-completed
     (omnisharp-navigate-to-solution-file))
    (ot--there-should-be-a-window-editing-the-file "MyClass.cs")))
