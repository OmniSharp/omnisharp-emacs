(describe "Navigate to region"
  (it "asks the user for a region in the current file to navigate to and goes there"

    (ot--open-the-minimal-solution-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "namespace Test {"
     "    #region awesome"
     "    public class Awesome {}"
     "    #endregion awesome"
     "$"
     "}")
    (spy-on 'omnisharp--choose-quickfix-ido :and-call-fake
            (lambda (quickfixes)
              (--first (s-contains? "awesome" (cdr (assoc 'Text it)))
                       quickfixes)))
    (omnisharp--wait-until-request-completed
     (omnisharp-navigate-to-region))
    (ot--point-should-be-on-a-line-containing "#region awesome")))
