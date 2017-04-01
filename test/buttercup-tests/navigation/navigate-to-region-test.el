(describe "Navigate to region"
  (it "asks the user for a region in the current file to navigate to and goes there"

    (ot--open-the-minimal-project-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "namespace Test {"
     "    #region awesome"
     "    public class Awesome {}"
     "    #endregion awesome"
     "$"
     "}")
    (ot--answer-omnisharp--ido-completing-read-with
     (lambda (choices)
       (--first (s-contains? "awesome" it)
                choices)))
    (omnisharp--wait-until-request-completed
     (omnisharp-navigate-to-region))
    (ot--point-should-be-on-a-line-containing "#region awesome")))
