(describe "Navigate to current file member"
  (before-each
    (ot--open-the-minimal-solution-source-file "MyClassContainer.cs"))

  (it "moves point to selected type"
    (ot--buffer-contents-and-point-at-$
     "using System;

      namespace minimal
      {
          public class Class_One {}
          // point location does not matter
          public class $Class_Two {
              public int Foo = 0;
          }
          public class Class_Three {}
      }")

    ;; automatically select the first candidate given to
    ;; omnisharp--choose-quickfix-ido.
    (spy-on 'omnisharp--choose-quickfix-ido :and-call-fake
            (lambda (quickfixes) (-first-item quickfixes)))

    (omnisharp--wait-until-request-completed
     (omnisharp-navigate-to-current-file-member))

    (ot--point-should-be-on-a-line-containing "public class Class_One {}")))
