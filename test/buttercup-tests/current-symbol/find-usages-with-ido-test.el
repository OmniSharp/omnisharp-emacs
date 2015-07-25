(describe "Find usages with ido"
  (before-each
    (ot--open-the-minimal-solution-source-file "MyClassContainer.cs"))

  (it "lists usages of the symbol under point"
    (ot--buffer-contents-and-point-at-$
     "using System;

      namespace minimal
      {
          public class Target {}
          public class JumpSite {
              Targ$et foo;
          }
      }")

    ;; automatically select the first candidate given to
    ;; omnisharp--choose-quickfix-ido. this could be done by
    ;; controlling ido with the keyboard like in other tests, but ido
    ;; is not easy to control programmatically.
    (spy-on 'omnisharp--choose-quickfix-ido :and-call-fake
            (lambda (quickfixes) (-first-item quickfixes)))

    (omnisharp--wait-until-request-completed (omnisharp-find-usages-with-ido))

    (ot--point-should-be-on-a-line-containing "public class Target {}")))
