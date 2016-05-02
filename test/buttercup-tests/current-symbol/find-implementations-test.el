(describe "Find implementations"
  (before-each
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs"))

  (it "navigates to the only implementation when only one found"
    (ot--buffer-contents-and-point-at-$
     "public class Base$Class {}"
     "public class SomeClass : BaseClass {}")

    (ot--evaluate-and-wait-for-server-response "(omnisharp-find-implementations)")
    (ot--point-should-be-on-a-line-containing "public class SomeClass : BaseClass {}"))

  (it "shows a list of implementations when more than one found"
    (ot--buffer-contents-and-point-at-$
     "public class Base$Class {}"
     "public class SomeClass : BaseClass {}"
     "public class SomeClass2 : BaseClass {}")

    (ot--evaluate-and-wait-for-server-response "(omnisharp-find-implementations)")
    (ot--switch-to-buffer "* OmniSharp : Implementations *")
    (ot--i-should-see "public class SomeClass : BaseClass {}")
    (ot--i-should-see "public class SomeClass2 : BaseClass {}")))
