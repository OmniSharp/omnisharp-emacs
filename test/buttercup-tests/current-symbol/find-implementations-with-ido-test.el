(describe "Find implementations with ido"
  (before-each
    (ot--open-the-minimal-solution-source-file "MyClassContainer.cs"))

  (it "navigates to the only implementation when only one found"
    (ot--buffer-contents-and-point-at-$
     "public class Base$Class {}"
     "public class SomeClass : BaseClass {}")

    (ot--evaluate-and-wait-for-server-response "(omnisharp-find-implementations-with-ido)")
    (ot--point-should-be-on-a-line-containing "public class SomeClass : BaseClass {}"))

  (it "lets the user choose one with ido when more than one found"
    (ot--buffer-contents-and-point-at-$
     "public class Base$Class {}"
     "public class SomeClass : BaseClass {}"
     "public class SomeClass2 : BaseClass {}")

    (ot--keyboard-input
     (ot--meta-x-command "omnisharp-find-implementations-with-ido")
     ;; choose the first item
     (ot--press-key "RET"))

    (ot--point-should-be-on-a-line-containing "public class SomeClass : BaseClass {}")))
