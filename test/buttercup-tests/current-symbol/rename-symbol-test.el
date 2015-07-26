(describe "Rename symbol"
  (it "renames a symbol referenced only in a single file"
    (ot--open-the-minimal-solution-source-file "RenameFileTest.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class OldClass {}"
     "    public class OtherClass {"
     "        Old$Class foo; // rename here"
     "    }"
     "}")
    (ot--keyboard-input
     (ot--press-key "M-x")
     (ot--type "omnisharp-rename")
     (ot--press-key "RET")
     (ot--type "Changed")
     (ot--press-key "RET"))

    (ot--point-should-be-on-line-number 6)

    (ot--i-should-see
     "using System;"
     "namespace minimal"
     "{"
     "    public class OldClassChanged {}"
     "    public class OtherClass {"
     "        OldClassChanged foo; // rename here"
     "    }"
     "}"))

  (it "renames a symbol referenced in multiple files"
    (ot--open-the-minimal-solution-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal$"
     "{"
     "    public class MyClass {}"
     "}")

    (ot--open-the-minimal-solution-source-file "MyClassContainer.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClassContainer"
     "    {"
     "        public My$Class foo;"
     "    }"
     "}")

    (ot--keyboard-input
     (ot--press-key "M-x")
     (ot--type "omnisharp-rename")
     (ot--press-key "RET")
     ;; The new name will be MyClass2
     (ot--type "2")
     (ot--press-key "RET"))

    (ot--buffer-should-contain
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClassContainer"
     "    {"
     "        public MyClass2 foo;"
     "    }"
     "}")

    (ot--switch-to-buffer "MyClass.cs")
    (ot--buffer-should-contain
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClass2 {}"
     "}")))
