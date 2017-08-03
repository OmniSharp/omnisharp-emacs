;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Rename symbol"
  (it "renames a symbol referenced only in a single file"
    (ot--open-the-minimal-project-source-file "RenameFileTest.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class OldClass {}"
     "    public class OtherClass {"
     "        Old$Class foo; // rename here"
     "    }"
     "}")

    ;; rename to:
    (spy-on 'read-string :and-return-value "OldClassChanged")
    (omnisharp-rename)
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
    (ot--open-the-minimal-project-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal$"
     "{"
     "    public class MyClass {}"
     "}")

    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClassContainer"
     "    {"
     "        public My$Class foo;"
     "    }"
     "}")

    (spy-on 'read-string :and-return-value "MyClass2")
    (omnisharp-rename)

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
