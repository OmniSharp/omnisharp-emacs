(describe "Go to definition"
  (it "goes to definition in the same file"
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class Target {}"
     "    public class JumpSite {"
     "        Target$ foo; // go to definition from here"
     "    }"
     "}")

    (omnisharp--wait-until-request-completed (omnisharp-go-to-definition))
    (ot--point-should-be-on-a-line-containing "public class Target {}"))


  (it "goes to a member defined in another file using another window"
    ;; We have to let the server know the contents of the files before
    ;; doing anything, otherwise the contents might not be what they
    ;; are on disk
    (ot--open-the-minimal-project-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClass"
     "    {"
     "        public MyClass ()"
     "        {"
     "        }$"
     "    }"
     "}")

    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClassContainer"
     "    {"
     "        public $MyClass foo;"
     "    }"
     "}")
    (omnisharp--wait-until-request-completed (omnisharp-go-to-definition-other-window))

    (ot--there-should-be-a-window-editing-the-file "MyClassContainer.cs")
    (ot--there-should-be-a-window-editing-the-file "MyClass.cs")

    (ot--switch-to-the-window-in-the-buffer "MyClass.cs")
    (ot--point-should-be-on-a-line-containing "public class MyClass")))

