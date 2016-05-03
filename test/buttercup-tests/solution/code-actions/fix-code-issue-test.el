;; Test a few different kinds of code actions to see we can support
;; each one in a sensible manner.
(describe "Fix code issue"
  (before-each (ot--open-the-minimal-project-source-file "MyClass.cs"))
  (xit "can replace a simple part of the buffer (Use 'var' keyword)"
    (ot--buffer-contents-and-point-at-$
     "public class Class1"
     "{"
     "    public void Whatever()"
     "    {"
     "        int$ i = 1;"
     "    }"
     "}")
    (ot--answer-omnisharp--ido-completing-read-with (lambda (choices) "Use 'var' keyword"))
    (omnisharp--wait-until-request-completed (omnisharp-run-code-action-refactoring))
    (ot--point-should-be-on-a-line-containing "var i = 1;"))

  (xit "can operate on the current region (Extract method)"
    (ot--buffer-contents-and-region
     "public class Class1"
     "{"
     "    public void Whatever()"
     "    {"
     "        (region-starts-here)int$ i = 1;"
     "        int i2 = 2;(region-ends-here)"
     "    }"
     "}")

    (ot--answer-omnisharp--ido-completing-read-with
     (lambda (choices)
       (--first (s-contains? "Extract Method" it)
                choices)))

    (omnisharp--wait-until-request-completed (omnisharp-run-code-action-refactoring))

    (ot--buffer-should-contain
     "public class Class1"
     "{"
     "    public void Whatever()"
     "    {"
     "        NewMethod();"
     "    }"
     ""
     "    private static void NewMethod()"
     "    {"
     "        int i = 1;"
     "        int i2 = 2;"
     "    }"
     "}"))

  (xit "can create new files (Generate class in new file)"
    (ot--delete-the-minimal-project-source-file "MyNewClass.cs")
    (ot--buffer-contents-and-point-at-$
     "namespace MyNamespace"
     "{"
     "    public class Class1"
     "    {"
     "        public void Whatever()"
     "        {"
     "            MyNew$Class.DoSomething();"
     "        }"
     "    }"
     "}")
    (ot--answer-omnisharp--ido-completing-read-with
     (lambda (choices)
       (--first (equal it
                       "Generate class for 'MyNewClass' in 'MyNamespace' (in new file)")
                choices)))
    (omnisharp--wait-until-request-completed (omnisharp-run-code-action-refactoring))
    (ot--there-should-be-a-window-editing-the-file "MyNewClass.cs")
    (ot--switch-to-buffer "MyNewClass.cs")
    (ot--buffer-should-contain
     "namespace MyNamespace"
     "{"
     "    internal class MyNewClass"
     "    {"
     "    }"
     "}")))
