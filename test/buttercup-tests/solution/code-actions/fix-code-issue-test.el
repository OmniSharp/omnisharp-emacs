;; License: GNU General Public License version 3, or (at your option) any later version
;;
;; Test a few different kinds of code actions to see we can support
;; each one in a sensible manner.
(describe "Fix code issue"
  (before-each (ot--open-the-minimal-project-source-file "MyClass.cs"))
  (it "can act on a simple part of the buffer (using System;)"
    (ot--buffer-contents-and-point-at-$
     "public class Class1"
     "{"
     "    public void Whatever()"
     "    {"
     "        Gu$id.NewGuid();"
     "    }"
     "}")
    (ot--answer-omnisharp--completing-read-with (lambda (choices) "using System;"))
    (omnisharp--wait-until-request-completed (omnisharp-run-code-action-refactoring))
    (ot--buffer-should-contain "using System;"))

  (it "can operate on the current region (Extract method)"
    (ot--buffer-contents-and-region
     "public class Class1"
     "{"
     "    public void Whatever()"
     "    {"
     "        (region-starts-here)int$ i = 1;"
     "        int i2 = 2;(region-ends-here)"
     "    }"
     "}")

    (ot--answer-omnisharp--completing-read-with
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
    ;; 2016-05-09 I cannot create a new type. The server doesn't support this yet.
    ;; https://github.com/OmniSharp/omnisharp-roslyn/commit/63c9eacf2f145ef20b642b6b11431f38e22bb99a#diff-8ff9938e7aa9073d7e49d52e84bacdaaR162
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
    (ot--answer-omnisharp--completing-read-with
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
