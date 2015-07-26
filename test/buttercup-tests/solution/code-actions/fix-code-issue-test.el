(describe "Fix code issue"
  (before-each (ot--open-the-minimal-solution-source-file "MyClass.cs"))
  (it "can use the 'var' keyword"
    (ot--buffer-contents-and-point-at-$
     "public class Class1"
     "{"
     "    public void Whatever()"
     "    {"
     "        int$ i = 1;"
     "    }"
     "}")
    (spy-on 'omnisharp--ido-completing-read :and-call-fake
            (lambda (&rest args) "Use 'var' keyword"))
    (omnisharp--wait-until-request-completed
     (omnisharp-run-code-action-refactoring))
    (ot--buffer-should-contain)
    (ot--point-should-be-on-a-line-containing "var i = 1;")))
