(describe "Navigate to solution member (find symbols)"
  (before-each
    (ot--open-the-minimal-solution-source-file "MyClassContainer.cs"))

  (it "moves point to selected type"
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClassContainer"
     "    {"
     "        $public MyClass foo;"
     "    }"
     "}")
    ;; automatically select the first candidate given to
    ;; omnisharp--choose-quickfix-ido.
    (spy-on 'omnisharp--choose-quickfix-ido :and-call-fake
            (lambda (quickfixes)
              (--first (s-contains? "MyClassContainer" (cdr (assoc 'Text it)))
                       quickfixes)))

    (omnisharp--wait-until-request-completed
     (omnisharp-navigate-to-solution-member))

    (ot--point-should-be-on-a-line-containing "public class MyClassContainer")
    (ot--i-should-be-in-buffer-name "MyClassContainer.cs")))
