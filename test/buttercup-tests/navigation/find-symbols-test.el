(describe "Navigate to solution member (find symbols)"
  (before-each
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs"))

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
    (ot--answer-omnisharp--ido-completing-read-with
     (lambda (choices)
       (--first (s-contains? "MyClassContainer" it)
                choices)))

    (omnisharp--wait-until-request-completed
     (omnisharp-navigate-to-solution-member))

    (ot--point-should-be-on-a-line-containing "public class MyClassContainer")
    (ot--i-should-be-in-buffer-name "MyClassContainer.cs")))
