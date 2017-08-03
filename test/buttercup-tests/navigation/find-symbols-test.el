;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Navigate to solution member (find symbols)"
  (it "moves point to selected type"
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
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

    ;; should filter to "foo", defined in this class
    (spy-on 'omnisharp--read-string :and-return-value "MyClassC")

    (omnisharp--wait-until-request-completed (omnisharp-navigate-to-solution-member))

    (ot--point-should-be-on-a-line-containing "public class MyClassContainer")
    (ot--i-should-be-in-buffer-name "MyClassContainer.cs")))
