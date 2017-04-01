(describe "Current type information"
  (it "lists usages of the symbol under point"
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (spy-on 'message :and-call-through)
    (ot--buffer-contents-and-point-at-$
     "namespace minimal"
     "{"
     "    public class Tar$get {}"
     "}")

    (omnisharp--wait-until-request-completed (omnisharp-current-type-information))
    (expect 'message :to-have-been-called-with "minimal.Target")))
