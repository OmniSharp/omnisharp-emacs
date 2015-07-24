(describe "Current type information"

  (before-each
    (ot--open-the-minimal-solution-source-file "MyClassContainer.cs")
    (spy-on 'message :and-call-through))

  (it "lists usages of the symbol under point"
    (ot--buffer-contents-and-point-at-$
     "namespace minimal
      {
          public class Tar$get {}
      }")

    (ot--evaluate-and-wait-for-server-response "(omnisharp-current-type-information)")
    (expect 'message :to-have-been-called-with "minimal.Target")))
