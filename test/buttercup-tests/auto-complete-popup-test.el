(describe "Auto-complete using the popup interface"
  (it "completes a member in the same file"
    (ot--open-the-minimal-solution-source-file "MyClassContainer.cs")
    (ot--set omnisharp--auto-complete-display-backend 'popup)
    (ot--buffer-contents-and-point-at-$
     "namespace Test {
          public class Awesome {
              StringWriter writer;
              public Awesome() {
                  wri$
              }
          }
      }")

    (ot--keyboard-input
     (ot--press-key "M-x")
     (ot--type "omnisharp-auto-complete")
     (ot--press-key "RET")
     ;; A pop-up.el menu is shown. Complete the first candidate.
     (ot--press-key "RET"))

    (ot--buffer-should-contain
     "namespace Test {
          public class Awesome {
              StringWriter writer;
              public Awesome() {
                  writer
              }
          }
      }")))
