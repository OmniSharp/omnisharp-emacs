(describe "Auto-complete using the company interface"

  (before-each
    (ot--open-the-minimal-solution-source-file "MyClassContainer.cs")
    (ot--keyboard-input
     (ot--meta-x-command "omnisharp-mode"))

    (require 'company)
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-omnisharp)))

  (it "completes a member in the same file"
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
     (ot--meta-x-command "company-complete"))

    (ot--buffer-should-contain
     "namespace Test {
          public class Awesome {
              StringWriter writer;
              public Awesome() {
                  writer
              }
          }
      }"))

  (it "completes a function that has parameters using snippets"
    (ot--buffer-contents-and-point-at-$
     "namespace Test {
          public class Awesome {
              public Awesome() {
                  object.Equa$
              }
          }
      }")

    (ot--keyboard-input
     (ot--meta-x-command "company-mode")
     (ot--meta-x-command "company-complete"))

    (ot--keyboard-input
     ;; will complete the current line to this:
     ;; object.Equals(object objA, object objB)
     ;;
     ;; Typing anything now will replace the first parameter
     (ot--type "this")
     (ot--press-key "TAB")
     ;; now fill the second parameter
     (ot--type "new object()")
     (ot--press-key "TAB"))

    (ot--buffer-should-contain
     "namespace Test {
          public class Awesome {
              public Awesome() {
                  object.Equals(this, new object())
              }
          }
      }")))
