;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Auto-complete using the company interface"

  (before-each
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (omnisharp-mode t)
    (require 'company)
    (company-mode t)
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

    (company-complete)

    (ot--buffer-should-contain
     "namespace Test {
         public class Awesome {
             StringWriter writer;
             public Awesome() {
                 writer
             }
         }
      }"))

  (it "completes after dot"
    (ot--buffer-contents-and-point-at-$
     "using System;
      namespace Test {
          public class Awesome {
              public Awesome() {
                  Console.$
              }
          }
      }")

    (expect (ot--get-completions) :to-contain "WriteLine()"))

  (it "completes a function that has parameters using snippets"
    (ot--buffer-contents-and-point-at-$
     "namespace Test {
         public class Awesome {
             public Awesome() {
                 object.Equa$
             }
         }
     }")

    (company-complete)

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

    (ot--buffer-should-contain "object.Equals(this, new object())")))
