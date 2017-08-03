;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Auto-complete using the popup interface"
  (before-each
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (ot--set omnisharp--auto-complete-display-backend 'popup))

  (after-each
    (require 'yasnippet)
    (yas-minor-mode nil))

  (it "completes a member in the same file"
    (ot--buffer-contents-and-point-at-$
     "namespace Test {"
     "    public class Awesome {"
     "        StringWriter writer;"
     "        public Awesome() {"
     "            wri$"
     "        }"
     "    }"
     "}")

    (ot--keyboard-input
     (ot--meta-x-command "omnisharp-auto-complete")
     ;; A pop-up.el menu is shown. Complete the first candidate.
     (ot--press-key "RET"))

    (ot--buffer-should-contain
     "namespace Test {"
     "    public class Awesome {"
     "        StringWriter writer;"
     "        public Awesome() {"
     "            writer"
     "        }"
     "    }"
     "}"))

  (it "when yasnippet is loaded, completes a function that has parameters using snippets"
    (yas-minor-mode)
    (ot--buffer-contents-and-point-at-$
     "namespace Test {"
     "    public class Awesome {"
     "        public Awesome() {"
     "            object.Equa$"
     "        }"
     "    }"
     "}")

    (ot--keyboard-input
     (ot--press-key "M-x")
     (ot--type "omnisharp-auto-complete")
     (ot--press-key "RET")
     ;; A pop-up.el menu is shown. Complete the first candidate.
     (ot--press-key "RET")

     ;; yasnippet will complete the current line to this:
     ;; object.Equals(object objA, object objB)
     ;;
     ;; Typing anything now will replace the first parameter
     (ot--type "this")
     (ot--press-key "TAB")
     ;; now fill the second parameter
     (ot--type "new object()")
     (ot--press-key "TAB"))

    (ot--buffer-should-contain "object.Equals(this, new object())")

    ;; if not done, other tests will fail due to "something something
    ;; yas overlay is active"
    (kill-buffer "MyClassContainer.cs")))

(describe "auto-complete's completion source"
  (it "provides valid completions as an auto-complete source"
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (ot--buffer-contents-and-point-at-$
     "namespace Test {"
     "    public class Awesome {"
     "        public Awesome() {"
     "            object.Equa$"
     "        }"
     "    }"
     "}")
    (expect
     (popup-item-value
      (-first-item
       (omnisharp--get-auto-complete-result-in-popup-format)))
     :to-be-truthy)))
