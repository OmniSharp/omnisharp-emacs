;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Find implementations with ido"
  (before-each
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs"))

  (it "navigates to the only implementation when only one found"
    (ot--buffer-contents-and-point-at-$
     "public interface IInter$face {}"
     "public class SomeClass : IInterface {}")

    (ot--evaluate-and-wait-for-server-response "(omnisharp-find-implementations-with-ido)")
    (ot--point-should-be-on-a-line-containing "public class SomeClass : IInterface {}"))

  (it "lets the user choose one with ido when more than one found"
    (ot--buffer-contents-and-point-at-$
     "public interface IInter$face {}"
     "public class SomeClass : IInterface {}"
     "public class SomeClass2 : IInterface {}")

    (ot--keyboard-input
     (ot--meta-x-command "omnisharp-find-implementations-with-ido")
     ;; choose the first item
     (ot--press-key "RET"))

    (ot--point-should-be-on-a-line-containing "public class SomeClass : IInterface {}")))
