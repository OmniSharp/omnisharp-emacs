;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Find implementations with ido"
  (before-each
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (spy-on completing-read-function))

  (it "navigates to the only implementation when only one found"
    (ot--buffer-contents-and-point-at-$
     "public interface IInter$face {}"
     "public class SomeClass : IInterface {}")

    (ot--evaluate-and-wait-for-server-response "(omnisharp-find-implementations-with-ido)")
    (ot--point-should-be-on-a-line-containing "public class SomeClass : IInterface {}")
    (expect completing-read-function :not :to-have-been-called))

  (it "lets the user choose one with ido when more than one found"
    (ot--buffer-contents-and-point-at-$
     "public interface IInter$face {}"
     "public class SomeClass : IInterface {}"
     "public class SomeClass2 : IInterface {}")

    (ot--evaluate-and-wait-for-server-response "(omnisharp-find-implementations-with-ido)")
    (expect completing-read-function :to-have-been-called)))
