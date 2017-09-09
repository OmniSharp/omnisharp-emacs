;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Current type information"
  (it "lists usages of the symbol under point"
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (spy-on 'omnisharp--message-at-point nil)
    (ot--buffer-contents-and-point-at-$
     "namespace minimal"
     "{"
     "    public class Tar$get {}"
     "}")

    (omnisharp--wait-until-request-completed (omnisharp-current-type-information))
    (expect 'omnisharp--message-at-point :to-have-been-called-with "minimal.Target")))
