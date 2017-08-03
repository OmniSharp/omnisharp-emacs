;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Find usages"
  (before-each
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs"))

  (it "lists usages of the symbol under point"
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class Target {}"
     "    public class JumpSite {"
     "        Targ$et foo;"
     "    }"
     "}")

    (ot--evaluate-and-wait-for-server-response "(omnisharp-find-usages)")
    (ot--switch-to-buffer "* OmniSharp : Usages *")
    (ot--i-should-see "Usages in the current solution:")
    (ot--i-should-see "public class Target {}")))
