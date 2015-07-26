(describe "Find usages"
  (before-each
    (ot--open-the-minimal-solution-source-file "MyClassContainer.cs"))

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
    (ot--wait-for-seconds 1)
    (ot--switch-to-buffer "* OmniSharp : Usages *")
    (ot--i-should-see "Usages in the current solution:")
    (ot--i-should-see "public class Target {}")))
