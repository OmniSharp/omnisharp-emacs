;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Find usages with ido"
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

    (ot--answer-omnisharp--completing-read-with #'-first-item)

    (omnisharp--wait-until-request-completed (omnisharp-find-usages-with-ido))

    (ot--point-should-be-on-a-line-containing "public class Target {}")))
