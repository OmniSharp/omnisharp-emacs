;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Imenu integration"
  (it "builds an index of members in the current file"
    (ot--open-the-minimal-project-source-file "ImenuTest.cs")
    (ot--buffer-contents-and-point-at-$
     "namespace minimal"
     "{"
     "    public class Tar$get {"
     "        public int Zero = 0;"
     "        public int One  = 2; // haha"
     "    }"
     "}")

    (imenu (-first (-lambda ((name . location-marker))
                            (s-equals? name "Zero"))
                   (omnisharp-imenu-create-index)))
    (ot--point-should-be-on-a-line-containing "public int Zero = 0;")))
