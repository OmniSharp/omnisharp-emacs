;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
