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


(describe "Navigate to region"
  (it "asks the user for a region in the current file to navigate to and goes there"

    (ot--open-the-minimal-project-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "namespace Test {"
     "    #region awesome"
     "    public class Awesome {}"
     "    #endregion awesome"
     "$"
     "}")
    (ot--answer-omnisharp--completing-read-with
     (lambda (choices)
       (--first (s-contains? "awesome" it)
                choices)))
    (omnisharp--wait-until-request-completed
     (omnisharp-navigate-to-region))
    (ot--point-should-be-on-a-line-containing "#region awesome")))
