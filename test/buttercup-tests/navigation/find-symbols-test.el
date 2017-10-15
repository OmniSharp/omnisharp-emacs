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


(describe "Navigate to solution member (find symbols)"
  (it "moves point to selected type"
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClassContainer"
     "    {"
     "        $public MyClass foo;"
     "    }"
     "}")
    ;; automatically select the first candidate given to
    ;; omnisharp--choose-quickfix-ido.
    (ot--answer-omnisharp--completing-read-with
     (lambda (choices)
       (--first (s-contains? "MyClassContainer" it)
                choices)))

    ;; should filter to "foo", defined in this class
    (spy-on 'omnisharp--read-string :and-return-value "MyClassC")

    (omnisharp--wait-until-request-completed (omnisharp-navigate-to-solution-member))

    (ot--point-should-be-on-a-line-containing "public class MyClassContainer")
    (ot--i-should-be-in-buffer-name "MyClassContainer.cs")))
