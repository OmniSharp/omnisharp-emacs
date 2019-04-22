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


(describe "omnisharp-solution-errors"
  (it "lists solution errors/warnings in a separate buffer"
    (ot--open-the-minimal-project-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClass"
     "    {"
     "        error-trigger$"
     "    }"
     "}")

    (omnisharp--wait-until-request-completed
     (omnisharp-solution-errors))

    (ot--switch-to-the-window-in-the-buffer "*omnisharp-solution-errors*")

    (ot--buffer-should-contain "MyClass.cs(6,14): error CS1519: Invalid token '-' in class, struct, or interface member declaration")
    (ot--buffer-should-contain "MyClass.cs(7,5): error CS1519: Invalid token '}' in class, struct, or interface member declaration")
    (ot--buffer-should-contain "omnisharp-solution-errors: finished")))
