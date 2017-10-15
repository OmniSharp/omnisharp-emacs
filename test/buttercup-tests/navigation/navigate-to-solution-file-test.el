;; License: GNU General Public License version 3, or (at your option) any later version
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


(describe "Navigate to solution file"
  (it "asks the user for a file in the current solution to navigate to and goes there"
    (-when-let (buffer (get-buffer "MyClass.cs"))
      (kill-buffer buffer))

    (ot--answer-omnisharp--completing-read-with
     (lambda (choices)
       (--first (s-contains? "MyClass.cs" it)
                choices)))
    (omnisharp--wait-until-request-completed
     (omnisharp-navigate-to-solution-file))
    (ot--there-should-be-a-window-editing-the-file "MyClass.cs")))
