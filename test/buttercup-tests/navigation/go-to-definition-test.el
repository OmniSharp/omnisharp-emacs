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


(describe "Go to definition"
  (it "goes to definition in the same file"
    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class Target {}"
     "    public class JumpSite {"
     "        Target$ foo; // go to definition from here"
     "    }"
     "}")

    (omnisharp--wait-until-request-completed (omnisharp-go-to-definition))
    (ot--point-should-be-on-a-line-containing "public class Target {}"))


  (it "goes to a member defined in another file"
    ;; We have to let the server know the contents of the files before
    ;; doing anything, otherwise the contents might not be what they
    ;; are on disk
    (ot--open-the-minimal-project-source-file "MyClass.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClass"
     "    {"
     "        public MyClass ()"
     "        {"
     "        }$"
     "    }"
     "}")

    (ot--open-the-minimal-project-source-file "MyClassContainer.cs")
    (ot--buffer-contents-and-point-at-$
     "using System;"
     "namespace minimal"
     "{"
     "    public class MyClassContainer"
     "    {"
     "        public $MyClass foo;"
     "    }"
     "}")
    (omnisharp--wait-until-request-completed (omnisharp-go-to-definition-other-window))

    (ot--switch-to-the-window-in-the-buffer "MyClass.cs")
    (ot--point-should-be-on-a-line-containing "public class MyClass"))

  (it "goes to a member defined in metadata"
      (ot--open-the-minimal-project-source-file "MyClass.cs")
      (ot--buffer-contents-and-point-at-$
       "using System;"
       "namespace minimal"
       "{"
       "    public class MyClass"
       "    {"
       "        public st$ring foo;"
       "    }"
       "}")

      (omnisharp-go-to-definition)
      (ot--wait-until-all-requests-completed)

      ;; TODO: for some reason I need to set current buffer from window list
      ;;       with with-current-buffer..
      (with-current-buffer (car (mapcar #'window-buffer (window-list)))
        (ot--i-should-be-in-buffer-name "*omnisharp-metadata:MinimalProject:netstandard:System.String*")
        (ot--point-should-be-on-a-line-containing "public sealed class String"))))
