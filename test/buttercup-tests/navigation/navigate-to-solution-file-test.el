;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Navigate to solution file"
  (it "asks the user for a file in the current solution to navigate to and goes there"
    (-when-let (buffer (get-buffer "MyClass.cs"))
      (kill-buffer buffer))

    (ot--answer-omnisharp--ido-completing-read-with
     (lambda (choices)
       (--first (s-contains? "MyClass.cs" it)
                choices)))
    (omnisharp--wait-until-request-completed
     (omnisharp-navigate-to-solution-file))
    (ot--there-should-be-a-window-editing-the-file "MyClass.cs")))
