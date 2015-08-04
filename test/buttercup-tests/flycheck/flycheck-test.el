(describe "Flycheck integration"
  (it "integrates flycheck with omnisharp"
    (ot--open-the-minimal-solution-source-file "FlycheckTest.cs")
    (ot--buffer-contents-and-point-at-$
     "$public class MyClass {
         DoesNotExist foo;
       }")

    (flycheck-mode)
    (flycheck-list-errors)
    ;; (ot--wait-for (lambda() (print "waiting")(get-buffer "*Flycheck errors*")) 5)
    (ot--wait-for (lambda() (and (boundp 'flycheck-current-errors)
                                 (> (length flycheck-current-errors) 0))) 10)


    (ot--switch-to-the-window-in-the-buffer "*Flycheck errors*")
    (ot--point-should-be-on-a-line-containing
     (concat "The type or namespace name 'DoesNotExist' could not be found"
             " (are you missing a using directive or an assembly reference?)"))

    (ot--switch-to-the-window-in-the-buffer "FlycheckTest.cs")
    (flycheck-next-error)
    (expect (thing-at-point 'word) :to-equal "DoesNotExist")))
