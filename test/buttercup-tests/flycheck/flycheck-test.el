(describe "Flycheck integration"
  (it "integrates flycheck with omnisharp"
    (ot--open-the-minimal-solution-source-file "FlycheckTest.cs")
    (ot--buffer-contents-and-point-at-$
     "$public class OtherClass {
         DoesNotExist foo;
       }")

    (flycheck-mode)
    (ot--wait-for-seconds 1)

    (flycheck-next-error)
    (expect (thing-at-point 'word) :to-equal "DoesNotExist")

    (flycheck-hide-error-buffer) ;; makes this test repeatable 
    (flycheck-list-errors)
    (ot--switch-to-the-window-in-the-buffer "*Flycheck errors*")
    (ot--point-should-be-on-a-line-containing "error")
    (ot--point-should-be-on-a-line-containing
     (concat "The type or namespace name 'DoesNotExist' could not be found"
     " (are you missing a using directive or an assembly reference?)"))))


