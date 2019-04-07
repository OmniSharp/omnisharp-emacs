;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Flycheck integration"
  (it "integrates flycheck with omnisharp"
    (ot--open-the-minimal-project-source-file "FlycheckTest.cs")
    (ot--buffer-contents-and-point-at-$
     "$public class MyClass {
         DoesNotExist foo;
       }")

    ;; required so the correct checker can be found by flycheck
    (omnisharp-mode t)
    (flycheck-mode)

    ;; make all flycheck calls wait until the server response has been handled
    (let ((original-function (symbol-function 'omnisharp--flycheck-start)))
      (spy-on 'omnisharp--flycheck-start :and-call-fake
              (lambda (&rest args)
                (omnisharp--wait-until-request-completed
                 (apply original-function args)))))

    (flycheck-buffer)
    ;; when running this test from the command line, this function is
    ;; not called if a spy is not defined for it
    (expect 'omnisharp--flycheck-start :to-have-been-called)

    ;; open error list and verify its contents
    (flycheck-list-errors)

    (ot--switch-to-the-window-in-the-buffer "*Flycheck errors* for buffer FlycheckTest.cs")
    (ot--point-should-be-on-a-line-containing
     (concat "The type or namespace name 'DoesNotExist' could not be found"
             " (are you missing a using directive or an assembly reference?)"))

    ;; navigating to the next error will take the user to the correct place
    (ot--switch-to-buffer "FlycheckTest.cs")
    (flycheck-next-error)
    (expect (thing-at-point 'word) :to-equal "DoesNotExist")))
