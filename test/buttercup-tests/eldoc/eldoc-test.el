(describe "eldoc"
  (before-each
    (ot--open-the-minimal-project-source-file "EldocTest.cs"))

  (it "prints the symbol information at point"
      (ot--buffer-contents-and-point-at-$
       "public class EldocTest
        {
           public EldocTest()
           {
               System.Console.Write$Line
           }")

      (ot--press-key "<left>")
      ;; (view-echo-area-messages)
      ;; (goto-char (point-max))
      ;; (goto-char (point-min))
      ;; (forward-line (1- count))
      (expect (read-string) :to-equal "void Console.WriteLine()")))

