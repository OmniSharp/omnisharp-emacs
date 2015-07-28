(describe "Code format"
  (before-each
    (ot--open-the-minimal-solution-source-file "CodeFormatTest.cs"))

  (it "can format the entire buffer contents"
    (ot--buffer-contents-and-point-at-$
     "public class CodeFormatTest {                               }")
    (omnisharp-code-format-entire-file)
    (ot--buffer-should-contain "public class CodeFormatTest { }")))
