;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Code format"
  (before-each
    (ot--open-the-minimal-project-source-file "CodeFormatTest.cs"))

  (it "can format the entire buffer contents"
    (ot--buffer-contents-and-point-at-$
     "public class CodeFormatTest {                         $     }")
    (omnisharp-code-format-entire-file)
    (ot--buffer-should-contain "public class CodeFormatTest { }"))

  (it "can format the current region"
    (ot--buffer-contents-and-region
     "public class Foo{"
     "    public int Lol (region-starts-here){get;  $    set;}(region-ends-here)"
     "}")
    (omnisharp-code-format-region)
    (ot--buffer-should-contain
     "public class Foo{"
     "    public int Lol { get; set; }"
     "}")))
