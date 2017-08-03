;; License: GNU General Public License version 3, or (at your option) any later version

(describe "Format on keystroke"

  (before-each
    (ot--open-the-minimal-project-source-file "KeystrokeTest.cs"))

  (it "formats on pressing semicolon"
    (ot--buffer-contents-and-point-at-$
     "public class KeystrokeTest"
     "{"
     "    public KeystrokeTest()"
     "    {"
     "   var    i  =1$"
     "    }")

    (omnisharp-format-on-keystroke ";")
    (ot--buffer-should-contain
     "public class KeystrokeTest"
     "{"
     "    public KeystrokeTest()"
     "    {"
     "        var i = 1;"
     "    }"))

  (it "formats on pressing closing brace"
    (ot--buffer-contents-and-point-at-$
     "public class KeystrokeTest"
     "{"
     "    public KeystrokeTest()"
     "      {"
     "   var    i  =1;$")

    (omnisharp-format-on-keystroke "}")
    (ot--buffer-should-contain
     "public class KeystrokeTest"
     "{"
     "    public KeystrokeTest()"
     "    {"
     "        var i = 1;"
     "    }")))

