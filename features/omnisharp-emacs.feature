Feature: Fix usings
  In order to code without having to worry about using statements
  As a user
  I want to automatically insert missing using statements based on the symbols in the current file

  Background:
    # TODO start omnisharp server somehow
    Given I open temp file "some-file.cs"

  Scenario: A single import is added automatically
    When I insert:
      """
      public class Awesome {
          StringWriter writer;
      }
      """
    And I evaluate the command "(omnisharp-fix-usings)"
    Then I should see, ignoring line endings:
      """
      using System.IO;

      public class Awesome {
          StringWriter writer;
      }
      """

  Scenario: Multiple imports let the user choose the import they want manually
    When I insert:
      """
      namespace mika {
          public class test {
              class1 classOne;
          }
      }

      namespace ns1
      {
          public class class1{}
      }

      namespace ns2
      {
          public class class1{}
      }
      """
    And I evaluate the command "(omnisharp-fix-usings)"
    When I switch to the existing buffer "* OmniSharp : Ambiguous unresolved symbols *"
    Then I should see, ignoring line endings:
      """
      These results are ambiguous. You can run
      (omnisharp-run-code-action-refactoring) when point is on them to see
      options for fixing them.
      """
