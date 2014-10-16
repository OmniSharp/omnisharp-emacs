Feature: Fix usings
  In order to code without having to worry about using statements
  As a user
  I want to automatically insert missing using statements based on the symbols in the current file

  Background:
    # TODO start omnisharp server somehow
    Given I open temp file "some-file.cs"
    And I bind key "C-c i" to "omnisharp-fix-usings"

  Scenario: Insert missing namespace import
    When I insert:
      """
      public class Awesome {
          StringWriter writer;
      }
      """
    And I press "C-c i"
    Then I should see, ignoring line endings:
      """
      using System.IO;

      public class Awesome {
          StringWriter writer;
      }
      """
