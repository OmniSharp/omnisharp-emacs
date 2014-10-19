Feature: Auto-complete
  In order to code effectively
  As a user
  I want to to complete valid symbols for the current point location

  Background:
    Given I open temp file "some-file.cs"
    Given The buffer is empty
    Given I set "omnisharp--auto-complete-display-backend" to "'popup"

  Scenario: Auto-complete a member with the popup interface
    When My buffer contents are, and my point is at $:
      """
      namespace Test {
          public class Awesome {
              StringWriter writer;
              public Awesome() {
                  wri$
              }
          }
      }
      """
    Given I start an action chain
    And I press "M-x"
    And I type "omnisharp-auto-complete"
    # Finish with M-x
    And I press "RET"
    # A pop-up.el menu is shown. Complete the first candidate.
    And I press "RET"
    And I execute the action chain

    Then I should see, ignoring line endings:
      """
      namespace Test {
          public class Awesome {
              StringWriter writer;
              public Awesome() {
                  writer
              }
          }
      }
      """

  Scenario: Auto-completing a member from another namespace should insert the member and the required using statement
    When My buffer contents are, and my point is at $:
      """
      namespace Test {
          public class Awesome {
              threadabortexceptio$
          }
      }
      """
    Given I start an action chain
    #Given I set "omnisharp-auto-complete-want-importable-types" to "t"
    And I press "C-u"
    And I press "M-x"
    And I type "omnisharp-auto-complete"
    # Finish with M-x
    And I press "RET"
    # A pop-up.el menu is shown. Complete the first candidate.
    And I press "RET"
    And I execute the action chain

    Then I should see, ignoring line endings:
      """
      using System.Threading;
      namespace Test {
          public class Awesome {
              ThreadAbortException
          }
      }
      """
