Feature: Complete overrides

  Background:
    Given I open the MinimalSolution source file "minimal/MyClassContainer.cs"
    Given The buffer is empty

  Scenario: Complete overrides in the current buffer with ido
    When My buffer contents are, and my point is at $:
      """
      namespace Test {
          public class Awesome {
              $
          }
      }
      """
    Given I start an action chain
    And I press "M-x"
    And I type "omnisharp-auto-complete-overrides"
    # Finish M-x
    And I press "RET"
    # Select the first proposed override
    And I press "RET"
    And I execute the action chain

    Then I should see, ignoring line endings:
      """
      public override bool Equals(object obj)
      """
