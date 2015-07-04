Feature: Rename symbol
  In order to improve the clarity of the codebase
  A user will need to change one symbol to another throughout the codebase

  Scenario: Rename in same file
    Given I open the MinimalSolution source file "minimal/RenameFileTest.cs"
    When My buffer contents are, and my point is at $:
    """
    using System;

    namespace minimal
    {
        public class OldName {}
        public class OtherClass {
            Old$Name foo; // rename here
        }
    }
    """
    And I start an action chain
    And I press "M-x"
    And I type "omnisharp-rename"
    And I press "RET"
    # At this point the user is asked what the new symbol should be.
    # The current symbol is the default. Add something to its end to change it
    And I type "Changed"
    And I press "RET"
    And I execute the action chain

    Then point should be on line number "7"
    Then I should see, ignoring line endings:
      """
      using System;

      namespace minimal
      {
          public class OldNameChanged {}
          public class OtherClass {
              OldNameChanged foo; // rename here
          }
      }
      """

