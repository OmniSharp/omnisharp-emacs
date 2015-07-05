Feature: Rename symbol
  In order to improve the clarity of the codebase
  A user will need to change one symbol to another throughout the codebase

  Scenario: Rename a symbol referenced in a single file
    Given I open the MinimalSolution source file "minimal/RenameFileTest.cs"
    When My buffer contents are, and my point is at $:
    """
    using System;

    namespace minimal
    {
        public class OldClass {}
        public class OtherClass {
            Old$Class foo; // rename here
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
          public class OldClassChanged {}
          public class OtherClass {
              OldClassChanged foo; // rename here
          }
      }
      """

  Scenario: Rename a symbol referenced in multiple files
    Given I open the MinimalSolution source file "minimal/MyClass.cs"
    # Point position is irrelevant for this file
    When My buffer contents are, and my point is at $:
    """
    using System;

    namespace minimal$
    {
        public class MyClass {}
    }
    """

    Given I open the MinimalSolution source file "minimal/MyClassContainer.cs"
    When My buffer contents are, and my point is at $:
    """
    using System;

    namespace minimal
    {
        public class MyClassContainer
        {
            public My$Class foo;
        }
    }
    """

    And I start an action chain
    And I press "M-x"
    And I type "omnisharp-rename"
    And I press "RET"
    # The new name will be MyClass2
    And I type "2"
    And I press "RET"
    And I execute the action chain

    Then I should see, ignoring line endings:
    """
    using System;

    namespace minimal
    {
        public class MyClassContainer
        {
            public MyClass2 foo;
        }
    }
    """

    When I switch to buffer "MyClass.cs"
    Then I should see, ignoring line endings:
    """
    using System;

    namespace minimal
    {
        public class MyClass2 {}
    }
    """
