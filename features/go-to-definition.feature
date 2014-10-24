Feature: Go to definition
  In order to quickly clarify the origin or implementation of a symbol
  A user will need to navigate to the definiiton of the symbol at point

  Scenario: Go to definition in same file
    Given I open the Omnisharp server source file "minimal/MyClassContainer.cs"
    When My buffer contents are, and my point is at $:
    """
    using System;

    namespace minimal
    {
        public class Target {}
        public class JumpSite {
            Targ$et foo; // go to definition from here
        }
    }
    """
    And I evaluate the command "(omnisharp-go-to-definition)"
    Then point should be on line number "5"
    Then point should be on a line containing "public class Target"

  Scenario: Go to definition in another window
    Given I open the Omnisharp server source file "minimal/MyClassContainer.cs"
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
    And I evaluate the command "(omnisharp-go-to-definition-other-window)"

    Then there should be a window editing the file "MyClassContainer.cs"
    Then there should be a window editing the file "MyClass.cs"

    When I switch to the window in the buffer "MyClass.cs"
    Then point should be on a line containing "public class MyClass"
