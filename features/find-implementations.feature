Feature: Find implementations

  Scenario: Jump to the only implementation
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

        public class test : MyClass{}
    }
    """
    And I evaluate the command "(omnisharp-find-implementations)"
    And I wait "1" seconds
    Then point should be on a line containing "public class test : MyClass{}"
