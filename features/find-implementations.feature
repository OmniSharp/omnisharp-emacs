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

  Scenario: Show a list of implementations when there is more than one
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
        public class test2 : MyClass{}
    }
    """
    And I evaluate the command "(omnisharp-find-implementations)"
    And I wait "1" seconds

    When I switch to the existing buffer "* OmniSharp : Implementations *"
    Then I should see "public class test : MyClass{}"
    Then I should see "public class test2 : MyClass{}"

