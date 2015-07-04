Feature: Find implementations

  Scenario: Jump to the only implementation
    Given I open the MinimalSolution source file "minimal/MyClassContainer.cs"
    When My buffer contents are, and my point is at $:
    """
    public class Base$Class {}
    public class SomeClass : BaseClass {}
    """
    And I evaluate the command "(omnisharp-find-implementations)"
    And I wait "1" seconds
    Then point should be on a line containing "public class SomeClass : BaseClass {}"

  Scenario: Show a list of implementations when there is more than one
    Given I open the MinimalSolution source file "minimal/MyClassContainer.cs"
    When My buffer contents are, and my point is at $:
    """
    public class Base$Class {}
    public class SomeClass : BaseClass {}
    public class SomeClass2 : BaseClass {}
    """
    And I evaluate the command "(omnisharp-find-implementations)"
    And I wait "1" seconds

    When I switch to the existing buffer "* OmniSharp : Implementations *"
    Then I should see "public class SomeClass : BaseClass {}"
    Then I should see "public class SomeClass2 : BaseClass {}"

