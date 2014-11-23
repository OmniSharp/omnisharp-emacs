Feature: Find usages

  Scenario: Find usages of a symbol
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
    And I evaluate the command "(omnisharp-find-usages)"
    And I wait "1" seconds
    When I switch to the existing buffer "* OmniSharp : Usages *"
    Then I should see "Usages in the current solution:"
    Then I should see "public class Target {}"
