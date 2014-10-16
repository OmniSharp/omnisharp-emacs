;; http://tuxicity.se/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html
;; This file is included in each test run before any test file is
;; loaded. This is a good place for common test helper functions.
(require 'ert)

(load-file "omnisharp.el")
(require 'omnisharp)
(require 'evil) ; some tests test evil functionality specifically
(require 'el-mock)

