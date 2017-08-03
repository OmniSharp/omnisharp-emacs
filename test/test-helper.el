
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;
;; http://tuxicity.se/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html
;; This file is included in each test run before any test file is
;; loaded. This is a good place for common test helper functions.
(require 'ert)
(require 'ert-async)
(require 's)
(require 'cl)

;; these are run in the omnisharp source code root directory
(add-to-list 'load-path (expand-file-name "./"))
(require 'omnisharp)
(require 'evil) ; some tests test evil functionality specifically
(require 'el-mock)
(require 'noflet)
(require 'buttercup)

(defmacro with-server-returning (called-api-name return-value &rest test-forms)
  "Allows mocking calling the omnisharp-roslyn stdio server to test
callback effects directly, without the need of a running
omnisharp-roslyn process."
  `(noflet ((omnisharp--send-command-to-server (_api-name _payload &optional response-handler)
                                               (apply response-handler (list ,return-value))))
           ,@test-forms))
