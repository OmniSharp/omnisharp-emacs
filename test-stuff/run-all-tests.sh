#! /bin/bash -e
set -o pipefail

assertTestPasses() {
    echo $1
    eval $1
}

# https://github.com/cask/cask/issues/241
find .cask -name "*.elc" -delete

assertTestPasses "./test-stuff/run-tests.sh"
assertTestPasses "./test-stuff/run-integration-tests.sh"
assertTestPasses "./test-stuff/run-melpa-build-test.sh"
