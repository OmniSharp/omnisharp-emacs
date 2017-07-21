#! /bin/bash -e
set -o pipefail


assertTestPasses() {
    echo $1
    eval $1
}

# https://github.com/cask/cask/issues/241
find .cask -name "*.elc" -delete

assertTestPasses "./run-tests.sh"
assertTestPasses "./run-integration-tests.sh"
assertTestPasses "./run-melpa-build-test.sh"
