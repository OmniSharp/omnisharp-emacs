#! /bin/bash

# bash - Exit Shell Script Based on Process Exit Code - Stack Overflow
# http://stackoverflow.com/questions/90418/exit-shell-script-based-on-process-exit-code
set -e
set -o pipefail

assertTestPasses() {
    echo $1
    eval $1
}

# https://github.com/cask/cask/issues/241
find .cask -name "*.elc" | xargs rm

assertTestPasses "./run-tests.sh"
assertTestPasses "./run-integration-tests.sh"
assertTestPasses "./run-melpa-build-test.sh"
