#! /bin/sh
echo "./run-integration-tests.sh"
./run-integration-tests.sh

echo "./run-tests.sh"
./run-tests.sh

echo "./run-melpa-build-test.sh"
./run-melpa-build-test.sh
