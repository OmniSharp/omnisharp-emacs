#! /bin/sh
# Tests that the program can be installed without an error from melpa.
# 
# Will break if dependencies are not configured correctly, in which
# case we should update dependency versions and/or file issues to the
# relative projects.

git clone https://github.com/milkypostman/melpa

cd melpa
git pull
make clean
make recipes/omnisharp
emacs -Q --script ../ignored-from-melpa-build/melpa-build-test.el
