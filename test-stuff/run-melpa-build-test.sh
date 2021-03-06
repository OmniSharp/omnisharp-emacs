#! /bin/bash -e
set -o pipefail

# Tests that the program can be installed without an error from melpa.
# 
# Will break if dependencies are not configured correctly, in which
# case we should update dependency versions and/or file issues to the
# relative projects.

if [ -d melpa ]; then
    rm -rf melpa
fi

git clone https://github.com/melpa/melpa

# Custom recipe that uses the melpa-testing branch instead of the
# usual develop, to showcase a minimal broken setup.
recipeFile=./test-stuff/melpa-testing.recipe

cat $recipeFile
cp $recipeFile ./melpa/recipes/omnisharp

cd melpa
make clean
make recipes/omnisharp

cd ..

# No cask here. Use a fresh emacs so installation is as natural as possible
homeDir=`mktemp -d`
HOME=$homeDir emacs -Q \
    --eval '(setq user-emacs-directory "./sandbox")' \
    -l package \
    --script ignored-from-melpa-build/melpa-build-test.el 2>&1 | tee installation-output.txt

# Return value hack. Emacs above does not report the correct exit code.
# Grep returns 0 when the searched line is found, see man grep.
#
# Trying to match this line:
grep "Installation successful lololololol" installation-output.txt
