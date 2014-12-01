#! /bin/sh
# Tests that the program can be installed without an error from melpa.
# 
# Will break if dependencies are not configured correctly, in which
# case we should update dependency versions and/or file issues to the
# relative projects.

if [ -d melpa ]; then
    rm -rf melpa
fi

git clone https://github.com/milkypostman/melpa

# Custom recipe that uses the melpa-testing branch instead of the
# usual develop, to showcase a minimal broken setup.
cp ./melpa-testing.recipe melpa/recipes/omnisharp

cd melpa
make clean
make recipes/omnisharp

cd ..

# No cask here. Use a fresh emacs so installation is as natural as possible
emacs -Q \
    --eval '(setq user-emacs-directory "./sandbox")' \
    -l package \
    --script ignored-from-melpa-build/melpa-build-test.el
