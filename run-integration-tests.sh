#! /bin/bash

TERM=dumb SHELL=sh cask exec emacs \
    -Q \
    -batch \
    -f package-initialize \
    -l buttercup \
    -l "test/buttercup-tests/setup.el" \
    -f buttercup-run-discover \
    "test/buttercup-tests/" \
