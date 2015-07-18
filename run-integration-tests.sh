#! /bin/bash

TERM=dumb cask exec emacs \
    -batch \
    -f package-initialize \
    -l buttercup \
    -l "test/buttercup-tests/setup.el" \
    -f buttercup-run-discover \
    "test/buttercup-tests/" \
