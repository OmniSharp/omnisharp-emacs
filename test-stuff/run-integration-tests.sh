#!/bin/bash

echo "'dotnet --version' reports:" $(dotnet --version)

if [[ $(dotnet --version) != "2.1"* ]]; then
    echo "Must install the .NET CLI 2.1.* http://dotnet.github.io/"
    exit 1
fi

echo "Building MinimalProject"
pushd test/MinimalProject
dotnet build
popd

TERM=dumb SHELL=sh cask exec emacs \
    -Q \
    -batch \
    -f package-initialize \
    -l buttercup \
    -l "test/buttercup-tests/setup.el" \
    -f buttercup-run-discover "test/buttercup-tests/"
