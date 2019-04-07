#!/bin/bash

echo "'dotnet --version' reports:" $(dotnet --version)

if [[ $(dotnet --version) != "2.2"* ]]; then
    echo "Must install the .NET CLI 2.2.* http://dotnet.github.io/"
    exit 1
fi

if [[ ! -r test/MinimalProject/project.lock.json ]]; then
    echo "Restoring MinimalProject packages"
    pushd test/MinimalProject
    dotnet restore
    popd
fi

TERM=dumb SHELL=sh cask exec emacs \
    -Q \
    -batch \
    -f package-initialize \
    -l buttercup \
    -l "test/buttercup-tests/setup.el" \
    -f buttercup-run-discover "test/buttercup-tests/"
