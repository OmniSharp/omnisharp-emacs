#! /bin/bash

if [[ $(dotnet --version) != "1.0.0-rc2-"* ]]; then
    echo "Must install the .NET CLI http://dotnet.github.io/"
    exit 1
fi

if [[ ! -r test/MinimalProject/project.lock.json ]]; then
    echo "Restoring MinimalProject packages"
    pushd test/MinimalProject
    dotnet restore -v Warning
    popd
fi

TERM=dumb SHELL=sh cask exec emacs \
    -Q \
    -batch \
    -f package-initialize \
    -l buttercup \
    -l "test/buttercup-tests/setup.el" \
    -f buttercup-run-discover \
    "test/buttercup-tests/" \
