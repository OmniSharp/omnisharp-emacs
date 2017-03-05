#! /bin/bash

if [[ $(dotnet --version) != "1.0"* ]]; then
    echo "Must install the .NET CLI 1.0.* http://dotnet.github.io/"
    exit 1
fi

echo "Restoring MinimalProject packages"
pushd test/MinimalProject
dotnet restore
dotnet build
popd

TERM=dumb SHELL=sh cask exec emacs \
    -Q \
    -batch \
    -f package-initialize \
    -l buttercup \
    -l "test/buttercup-tests/setup.el" \
    -f buttercup-run-discover \
    "test/buttercup-tests/" \
