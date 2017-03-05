#!/usr/bin/env bash
mkdir omnisharp-roslyn

cd omnisharp-roslyn
wget https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.10.0/omnisharp-ubuntu14-x64-netcoreapp1.1.tar.gz
tar xzvf omnisharp-ubuntu14-x64-netcoreapp1.1.tar.gz
chmod -R 755 .
cd ..
