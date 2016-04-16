#!/usr/bin/env bash
mkdir omnisharp-roslyn

cd omnisharp-roslyn
wget $(curl -s https://api.github.com/repos/omnisharp/omnisharp-roslyn/releases/latest | grep "omnisharp-coreclr-linux-x64.tar.gz" | awk '{ print $2 }' | sed s/\"//g | sed s/,//g)
tar xzvf omnisharp-coreclr-linux-x64.tar.gz
cd ..
