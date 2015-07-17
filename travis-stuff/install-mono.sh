#! /bin/bash -e
# Install mono and libuv. These are dependencies of omnisharp-roslyn
# http://docs.asp.net/en/latest/getting-started/installing-on-linux.html

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
sudo apt-get update
sudo apt-get install mono-complete

# install dnvm
# https://github.com/aspnet/Home/blob/dev/GettingStartedDeb.md
sudo apt-get install unzip
curl -sSL https://raw.githubusercontent.com/aspnet/Home/dev/dnvminstall.sh | DNX_BRANCH=dev sh && source ~/.dnx/dnvm/dnvm.sh
# should show help text if installed successfully
dnvm

ls -alR ~/.dnx
