#!/bin/sh

readonly local WORKDIR=/tmp/macos-setup
readonly local DOTNET_PKGFILE=/tmp/macos-setup/dotnet.pkg

if [ ! -d ${WORKDIR} ]; then
  mkdir -p /tmp/macos-setup
fi

# Download installer
if [ ! -f ${DOTNET_PKGFILE} ]; then
  echo "Download .NET SDK installer..."

  # dotnet-sdk-6.0.102-osx-arm64.pkg
  pkg_url="https://download.visualstudio.microsoft.com/download/pr/cc2a94b1-7f3c-44f8-a842-f288a0cff04e/32446a93655522a1f933d4afb5e15836/dotnet-sdk-6.0.102-osx-arm64.pkg"
  curl -# -L -G -o ${DOTNET_PKGFILE} $pkg_url
  if [ ! -r ${DOTNET_PKGFILE} ]; then
    echo "Not found .NET SDK installer."
    exit -1
  fi
fi

# Run installer
echo "Install .NET SDK..."
sudo installer -pkg /tmp/macos-setup/dotnet.pkg -target /Volumes/Macintosh\ HD
if [ -x /usr/libexec/path_helper ]; then
  eval `/usr/libexec/path_helper -s`
fi

# Clean up
echo "Clean up"
rm -rf ${WORKDIR}
