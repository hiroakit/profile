#!/bin/sh

readonly local DOTNET_PKG_URL="https://download.visualstudio.microsoft.com/download/pr/1bfd8adc-359a-4fb0-b2d1-0cad866b4ea3/04cb083ce577a7ba60b8c46b6d25a72a/dotnet-sdk-6.0.126-osx-arm64.pkg"
readonly local DOTNET_PKG=`echo ${DOTNET_PKG_URL} | awk -F/ '{print $(NF-0)}'`
readonly local WORK_DIR=./resources/mac/dotnet
readonly local INSTALL_TARGET_DIR=/Volumes/Macintosh\ HD

echo "dotnet installation start."

if [ ! -d ${WORK_DIR} ]; then
  mkdir -p ${WORK_DIR}
fi

cd ${WORK_DIR}

# Download installer
if [ ! -f ${DOTNET_PKG} ]; then
  echo "Download .NET SDK installer (${DOTNET_PKG}) ..."
  curl -# -L -G -o ${DOTNET_PKG} ${DOTNET_PKG_URL}
fi

# Checksum
echo "Checksum ..."
if [ ! -f ${DOTNET_PKG}.sha512 ]; then
  echo "Checksum file not found."
  exit  
fi

shasum -a 512 -c ${DOTNET_PKG}.sha512
if [ $? -gt 0 ]; then
  exit
fi

# Really exist installer?
if [ ! -r ${DOTNET_PKG} ]; then
  echo "Not found .NET SDK installer."
  exit -1
fi

# Run installer
echo "Install .NET SDK (${DOTNET_PKG}) ..."
sudo installer -pkg ${DOTNET_PKG} -target "${INSTALL_TARGET_DIR}"
if [ $? -gt 0 ]; then
  exit
fi

if [ -x /usr/libexec/path_helper ]; then
  eval `/usr/libexec/path_helper -s`
fi

