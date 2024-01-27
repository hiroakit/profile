#!/bin/sh

echo "Check dotnet";
if !(type dotnet >/dev/null 2>&1); then
  echo "dotnet doesn't exsist."
  ./scripts/install-dotnet.sh  
fi

if !(dotnet tool list -g | grep dotnet-script > /dev/null 2>&1); then
  echo "Install dotnet-script";
  dotnet tool install -g dotnet-script
fi

# echo "Run installation scripts"
# dotnet script scripts/main.csx
