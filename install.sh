#!/bin/sh

if !(type dotnet >/dev/null 2>&1); then
  # Check /etc/paths.d/dotnet
  if [ -f /etc/paths.d/dotnet ]; then
    if [ -x /usr/libexec/path_helper ]; then
      eval `/usr/libexec/path_helper -s`
    fi    
  else
    ./scripts/install-dotnet.sh
  fi
fi

if !(dotnet tool list -g | grep dotnet-script > /dev/null 2>&1); then
  dotnet tool install -g dotnet-script
fi

dotnet script scripts/main.csx
