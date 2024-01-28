#!/bin/bash

echo "Checking dotnet";
if !(type dotnet >/dev/null 2>&1); then
    if [ ! -f /usr/local/share/dotnet/dotnet ]; then
        echo "dotnet doesn't exsist."
        ./scripts/install-dotnet.sh
    fi
    
    eval `/usr/libexec/path_helper -s`    
fi

echo "Checking dotnet script";
if !(dotnet tool list -g | grep dotnet-script > /dev/null 2>&1); then
    echo "Install dotnet-script";
    dotnet tool install -g dotnet-script
fi

echo "Checking Homebrew"
if !(type brew > /dev/null); then
    if [ ! -d /opt/homebrew/bin ]; then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

echo "Run installation scripts"
dotnet script scripts/Main.csx
