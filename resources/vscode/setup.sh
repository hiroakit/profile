#!/bin/bash

# Visual Studio Code setup script
# Supported macOS, MSYS2 on Windows

set -o pipefail
set -e

# Show usage.
help() {
cat <<EOF
Help: sh $0 command
command:
  all:                    setup vscode
  install-extentions:     install vscode extentions
  install-configurations: install configuration files
EOF
}

# Validate input arguments.
# $# return total count of arguments.
if [ "$#" -eq 0 -o "$#" -gt 2 ]; then
    help
    exit 1
fi

# Install vscode extentions.
#
# code --install-extension
# See also: https://code.visualstudio.com/docs/editor/extension-gallery#_command-line-extension-management
install_extentions() {
    echo "Run ${FUNCNAME[0]}"
    
    PKG_LIST="${BASE_PATH%/}/packages.txt"

    echo "Using ${PKG_LIST}"
    for package in $(cat ${PKG_LIST}); do
        code --install-extension $package 
    done
}

# Linking configuration files.
install_configurations() {
    echo "Run ${FUNCNAME[0]}"
    
    case "${OSTYPE}" in
        darwin*)
            set -x
            # settings.json
            ln -snf \
               "${BASE_PATH%/}/Code/User/settings.json" \
               "${HOME}/Library/Application Support/Code/User/settings.json"

            # snippets
            ln -snf \
               "${BASE_PATH%/}/Code/User/snippets" \
               "${HOME}/Library/Application Support/Code/User"
            set +x
            ;;
        linux*)
            ;;
        msys*)
            set -x

            # convert windows style path to UNIX style path
            # e.g, 
            #   Before: C:\Users\johndoe\AppData\Roaming
            #   After:  /c/Users/johndoe/AppData/Roaming
            UNIX_STYLE_APPDATA=`cygpath -u ${APPDATA}`

            # settings.json
            cp \
               "${BASE_PATH%/}/Code/User/settings.json" \
               "${UNIX_STYLE_APPDATA}/Code/User/settings.json"

            # snippets
            cp -r\
               "${BASE_PATH%/}/Code/User/snippets" \
               "${UNIX_STYLE_APPDATA}/Code/User"
            set +x        
            ;;    
    esac
}

COMMAND="$1"                 # Using 1st argument as command
BASE_PATH="$(dirname "$0")"  # Calling script location

case "$COMMAND" in
    "help")
        help
        exit 0
        ;;
    "all")
        install_extentions
        install_configurations
        exit 0
        ;;    
    "install-extentions")
        install_extentions
        exit 0
        ;;
    "install-configurations")
        install_configurations
        exit 0
        ;;
    *)
        echo "Unknown command '${COMMAND}'\n"
        help
        exit 1
        ;;
esac
