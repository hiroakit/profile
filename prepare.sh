#!/bin/bash

# 
# preparate for macOS
#

set -o pipefail
set -e
NSUnbufferedIO=YES

MacCLToolsPkgId=com.apple.pkg.CLTools_Executables
COMMAND="$1"

# 
# Show help
#
usage() {
    cat <<EOF
Usage: sh $0 Command

Command:
  usage:  
    show help
  full:
    run full setup
EOF
}

# 
# Run full setup
#
full() {
    # installer command and /Applications folder require sudo password
    printf "password: "
    read password    

    # Create working directory
    mkdir ./workdir
    cd ./workdir
     
    # Install Google Chrome
    curl -O -L https://dl.google.com/chrome/mac/stable/GGRO/googlechrome.dmg
    hdiutil mount ./googlechrome.dmg
    echo "$password" | sudo -S cp -r "/Volumes/Google Chrome/Google Chrome.app" /Applications/
    hdiutil unmount "/Volumes/Google Chrome"
     
    # Install node.js
    curl -O -L https://nodejs.org/dist/v8.11.1/node-v8.11.1.pkg
    echo "$password" | sudo -S installer -pkg "node-v8.11.1.pkg" -target /
     
    # Install puppeteer
    npm i puppeteer

    # Download Command Line Tools installer
    curl -O -L https://raw.githubusercontent.com/hiroakit/profile/AutomaticInstallation/apple-command-line-tools-downloader.js
    
    # Run Command Line Tools installer
    node ./apple-command-line-tools-downloader.js
     
    # Install Command Line Tools
    hdiutil mount ./Download/Command_Line_Tools_macOS_10.13_for_Xcode_9.3.dmg
    echo "$password" | sudo -S installer -pkg "/Volumes/Command Line Developer Tools/Command Line Tools (macOS High Sierra version 10.13).pkg" -target /
    hdiutil unmount "/Volumes/Command Line Developer Tools"
     
    echo "Check Apple Command Line Tools"
    pkgutil --pkg-info=${MacCLToolsPkgId} >/dev/null 2>&1
    if [[ $? != 0 ]] ; then
        echo "Require Apple Command Line Tools"
        exit -1
    else
        echo "Apple Command Line Tools installed"
    fi
     
    # Install Homebrew
    echo "" > homebrew-PRESS_ENTER.txt
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" < ./homebrew-PRESS_ENTER.txt
     
    # Install Python 3
    curl -O -L https://www.python.org/ftp/python/3.6.5/python-3.6.5-macosx10.9.pkg
    echo "$password" | sudo -S installer -pkg python-3.6.5-macosx10.9.pkg -target /
    export PATH=/usr/local/bin:/usr/local/sbin:$PATH
    export PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
    pip3.6 install ansible
     
    # Get my profile
    mkdir ~/src
    cd ~/src
    git clone https://github.com/hiroakit/profile --recursive
    cd profile/provisioning
    ansible-playbook -i hosts osx.yml -K    
}

# 
# Handle COMMAND argument
#
case "$COMMAND" in
    "usage")
	usage
	exit 0
	;;
    "full")
	full
	exit 0
	;;
    *)
	usage
        exit 0
esac
