#!/bin/sh

HOST=$(hostname | sed "s/.local//")

if [ ! ${EUID:-${UID}} = 0 ]; then
    echo '管理者権限が必要です。'
    exit -1
fi

INSTALL_SCRIPT_PATH=$(cd $(dirname $0);pwd)
/bin/echo cp ${INSTALL_SCRIPT_PATH}/autoupgrade-macports.sh /usr/local/bin/ 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
cp ${INSTALL_SCRIPT_PATH}/autoupgrade-macports.sh /usr/local/bin/

/bin/echo chmod 755 /usr/local/bin/autoupgrade-macports.sh 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
chmod 755 /usr/local/bin/autoupgrade-macports.sh

/bin/echo cp ${INSTALL_SCRIPT_PATH}/com.hiroakit.autoupgrade-macports.plist /Library/LaunchDaemons/ 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
cp ${INSTALL_SCRIPT_PATH}/com.hiroakit.autoupgrade-macports.plist /Library/LaunchDaemons/

/bin/echo launchctl unload /Library/LaunchDaemons/com.hiroakit.autoupgrade-macports.plist 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
launchctl unload /Library/LaunchDaemons/com.hiroakit.autoupgrade-macports.plist

/bin/echo launchctl load /Library/LaunchDaemons/com.hiroakit.autoupgrade-macports.plist 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
launchctl load /Library/LaunchDaemons/com.hiroakit.autoupgrade-macports.plist
