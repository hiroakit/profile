#!/bin/sh

HOST=$(hostname | sed "s/.local//")

/bin/echo "Auto upgrade MacPorts -- start" | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
/opt/local/bin/port -v selfupdate 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
/bin/echo "" | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'

/bin/echo "Upgrade outdated ports" | /opt/local/bin/gawk '{print strftime("%b %d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
/opt/local/bin/port -v upgrade outdated 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
/bin/echo "" | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'

/bin/echo "Clean ports" | /opt/local/bin/gawk '{print strftime("%b %d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
/opt/local/bin/port clean --dist --work --logs installed 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
/opt/local/bin/port clean --dist --work --logs inactive 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
/opt/local/bin/port clean --dist --work --logs active 2>&1 | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'

/bin/echo "Auto upgrade MacPorts -- end" | /opt/local/bin/gawk '{print strftime("%b/%d %H:%M:%S"),"'${HOST}'",$0; fflush() }'
