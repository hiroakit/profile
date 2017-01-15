MacCLToolsPkgId=com.apple.pkg.CLTools_Executables
MacPortsPkgId=org.macports.MacPorts
MacPortsPkgFileName=MacPorts-2.3.4-10.10-Yosemite.pkg

echo "Check Command Line Tools"
pkgutil --pkg-info=${MacCLToolsPkgId} >/dev/null 2>&1
if [[ $? != 0 ]] ; then
	echo "Mac Command Line Tools Installation"
    sudo xcode-select --install
 	echo ""
else
	echo "Mac Command Line Tools installed"
fi

# echo "Check MacPorts"
# pkgutil --pkg-info=${MacPortsPkgId} >/dev/null 2>&1
# if [[ $? != 0 ]] ; then
#  	echo "MacPorts installation"
#     mkdir ./downloads
#     cd ./downloads
#  	curl -O https://distfiles.macports.org/MacPorts/${MacPortsPkgFileName}
#  	# sudo installer -verbose -pkg ${MacPortsPkgFileName} -target /
#     cd ../
#  	echo ""
#     echo "MacPorts selfupdate"
#  	sudo /opt/local/bin/port -v selfupdate
#  	echo ""
# else
#  	echo "MacPorts installed"
# fi
#  
# echo "Check Ansible"
# which -s ansible
# if [[ $? != 0 ]] ; then
#     echo "Ansible Installation"
#     sudo /opt/local/bin/port install ansible
#     echo ""
# else
#  	echo "Ansible installed"
# fi
