MacCLToolsPkgId=com.apple.pkg.CLTools_Executables

echo "Check Command Line Tools"
pkgutil --pkg-info=${MacCLToolsPkgId} >/dev/null 2>&1
if [[ $? != 0 ]] ; then
    echo "Mac Command Line Tools Installation"
    sudo xcode-select --install
    echo ""
else
    echo "Mac Command Line Tools installed"
fi
