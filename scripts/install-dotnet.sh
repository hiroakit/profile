echo "Download .NET SDK installer..."

# dotnet-sdk-6.0.102-osx-arm64.pkg
pkg_url="https://download.visualstudio.microsoft.com/download/pr/cc2a94b1-7f3c-44f8-a842-f288a0cff04e/32446a93655522a1f933d4afb5e15836/dotnet-sdk-6.0.102-osx-arm64.pkg"

mkdir -p /tmp/macos-setup

# Download installer
curl -# -L -G -o /tmp/macos-setup/dotnet.pkg $pkg_url
if [ ! -r /tmp/macos-setup/dotnet.pkg ]; then
    echo "Not found .NET SDK installer."
    exit -1
fi

# Run installer
echo "Install .NET SDK..."
sudo installer -pkg /tmp/macos-setup/dotnet.pkg -target /Volumes/Macintosh\ HD

# Clean up
rm -rf /tmp/macos-setup
echo "Clean up"