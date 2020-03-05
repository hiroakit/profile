# code --install-extension
# See also: https://code.visualstudio.com/docs/editor/extension-gallery#_command-line-extension-management
for package in $(cat packages.txt); do
    code --install-extension $package 
done
