echo "Download .NET Core SDK installer..."

# HTTPヘッダのLOCATIONパラメーターからインストーラーのURLを抽出する
pkg_url=$(curl -sI "https://go.microsoft.com/fwlink/?LinkID=835011" | grep -o -E '^Location:.*$' | sed -e 's/Location: //')

pkg_url=${pkg_url%$'\r'}
pkg=$(basename ${pkg_url})

# curlでファイルをダウンロードする
curl -# -L -O -G $pkg_url

if [ ! -r $pkg ]; then
    echo "Not found .NET Core SDK installer."
    exit -1
fi

echo "Install .NET Core SDK..."
sudo installer -pkg ./$pkg -target /Volumes/Macintosh\ HD    
