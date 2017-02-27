echo "Download .NET Core SDK installer..."

# HTTPヘッダのLOCATIONパラメーターからインストーラーのURLを抽出する
pkg_url=$(curl -sI "https://go.microsoft.com/fwlink/?LinkID=835011" | grep -o -E '^Location:.*$' | sed -e 's/Location: //')

# curlは末尾に\rがあると"Illegal characters found in URL"と怒るので、取り除く
pkg_url=${pkg_url%$'\r'} 

# curlでファイルをダウンロードする
curl -# -L -O -G $pkg_url

# インストーラーがあるのか確認する
pkg=$(basename ${pkg_url})
if [ ! -r $pkg ]; then
    echo "Not found .NET Core SDK installer."
    exit -1
fi

# インストーラーを実行する
echo "Install .NET Core SDK..."
sudo installer -pkg ./$pkg -target /Volumes/Macintosh\ HD    
