#!/bin/sh

# LICENSE
#
# rbenv setup assistance
# Copyright (C) 2013-2014 Hiroaki ENDOH
#
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU 
# General Public License as published by the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with this program. 
# If not, see <http://www.gnu.org/licenses/>.

# 動作条件
# - Mac OS X Mountain Lion以上であること
# - MacPortsがインストールされていること
# - gitが動作すること

readonly RBENV_GIT_SRC_PATH="git://github.com/sstephenson/rbenv.git"
readonly RBENV_GIT_DST_PATH=${HOME}/src/rbenv
readonly RBENV_PATH=${HOME}/.rbenv
readonly RBENV_PLUGINS_PATH=${RBENV_GIT_DST_PATH}/plugins
readonly RUBY_BUILD_GIT_SRC_PATH="git://github.com/sstephenson/ruby-build.git"
readonly RUBY_BUILD_GIT_DST_PATH=${RBENV_PLUGINS_PATH}/ruby-build
readonly PACKAGE_MANAGER_BIN_PATH=/opt/local/bin/port

# Mac OS X以外ははじく (Macでしかテストしていないので, 他のOSでは何がおきるか予測できない)
if [ "$(uname)" != "Darwin" ]; then
    echo "Error\t: Mac OS X 以外はサポート対象外です。"
    exit 1
fi

# MacPortsがインストールされているか簡易的にチェックする
if [ ! -e $PACKAGE_MANAGER_BIN_PATH ] ; then
    echo "Error\t: MacPortsがインストールされていませんでした。このシェルスクリプトはMacPortsが必要です。MacPortsインストール後に再度実行してください。"
    exit 1
fi

# rbenvが既に手元にあるか確認する (ruby-buildはrbenv直下のpluginsにあるはずなのでチェックしない)
if [ -d ${RBENV_GIT_DST_PATH} ] ; then
    echo "Warning\t: 既にrbenvが存在します．続行するにはrubyをOSが標準で持っていたバージョンに戻し、かつ既存のrbenvを消去する必要があります. 続行しますか？ [yes/no]"
    read DELETE_RBENV_IF_NEEDED

    if [ ${DELETE_RBENV_IF_NEEDED} = 'yes' ]; then
        echo ""
        echo rbenv global system でOSが標準で持っていたrubyバージョンに戻します
        rbenv global system
        echo "`ruby -v` に変更しました"
        echo rm -rf $RBENV_GIT_DST_PATH
        rm -rf $RBENV_GIT_DST_PATH
        echo "既存のrbenvを消去しました."
    else
        echo "既存のrbenvは消去せず, 処理を終了しました."
        exit 1
    fi
fi

echo ""
echo "rbenvをgithubからクローンします......"
echo mkdir -p ${HOME}/src
mkdir -p ${HOME}/src
git clone $RBENV_GIT_SRC_PATH $RBENV_GIT_DST_PATH
echo ""
echo "ruby-buildをgithubからクローンします......"
echo mkdir -p $RBENV_PLUGINS_PATH
mkdir -p $RBENV_PLUGINS_PATH
git clone $RUBY_BUILD_GIT_SRC_PATH $RUBY_BUILD_GIT_DST_PATH

echo ""
echo "rbenvを~/.rbenvとしてシンボリックリンクを貼ります......"
if [ -L $RBENV_PATH ] ; then
    echo "Warning\t: 既にシンボリックリンクが存在します．差し替えますか？ [yes/no]"
    read DELETE_RBENV_PATH

    if [ $DELETE_RBENV_PATH = 'yes' ]; then
        rm $RBENV_PATH
        ln -s $RBENV_GIT_DST_PATH $RBENV_PATH 
        echo "シンボリックリンクの差し替えが完了しました"
        echo ""
    else
        echo "シンボリックリンクを差し替えを中止します"
        echo ""
        exit 1
    fi
else
    ln -s $RBENV_GIT_DST_PATH $RBENV_PATH 
    echo "シンボリックリンクの差し替えが完了しました"
    echo "" 
fi

# rbenvを環境変数にセットする
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# Macportsでインストールすべきソフトウェアがあるか診断する
# 下記3つの変数は0が真、1を偽とする
EXSISTED_OPENSSL=1  
EXSISTED_READLINE=1
EXSISTED_LIBICONV=1

for portname in `port installed active | grep -v 'The' | awk '{ print $1 }'`
do
    # 条件式が真の場合のみ処理
    if [ $portname = "openssl" ] ; then
        EXSISTED_OPENSSL=0
    fi

    if [ $portname = "readline" ] ; then
        EXSISTED_READLINE=0
    fi

    if [ $portname = "libiconv" ] ; then
        EXSISTED_LIBICONV=0
    fi
done

# openssl, readline, libiconvのいずれかがインストールされていない場合は, MacPortsを更新する
if [ $EXSISTED_OPENSSL = 1 -o $EXSISTED_READLINE = 1 -o $EXSISTED_LIBICONV = 1 ]; then 
    echo "MacPortsのportを更新します......"
    sudo port selfupdate
fi

# 不足しているソフトウェアをMacPortsでインストールする
if [ $EXSISTED_OPENSSL = 1 ]; then
    echo "MacPortsでopensslをインストールします......"
    sudo port install openssl
fi
if [ $EXSISTED_READLINE = 1 ]; then
    echo "MacPortsでreadlineをインストールします......"
    sudo port install readline
fi
if [ $EXSISTED_LIBICONV = 1 ]; then
    echo "MacPortsでlibiconvをインストールします......"
    sudo port install libiconv
fi

# ユーザにインストールしたいrubyのバージョンを選択させる
RUBY_VERSION=""
echo "インストールしたいrubyのバージョンを選択してください(項目の番号を指定してください)"
echo "インストールを望まない場合はCtrl -dで処理を中断してください"
select i in `rbenv install -l | grep -v 'Available\|rbx\|ree\|jruby\|mruby\|maglev\|topaz\|1.8.6\|1.8.7\|1.9.1\|1.9.2' | awk '{ print $1 }'`
do
if rbenv install -l | grep $i >/dev/null 2>&1; then
    echo ${i}を選択しました。
    RUBY_VERSION=$i
    break
else 
    echo "リストの中にある番号を入力してください"
fi
done

# rubyをインストールする
if rbenv install -l | grep $RUBY_VERSION >/dev/null 2>&1; then
    CONFIGURE_OPTS="--with-openssl-dir=/opt/local --with-readline-dir=/opt/local --with-iconv-dir=/opt/local" rbenv install $RUBY_VERSION
    rbenv rehash
else
    echo "そのバージョンはインストール済みです"
    echo ""
    exit 1
fi

echo "このOSでは以下のrubyが入っています"
rbenv versions
echo ""

# shellにパスを通す
echo "最後に以下の作業をお願いします"
echo "シェルの環境変数に $HOME/.rbenv/bin を追加してください"
echo "普段使うrubyのバージョンを指定するために $rbenv global rubyのバージョン番号 の実行"
exit 0
