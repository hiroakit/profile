#!/bin/sh

# LICENSE
#
# pyenv setup assistant
# Copyright (C) 2014 Hiroaki ENDOH
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
# - Mac OS X Mavericks以上であること
# - MacPortsがインストールされていること
# - gitが動作すること
# - Xcode command line toolsをインストール済みであること
# - Python 2.7.5以上が入っていること

readonly GIT_PATH=`which git`
readonly PYTHON_PATH=`which python`
readonly SRC_ROOT_PATH=${HOME}/src
readonly PYENV_GIT_SRC_PATH="git://github.com/yyuu/pyenv.git"
readonly PYENV_GIT_DST_PATH=${SRC_ROOT_PATH}/pyenv
readonly PYENV_PATH=${HOME}/.pyenv

# Mac OS X以外は処理を終了させる (Macでしかテストしていないので, 他のOSでは何がおきるか予測できない)
if [ "$(uname)" != "Darwin" ]; then
    echo "Error\t: Mac OS X 以外はサポート対象外です。"
    exit 1
fi

# Gitがインストールされているかチェックする
if [ ! ${#GIT_PATH} -gt 0 ]; then
    echo "Error\t: Gitがインストールされていませんでした。このシェルスクリプトはGitが必要です。インストール後に再度実行してください。"
fi

if [ ! -e ${GIT_PATH} ]; then
    echo "Error\t: Gitがインストールされていませんでした。このシェルスクリプトはGitが必要です。インストール後に再度実行してください。"
fi

# Pythonがインストールされているかチェックする
if [ ! ${#PYTHON_PATH} -gt 0 ]; then
    echo "Error\t: Pythonがインストールされていませんでした。このシェルスクリプトはPythonが必要です。Pythonインストール後に再度実行してください。"
fi

if [ ! -e ${PYTHON_PATH} ]; then
    echo "Error\t: Pythonがインストールされていませんでした。このシェルスクリプトはPythonが必要です。Pythonインストール後に再度実行してください。"
fi

# PYENVがない場合
if [ ! -d ${PYENV_GIT_DST_PATH} ] ; then
    echo "pyenvをgithubからクローンします......"
    echo ""

    if [ ! -d ${SRC_ROOT_PATH} ] ; then
        echo mkdir -p ${SRC_ROOT_PATH}
        mkdir -p ${SRC_ROOT_PATH}
    fi
    git clone ${PYENV_GIT_SRC_PATH} $PYENV_GIT_DST_PATH
fi

echo "pyenvでシンボリックリンク~/.pyenvを貼ります......"
if [ -L ${PYENV_PATH} ] ; then
    echo "Warning\t: 既にシンボリックリンクが存在します．差し替えますか？ [yes/no]"
    read DELETE_PYENV_PATH
    if [ $DELETE_PYENV_PATH = 'yes' ]; then
        rm $PYENV_PATH
        ln -s $PYENV_GIT_DST_PATH $PYENV_PATH 
        echo "シンボリックリンクの差し替えが完了しました"
        echo ""
    else
        echo "シンボリックリンクを差し替えを中止します"
        echo "pyenvのセットアップを終了します"
        exit 1
    fi
else
    echo ln -s $PYENV_GIT_DST_PATH $PYENV_PATH 
    ln -s $PYENV_GIT_DST_PATH $PYENV_PATH 
    echo "シンボリックリンクの作成が完了しました"
    echo "" 
fi

# ユーザにインストールしたいpythonのバージョンを選択させる
PYTHON_VERSION=""
echo "インストールしたいpythonのバージョンを選択してください(項目の番号を指定してください)"
echo "中止する場合はCtrl -dで処理を中断してください"
select i in `pyenv install -l | grep -v 'Available\|dev\|src\|pypy\|jython\|stackless\|miniconda\|ironpython\|anaconda' | awk '{ print $1 }'`
do
if pyenv install -l | grep $i >/dev/null 2>&1; then
    echo ${i}を選択しました。
    PYTHON_VERSION=$i
    break
else 
    echo "リストの中にある番号を入力してください"
fi
done

# 指定したpythonをインストールする
if pyenv install -l | grep $PYTHON_VERSION >/dev/null 2>&1; then
    pyenv install $PYTHON_VERSION
else
    echo "そのバージョンはインストール済みです"
    echo ""
    exit 1
fi
