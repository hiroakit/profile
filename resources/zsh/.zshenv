export TERM='xterm-256color'
export ZDOTDIR=$HOME/.zsh
export LANG=ja_JP.UTF-8

#------------------
# Path config
#
# See also /etc/paths.d/
#------------------ 

#   typeset
#    -U 重複パスを登録しない
#    -x exportも同時に行う
#    -T 環境変数へ紐付け
#
#   path=xxxx(N-/)
#     (N-/): 存在しないディレクトリは登録しない
#     パス(...): ...という条件にマッチするパスのみ残す
#        N: NULL_GLOBオプションを設定。
#           globがマッチしなかったり存在しないパスを無視する
#        -: シンボリックリンク先のパスを評価
#        /: ディレクトリのみ残す
#        .: 通常のファイルのみ残す
typeset -xU path cdpath fpath manpath

# XQuartz
if [ -d "/opt/X11/bin" ]; then
    export PATH=$PATH:/opt/X11/bin
fi

# MySQL
if [ -d "/usr/local/mysql/bin" ]; then
    export PATH=/usr/local/mysql/bin:$PATH
fi

# Go
if [ -d "$HOME/go/bin" ]; then
    export GOPATH=$HOME/go
    export PATH=$GOPATH/bin:$PATH
fi

## Flutter
export PATH=$HOME/src/flutter/bin:$PATH

## Java
# export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
# export CATALINA_HOME=/Library/Tomcat
# export MAVEN3_HOME=/usr/local/apache-maven-3.2.5
# export PATH=$PATH:$MAVEN3_HOME/bin

## Android
# export ANDROID_HOME="$HOME/Library/Android/sdk"
# export PATH="$ANDROID_HOME/platform-tools:$PATH"
# export PATH="$ANDROID_HOME/tools:$PATH"

## Gtags - Global
export GTAGSCONF=/usr/local/share/gtags/gtags.conf
export GTAGSLABEL=exuberant-ctags

## Pixar RenderMan
# export RMANTREE=/Applications/Pixar/RenderManProServer-19.0
# export PATH=$PATH:$RMANTREE/bin

## Maya
# export MAYA_UI_LANGUAGE="en_US"

## .NET (dotnet)
export PATH=$PATH:$HOME/.dotnet/tools

# pyenv
if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi

# $HOME/dev ... my resouces
export PATH="$HOME/dev/bin:$PATH"

