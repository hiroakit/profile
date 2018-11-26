export TERM='xterm-256color'
export ZDOTDIR=$HOME/.zsh
export LANG=ja_JP.UTF-8
# export EDITOR="/usr/local/bin/emacs -q -nw"

#------------------
# Path config
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

## XQuartz
export PATH=$PATH:/opt/X11/bin

## MySQL
export PATH=/usr/local/mysql/bin:$PATH

## Go
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH

## Flutter
export PATH=$HOME/src/flutter/bin:$PATH

## Java
# export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
# export CATALINA_HOME=/Library/Tomcat
# export MAVEN3_HOME=/usr/local/apache-maven-3.2.5
# export PATH=$PATH:$MAVEN3_HOME/bin

## Android
export ANDROID_HOME="$HOME/Library/Android/sdk"
export PATH="$ANDROID_HOME/platform-tools:$PATH"
export PATH="$ANDROID_HOME/tools:$PATH"

## Gtags - Global
export GTAGSCONF=/usr/local/share/gtags/gtags.conf
export GTAGSLABEL=exuberant-ctags

## Pixar RenderMan
export RMANTREE=/Applications/Pixar/RenderManProServer-19.0
export PATH=$PATH:$RMANTREE/bin

## Maya
export MAYA_UI_LANGUAGE="en_US"
