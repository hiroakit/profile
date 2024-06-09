#
# .zshenv
#
readonly local ZSHENV_DEBUG_MODE=0

function get_this_file_path {
  local dir
  dir=$(dirname 0)
  echo ${dir%/}/.zshenv
}

if [ ${ZSHENV_DEBUG_MODE} -gt 0 ]; then
    get_this_file_path 
fi

export TERM='xterm-256color'
export ZDOTDIR=$HOME/.zsh
export LANG=ja_JP.UTF-8

#------------------
# Loading Path rules
#
# 1.  /etc/zshenv
# 2.  $ZDOTDIR/.zshenv
# 3.  /etc/zprofile
#       On macOS, exec /usr/libexec/path_helper in /etc/zprofile
#       See /etc/paths.d/
# 4.  $ZDOTDIR/.zprofile
# 5.  /etc/zshrc
# 6.  $ZDOTDIR/.zshrc
# 7.  /etc/zlogin
# 8.  $ZDOTDIR/.zlogin
# 9.  /etc/zlogout
# 10. $ZDOTDIR/.zlogout
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
typeset -U path PATH
path=(
  "$HOME/.rbenv/bin"
  "/opt/homebrew/bin"  
  "/bin"    
  "/usr/bin"  
  "/Applications/Visual Studio Code.app/Contents/Resources/app/bin"(N-/)
)

# node.js v12 for Azure Function Runtime
# export PATH="/usr/local/opt/node@12/bin:$PATH"

# # MySQL
# if [ -d "/usr/local/mysql/bin" ]; then
#     export PATH=/usr/local/mysql/bin:$PATH
# fi
#  
# # Go
# if [ -d "$HOME/go/bin" ]; then
#     export GOPATH=$HOME/go
#     export PATH=$GOPATH/bin:$PATH
# fi

## Flutter
# export PATH=$HOME/src/flutter/bin:$PATH

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
# export GTAGSCONF=/usr/local/share/gtags/gtags.conf
# export GTAGSLABEL=exuberant-ctags

## Pixar RenderMan
# export RMANTREE=/Applications/Pixar/RenderManProServer-19.0
# export PATH=$PATH:$RMANTREE/bin

## Maya
# export MAYA_UI_LANGUAGE="en_US"

# rbenv
# export PATH=$HOME/.rbenv/bin:$PATH # zshのpathで指定しているため不要
eval "$(rbenv init - zsh)"
eval "$(nodenv init - zsh)"
