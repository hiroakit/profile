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

# Claude Codeが実行したコマンドの履歴。
# ~/.claude/hooks/record-command-history.py が追記し、$ZDOTDIR/.zclaude が読む。
# zsh本来の$HISTFILEには混ぜないため、別ファイルにしている。
export CLAUDE_HISTFILE=$HOME/.claude/command-history.tsv

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
#
# 自分で優先させたいパスの定義。ここを唯一の定義元とする。
#
# /etc/zprofile の path_helper が PATH を組み直すとき、ここで並べた順序は
# 保たれず、環境によっては落ちる。そのため .zshrc でも $my_path を使って
# 積み直している。この配列を .zshrc から参照するので -g で残す。
#
# shims は各バージョン管理ツールの init が足すものだが、init 自体は
# .zshrc で遅延ロードしている。ruby/python/node の解決を起動直後から
# 効かせたいので、shims はここで明示的に通す。
#
# システムのパスより優先させたいもの。
typeset -ga my_path_head
my_path_head=(
  "$HOME/.local/bin"(N-/)
  "$HOME/.nodenv/shims"(N-/)
  "$HOME/.pyenv/shims"(N-/)
  "$HOME/.pyenv/bin"(N-/)
  "$HOME/.rbenv/shims"(N-/)
  "$HOME/.rbenv/bin"(N-/)
  "/opt/homebrew/bin"(N-/)
  "/opt/homebrew/sbin"(N-/)
  "/opt/homebrew/opt/mysql@8.0/bin"(N-/)
)

# アプリケーションバンドル内のコマンド。
# ここを前に出すと Emacs.app の ctags/etags/emacsclient が
# /usr/bin や Homebrew のものより優先されてしまうため、後ろに置く。
typeset -ga my_path_tail
my_path_tail=(
  "/Library/TeX/texbin"(N-/)
  "/Applications/Visual Studio Code.app/Contents/Resources/app/bin"(N-/)
  "/Applications/Emacs.app/Contents/MacOS/bin"(N-/)
)

path=(
  $my_path_head
  "/usr/local/bin"
  "/bin"
  "/usr/bin"
  $my_path_tail
)

#------------------
# Editor
#
# emacsを既定のエディタにする。
# macOSではEmacsを/Applications/Emacs.appに寄せているため、そこを見る。
# 起動済みのEmacs (GUI/端末どちらでも可) のサーバーに繋ぎたいので
# emacsclientを優先する。サーバーが無い時は -a '' でdaemonを起動する。
# GUIを開かないように -nw (--no-window-system) を付与する。
#------------------
case "${OSTYPE}" in
    darwin*)
        emacs_bin="/Applications/Emacs.app/Contents/MacOS/Emacs"
        emacsclient_bin="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
        ;;
    *)
        emacs_bin="/usr/bin/emacs"
        emacsclient_bin="/usr/bin/emacsclient"
        ;;
esac

if [ ! -x "${emacs_bin}" ]; then
    emacs_bin=$(command -v emacs 2>/dev/null)
fi

if [ ! -x "${emacsclient_bin}" ]; then
    emacsclient_bin=$(command -v emacsclient 2>/dev/null)
fi

if [ -n "${emacsclient_bin}" ] && [ -x "${emacsclient_bin}" ]; then
    export EDITOR="${emacsclient_bin} -nw -a ''"
    export VISUAL="${EDITOR}"
elif [ -n "${emacs_bin}" ] && [ -x "${emacs_bin}" ]; then
    export EDITOR="${emacs_bin} -nw"
    export VISUAL="${EDITOR}"
fi

unset emacs_bin emacsclient_bin

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

# rbenv / nodenv / pyenv の init は .zshrc で遅延ロードしている。
# ここで eval すると .zprofile と二重に走り、その分だけ起動が遅くなる。
