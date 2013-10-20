export ZDOTDIR=$HOME/.zsh
export LANG=ja_JP.UTF-8

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

## init path
case "${OSTYPE}" in
    darwin*)
        # TODO : MacPortsが入っていない環境も考慮した記述に変更する
        path=(
            /usr/local/{bin,sbin}(N-/)
            /opt/local/{bin,sbin}(N-/)
            /usr{/bin,/sbin}(N-/)
            /{bin,sbin}(N-/)
        )
        ;;
    # freebsd*)
    #     ;;
    # linux*)
    # case ${UID} in
    #     0)
    #         ;;
    # esac
    #     ;;
esac

## init manpath
case "${OSTYPE}" in
    darwin*)
        manpath=(
            /usr/local/share/man(N-/)
            /opt/local/share/man(N-/)
            /usr/share/man(N-/)
        )
        ;;
    # freebsd*)
    #     ;;
    # linux*)
    #     ;;
esac

## XQuartz
export PATH=$PATH:/opt/X11/bin

## MySQL
export PATH=/usr/local/mysql/bin:$PATH

## Java
export JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home
export CATALINA_HOME=/Library/Tomcat

## Scala
export PATH=/usr/local/bin/scala-2.10.3/bin:$PATH

## Play
export PATH=/usr/local/bin/play:$PATH

## Gtags - Global
export GTAGSCONF=/usr/local/share/gtags/gtags.conf
export GTAGSLABEL=exuberant-ctags

## Maya
export MAYA_UI_LANGUAGE="en_US"
