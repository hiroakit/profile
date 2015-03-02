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
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export CATALINA_HOME=/Library/Tomcat
export MAVEN3_HOME=/usr/local/apache-maven-3.2.5
export PATH=$PATH:$MAVEN3_HOME/bin

## PMD
export PMD_HOME=/usr/local/pmd-bin-5.1.3
export PATH=$PATH:$PMD_HOME/bin

## Scala
export PATH=/usr/local/bin/scala-2.10.3/bin:$PATH

## Play
export PATH=/usr/local/bin/play:$PATH

## Gtags - Global
export GTAGSCONF=/usr/local/share/gtags/gtags.conf
export GTAGSLABEL=exuberant-ctags

## Maya
export MAYA_UI_LANGUAGE="en_US"

## rbenv (on Mac OS X Mountain Lion)
export RBENV_ROOT=$HOME/.rbenv
if [ -d ${RBENV_ROOT} ]; then
    export PATH="$RBENV_ROOT/bin:$PATH"
    eval "$(rbenv init -)" 
fi

# pyenv
export PYENV_ROOT=${HOME}/.pyenv
if [ -L ${PYENV_ROOT} -o -d ${PYENV_ROOT} ]; then
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi

