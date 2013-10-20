#!/bin/sh

readonly ZSHENV_DST_PATH=${HOME}/.zshenv
readonly ZSHENV_ORG_PATH=$(cd $(dirname ${0});pwd)/zsh/.zshenv
readonly ZSH_CONF_DST_PATH=${HOME}/.zsh
readonly ZSH_CONF_ORG_PATH=$(cd $(dirname ${0});pwd)/zsh
readonly EMACS_CONF_DST_PATH=${HOME}/.emacs.d
readonly EMACS_CONF_ORG_PATH=$(cd $(dirname ${0});pwd)/emacs

echo "シンボリックリンクを作成します"
echo ""

## Zsh用
echo ".zsh \t\t\t: シンボリックリンク判定"
if [ -L ${ZSH_CONF_DST_PATH} ] ; then
    echo ".zsh \t\t\t: 既にシンボリックリンクが存在します．差し替えますか？ [yes/no]"
    read DELETE_ZSH_CONF

    if [ ${DELETE_ZSH_CONF} = 'yes' ]; then
        rm ${ZSH_CONF_DST_PATH}
        ln -s ${ZSH_CONF_ORG_PATH} ${ZSH_CONF_DST_PATH}
        echo ".zsh \t\t\t: シンボリックリンクの差し替えが完了しました"
    elif [ ${DELETE_ZSH_CONF} = 'no' ]; then
        echo ".zsh \t\t\t: シンボリックリンクを差し替えを中止しました"
    else
        exit 1
    fi
elif [ -d ${ZSH_CONF_DST_PATH} ]; then
    echo ".zsh \t\t\t: シンボリックリンクではなく，ディレクトリとして.zshが存在します. .zshを削除してシンボリックリンクを作成しますか？ [yes/no]"
    read DELETE_ZSH_CONF

    if [ ${DELETE_ZSH_CONF} = 'yes' ]; then
        echo ".zsh \t\t: .zshを削除します"
        rm -r ${ZSHENV_DST_PATH}

        echo ".zsh \t\t\t: シンボリックリンクを作成します"
        ln -s ${ZSH_CONF_ORG_PATH} ${ZSH_CONF_DST_PATH}
        echo ".zsh \t\t\t: シンボリックリンクの作成が完了しました" 
    elif [ ${DELETE_ZSH_CONF} = 'no' ]; then
        echo ".zsh \t\t\t: シンボリックリンク作成を中止しました"
    else
        exit 1
    fi
else 
    echo ".zsh \t\t\t: シンボリックリンクを作成します"
    ln -s ${ZSH_CONF_ORG_PATH} ${ZSH_CONF_DST_PATH}
    echo ".zsh \t\t\t: シンボリックリンクの作成が完了しました" 
fi

echo ".zshenv \t\t: シンボリックリンク判定"
if [ -L ${ZSHENV_DST_PATH} ] ; then
    echo ".zshenv \t\t: 既にシンボリックリンクが存在します．差し替えますか？ [yes/no]"
    read DELETE_ZSHENV_FILE

    if [ ${DELETE_ZSHENV_FILE} = 'yes' ]; then
        rm ${ZSHENV_DST_PATH}
        ln -s ${ZSHENV_ORG_PATH} ${ZSHENV_DST_PATH}
        echo ".zshenv \t\t: シンボリックリンクの差し替えが完了しました"
    elif [ ${DELETE_ZSHENV_FILE} = 'no' ]; then
        echo ".zshenv \t\t: シンボリックリンクを差し替えを中止しました"
    else
        exit 1
    fi
elif [ -f ${ZSHENV_DST_PATH} ]; then
    echo ".zshenv \t\t: シンボリックリンクではなく，ファイルとして.zshenvが存在します. .zshenvを削除してシンボリックリンクを作成しますか？ [yes/no]"
    read DELETE_ZSHENV_FILE

    if [ ${DELETE_ZSHENV_FILE} = 'yes' ]; then
        echo ".zshenv \t\t: .zshenvを削除します"
        rm ${ZSHENV_DST_PATH}

        echo ".zshenv \t\t: シンボリックリンクを作成します"
        ln -s ${ZSHENV_ORG_PATH} ${ZSHENV_DST_PATH}

        echo ".zshenv \t\t: シンボリックリンクの作成が完了しました" 
    elif [ ${DELETE_ZSHENV_FILE} = 'no' ]; then
        echo ".zshenv \t\t: シンボリックリンク作成を中止しました"
    else
        exit 1
    fi
else 
    echo ".zshenv \t\t: シンボリックリンクを作成します"
    ln -s ${ZSHENV_ORG_PATH} ${ZSHENV_DST_PATH}

    echo ".zshenv \t\t: シンボリックリンクの作成が完了しました" 
fi

## Emacs用
echo ".emacs.d \t\t: シンボリックリンク判定"
if [ -L ${EMACS_CONF_DST_PATH} ] ; then
    echo ".emacs.d \t\t: 既にシンボリックリンクが存在します．差し替えますか？ [yes/no]"
    read DELETE_EMACS_CONF

    if [ ${DELETE_EMACS_CONF} = 'yes' ]; then
        rm ${EMACS_CONF_DST_PATH}
        ln -s ${EMACS_CONF_ORG_PATH} ${EMACS_CONF_DST_PATH}
        echo ".emacs.d \t\t: シンボリックリンクの差し替えが完了しました"
    elif [ ${DELETE_EMACS_CONF} = 'no' ]; then
        echo ".emacs.d \t\t: シンボリックリンクを差し替えを中止しました"
    else
        exit 1
    fi
elif [ -d ${EMACS_CONF_DST_PATH} ]; then
    echo ".emacs.d \t\t: シンボリックリンクではなく，ディレクトリとして.emacs.dが存在します. .emacs.dを削除してシンボリックリンクを作成しますか？ [yes/no]"
    read DELETE_EMACS_CONF

    if [ ${DELETE_EMACS_CONF} = 'yes' ]; then
        echo ".emacs.d \t\t: .emacs.dを削除します"
        rm -r ${EMACS_CONF_DST_PATH}

        echo ".emacs.d \t\t: シンボリックリンクを作成します"
        ln -s ${EMACS_CONF_ORG_PATH} ${EMACS_CONF_DST_PATH}
        echo ".emacs.d \t\t: シンボリックリンクの作成が完了しました" 
    elif [ ${DELETE_EMACS_CONF} = 'no' ]; then
        echo ".emacs.d \t\t: シンボリックリンク作成を中止しました"
    else
        exit 1
    fi
else 
    echo ".emacs.d \t\t: シンボリックリンクを作成します"
    ln -s ${EMACS_CONF_ORG_PATH} ${EMACS_CONF_DST_PATH}
    echo ".emacs.d \t\t: シンボリックリンクの作成が完了しました" 
fi

exit 0
