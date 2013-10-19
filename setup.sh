#!/bin/sh

readonly EMACS_CONF_DIR=${HOME}/.emacs.d
readonly EMACS_CONF_ORG=$(cd $(dirname ${0});pwd)/emacs

echo "シンボリックリンクを作成します"
echo ""

echo ".emacs.d \t\t: シンボリックリンク判定"
if [ -L ${EMACS_CONF_DIR} ] ; then
    echo ".emacs.d \t\t: 既にシンボリックリンクが存在します．差し替えますか？ [yes/no]"
    read DELETE_EMACS_CONF_DIR

    if [ ${DELETE_EMACS_CONF_DIR} = 'yes' ]; then
        rm ${EMACS_CONF_DIR}
        ln -s ${EMACS_CONF_ORG} ${EMACS_CONF_DIR}
        echo ".emacs.d \t\t: シンボリックリンクの差し替えが完了しました"
    elif [ ${DELETE_EMACS_CONF_DIR} = 'no' ]; then
        echo ".emacs.d \t\t: シンボリックリンクを差し替えを中止します"
    else
        exit 1
    fi
elif [ -d ${EMACS_CONF_DIR} ]; then
    echo ".emacs.d \t\t: シンボリックリンクではなく，ディレクトリとして.emacs.dが存在します. .emacs.dを削除してシンボリックリンクを作成しますか？ [yes/no]"
    read DELETE_EMACS_CONF_DIR

    if [ ${DELETE_EMACS_CONF_DIR} = 'yes' ]; then
        echo ".emacs.d \t\t: .emacs.dを削除します"
        rm -r $HOME/.emacs.d

        echo ".emacs.d \t\t: シンボリックリンクを作成します"
        ln -s ${EMACS_CONF_ORG} ${EMACS_CONF_DIR}
        echo ".emacs.d \t\t: シンボリックリンクの作成が完了しました" 
    elif [ ${DELETE_EMACS_CONF_DIR} = 'no' ]; then
        echo ".emacs.d \t\t: シンボリックリンク作成をスキップします"
    else
        exit 1
    fi
else 
    echo ".emacs.d \t\t: シンボリックリンクを作成します"
    ln -s ${EMACS_CONF_ORG} ${EMACS_CONF_DIR}
    echo ".emacs.d \t\t: シンボリックリンクの作成が完了しました" 
fi

exit 0
