#!/usr/local/bin/zsh

######################################################
#
# マウント元とマウント先の確認
#
######################################################
echo -n "[ iTunes Mounter \e[31m確認\e[0m ] : A)勤務先のiTunesをマウントしますか？ B)自宅のiTunesをマウントしますか？ [A/B] "
read SELECT_iTunes
if [ ${SELECT_iTunes} = 'a' -o ${SELECT_iTunes} = 'A' ]; then
    echo "[ iTunes Mounter 通知 ] : 勤務先のiTunesをマウントする準備を開始します..."
    SERVER_HOST_NAME="192.168.0.22"
	SERVER_DIRECTORY="/Engineering/Members/hiroaki/iTunes"
elif [ ${SELECT_iTunes} = 'b' -o ${SELECT_iTunes} = 'B' ]; then
    echo "[ iTunes Mounter 通知 ] : 自宅のiTunesをマウントする準備を開始します..."
    SERVER_HOST_NAME="192.168.2.102"
	SERVER_DIRECTORY="/disk/Hiroaki/iTunes/"    
else
    echo "[ iTunes Mounter \e[31mN G\e[0m  ] : マウント処理を中断しました．"
    exit 1
fi

CLIENT_HOST_NAME="${HOST}"
CLIENT_DIRECTORY="${HOME}/Music/iTunes"

echo "[ iTunes Mounter 通知 ] CLIENT_HOST_NAME: ${CLIENT_HOST_NAME}"
echo "[ iTunes Mounter 通知 ] CLIENT_DIRECTORY: ${CLIENT_DIRECTORY}"
echo "[ iTunes Mounter 通知 ] SERVER_HOST_NAME: ${SERVER_HOST_NAME}"
echo "[ iTunes Mounter 通知 ] SERVER_DIRECTORY: ${SERVER_DIRECTORY}"
echo -n "[ iTunes Mounter \e[31m確認\e[0m ] : ${SERVER_HOST_NAME} の ${SERVER_DIRECTORY} を ${CLIENT_HOST_NAME} の ${CLIENT_DIRECTORY} にマウントしますか？ [y/n] "
read ANSWER

if [ ${ANSWER} = 'y' -o ${ANSWER} = 'yes' ]; then
    echo "[ iTunes Mounter 通知 ] : マウント処理を開始します..."
else  
    echo "[ iTunes Mounter \e[31mN G\e[0m  ] : マウント処理を中断しました．"
    exit 1
fi

######################################################
#
# ユーザ認証
#
######################################################
echo -n "[ iTunes Mounter \e[31m確認\e[0m ] : ユーザ名を入力 "
read USER                       # ユーザ名を受け付ける

echo -n "[ iTunes Mounter \e[31m確認\e[0m ] : ${USER}, パスワードを入力 "
stty -echo                      # エコーバックを許可しない (sttyはCUI表示設定を変更する)
read password                   # パスワードを受け付ける
stty echo                       # エコーバックを許可する

######################################################
#
# クライアントにマウントディレクトリがあるか確認する
#
######################################################
echo "[ iTunes Mounter 通知 ] : マウント用のディレクトリを作成します..."
if [ ! -e ${CLIENT_DIRECTORY} ]; then
   echo "mkdir -p ${CLIENT_DIRECTORY}"
   mkdir -p ${CLIENT_DIRECTORY}
fi

######################################################
#
# クライアントに対象のディレクトリをマウントする
#
######################################################
echo "[ iTunes Mounter 通知 ] : マウント中..."
echo "mount_smbfs //${USER}:${password}@${SERVER_HOST_NAME}${SERVER_DIRECTORY} ${CLIENT_DIRECTORY} >/dev/null 2>&1"
mount_smbfs //${USER}:${password}@${SERVER_HOST_NAME}${SERVER_DIRECTORY} ${CLIENT_DIRECTORY} >/dev/null 2>&1
if [ $? -eq 0 ]; then
    echo -e "[ iTunes Mounter \e[32mO K\e[0m  ] : マウントに成功．" # 正常終了の場合
else  
    echo -e "[ iTunes Mounter \e[31mN G\e[0m  ] : マウントに失敗．" # 異常終了の場合
fi
