## .zshrc

# # The Z Shell Manual v5.8
# # http://zsh.sourceforge.net/Doc/zsh_us.pdf
# #
# # 13.2.5 Visual effects
# # 理解できていないもの
# # %E
# # %S (%s)
# # %{...%}
# # %G

# echo "hoge1"

# export LSCOLORS=exfxcxdxbxegedabagaxex
# #export LSCOLORS=gxfxcxdxbxegexabagacad
# export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# autoload -Uz compinit && compinit
# autoload -Uz colors && colors
# [[ $COLORTERM = *(24bit|truecolor)* ]] || zmodload zsh/nearcolor

# zstyle ':completion:*' list-colors "${LS_COLORS}"

# PROMPT="%B%F{green}%n@%m%b%f %~ %# " 

# #--------------------------
# # Command aliases
# #--------------------------
# if [ -r $ZDOTDIR/.zalias ]; then
#    source $ZDOTDIR/.zalias
# fi




#-------------------------------------------
# Common
#-------------------------------------------

setopt no_beep           # ビープ音を鳴らさない
setopt ignore_eof        # ctr-d でログアウトしない
setopt auto_cd           # ディレクトリ名の入力だけ移動可能にする
setopt auto_pushd        # ディレクトリ移動時、自動でディレクトリスタックに追加する
# setopt correct           # 入力したコマンドのミスを指摘する
# setopt correct_all       # 入力内容全て(ファイル名含む)を判断対象とする
setopt magic_equal_subst # = 以降でも補完できるようにする

autoload zed             # zsh editorを読み込む

case "${OSTYPE}" in
    # macOS
    darwin*)
	# Emacsが起動しており、M-x server-startを実行済みであると想定している
	export EDITOR="/Applications/Emacs-takaxp/Emacs.app/Contents/MacOS/Emacs"

	# XcodeのDerivedDataディレクトリ
	export XCODE_DERIVED_DATA="${HOME}/Library/Developer/Xcode/DerivedData"

	# iOSシミュレーターのデータがあるディレクトリ
	export XCODE_SIMULATOR_DATA="${HOME}/Library/Developer/CoreSimulator/Devices"

	# 様々なバージョンのFBXのSDKが格納されるディレクトリ
	export FBX_SDK_HOME="/Applications/Autodesk/FBX SDK"

	# node.js v12 for Azure Function Runtime
	export PATH="/usr/local/opt/node@12/bin:$PATH"
	
    # Projectsディレクトリ
    export PROJ="${HOME}/Documents/Projects"
    export BLOG="${HOME}/Documents/Projects/Personal/Blog"
    export TIPS="${HOME}/Documents/Projects/Personal/tips"
    export EDEV="${HOME}/Documents/Projects/Personal/emacs-on-apple"
esac

#---------------------------------
# シェルの標準設定
# prompt設定(着色)
#---------------------------------

export TERM=xterm-256color

autoload colors
colors
setopt prompt_subst
 
case ${UID} in
0)
    PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') %{${fg[red]}%}%n@%m%#%{${reset_color}%} "
    PROMPT2="%B%{${fg[red]}%}%_#%{${reset_color}%}%b "
    SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
    RPROMPT="%{${fg[green]}%}[%~:%T]%{${reset_color}%}"
    ;;
*)
    # %n : ユーザ名
    # %m : 短いホスト名

    PROMPT="%{${fg_bold[red]}%}%n%%%{${reset_color}%} "
    PROMPT2="%{${fg[red]}%}%_%%%{${reset_color}%} "
    SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
    RPROMPT="%{${fg_bold[black]}%}[%~ %T]%{${reset_color}%}"
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
    ;;
esac

# ls
# LSCOLORS: BSD ls
# LS_COLORS: GNU ls
export LSCOLORS=exfxcxdxbxegedabagaxex
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# プロンプトを表示する直前に呼ばれるHook関数
precmd() {
    # タイトル欄を user@hostname にする
    echo -ne "\033]0;${USER}@${HOST%%.*}\007"
}
 
#-------------------------------------------
# Complement
#-------------------------------------------

setopt auto_list         # 補完候補を一覧表示する
setopt auto_menu         # TABで順に保管候補を切り替える
setopt auto_param_slash  # 補完候補がディレクトリのとき、最後にスラッシュを追加する
setopt auto_param_keys   # カッコの対応も補完する
setopt list_packed       # 補完候補を詰めて表示する
setopt list_types        # 補完候補にファイルの拡張子も含める
setopt noautoremoveslash # パスの最後に付くスラッシュを自動で削除させない
setopt nolistbeep        # 補完候補表示時にビープ音を鳴らさせない
setopt print_eight_bit   # 補完時の日本語を正しく表示する
setopt always_to_end     # 補完時に文字列末尾へカーソル移動

# setopt extended_glob
# unsetopt caseglob      # 大文字小文字の区別にファイルグロブを使わない
 
zstyle ':completion:*:default' menu select=1        # 保管候補のカーソル選択を有効にする
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # 補完時に大文字小文字を区別しない
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

# 補完候補の増やすためのディレクトリをfpathに追加
fpath=(${ZDOTDIR}/functions/completion ${fpath})
autoload -U compinit; compinit

#-------------------------------------------
# History
#-------------------------------------------

HISTFILE=${HOME}/.zsh_history # ヒストリの保存先
HISTSIZE=10000                # historyコマンド(メモリ上)で表示する最大件数
SAVEHIST=100000000            # HISTFILEで指定したファイルに保存する履歴の件数

setopt append_history         # 履歴を追加する
setopt extended_history       # 履歴を時刻も付けて保存する
setopt inc_append_history     # コマンド実行後に履歴ファイルに保存する(標準はexit時)
setopt hist_no_store          # historyコマンドは履歴ファイルに保存しない
setopt hist_ignore_dups       # 直前と同じコマンドをヒストリに追加しない
setopt share_history          # 履歴を共有する 
setopt hist_ignore_space      # 先頭がスペースで始まる履歴は保存対象外にする(厳密には履歴ファイルの中に記録したあとに整理され削除される仕組み)
setopt hist_ignore_all_dups   # 重複するコマンドは古いものを削除して，新しい方を履歴ファイルに残す
setopt hist_reduce_blanks     # 余分なスペースを削除してから履歴に保存
setopt hist_verify            # ヒストリからコマンドを選んでも，すぐに実行しない

function history-all { history -E 1 } # 全履歴の一覧を出力 

autoload history-search-end   # マッチしたコマンドのヒストリを検索する
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end


#-------------------------------------------
# cdr
#-------------------------------------------

autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both


#-------------------------------------------
# MyZaw ; zaw custom
#-------------------------------------------

export ZAWZSH=${ZDOTDIR}/zaw/zaw.zsh
if [ -r ${ZAWZSH} ]
then
   source ${ZAWZSH}

   zstyle ':filter-select' case-insensitive yes
   bindkey '^h'   zaw-history # コマンド履歴一覧を表示
   bindkey '^h^h' zaw-cdr     # 素早く押すとディレクトリ移動履歴一覧を表示
   bindkey '^@'   zaw-gitdir
fi
  

#-----------------------------
# Key binding (Emacs like)
# How to check ? -> $ bindkey
#-----------------------------
bindkey -e
bindkey "\e[Z" reverse-menu-complete               # 補完候補表示時、Shift-Tabでの移動を可能にする
bindkey "^p" history-beginning-search-backward-end # ヒストリ検索時、Ctrl-pで戻る
bindkey "^n" history-beginning-search-forward-end  # ヒストリ検索時、Ctrl-nで進む


#--------------------------
# Command aliases
#--------------------------
if [ -r $ZDOTDIR/.zalias ]
then
   source $ZDOTDIR/.zalias
fi
setopt complete_aliases


#------------------------------------------------------------
# Local configuration if needed
#------------------------------------------------------------
[ -f ${HOME}/.zshrc.mine ] && source ${HOME}/.zshrc.mine
