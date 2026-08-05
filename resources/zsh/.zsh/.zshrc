#
# .zshrc
#
readonly local ZSHRC_DEBUG_MODE=0

function get_this_file_path {
  local dir
  dir=$(dirname 0)
  echo ${dir%/}/.zshrc
}

if [ ${ZSHRC_DEBUG_MODE} -gt 0 ]; then
    get_this_file_path
fi

#-------------------------------------------
# Common
#-------------------------------------------
setopt no_beep              # ビープ音を鳴らさない
setopt ignore_eof           # Ctrl-D でログアウトしない
setopt auto_cd              # ディレクトリ名の入力だけで移動する
setopt auto_pushd           # ディレクトリ移動時、自動でディレクトリスタックに追加する
setopt pushd_ignore_dups    # ディレクトリスタックに重複を積まない
setopt pushd_minus          # cd -<TAB> の +/- を直感的な向きにする
setopt magic_equal_subst    # = 以降でも補完できるようにする
setopt interactive_comments # 対話シェルでも # 以降をコメントとして扱う
setopt long_list_jobs       # jobs をロングフォーマットで表示する
setopt no_flow_control      # Ctrl-S / Ctrl-Q によるフロー制御を無効にする
autoload zed                # zsh editorを読み込む

#-------------------------------------------
# Prompt
#-------------------------------------------
autoload -Uz colors && colors
setopt prompt_subst

PROMPT="%{$reset_color%}%% "

#-------------------------------------------
# Colors
#-------------------------------------------
export LSCOLORS=exfxcxdxbxegedabagaxex
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

#-------------------------------------------
# Completion
#-------------------------------------------
setopt auto_list            # 補完候補を一覧表示する
setopt auto_menu            # TABで順に補完候補を切り替える
setopt auto_param_slash     # 補完候補がディレクトリのとき、最後にスラッシュを追加する
setopt auto_param_keys      # カッコの対応も補完する
setopt list_packed          # 補完候補を詰めて表示する
setopt list_types           # 補完候補にファイルの種別も含める
setopt no_auto_remove_slash # パスの最後に付くスラッシュを自動で削除させない
setopt no_list_beep         # 補完候補表示時にビープ音を鳴らさせない
setopt print_eight_bit      # 補完時の日本語を正しく表示する
setopt always_to_end        # 補完時に文字列末尾へカーソル移動する
setopt complete_in_word     # 単語の途中でもカーソル位置で補完する

zstyle ':completion:*:default' menu select=1        # 補完候補のカーソル選択を有効にする
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # 補完時に大文字小文字を区別しない
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

# https://docs.brew.sh/Shell-Completion#configuring-completions-in-zsh
# HOMEBREW_PREFIX は .zprofile の brew shellenv が設定する。
FPATH="${HOMEBREW_PREFIX}/share/zsh/site-functions:${FPATH}"
fpath=(${ZDOTDIR}/functions/completion ${fpath})

autoload -Uz compinit && compinit

#-------------------------------------------
# History
#
# HISTFILE は macOS の /etc/zshrc が ${ZDOTDIR:-$HOME}/.zsh_history に
# 設定する。ここでも同じ式で明示しておく。${HOME} 直下を指すと
# ZDOTDIR 側に貯めた履歴と切り離されるので注意。
#-------------------------------------------
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=50000              # historyコマンド(メモリ上)で扱う最大件数
SAVEHIST=100000000          # HISTFILEに保存する履歴の件数

setopt append_history          # 履歴を追加する
setopt extended_history        # 履歴を時刻も付けて保存する
setopt inc_append_history      # コマンド実行の都度、履歴ファイルに保存する(標準はexit時)
setopt hist_no_store           # historyコマンド自体は履歴に保存しない
setopt hist_ignore_dups        # 直前と同じコマンドを履歴に追加しない
setopt hist_ignore_all_dups    # 重複するコマンドは古い方を削除して新しい方を残す
setopt hist_expire_dups_first  # 履歴が溢れるときは重複から先に捨てる
setopt hist_ignore_space       # 先頭がスペースのコマンドを保存対象外にする
setopt hist_reduce_blanks      # 余分なスペースを削除してから保存する
setopt hist_verify             # 履歴から選んだコマンドをすぐには実行しない
setopt share_history           # 複数のシェルで履歴を共有する

function history-all { history -E 1 } # 全履歴を出力する(.zalias の ha が使う)

autoload -Uz history-search-end # 入力済みの文字列にマッチする履歴を検索する
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

#-------------------------------------------
# Recent directories
#
# cdr でディレクトリ移動履歴を辿れるようにする。
# zaw-cdr がこの仕組みに依存している。
#-------------------------------------------
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

#-------------------------------------------
# Version managers
#
# rbenv init などは1回あたり40〜50ms かかる。初回に呼ばれるまで先送りし、
# 呼ばれた時点で本来の init に差し替える。
#
# shims は .zshenv で PATH に通してあるので、遅延させても ruby/python/node
# はバージョン管理下のものが起動直後から使われる。ここで遅らせているのは
# init が用意する補完と `rbenv shell` などのサブコマンドだけ。
#-------------------------------------------
rbenv()  { unfunction rbenv;  eval "$(command rbenv init - zsh)";  rbenv "$@" }
pyenv()  { unfunction pyenv;  eval "$(command pyenv init - zsh)";  pyenv "$@" }
nodenv() { unfunction nodenv; eval "$(command nodenv init - zsh)"; nodenv "$@" }

#-------------------------------------------
# Functions
#-------------------------------------------
[ -r ${ZDOTDIR}/functions/showpath.zsh ] && source ${ZDOTDIR}/functions/showpath.zsh

#-------------------------------------------
# Aliases
#-------------------------------------------
[ -r ${ZDOTDIR}/.zalias ] && source ${ZDOTDIR}/.zalias
setopt complete_aliases

#-------------------------------------------
# zaw
# https://github.com/zsh-users/zaw
#-------------------------------------------
export ZAWZSH=${ZDOTDIR}/zaw/zaw.zsh
if [ -r ${ZAWZSH} ]
then
   source ${ZAWZSH}

   zstyle ':filter-select' case-insensitive yes
   bindkey '^h'   zaw-history # コマンド履歴一覧を表示
   bindkey '^h^h' zaw-cdr     # 素早く押すとディレクトリ移動履歴一覧を表示

   # 全ソースの一覧から選ぶ。git-status や process などはここから辿る。
   # zaw-select-src は選ばせるだけで実行しないので、zaw を割り当てる。
   #
   # 既定の accept-line-and-down-history を潰しているが、履歴を順に
   # 実行し直す用途で使っていないため。^@ (Ctrl+Space) は macOS の
   # 入力ソース切り替えと取り合いになるので避けた。
   bindkey '^o'   zaw
fi

#-------------------------------------------
# Key bindings
#-------------------------------------------
bindkey -e
bindkey "\e[Z" reverse-menu-complete               # 補完候補表示時、Shift-Tabで逆順に移動する
bindkey "^p" history-beginning-search-backward-end # ヒストリ検索時、Ctrl-pで戻る
bindkey "^n" history-beginning-search-forward-end  # ヒストリ検索時、Ctrl-nで進む

# 空行での Ctrl-D を握り潰す。
#
# ignore_eof はログアウトこそ防ぐが、代わりに毎回
# "zsh: use 'logout' to logout." を出す。この文言だけを黙らせる
# オプションは zsh に無いため、EOF がシェルへ届く前に ZLE 側で捨てる。
# ignore_eof 自体は、この widget が効かない経路への保険として残す。
#
# 入力中の Ctrl-D は既定の delete-char-or-list に流し、
# 前方削除と補完候補表示をそのまま使えるようにする。
function ignore-eof-silently {
    if [[ -n $BUFFER ]]; then
        zle delete-char-or-list
    fi
    return 0
}
zle -N ignore-eof-silently
bindkey "^d" ignore-eof-silently

#-------------------------------------------
# Claude Code command history
# Claudeが実行したコマンドの履歴を検索する
# (zsh本来の履歴とは別ファイルで管理する)
#-------------------------------------------
[ -e ${ZDOTDIR}/.zclaude ] && source ${ZDOTDIR}/.zclaude

#-------------------------------------------
# fzf
# .zprofileに設定するとCTRL-Rがbck-i-searchのままで
# fzfのCTRL-Rが適用されないため.zshrcに記述した
#-------------------------------------------
if [ -x "${HOMEBREW_PREFIX}/bin/fzf" ]; then
    source <(fzf --zsh)
fi

#-------------------------------------------
# PATH
#
# /etc/zprofile の path_helper が PATH を組み直すとき、.zshenv で並べた
# 順序は保たれず、環境によっては落ちる。優先させたいものは起動シーケンスの
# 最後であるここで積み直す。
#
# shims を通しておくことで、バージョン管理ツールの init を遅延させても
# ruby/python/node は起動直後から管理下のものが使われる。
#-------------------------------------------
path=($my_path_head $path $my_path_tail)
typeset -U path PATH

#------------------------------------------------------------
# Local configuration
#------------------------------------------------------------
[ -e ${ZDOTDIR}/.zshrc.mine ] && source ${ZDOTDIR}/.zshrc.mine
