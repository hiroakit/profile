#
# .zprofile
#
readonly local ZPROFILE_DEBUG_MODE=0

function get_this_file_path {
  local dir
  dir=$(dirname 0)
  echo ${dir%/}/.zprofile
}

if [ ${ZPROFILE_DEBUG_MODE} -gt 0 ]; then
    get_this_file_path 
fi

# Homebrew
#
# HOMEBREW_PREFIX などを設定する。brew --prefix の呼び出しはプロセス起動を
# 伴うため、既知の位置にある brew を直接見る。
for brew_bin in /opt/homebrew/bin/brew /usr/local/bin/brew; do
    if [ -x "${brew_bin}" ]; then
        eval "$("${brew_bin}" shellenv)"
        break
    fi
done
unset brew_bin

# direnv
# ディレクトリ移動のフックを張る必要があるため、遅延ロードできない。
if [ -x "${HOMEBREW_PREFIX}/bin/direnv" ]; then
    eval "$(direnv hook zsh)"
fi

# rbenv / pyenv / nodenv
#
# 各ツールの init は .zshrc で遅延ロードする。shims は .zshenv で
# PATH に入れているので、ruby/python/node の解決はここでは不要。
# ROOT 変数は他のツールが参照しうるので設定しておく。
export RBENV_ROOT=${HOME}/.rbenv
export PYENV_ROOT=${HOME}/.pyenv
export NODENV_ROOT=${HOME}/.nodenv

# $HOME/dev ... my resouces
# export PATH="$HOME/dev/bin:$PATH"
