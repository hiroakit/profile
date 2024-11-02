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
if [ -d /opt/homebrew/bin ]; then
   export PATH="$RBENV_ROOT/bin:$PATH"
   eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# direnv
if [ -e $(brew --prefix)/bin/direnv ]; then
    eval "$(direnv hook zsh)"    
fi

# fzf
if [ -e $(brew --prefix)/bin/fzf ]; then
    source <(fzf --zsh)
fi

## rbenv
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

# nodenv
export NODENV_ROOT=${HOME}/.nodenv
if [ -L ${NODENV_ROOT} -o -d ${NODENV_ROOT} ]; then
   eval "$(nodenv init -)"
fi

# $HOME/dev ... my resouces
# export PATH="$HOME/dev/bin:$PATH"
