#
# .zprofile
# macOS https://qiita.com/github0013@github/items/c910ebb47d9be0a45282
#

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
