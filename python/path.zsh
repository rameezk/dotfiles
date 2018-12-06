PATH="/Users/rameezk/.pyenv:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# Setup virtualenv home
export WORKON_HOME=$HOME/.virtualenvs
source /Users/rameezk/.pyenv/versions/3.7.0/bin/virtualenvwrapper.sh

# Tell pyenv-virtualenvwrapper to use pyenv when creating new Python environments
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
