PATH="/Users/rameezk/.pyenv:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# Setup virtualenv home
export WORKON_HOME=$HOME/.virtualenvs

if which pipenv >/dev/null 2>&1; then
	# If neccessary, do some pipenv stuff here
else
	source /Users/rameezk/.pyenv/versions/3.7.0/bin/virtualenvwrapper.sh
fi

# Tell pyenv-virtualenvwrapper to use pyenv when creating new Python environments
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"

#### Poetry
if [ -d "$HOME/.poetry" ]; then
    PATH="$PATH:$HOME/.poetry/bin"
fi
