#!/bin/sh
test -d /usr/local/opt/fzf/shell || return 0

export FZF_COMPLETION_TRIGGER='**'

if [[ "$(uname)" == "Linux" ]] && [[ -d "/home/linuxbrew/.linuxbrew/bin" ]]; then
	# shellcheck disable=SC1091
	. /home/linuxbrew/.linuxbrew/opt/fzf/shell/completion.zsh
fi

if [[ "$(uname)" == "Darwin" ]] && [[ -d "/home/linuxbrew/.linuxbrew/bin" ]]; then
	# shellcheck disable=SC1091
	. /usr/local/opt/fzf/shell/completion.zsh
fi
