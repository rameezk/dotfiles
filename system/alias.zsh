#!/bin/sh

# editors
if which emacsclient >/dev/null 2>&1; then
    alias e='emacsclient --no-wait'
    alias E="SUDO_EDITOR=\"emacsclient\" sudo -e"
fi

if command -v nvim >/dev/null 2>&1; then
    export EDITOR=nvim
    export VISUAL=nvim
elif command -v vim >/dev/null 2>&1; then
    export EDITOR=vim
    export VISUAL=vim
elif command -v vi >/dev/null 2>&1; then
    export EDITOR=vi
    export VISUAL=vi
fi

# paging
PAGER=LESS

# modern make
if which mmake >/dev/null 2>&2; then
	alias make='mmake'
fi

# exa is a better ls tool
if which exa >/dev/null 2>&1; then
	alias ls='exa'
	alias l='exa -la --git'
	alias la='exa -laa --git'
	alias ll='exa -l --git'
  alias tree='ls --tree'
else
	if [ "$(uname -s)" = "Darwin" ]; then
		alias ls="ls -FG"
	else
		alias ls="ls -F --color"
	fi
	alias l="ls -lAh"
	alias la="ls -A"
	alias ll="ls -l"
fi

alias grep="grep --color=auto"
alias duf="du -sh * | sort -hr"
alias less="less -r"

# quick hack to make watch work with aliases
alias watch='watch '

# open, pbcopy and pbpaste on linux
if [ "$(uname -s)" != "Darwin" ]; then
	if [ -z "$(command -v pbcopy)" ]; then
		if [ -n "$(command -v xclip)" ]; then
			alias pbcopy="xclip -selection clipboard"
			alias pbpaste="xclip -selection clipboard -o"
		elif [ -n "$(command -v xsel)" ]; then
			alias pbcopy="xsel --clipboard --input"
			alias pbpaste="xsel --clipboard --output"
		fi
	fi
	if [ -e /usr/bin/xdg-open ]; then
		alias open="xdg-open"
	fi
fi

# use trash util for safer deleting
if which trash >/dev/null 2>&1; then
    alias rm='trash'
fi

# directory navigation
alias grt='cd $(git rev-parse --show-toplevel)'
alias ..='cd ..'


# bat, a better cat
if which bat >/dev/null 2>&1; then
    export BAT_THEME="TwoDark"
    alias cat='bat'
fi

# check what processes are stopping macos from sleeping
alias why_cant_i_sleep='pmset -g assertions'

# get my outgoing IP
alias ip_plez='curl https://canihazip.com/s'
