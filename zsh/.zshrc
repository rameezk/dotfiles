#
#  _____                               _                 _
# |  __ \                             ( )               | |
# | |__) |__ _ _ __ ___   ___  ___ ___|/ ___     _______| |__  _ __ ___
# |  _  // _` | '_ ` _ \ / _ \/ _ \_  / / __|   |_  / __| '_ \| '__/ __|
# | | \ \ (_| | | | | | |  __/  __// /  \__ \  _ / /\__ \ | | | | | (__
# |_|  \_\__,_|_| |_| |_|\___|\___/___| |___/ (_)___|___/_| |_|_|  \___|
#
#

# Load antigen {{{
source ~/.dotfiles/antigen/antigen.zsh
# }}}

# Appearance {{{
antigen theme https://github.com/denysdovhan/spaceship-prompt spaceship

# Syntax highlighting {{{
antigen bundle zsh-users/zsh-syntax-highlighting
# }}}
# }}}

# Completion {{{
antigen bundle zsh-users/zsh-completions

# Initialize the completion system
autoload -Uz compinit
compinit

zmodload -i zsh/complist

unsetopt menu_complete
unsetopt flowcontrol
setopt auto_menu
setopt complete_in_word
setopt always_to_end

setopt auto_list
setopt no_list_beep

# Completions are case- and hypen-insensitive, and do substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Use the menu select style
zstyle ':completion:*:*:*:*:*' menu select
bindkey '^[[Z' reverse-menu-complete # SHIFT-TAB to go back

# Color completions when they’re files
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Colors for processes in kill
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
# List all processes owned by current user
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"

# Disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path "$HOME/.cache/zsh"

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
adm amanda apache at avahi avahi-autoipd beaglidx bin cacti \
canna clamav daemon dbus distcache dnsmasq dovecot fax ftp games \
gdm gkrellmd gopher hacluster haldaemon halt hsqldb ident \
junkbust kdm ldap lp mail mailman mailnull man messagebus \
mldonkey mysql nagios named netdump news nfsnobody nobody nscd \
ntp nut nx obsrun openvpn operator pcap polkitd postfix postgres \
privoxy pulse pvm quagga radvd rpc rpcuser rpm rtkit scard \
shutdown squid sshd statd svn sync tftp usbmux uucp vcsa wwwrun \
xfs '_*'
# … unless we really want to.
zstyle '*' single-ignored show

expand-or-complete-with-dots() {
    # toggle line-wrapping off and back on again
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti rmam
    print -Pn "%{%F{blue}……⌛%f%}"
    [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti smam
    zle expand-or-complete
    zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots
# }}}

# Behaviour {{{
# Make sure we can register hooks
autoload -Uz add-zsh-hook

# No autocorrect, thank you {{{
unsetopt correct_all
# }}}

# Show time for long commands {{{
REPORTTIME=5
TIMEFMT="%U user %S system %P cpu %*Es total"
# }}}

# History search {{{
#antigen bundle zsh-users/zsh-history-substring-search zsh-history-substring-search.zsh
antigen bundle zsh-users/zsh-history-substring-search
#bindkey -M vicmd 'k' history-substring-search-up
#bindkey -M vicmd 'j' history-substring-search-down
# }}}

# Change directories more easily {{{
setopt auto_cd
# }}}

# Configure history {{{
export HISTSIZE=100000 SAVEHIST=100000 HISTFILE=~/.zhistory
setopt share_history
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_verify
# }}}

# }}}

# Command, functions and aliases {{{

# EDITOR, VISUAL, and PAGER {{{
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
# ‘e’ for ‘edit’
#alias e="$VISUAL"
#alias e='open -a Emacs.app'
alias e='emacsclient --no-wait'

PAGER=less
# }}}

# Auto brackets and quotes {{{
antigen bundle hlissner/zsh-autopair
# }}}

# Aliases {{{
antigen bundle djui/alias-tips
autoload -U is-at-least	# needed for the common-aliases plugin
antigen bundle common-aliases
alias ..='cd ..'
alias ~='cd ~'
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'
# }}}

# Create and enter directory {{{
mcd() {
    mkdir -p "$1" && cd "$1";
}
# }}}

# Alt-left to go back {{{
cdUndoDir() {
    popd
    zle reset-prompt
    echo
    ls -l
    zle reset-prompt
}
zle -N cdParentDir
bindkey '^[[1;3A' cdParentDir
# }}}

# Alt-up to go to parent directory {{{
cdParentDir() {
    pushd ..
    zle reset-prompt
    echo
    ls -l
    zle reset-prompt
}
zle -N cdUndoDir
bindkey '^[[1;3D' cdUndoDir
# }}}

# git {{{
alias g='git'
alias lzg='lazygit'
alias g-all-st='find . -maxdepth 1 -mindepth 1 -type d -exec sh -c "(echo {} && cd {} && git status -s && echo)" \;'
alias g-all-pull='find . -maxdepth 1 -mindepth 1 -type d -exec sh -c "(echo {} && cd {} && git pull && echo)" \;'
# }}}

# vim {{{
alias vim='nvim'
# }}}

# kubernetes {{{
alias kc='kubectl'

  function kcssh() {
    kubectl -n $1 exec -it $2 -- bash
  }
# }}}

# helm {{{
antigen bundle helm
# }}}

# docker {{{
antigen bundle docker
antigen bundle docker-compose
# }}}

# pyenv {{{
  PATH="/Users/username/.pyenv:$PATH"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
# }}}

# bat {{{
  alias cat='bat'
# }}}

# exa {{{
  alias ls='exa'
# }}}

# history {{{
  antigen bundle zdharma/history-search-multi-word
# }}}

# watch {{{
  alias watch='watch '
# }}}
# }}}

# Apply antigen {{{
antigen apply
# }}}

# Hack {{{
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE=1
# }}}

# Export vars {{{
# Export it only once for better performance
export NVM_DIR="$HOME/.nvm"
. "/usr/local/opt/nvm/nvm.sh"
export PATH
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
# }}}

# vim: set foldmethod=marker foldlevel=0 foldcolumn=3 textwidth=78:
