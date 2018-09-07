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

# Productivity {{{
  antigen bundle command-not-found
# }}}

# Apply antigen {{{
	antigen apply
# }}}

# Export PATH {{{
	# Export it only once for better performance
	export PATH
# }}}

# vim: set foldmethod=marker foldlevel=0 foldcolumn=3 textwidth=78:
