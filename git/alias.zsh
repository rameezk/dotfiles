#!/bin/sh
if which hub >/dev/null 2>&1; then
	  alias git='hub'
fi

alias gfpp='git pull --prune'
alias glg="git log --graph --decorate --oneline --abbrev-commit"
alias glga="glg --all"
alias gp='git push origin HEAD'
alias gpa='git push origin --all'
alias gd='git diff'
alias gdc='git diff --cached'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gb='git branch -v'
alias ga='git fza'
alias gaa='git add -A'
alias gcm='git commit -m'
alias gcam='git commit -a -m'
alias gs='git status -sb'
# alias gpr='gp && git pr' #Removing to make pr's portal in ./function.zsh
alias glnext='git log --oneline $(git describe --tags --abbrev=0 @^)..@'
alias gclog="find . -name '*.orig' -delete"

if which svu >/dev/null 2>&1; then
	  alias gtpatch='echo `svu p`; git tag `svu p`'
	  alias gtminor='echo `svu m`; git tag `svu m`'
fi

gi() {
	  curl -Ls "https://www.gitignore.io/api/$*"
}
