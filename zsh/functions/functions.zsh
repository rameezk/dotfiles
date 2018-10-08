# Update Zsh plugins
uz(){
    antibody bundle <~/.dotfiles/zsh/plugins.txt >~/.zsh_plugins.sh
    antibody update
}
