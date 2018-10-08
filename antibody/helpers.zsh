# Update Zsh plugins
uz(){
    antibody bundle -p 4 <~/.dotfiles/zsh/plugins.txt >~/.zsh_plugins.sh
    antibody update -p 4
}
