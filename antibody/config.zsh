if [[ ! -x "$(command -v antibody)" ]]; then
    echo "[..] Antibody not installed. Installing..."
	  curl -sL https://git.io/antibody | sh -s
    echo "[..] Getting zsh plugins"
    antibody bundle <"$DOTFILES/antibody/bundles.txt" >~/.zsh_plugins.sh
    echo "[..] Updating zsh plugins"
    antibody update
fi
