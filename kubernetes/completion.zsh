if [ -x "$(command -v kubectl)" ]; then
    source <(command kubectl completion zsh)
fi
