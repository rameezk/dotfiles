if [ -x "$(command -v kubectl)" ]; then
    # source <(kubectl completion zsh) ## workaround below
    kubectl completion zsh > ~/.zsh/completion/_kubectl
fi
