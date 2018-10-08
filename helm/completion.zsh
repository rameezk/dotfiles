if which helm >/dev/null 2>&1; then
    echo "sourcing completions"
    source <(command helm completion zsh)
fi
