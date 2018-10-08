if which az >/dev/null 2>&1; then

    if [ ! -d ~/.azure ]; then
        mkdir -p ~/.azure
    fi

    if [ ! -f ~/.azure/az.completion ]; then
        pushd ~/.azure
        curl -O https://raw.githubusercontent.com/Azure/azure-cli/dev/az.completion
        source ~/.azure/az.completion
        popd
    else
        source ~/.azure/az.completion
    fi
fi
