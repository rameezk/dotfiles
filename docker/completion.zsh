if which docker >/dev/null 2>&1; then

    if [ ! -d ~/.zsh/completion ]; then
        mkdir -p ~/.zsh/completion
    fi

    if [ ! -f ~/.zsh/completion/_docker ]; then
        curl -L https://raw.githubusercontent.com/docker/docker-ce/master/components/cli/contrib/completion/zsh/_docker > ~/.zsh/completion/_docker
    fi
fi

if which docker-compose >/dev/null 2>&1; then

    if [ ! -d ~/.zsh/completion ]; then
        mkdir -p ~/.zsh/completion
    fi

    if [ ! -f ~/.zsh/completion/_docker-compose ]; then
        curl -L https://raw.githubusercontent.com/docker/compose/master/contrib/completion/zsh/_docker-compose > ~/.zsh/completion/_docker-compose
    fi
fi
