if [ -x "$(command -v tmux)" ] && [ ! -d  ~/.tmux/plugins/tpm ]; then
    echo "[..] Missing tmux plugin manager. Downloading..."
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
