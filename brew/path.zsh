# Check for linux brew
if [[ "$(uname)" == "Linux" ]] && [[ -d "/home/linuxbrew/.linuxbrew/bin" ]]; then
	export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
fi
