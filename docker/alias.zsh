alias dc='docker-compose'
alias dcb='docker-compose build'
alias dcd="docker-compose down"
alias dcu='docker-compose up'
alias dcr='docker-compose run --rm'

# Docker behind a proxy, assumes http env vars are present
alias docker-build-behind-proxy='docker build --build-arg http_proxy=$http_proxy --build-arg https_proxy=$https_proxy'
