mcd() {
    mkdir -p "$1" && cd "$1";
}

fid() {
    if [ -z "$1" ]; then
        echo "Usage: fid <expression>"
    else
        find . -iname "$1"
    fi
}

fix_clock_manually() {
	sudo date -s "$(wget --no-cache -S -O /dev/null google.com 2>&1 | sed -n -e '/  *Date: */ {' -e s///p -e q -e '}')"
}

whois_on_port() {
    sudo lsof -i :$1
}

xorg-switch-to-4k() {
    if [ -f ~/.Xresources.4k-27inch.deactivated ]; then
            echo "[..] Adding xorg config for 4k screen..."
            mv ~/.Xresources.4k-27inch.deactivated ~/.Xresources
            echo "[..] Done..."
            echo "[..] Please reboot now..."
    else
            echo "[..] Already switched"
    fi
}

xorg-switch-to-1k() {
    if [ -f ~/.Xresources ]; then
            echo "[..] Adding xorg config for 1k screen..."
            mv ~/.Xresources ~/.Xresources.4k-27inch.deactivated
            echo "[..] Done..."
            echo "[..] Please reboot now..."
    else
            echo "[..] Already switched"
    fi
}

b64_encode() {
    to_encode="$1"

    encoded=$(echo -n "$to_encode" | base64 -w0)

    echo "Encoded: [$encoded]"

    echo "$encoded" | pbcopy

    echo "Copied to clipboard."
}

b64_decode() {
    to_decode="$1"

    decoded=$(echo "$to_decode" | base64 -d -w0)

    echo "Decoded: [$decoded]"

    echo "$decoded" | pbcopy

    echo "Copied to clipboard."
}

unsource_exports() {
    file="$1"

    if [ -z "$file" ]; then
        echo "Please specify path to file"
        return 1
    fi

    unset $(awk -F'[ =]+' '/^export/{print $2}' "$file")
}

create_tcp_tunnel() {
    remote_host=$1
    remote_port=$2
    local_port=$3
    
    echo "This will create a TCP tunnel via a Kubernetes Pod using netcat"
    echo ""

    echo "Checking if Kubernetes pod is available"
    if kubectl -n default get pod debuggery; then
        if [ -z "$remote_host" ]; then
            echo "Remote host? "
            read remote_host
        fi

        if [ -z "$remote_port" ]; then
            echo "Remote port? "
            read remote_port
        fi


        if [ -z "$local_port" ]; then
            echo "Local host? "
            read local_port
        fi

        echo ""

        echo "remote_host = $remote_host"
        echo "remote_port = $remote_port"
        echo "local_port = $local_port"

        echo ""
        echo "Attempting to create tunnel. Don't expect any output."

        tcpserver 127.0.0.1 "$local_port" kubectl -n default exec -i debuggery -- nc "$remote_host" "$remote_port"
    else
        echo "Kubernetes pod not found. Please run kdebug in the default namespace in another window. Exiting."
        return 1
    fi
}
