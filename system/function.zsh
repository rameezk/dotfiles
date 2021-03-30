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

Cyan() {
    printf "\e[96m%s\e[0m\n" "$@"
}

Red() {
    printf "\e[31m%s\e[0m\n" "$@"
}

LightYellow() {
    printf "\e[93m%s\e[0m\n" "$@"
}

LightGreen() {
    printf "\e[92m%s\e[0m\n" "$@"
}

deploy_static_site_via_gh_pages() {
    project_name="$1"

    if [ -z "$project_name" ]; then
        Red "Please provide a project name"
        return 1
    fi

    if ! command -v hub >/dev/null 2>&1; then
        Red "hub binary does not exist. please install it."
        return 1
    fi

    Cyan "Creating a repo if it doesn't exist."
    git init
    git setup-personal-repo

    Cyan "Creating repo on github. Will open in browser."
    hub create -o

    Cyan "Creating CNAME"
    echo "$project_name.rameezkhan.dev" > CNAME

    Cyan "Pushing sources"
    git add .
    git commit -m "Creating site"
    git push
    git checkout gh-pages || git checkout -b gh-pages
    git publish
    git checkout master

    LightGreen "Done."
    LightGreen "If you want to enforce HTTPS, please check the box in your repo's settings."
    LightGreen "It can take up to 30 min for HTTPS to be available."
}
