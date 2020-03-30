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
