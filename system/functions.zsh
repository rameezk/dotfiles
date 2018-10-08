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
