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
