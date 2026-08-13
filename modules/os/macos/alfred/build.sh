#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

build() {
	local src="$1"
	local name="$2"
	local out="${name}.alfredworkflow"
	rm -f "$out"
	(cd "$src" && zip -q -X "../${out}" info.plist)
	echo "built ${out}"
}

build keylights "Key Lights"
build launcher "launcher"
