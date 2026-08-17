#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

wf_dir="$HOME/Library/Application Support/Alfred/Alfred.alfredpreferences/workflows"

if [ ! -d "$wf_dir" ]; then
	echo "alfred: workflows directory not found, skipping ($wf_dir)"
	exit 0
fi

changed=0

for dir in */; do
	name="${dir%/}"
	[ -f "$name/info.plist" ] || continue

	src="$PWD/$name"
	link="$wf_dir/user.workflow.dotfiles-$name"

	if [ -L "$link" ] && [ "$(readlink "$link")" = "$src" ]; then
		continue
	fi

	if [ -e "$link" ] && [ ! -L "$link" ]; then
		echo "alfred: refusing to replace non-symlink $link" >&2
		continue
	fi

	ln -sfn "$src" "$link"
	echo "alfred: linked $name"
	changed=1
done

for link in "$wf_dir"/user.workflow.dotfiles-*; do
	[ -L "$link" ] || continue
	if [ ! -e "$link" ]; then
		rm -f "$link"
		echo "alfred: pruned stale link $(basename "$link")"
		changed=1
	fi
done

if [ "$changed" -eq 1 ]; then
	echo "alfred: reloading"
	alfred_bundle_id="com.runningwithcrayons.Alfred"
	/usr/bin/osascript -e "tell application id \"$alfred_bundle_id\" to quit" >/dev/null 2>&1 || true
	sleep 1
	/usr/bin/open -b "$alfred_bundle_id" >/dev/null 2>&1 || true
fi
