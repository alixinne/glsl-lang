#!/bin/bash

CHECK_MODE=0

while getopts ":c" opt; do
	case "$opt" in
		c)
			CHECK_MODE=1
			;;
		*)
			;;
	esac
done

make_readme () {
	if [ -f 'README.tpl' ]; then
		cargo readme -t README.tpl "$@"
	else
		cargo readme -t '../README.tpl' "$@"
	fi
}

EXIT_CODE=0

if [ $CHECK_MODE -gt 0 ]; then
	process_readme () {
		local D="$(basename "$(pwd)")"
		printf "% 20s " "$D" >&2

		if [[ "$D" == xtask ]]; then
			exit 0
		fi

		if diff <(make_readme) README.md >/dev/null 2>&1; then
			echo ok
			exit 0
		else
			echo not ok
			exit 1
		fi
	}
else
	process_readme () {
		local D="$(basename "$(pwd)")"
		echo $D >&2

		if [[ "$D" == xtask ]]; then
			exit 0
		fi

		make_readme -o README.md
	}
fi

export -f make_readme
export -f process_readme
cargo workspaces exec --no-bail /bin/bash -c process_readme

exit $EXIT_CODE
