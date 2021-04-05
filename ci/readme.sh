#!/bin/bash

. ci/vars

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
	for D in "${GLSL_DIRS[@]}"; do
		printf "% 20s " "$D" >&2

		if (cd $D && diff <(make_readme) README.md >/dev/null 2>&1); then
			echo ok
		else
			echo not ok
			EXIT_CODE=$((EXIT_CODE + 1))
		fi
	done
else
	for D in "${GLSL_DIRS[@]}"; do
		echo $D >&2

		(
			cd $D
			make_readme -o README.md
		)
	done
fi

exit $EXIT_CODE
