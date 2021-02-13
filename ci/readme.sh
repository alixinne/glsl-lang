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

EXIT_CODE=0

if [ $CHECK_MODE -gt 0 ]; then
	for D in "${GLSL_DIRS[@]}"; do
		printf "% 20s " "$D" >&2

		(
			cd $D
			if diff <(cargo readme -t ../README.tpl) README.md >/dev/null 2>&1; then
				echo ok
			else
				echo not ok
				EXIT_CODE=$((EXIT_CODE + 1))
			fi
		)
	done
else
	for D in "${GLSL_DIRS[@]}"; do
		echo $D >&2

		(
			cd $D
			cargo readme -t ../README.tpl -o README.md
		)
	done
fi

exit $EXIT_CODE
