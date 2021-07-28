#!/bin/bash

set -euo pipefail

rm -rf corpus
cp -rL "$1" corpus
shift

export AFL_SKIP_CPUFREQ=1
export AFL_NO_UI=1
cargo afl fuzz -i corpus -o out -M fuzzer01 "$@" &
for EXTRA in $(seq 2 $(nproc)); do
	cargo afl fuzz -i corpus -o out -S fuzzer$(printf "%02d" $EXTRA) "$@" &
done

wait
