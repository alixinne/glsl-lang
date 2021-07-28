#!/bin/bash

set -euo pipefail
rm -rf corpus
cp -rL in corpus

export AFL_SKIP_CPUFREQ=1
export AFL_NO_UI=1
cargo afl fuzz -i corpus -o out -M fuzzer01 -- ../target/debug/pp-fuzz &
for EXTRA in $(seq 2 $(nproc)); do
	cargo afl fuzz -i corpus -o out -S fuzzer$(printf "%02d" $EXTRA) -- ../target/debug/pp-fuzz &
done

wait
