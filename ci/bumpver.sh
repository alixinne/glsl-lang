#!/bin/bash

set -eux

. ci/vars

perl -pi -e "BEGIN { our \$a = 0; } if (!\$a) { s/^version = .*/version = \"$1\"/; \$a = 1 }" */Cargo.toml
cargo upgrade --workspace "glsl-lang@$1" "glsl-lang-quote@$1" "lang-util@$1" "lang-util-derive@$1"
