#!/bin/bash

set -eux

. ci/vars

cargo set-version --workspace "$1"
cargo upgrade --workspace "glsl-lang-types@$1" "glsl-lang-pp@$1" "glsl-lang-lexer@$1" "glsl-lang@$1" "glsl-lang-quote@$1" "lang-util@$1" "lang-util-derive@$1" "lang-util-dev@$1"
