#!/bin/bash

set -e

if [ "$CARGO_REGISTRY_TOKEN" = "" ]; then
  echo "No CARGO_REGISTRY_TOKEN, skipping" >&2
  exit 0
fi

for P in lang-util-dev \
         lang-util-derive \
         lang-util \
         lang-types \
         lang-pp \
         lang-lexer \
         lang \
         lang-quote \
         lang-cli ; do
  (
    echo "Publishing $P" >&2
    cd $P
    cargo publish "$@"

    echo "Waiting for index update" >&2
    sleep 10s
  )
done
