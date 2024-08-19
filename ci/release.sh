#!/usr/bin/env bash

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
    set +e
    echo "Publishing $P" >&2
    cd $P

    SUCCESS=0
    for TRIES in $(seq 0 30); do
      OUTPUT=$(cargo publish 2>&1)
      if [[ $? == 0 ]] || [[ $OUTPUT =~ "already uploaded" ]]; then
        SUCCESS=1
        break
      else
        echo $OUTPUT
        echo "Waiting for index update" >&2
        sleep 10s
      fi
    done

    if [[ $SUCCESS != 1 ]]; then
      exit 1
    fi
  )
done
