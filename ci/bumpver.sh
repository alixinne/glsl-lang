#!/bin/bash

set -eux

cargo workspaces version \
  -a \
  --force \
  '*' \
  --yes \
  --no-git-commit \
  custom \
  "$1"
