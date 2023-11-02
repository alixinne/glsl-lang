#!/bin/bash

set -eux

cargo workspaces version \
  -a \
  --force \
  '*' \
  --yes \
  --no-git-commit \
  --exact \
  custom \
  "$1"
