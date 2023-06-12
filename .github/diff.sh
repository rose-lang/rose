#!/usr/bin/env bash
set -eo pipefail

# check that all tracked files are unmodified
CHANGES=$(git status --porcelain)
echo "$CHANGES"
git diff
[ -z "$CHANGES" ]
