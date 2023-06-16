#!/usr/bin/env bash
set -eo pipefail

# check that Git diff is empty
CHANGES=$(git status --porcelain)
echo "$CHANGES"
git diff
[ -z "$CHANGES" ]
