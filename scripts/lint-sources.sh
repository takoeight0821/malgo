#!/usr/bin/env bash
# Lint Malgo sources with `malgo lint --deny-warnings`, failing if any
# diagnostic is reported in any file.
#
# Usage: scripts/lint-sources.sh [DIR ...]
#   DIR ...  Directories to scan recursively for *.mlg (default: examples/malgo
#            and test/testcases).
#   MALGO    Optional path to the malgo executable; otherwise resolved via
#            `cabal list-bin exe:malgo`.
set -uo pipefail

malgo="${MALGO:-}"
if [ -z "$malgo" ]; then
  malgo="$(cabal list-bin exe:malgo)"
fi

dirs=("$@")
if [ "${#dirs[@]}" -eq 0 ]; then
  dirs=(examples/malgo test/testcases)
fi

status=0
while IFS= read -r f; do
  if ! "$malgo" lint --deny-warnings "$f"; then
    status=1
  fi
done < <(find "${dirs[@]}" -name '*.mlg' | sort)

if [ "$status" -ne 0 ]; then
  echo "lint-sources: findings detected (see warnings above)" >&2
fi
exit "$status"
