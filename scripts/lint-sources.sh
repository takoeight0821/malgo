#!/usr/bin/env bash
# Lint Malgo sources with `malgo lint --deny-warnings`, failing if any
# diagnostic is reported in any file.
#
# Usage: scripts/lint-sources.sh [DIR ...]
#   DIR ...  Directories to scan recursively for *.mlg (default: examples/malgo
#            and test/testcases).
#   MALGO    Optional path to the malgo executable (default: the Lean build).
set -uo pipefail

malgo="${MALGO:-lean/.lake/build/bin/malgo}"
if [ ! -x "$malgo" ]; then
  echo "malgo executable not found at '$malgo' (set MALGO, or run 'lake build' in lean/)." >&2
  exit 1
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
