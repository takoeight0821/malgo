#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"

MALGO=${MALGO:-cabal exec malgo --}
CASE_TIMEOUT=${CASE_TIMEOUT:-20}
PRECOMPILE_TIMEOUT=${PRECOMPILE_TIMEOUT:-180}
KEEP_WORK=${KEEP_WORK:-0}

if [[ "$KEEP_WORK" != "1" ]]; then
  rm -rf .malgo-work
fi

cabal build exe:malgo >/dev/null

precompile=(
  runtime/malgo/Builtin.mlg
  runtime/malgo/Prelude.mlg
  runtime/malgo/Either.mlg
  runtime/malgo/compiler/AST.mlg
  runtime/malgo/compiler/Token.mlg
  runtime/malgo/compiler/Lexer.mlg
  runtime/malgo/compiler/Parser.mlg
  runtime/malgo/compiler/Value.mlg
  runtime/malgo/compiler/Eval.mlg
)

for file in "${precompile[@]}"; do
  if ! timeout "$PRECOMPILE_TIMEOUT" $MALGO eval "$file" >/dev/null; then
    echo "precompile failed: $file" >&2
    exit 1
  fi
done

pass=0
fail=0
timeout_count=0
printed=0
max_print=${MAX_FAILURES:-40}

for dir in $(find .golden/Malgo.Sequent.Eval -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort); do
  src="test/testcases/malgo/$dir.mlg"
  expected=".golden/Malgo.Sequent.Eval/$dir/golden"
  [[ -f "$src" ]] || continue

  out=$(mktemp)
  err=$(mktemp)
  if timeout "$CASE_TIMEOUT" $MALGO eval runtime/malgo/compiler/Main.mlg < "$src" >"$out" 2>"$err"; then
    if cmp -s "$out" "$expected"; then
      pass=$((pass + 1))
    else
      fail=$((fail + 1))
      if [[ $printed -lt $max_print ]]; then
        first=$(head -n 1 "$out")
        [[ -n "$first" ]] || first="<empty>"
        printf '%s :: MISMATCH :: %s\n' "$dir" "$first"
        printed=$((printed + 1))
      fi
    fi
  else
    status=$?
    fail=$((fail + 1))
    if [[ $status -eq 124 ]]; then
      timeout_count=$((timeout_count + 1))
      reason="TIMEOUT :: <timeout>"
    else
      first=$(head -n 1 "$err")
      [[ -n "$first" ]] || first=$(head -n 1 "$out")
      [[ -n "$first" ]] || first="<empty>"
      reason="ERR($status) :: $first"
    fi
    if [[ $printed -lt $max_print ]]; then
      printf '%s :: %s\n' "$dir" "$reason"
      printed=$((printed + 1))
    fi
  fi
  rm -f "$out" "$err"
done

printf 'PASS: %s\nFAIL: %s\nTIMEOUT: %s\n' "$pass" "$fail" "$timeout_count"
[[ $fail -eq 0 ]]