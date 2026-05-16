#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"

MALGO=${MALGO:-cabal exec malgo --}
CASE_TIMEOUT=${CASE_TIMEOUT:-20}
PRECOMPILE_TIMEOUT=${PRECOMPILE_TIMEOUT:-180}
KEEP_WORK=${KEEP_WORK:-0}

timestamp() {
  date '+%Y-%m-%d %H:%M:%S'
}

log() {
  printf '[%s] %s\n' "$(timestamp)" "$*"
}

if [[ "$KEEP_WORK" != "1" ]]; then
  log "cleaning work directory: .malgo-work"
  rm -rf .malgo-work
fi

log "building malgo executable"
cabal build exe:malgo >/dev/null
log "build complete"

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
  start=$SECONDS
  log "precompile start: $file"
  if ! timeout "$PRECOMPILE_TIMEOUT" $MALGO eval "$file" >/dev/null; then
    log "precompile failed: $file"
    exit 1
  fi
  elapsed=$((SECONDS - start))
  log "precompile done: $file (${elapsed}s)"
done

log "precompile phase complete (${#precompile[@]} files)"

pass=0
fail=0
timeout_count=0
printed=0
max_print=${MAX_FAILURES:-40}

mapfile -t cases < <(find .golden/Malgo.Sequent.Eval -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort)
total_cases=${#cases[@]}
log "starting golden checks: ${total_cases} cases"

index=0
for dir in "${cases[@]}"; do
  index=$((index + 1))
  src="test/testcases/malgo/$dir.mlg"
  expected=".golden/Malgo.Sequent.Eval/$dir/golden"
  [[ -f "$src" ]] || continue

  case_start=$SECONDS
  log "[$index/$total_cases] start: $dir"

  out=$(mktemp)
  err=$(mktemp)
  if timeout "$CASE_TIMEOUT" $MALGO eval runtime/malgo/compiler/Main.mlg < "$src" >"$out" 2>"$err"; then
    if cmp -s "$out" "$expected"; then
      pass=$((pass + 1))
      case_elapsed=$((SECONDS - case_start))
      log "[$index/$total_cases] pass: $dir (${case_elapsed}s)"
    else
      fail=$((fail + 1))
      case_elapsed=$((SECONDS - case_start))
      log "[$index/$total_cases] fail(mismatch): $dir (${case_elapsed}s)"
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
    case_elapsed=$((SECONDS - case_start))
    if [[ $status -eq 124 ]]; then
      timeout_count=$((timeout_count + 1))
      reason="TIMEOUT :: <timeout>"
      log "[$index/$total_cases] fail(timeout): $dir (${case_elapsed}s)"
    else
      first=$(head -n 1 "$err")
      [[ -n "$first" ]] || first=$(head -n 1 "$out")
      [[ -n "$first" ]] || first="<empty>"
      reason="ERR($status) :: $first"
      log "[$index/$total_cases] fail(error $status): $dir (${case_elapsed}s)"
    fi
    if [[ $printed -lt $max_print ]]; then
      printf '%s :: %s\n' "$dir" "$reason"
      printed=$((printed + 1))
    fi
  fi
  rm -f "$out" "$err"
done

log "golden checks complete"
printf 'PASS: %s\nFAIL: %s\nTIMEOUT: %s\n' "$pass" "$fail" "$timeout_count"
[[ $fail -eq 0 ]]