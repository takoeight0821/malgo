#!/usr/bin/env bash
# selfhost-level2.sh — Level 2 metacircular interpreter test
#
# Level 1: main.scm (Scheme) evaluates a .mlg program directly
# Level 2: main.scm evaluates Main.mlg which evaluates a .mlg program
#
# This script verifies the metacircular property: the Malgo evaluator
# written in Malgo can evaluate itself while correctly running programs.

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"

MALGO=${MALGO:-cabal exec malgo --}
SCHEME=${SCHEME:-scheme}
# Level 2 is ~50–200x slower than Level 1; use a generous timeout
CASE_TIMEOUT=${CASE_TIMEOUT:-300}
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
  runtime/malgo/compiler/Diagnostic.mlg
  runtime/malgo/compiler/Lexer.mlg
  runtime/malgo/compiler/Parser.mlg
  runtime/malgo/compiler/Value.mlg
  runtime/malgo/compiler/Eval.mlg
  runtime/malgo/compiler/FunIR.mlg
  runtime/malgo/compiler/Rename.mlg
  runtime/malgo/compiler/ToFun.mlg
  runtime/malgo/compiler/Main.mlg
)

for file in "${precompile[@]}"; do
  start=$SECONDS
  log "precompile start: $file"
  if ! timeout "$PRECOMPILE_TIMEOUT" $MALGO eval "$file" </dev/null >/dev/null; then
    log "precompile failed: $file"
    exit 1
  fi
  elapsed=$((SECONDS - start))
  log "precompile done: $file (${elapsed}s)"
done

log "precompile phase complete (${#precompile[@]} files)"

SCHEME_MAIN=".malgo-work/main.scm"
mkdir -p .malgo-work
log "compiling Main.mlg to Scheme (Level 1 evaluator)"
if ! $MALGO eval --target scheme runtime/malgo/compiler/Main.mlg > "$SCHEME_MAIN"; then
  log "Scheme compilation failed"
  exit 1
fi
log "Scheme compilation done: $SCHEME_MAIN"

# Level 2 test cases: a small subset of simple programs that complete quickly
# even when interpreted by an interpreter.
level2_cases=(
  Echo
  Factorial
  Fib
  StringOps
  Bool
)

total_cases=${#level2_cases[@]}
log "starting Level 2 metacircular checks: ${total_cases} cases"
log "command: scheme --script main.scm runtime/malgo/compiler/Main.mlg <case.mlg>"

pass=0
fail=0
timeout_count=0

for dir in "${level2_cases[@]}"; do
  src="test/testcases/malgo/$dir.mlg"
  expected=".golden/Malgo.Sequent.Eval/$dir/golden"

  if [[ ! -f "$src" ]]; then
    log "skip (no source): $dir"
    continue
  fi
  if [[ ! -f "$expected" ]]; then
    log "skip (no golden): $dir"
    continue
  fi

  case_start=$SECONDS
  log "start: $dir"
  out=$(mktemp)
  err=$(mktemp)

  if printf 'Hello\n' | timeout "$CASE_TIMEOUT" $SCHEME --script "$SCHEME_MAIN" \
      runtime/malgo/compiler/Main.mlg "$src" >"$out" 2>"$err"; then
    case_elapsed=$((SECONDS - case_start))
    if cmp -s "$out" "$expected"; then
      log "pass: $dir (${case_elapsed}s)"
      pass=$((pass + 1))
    else
      case_elapsed=$((SECONDS - case_start))
      log "fail(mismatch): $dir (${case_elapsed}s)"
      out_lines=$(wc -l < "$out" | tr -d ' ')
      err_lines=$(wc -l < "$err" | tr -d ' ')
      out_first=$(head -n 3 "$out" | tr '\n' '|')
      err_first=$(head -n 3 "$err" | tr '\n' '|')
      printf '%s :: MISMATCH :: stdout(%s lines): %s | stderr(%s lines): %s\n' \
        "$dir" "$out_lines" "$out_first" "$err_lines" "$err_first"
      fail=$((fail + 1))
    fi
  else
    status=$?
    case_elapsed=$((SECONDS - case_start))
    if [[ $status -eq 124 ]]; then
      log "fail(timeout): $dir (${case_elapsed}s)"
      printf '%s :: TIMEOUT\n' "$dir"
      timeout_count=$((timeout_count + 1))
      fail=$((fail + 1))
    else
      first=$(head -n 1 "$err")
      [[ -n "$first" ]] || first=$(head -n 1 "$out")
      [[ -n "$first" ]] || first="<empty>"
      log "fail(error $status): $dir (${case_elapsed}s)"
      printf '%s :: ERR :: %s\n' "$dir" "$first"
      fail=$((fail + 1))
    fi
  fi
  rm -f "$out" "$err"
done

printf 'PASS: %s\nFAIL: %s\nTIMEOUT: %s\n' "$pass" "$fail" "$timeout_count"
[[ $fail -eq 0 ]]
