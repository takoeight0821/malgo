#!/usr/bin/env bash
# selfhost-level2.sh — Level 2 metacircular interpreter test
#
# Level 1: the Malgo evaluator (Main.mlg), built into a runnable artifact,
#          evaluates a .mlg program directly
# Level 2: that artifact evaluates Main.mlg, which evaluates a .mlg program
#
# TARGET selects how Main.mlg becomes runnable:
#   TARGET=zig    (default) `malgo compile --opt release-fast` -> native binary
#   TARGET=scheme           `malgo eval --target scheme` -> main.scm, run under Chez
#
# The Scheme path is retained as the cross-implementation performance reference
# for #385: it is the only baseline the Zig backend's numbers can be read
# against. Do not delete it until #385 closes -- see #400.
#
# This script verifies the metacircular property: the Malgo evaluator
# written in Malgo can evaluate itself while correctly running programs.

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"

MALGO=${MALGO:-lean/.lake/build/bin/malgo}
TARGET=${TARGET:-zig}
SCHEME=${SCHEME:-scheme}
# Level 2 is ~50-200x slower than Level 1, and the Zig backend is a further
# ~5x slower per case than the Chez Scheme path it replaced (measured serially
# on an M-series Mac: 154s vs 31s on Fib; was ~7.5x before #403/#405 -- see
# `mise run perf-baseline -- --tier=l2-ratio`). Hence the per-target default:
# 300 is the value the Chez path shipped with before #384, and 600 gives the Zig
# path ~3.4x headroom over the slowest case measured (177s).
case "$TARGET" in
  zig)    CASE_TIMEOUT=${CASE_TIMEOUT:-600} ;;
  scheme) CASE_TIMEOUT=${CASE_TIMEOUT:-300} ;;
  *) echo "unknown TARGET '$TARGET' (expected zig|scheme)" >&2; exit 1 ;;
esac
PRECOMPILE_TIMEOUT=${PRECOMPILE_TIMEOUT:-180}
KEEP_WORK=${KEEP_WORK:-0}
MALGO_WORK_DIR=${MALGO_WORK_DIR:-.malgo-work}

timestamp() {
  date '+%Y-%m-%d %H:%M:%S'
}

log() {
  printf '[%s] %s\n' "$(timestamp)" "$*"
}

# Each case's own log is collected only after `wait`, so that the final
# output stays in case order despite running in parallel. That leaves the
# terminal silent for the whole sweep -- ~16 minutes on CI, in which a hang,
# a timeout and a slow-but-healthy case look identical. fd 3 keeps a handle
# on the real stdout so a case can report starting and finishing while the
# rest are still running.
exec 3>&1
progress() {
  printf '[%s] %s\n' "$(timestamp)" "$*" >&3
}

if [[ "$KEEP_WORK" != "1" ]]; then
  log "cleaning work directory: $MALGO_WORK_DIR"
  rm -rf "$MALGO_WORK_DIR"
fi

if [[ ! -x "$MALGO" ]]; then
  echo "malgo executable not found at '$MALGO' (set MALGO, or run 'lake build' in lean/)." >&2
  exit 1
fi

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
  if ! timeout "$PRECOMPILE_TIMEOUT" "$MALGO" eval "$file" </dev/null >/dev/null; then
    log "precompile failed: $file"
    exit 1
  fi
  elapsed=$((SECONDS - start))
  log "precompile done: $file (${elapsed}s)"
done

log "precompile phase complete (${#precompile[@]} files)"

mkdir -p "$MALGO_WORK_DIR"

# Every arm must set RUNNER: under `set -u`, bash 3.2 errors on "${RUNNER[@]}"
# for an unset/empty array. The `*)` arm above already exited, so RUNNER is
# always populated by the time run_case uses it.
case "$TARGET" in
  zig)
    EVAL_MAIN="$MALGO_WORK_DIR/malgoc"
    RUNNER=("$EVAL_MAIN")
    if ! command -v zig >/dev/null 2>&1; then
      echo "zig not found on PATH (set ZIG_BIN_DIR or run 'mise install' / activate mise)." >&2
      exit 1
    fi
    log "compiling Main.mlg to a native binary (Level 1 evaluator) via the Zig backend"
    if ! "$MALGO" compile runtime/malgo/compiler/Main.mlg -o "$EVAL_MAIN" --opt release-fast; then
      log "native compilation failed"
      exit 1
    fi
    log "native compilation done: $EVAL_MAIN"
    ;;
  scheme)
    EVAL_MAIN="$MALGO_WORK_DIR/main.scm"
    RUNNER=("$SCHEME" --script "$EVAL_MAIN")
    if ! command -v "$SCHEME" >/dev/null 2>&1; then
      echo "'$SCHEME' not found on PATH (run 'mise install' to get chezscheme, or set SCHEME)." >&2
      exit 1
    fi
    log "compiling Main.mlg to Scheme (Level 1 evaluator)"
    if ! "$MALGO" eval --target scheme runtime/malgo/compiler/Main.mlg > "$EVAL_MAIN"; then
      log "Scheme compilation failed"
      exit 1
    fi
    log "Scheme compilation done: $EVAL_MAIN"
    ;;
esac

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
log "starting Level 2 metacircular checks: ${total_cases} cases (parallel, TARGET=$TARGET)"
log "command: ${RUNNER[*]} runtime/malgo/compiler/Main.mlg <case.mlg>"

results_dir="$MALGO_WORK_DIR/level2-results"
rm -rf "$results_dir"
mkdir -p "$results_dir"

# Runs one case in isolation, writing its interleaved log to <dir>.log and
# its final status (pass/fail/timeout/skip) to <dir>.status. Invoked as a
# background job per case so all cases run concurrently; the caller
# collects logs/status after `wait` to keep output deterministic despite
# the parallel execution.
run_case() {
  local dir="$1"
  local src="test/testcases/malgo/$dir.mlg"
  local expected=".golden/Malgo.Sequent.Eval/$dir/golden"
  local status_file="$results_dir/$dir.status"

  if [[ ! -f "$src" ]]; then
    log "skip (no source): $dir"
    printf 'skip\n' >"$status_file"
    return 0
  fi
  if [[ ! -f "$expected" ]]; then
    log "skip (no golden): $dir"
    printf 'skip\n' >"$status_file"
    return 0
  fi

  local case_start=$SECONDS
  log "start: $dir"
  local out err
  out=$(mktemp)
  err=$(mktemp)

  if printf 'Hello\n' | timeout "$CASE_TIMEOUT" "${RUNNER[@]}" \
      runtime/malgo/compiler/Main.mlg "$src" >"$out" 2>"$err"; then
    local case_elapsed=$((SECONDS - case_start))
    if cmp -s "$out" "$expected"; then
      log "pass: $dir (${case_elapsed}s)"
      printf 'pass\n' >"$status_file"
    else
      log "fail(mismatch): $dir (${case_elapsed}s)"
      local out_lines err_lines out_first err_first
      out_lines=$(wc -l < "$out" | tr -d ' ')
      err_lines=$(wc -l < "$err" | tr -d ' ')
      out_first=$(head -n 3 "$out" | tr '\n' '|')
      err_first=$(head -n 3 "$err" | tr '\n' '|')
      printf '%s :: MISMATCH :: stdout(%s lines): %s | stderr(%s lines): %s\n' \
        "$dir" "$out_lines" "$out_first" "$err_lines" "$err_first"
      printf 'fail\n' >"$status_file"
    fi
  else
    local case_status=$?
    local case_elapsed=$((SECONDS - case_start))
    if [[ $case_status -eq 124 ]]; then
      log "fail(timeout): $dir (${case_elapsed}s)"
      printf '%s :: TIMEOUT\n' "$dir"
      printf 'timeout\n' >"$status_file"
    else
      local first
      first=$(head -n 1 "$err")
      [[ -n "$first" ]] || first=$(head -n 1 "$out")
      [[ -n "$first" ]] || first="<empty>"
      log "fail(error $case_status): $dir (${case_elapsed}s)"
      printf '%s :: ERR :: %s\n' "$dir" "$first"
      printf 'fail\n' >"$status_file"
    fi
  fi
  rm -f "$out" "$err"
}

run_case_watched() {
  local dir="$1"
  local start=$SECONDS
  progress "start: $dir"
  run_case "$dir" >"$results_dir/$dir.log" 2>&1
  progress "$(cat "$results_dir/$dir.status" 2>/dev/null || echo fail): $dir ($((SECONDS - start))s)"
}

pids=()
for dir in "${level2_cases[@]}"; do
  run_case_watched "$dir" &
  pids+=("$!")
done

for pid in "${pids[@]}"; do
  wait "$pid" || true
done

pass=0
fail=0
timeout_count=0

for dir in "${level2_cases[@]}"; do
  cat "$results_dir/$dir.log"
  case "$(cat "$results_dir/$dir.status" 2>/dev/null || echo fail)" in
    pass) pass=$((pass + 1)) ;;
    skip) ;;
    timeout) timeout_count=$((timeout_count + 1)); fail=$((fail + 1)) ;;
    *) fail=$((fail + 1)) ;;
  esac
done

printf 'PASS: %s\nFAIL: %s\nTIMEOUT: %s\n' "$pass" "$fail" "$timeout_count"
[[ $fail -eq 0 ]]
