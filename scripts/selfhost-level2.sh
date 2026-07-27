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
# Building the evaluator and running the cases can be separated, which is how CI
# keeps any one job under 10 minutes:
#   BUILD_ONLY=1   build the evaluator and stop
#   EVAL_BIN=PATH  use an already-built evaluator; skip the precompile and the
#                  `malgo compile` entirely
#   L2_CASES="A B" run only these cases (default: all five)
#
# Splitting matters because the fixed cost dwarfs everything else: on CI the
# 14-module precompile is ~12s but `malgo compile Main.mlg` is ~105s, and a job
# that runs one case would otherwise spend more time building than testing. The
# built evaluator needs nothing from `.malgo-work` -- it reads `Main.mlg` and its
# imports as source at run time, and `.malgo-work` is the Lean compiler's own
# workspace (lean/Malgo/Module.lean) -- so the binary alone is enough to hand to
# another machine.
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
# Concurrent cases. Deliberately 2 rather than selfhost-golden.sh's `nproc`: a
# Level 2 case runs for minutes and allocates billions of times, so
# oversubscribing inflates every case's wall clock far more than the overlap
# saves. CI sets this explicitly; see the pool near the bottom of this file.
PARALLEL_JOBS=${PARALLEL_JOBS:-2}
KEEP_WORK=${KEEP_WORK:-0}
MALGO_WORK_DIR=${MALGO_WORK_DIR:-.malgo-work}
BUILD_ONLY=${BUILD_ONLY:-0}
EVAL_BIN=${EVAL_BIN:-}
# Space-separated. Kept as a string rather than an array so the CI matrix can
# pass a single name through `env:`.
L2_CASES=${L2_CASES:-"Echo Factorial Fib StringOps Bool"}

# Validate arguments before anything expensive or destructive happens -- the
# build phase wipes MALGO_WORK_DIR, so a bad argument must not get that far.
# (L2_CASES unset falls back to all five, as every other knob here does; this
# catches an explicitly whitespace-only value, which is a mistake, not a default.)
if [[ -z "${L2_CASES// /}" ]]; then
  echo "L2_CASES is empty -- nothing to run" >&2
  exit 1
fi

if [[ -n "$EVAL_BIN" ]]; then
  # The Scheme path's evaluator is a .scm run through `scheme --script`, not a
  # binary, and the ratio measurement it exists for wants both halves built the
  # same way in one place. Rather than guess which, refuse the combination.
  if [[ "$TARGET" != "zig" ]]; then
    echo "EVAL_BIN is TARGET=zig only (got TARGET=$TARGET)" >&2
    exit 1
  fi
  if [[ "$BUILD_ONLY" == "1" ]]; then
    echo "BUILD_ONLY and EVAL_BIN are mutually exclusive: one builds, the other skips building" >&2
    exit 1
  fi
  if [[ ! -x "$EVAL_BIN" ]]; then
    echo "EVAL_BIN '$EVAL_BIN' is not an executable file" >&2
    exit 1
  fi
fi

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

# With EVAL_BIN there is nothing to build, so there is nothing to clean and no
# Lean-side compiler needed -- the whole point is that a case can run on a
# machine that has neither a toolchain nor a workspace.
if [[ -z "$EVAL_BIN" ]]; then
  if [[ "$KEEP_WORK" != "1" ]]; then
    log "cleaning work directory: $MALGO_WORK_DIR"
    rm -rf "$MALGO_WORK_DIR"
  fi

  if [[ ! -x "$MALGO" ]]; then
    echo "malgo executable not found at '$MALGO' (set MALGO, or run 'lake build' in lean/)." >&2
    exit 1
  fi
fi

if [[ -n "$EVAL_BIN" ]]; then
  EVAL_MAIN="$EVAL_BIN"
  RUNNER=("$EVAL_BIN")
  log "using prebuilt Level 1 evaluator: $EVAL_MAIN (skipping precompile and compile)"
else
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

  if [[ "$BUILD_ONLY" == "1" ]]; then
    log "BUILD_ONLY: evaluator built, stopping before the case sweep"
    exit 0
  fi
fi

# Level 2 test cases: a small subset of simple programs that complete quickly
# even when interpreted by an interpreter.
# Word-split L2_CASES deliberately -- it is a space-separated list, and this
# keeps the script bash 3.2 clean (no mapfile). Validated near the top, before
# anything expensive runs.
# shellcheck disable=SC2206
level2_cases=($L2_CASES)

total_cases=${#level2_cases[@]}
log "starting Level 2 metacircular checks: ${total_cases} cases (TARGET=$TARGET, parallelism: ${PARALLEL_JOBS}, CASE_TIMEOUT: ${CASE_TIMEOUT}s)"
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

# Throttled to PARALLEL_JOBS concurrent workers, mirroring
# scripts/selfhost-golden.sh's pool. Launching all five at once is fine on a
# workstation with cores to spare, and pathological on a CI runner: five
# minutes-long, allocation-heavy processes contend, and each one's *wall clock*
# inflates even though its compute has not changed. That is what put every case
# over the 600s CASE_TIMEOUT on CI while the same sweep took 154-177s per case
# locally.
#
# Per-case status is read from the .status files either way, so nothing needs the
# pid array this replaces.
#
# `wait -n` is bash 4.3+; on macOS's bash 3.2 the fallback waits for every
# running job instead, which leaves `active_jobs` over-counted and effectively
# serialises the rest of the sweep. That is the safe direction to be wrong in.
active_jobs=0
for dir in "${level2_cases[@]}"; do
  run_case_watched "$dir" &
  active_jobs=$((active_jobs + 1))
  if [[ $active_jobs -ge $PARALLEL_JOBS ]]; then
    wait -n 2>/dev/null || wait
    active_jobs=$((active_jobs - 1))
  fi
done
wait

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
