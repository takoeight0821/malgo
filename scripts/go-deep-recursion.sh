#!/usr/bin/env bash
# Deep-recursion regression gate for the Go backend.
#
# The pipeline is CPS: every call is a tail call, and Go does not guarantee
# tail calls any more than Zig does. Emitting them as native calls means
# nothing returns until the program exits, so the stack grows by a frame per
# reduction step. Go grows stacks by copying rather than crashing at a fixed
# size, but only up to `debug.SetMaxStack`'s 1GB default -- and
# `BenchFibDeep.mlg` is ~18.8 million dispatches, which under a native-call
# convention would need well past that. `run`'s trampoline makes native stack
# O(1) in reduction steps.
#
# If the trampoline ever regresses -- a statement that emits a native call
# again, a helper that dispatches instead of returning an Action -- this is
# the only thing that catches it. Every golden-sweep case is shallow.
#
# The failure signature differs from the Zig gate's: Zig dies with SIGSEGV
# (139), Go prints "fatal error: goroutine stack exceeds ..." and exits 2.
#
# Env knobs (all optional):
#   MALGO             path to the malgo executable (default: the Lean build)
#   GO_BIN_DIR        directory containing the go binary, prepended to PATH
#   COMPILE_TIMEOUT   seconds allowed for `malgo compile` (default: 120)
#   CASE_TIMEOUT      seconds allowed for running the compiled binary (default: 60)
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

MALGO="${MALGO:-lean/.lake/build/bin/malgo}"
COMPILE_TIMEOUT="${COMPILE_TIMEOUT:-120}"
CASE_TIMEOUT="${CASE_TIMEOUT:-60}"

if [ -n "${GO_BIN_DIR:-}" ]; then
  export PATH="$GO_BIN_DIR:$PATH"
fi

if ! command -v go >/dev/null 2>&1; then
  echo "go not found on PATH (set GO_BIN_DIR or run 'mise install' / activate mise)." >&2
  exit 1
fi

if [ ! -x "$MALGO" ]; then
  echo "malgo executable not found at '$MALGO' (set MALGO, or run 'lake build' in lean/)." >&2
  exit 1
fi

SRC="bench/fixtures/BenchFibDeep.mlg"
EXPECTED="75025"

# Same workspace seeding as go-golden.sh: bare-name imports inside Prelude.mlg
# resolve only via the .malgo-work mirror, empty on a fresh checkout.
for module in Builtin Prelude Either; do
  "$MALGO" eval "runtime/malgo/$module.mlg" >/dev/null 2>&1
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

echo "=== compiling $SRC (--target go --opt release-fast) ==="
if ! timeout "$COMPILE_TIMEOUT" "$MALGO" compile --target go "$SRC" \
     -o "$WORK/fibdeep" --opt release-fast; then
  echo "FAIL: malgo compile failed" >&2
  exit 1
fi

echo "=== running (a stack-overflow fatal error here means the trampoline regressed) ==="
set +e
actual="$(MALGO_RC_STATS=1 timeout "$CASE_TIMEOUT" "$WORK/fibdeep" 2>"$WORK/stats")"
status=$?
set -e

if [ "$status" -eq 124 ]; then
  echo "FAIL: timed out after ${CASE_TIMEOUT}s" >&2
  exit 1
fi
if [ "$status" -ne 0 ]; then
  echo "FAIL: exited $status (2 with 'goroutine stack exceeds' on stderr means the" >&2
  echo "      native stack grew with reduction steps again)" >&2
  sed -n '1,5p' "$WORK/stats" >&2
  exit 1
fi
if [ "$actual" != "$EXPECTED" ]; then
  echo "FAIL: expected '$EXPECTED', got '$actual'" >&2
  exit 1
fi

cat "$WORK/stats"

# This run already produced the counters, so gating them here costs no extra
# compile and no extra execution. Only `dispatches` and `force_depth_max` are
# gated: both are deterministic and machine-independent. `total_allocs` counts
# only the runtime's own value constructors, not Go's allocations, so it is
# reported rather than gated.
BASELINE="${BASELINE:-bench/perf-baseline.json}"
if [ ! -f "$BASELINE" ] || ! command -v jq >/dev/null 2>&1; then
  echo "NOTICE: no $BASELINE (or no jq) -- skipping the perf gate."
else
  stats_line="$(grep '^MALGO-STATS:' "$WORK/stats" | tail -n 1)"
  case "$stats_line" in
    ''|*'?'*)
      echo "FAIL: unusable MALGO-STATS line ('$stats_line')" >&2
      exit 1
      ;;
  esac
  perf_fail=0
  for field in dispatches force_depth_max; do
    actual_v="$(printf '%s\n' "$stats_line" | sed -n "s/.*$field=\([0-9]*\).*/\1/p")"
    base_v="$(jq -r --arg f "$field" '.tiers["go-fib-deep"].counters[$f] // empty' "$BASELINE")"
    if [ -z "$actual_v" ]; then
      echo "FAIL: could not parse $field from '$stats_line'" >&2
      exit 1
    fi
    if [ -z "$base_v" ]; then
      echo "NOTICE: no go-fib-deep baseline for $field -- skipping"
      continue
    fi
    if [ "$field" = "force_depth_max" ]; then
      if [ "$actual_v" -ne "$base_v" ]; then
        echo "FAIL: force_depth_max changed ($base_v -> $actual_v)" >&2
        perf_fail=1
      fi
    elif [ "$actual_v" -gt "$base_v" ]; then
      echo "FAIL: $field rose ($base_v -> $actual_v, +$((actual_v - base_v)))" >&2
      perf_fail=1
    elif [ "$actual_v" -lt "$base_v" ]; then
      echo "  $field improved ($base_v -> $actual_v); update bench/perf-baseline.json"
    fi
  done
  [ "$perf_fail" -eq 0 ] || exit 1
  echo "=== perf counters within baseline ==="
fi

echo "=== deep recursion OK: $actual ==="
