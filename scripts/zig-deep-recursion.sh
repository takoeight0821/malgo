#!/usr/bin/env bash
# Deep-recursion regression gate for the Zig backend (issue #360).
#
# The pipeline is CPS: every call is a tail call. Emitting those as plain Zig
# calls meant nothing ever returned until the program exited, so the stack grew
# by one frame (~98.6 bytes, measured) per reduction step and any sufficiently
# long-running program died with SIGSEGV -- `fib 16` was enough.
#
# Generated code now uses `@call(.always_tail, ...)`. That moves *one* of the
# two regressions this gate watched for into the compiler and leaves the other
# here:
#
#   * A runtime helper that dispatches instead of tail-calling is now a compile
#     error -- a non-`inline` helper containing `@call(.always_tail, ..)` is
#     rejected, which is why `destructorProbe` exists in runtime.zig.
#   * **A terminator that emits a plain call again is not checked by anything
#     else.** Zig rejects an `.always_tail` it cannot honor; it never demands
#     that a call be one. `return someFn(self, a0, a1)` out of `emitTerminator`
#     compiles happily and grows the stack a frame per reduction step again.
#     Nothing else in CI reads emitted Zig text -- `zig-corpus` works on IR and
#     there are no emit goldens -- so this script is still the only guard.
#
# `BenchFibDeep.mlg` is ~18.8 million reductions, a depth no golden case
# reaches (~1.85 GB of stack under a plain-call convention), which is what
# makes it the one that catches it. The perf ratchet below needs a long run
# to be meaningful too.
#
# Deliberately NOT a `test/testcases/malgo` case: `zig-golden.sh` compiles at
# `--opt debug`, where Zig's DebugAllocator captures a stack trace per
# allocation. A case with enough dispatches to clear the old stack cliff costs
# ~13s there and would be the slowest case in the sweep by an order of
# magnitude. Compiled release-fast here, 100x the dispatches run in ~0.5s.
#
# Env knobs (all optional):
#   MALGO             path to the malgo executable (default: the Lean build)
#   ZIG_BIN_DIR       directory containing the zig binary, prepended to PATH
#   COMPILE_TIMEOUT   seconds allowed for `malgo compile` (default: 120)
#   CASE_TIMEOUT      seconds allowed for running the compiled binary (default: 60)
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

MALGO="${MALGO:-lean/.lake/build/bin/malgo}"
COMPILE_TIMEOUT="${COMPILE_TIMEOUT:-120}"
CASE_TIMEOUT="${CASE_TIMEOUT:-60}"

if [ -n "${ZIG_BIN_DIR:-}" ]; then
  export PATH="$ZIG_BIN_DIR:$PATH"
fi

if ! command -v zig >/dev/null 2>&1; then
  echo "zig not found on PATH (set ZIG_BIN_DIR or run 'mise install' / activate mise)." >&2
  exit 1
fi

if [ ! -x "$MALGO" ]; then
  echo "malgo executable not found at '$MALGO' (set MALGO, or run 'lake build' in lean/)." >&2
  exit 1
fi

SRC="bench/fixtures/BenchFibDeep.mlg"
EXPECTED="75025"

# Same workspace seeding as zig-golden.sh: bare-name imports inside
# Prelude.mlg resolve only via the .malgo-work mirror, empty on a fresh
# checkout.
for module in Builtin Prelude Either; do
  "$MALGO" eval "runtime/malgo/$module.mlg" >/dev/null 2>&1
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

echo "=== compiling $SRC (--opt release-fast) ==="
if ! timeout "$COMPILE_TIMEOUT" "$MALGO" compile "$SRC" -o "$WORK/fibdeep" --opt release-fast; then
  echo "FAIL: malgo compile failed" >&2
  exit 1
fi

echo "=== running (a SIGSEGV here means a terminator emits a plain call again) ==="
set +e
actual="$(MALGO_RC_STATS=1 timeout "$CASE_TIMEOUT" "$WORK/fibdeep" 2>"$WORK/stats")"
status=$?
set -e

if [ "$status" -eq 124 ]; then
  echo "FAIL: timed out after ${CASE_TIMEOUT}s" >&2
  exit 1
fi
# 83 = the runtime's leak gate.
if [ "$status" -ne 0 ]; then
  echo "FAIL: exited $status (139 = SIGSEGV: native stack grew with reduction steps again;" >&2
  echo "      83 = leak gate)" >&2
  exit 1
fi
if [ "$actual" != "$EXPECTED" ]; then
  echo "FAIL: expected '$EXPECTED', got '$actual'" >&2
  exit 1
fi

cat "$WORK/stats"

# This run already produced the fib-deep counters, so gating them here costs no
# extra compile and no extra execution -- the numbers were previously printed and
# thrown away (#399). Only total_allocs / dispatches / force_depth_max are gated;
# see scripts/perf-baseline.sh for why reuse_hits is reported rather than gated.
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
  for field in total_allocs dispatches force_depth_max; do
    actual_v="$(printf '%s\n' "$stats_line" | sed -n "s/.*$field=\([0-9]*\).*/\1/p")"
    base_v="$(jq -r --arg f "$field" '.tiers["fib-deep"].counters[$f] // empty' "$BASELINE")"
    # A ratchet with no floor reads a counter that stopped counting as a
    # win: `dispatches=0` would take the "improved" branch below and exit 0.
    # Every field here is a count of work this fixture certainly does.
    if [ -n "$actual_v" ] && [ "$field" != "force_depth_max" ] && [ "$actual_v" -eq 0 ]; then
      echo "FAIL: $field is 0 -- the counter is not counting" >&2
      perf_fail=1
      continue
    fi
    if [ -z "$actual_v" ]; then
      echo "FAIL: could not parse $field from '$stats_line'" >&2
      exit 1
    fi
    if [ -z "$base_v" ]; then
      echo "NOTICE: no fib-deep baseline for $field -- skipping"
      continue
    fi
    if [ "$field" = "force_depth_max" ]; then
      if [ "$actual_v" -ne "$base_v" ]; then
        echo "FAIL: force_depth_max changed ($base_v -> $actual_v); see #382" >&2
        perf_fail=1
      fi
    elif [ "$actual_v" -gt "$base_v" ]; then
      echo "FAIL: $field rose ($base_v -> $actual_v, +$((actual_v - base_v)))" >&2
      perf_fail=1
    elif [ "$actual_v" -lt "$base_v" ]; then
      echo "  $field improved ($base_v -> $actual_v); record it with 'mise run perf-baseline -- --tier=fib-deep --update'"
    fi
  done
  [ "$perf_fail" -eq 0 ] || exit 1
  echo "=== perf counters within baseline ==="
fi

echo "=== deep recursion OK: $actual, no leak ==="
