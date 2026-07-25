#!/usr/bin/env bash
# Deep-recursion regression gate for the Zig backend (issue #360).
#
# The pipeline is CPS: every call is a tail call. Emitting those as native Zig
# calls meant nothing ever returned until the program exited, so the stack grew
# by one frame (~98.6 bytes, measured) per reduction step and any sufficiently
# long-running program died with SIGSEGV -- `fib 16` was enough. `rt.run`'s
# trampoline makes native stack O(1) in reduction steps.
#
# `BenchFibDeep.mlg` is ~18.8 million dispatches, which under the old calling
# convention would have needed ~1.85 GB of stack. If the trampoline ever
# regresses -- a terminator that emits a native call again, a helper that
# dispatches instead of returning an Action -- this crashes immediately, at any
# stack size, on any platform.
#
# Deliberately NOT a `test/testcases/malgo` case: `zig-golden.sh` compiles at
# `--opt debug`, where Zig's DebugAllocator captures a stack trace per
# allocation. A case with enough dispatches to clear the old stack cliff costs
# ~13s there and would be the slowest case in the sweep by an order of
# magnitude. Compiled release-fast here, 100x the dispatches run in ~0.5s.
#
# Env knobs (all optional):
#   MALGO             path to the malgo executable (default: cabal-built one)
#   ZIG_BIN_DIR       directory containing the zig binary, prepended to PATH
#   COMPILE_TIMEOUT   seconds allowed for `malgo compile` (default: 120)
#   CASE_TIMEOUT      seconds allowed for running the compiled binary (default: 60)
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

if [ -z "${MALGO:-}" ]; then
  MALGO="$(cabal list-bin exe:malgo 2>/dev/null || true)"
fi
COMPILE_TIMEOUT="${COMPILE_TIMEOUT:-120}"
CASE_TIMEOUT="${CASE_TIMEOUT:-60}"

if [ -n "${ZIG_BIN_DIR:-}" ]; then
  export PATH="$ZIG_BIN_DIR:$PATH"
fi

if ! command -v zig >/dev/null 2>&1; then
  echo "zig not found on PATH (set ZIG_BIN_DIR or run 'mise install' / activate mise)." >&2
  exit 1
fi

if [ -z "$MALGO" ] || [ ! -x "$MALGO" ]; then
  echo "malgo executable not found (set MALGO explicitly or run 'cabal build exe:malgo')." >&2
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

echo "=== running (a SIGSEGV here means the trampoline regressed) ==="
set +e
actual="$(MALGO_RC_STATS=1 timeout "$CASE_TIMEOUT" "$WORK/fibdeep" 2>"$WORK/stats")"
status=$?
set -e

if [ "$status" -eq 124 ]; then
  echo "FAIL: timed out after ${CASE_TIMEOUT}s" >&2
  exit 1
fi
# 139 = SIGSEGV, the exact pre-#360 failure mode. 83 = the runtime's leak gate.
if [ "$status" -ne 0 ]; then
  echo "FAIL: exited $status (139 = SIGSEGV: native stack grew with reduction steps again)" >&2
  exit 1
fi
if [ "$actual" != "$EXPECTED" ]; then
  echo "FAIL: expected '$EXPECTED', got '$actual'" >&2
  exit 1
fi

cat "$WORK/stats"
echo "=== deep recursion OK: $actual, no leak ==="
