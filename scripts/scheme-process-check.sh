#!/usr/bin/env bash
# Subprocess-exec regression gate for the Scheme backend (#416 Phase 1b).
#
# Deliberately NOT a `test/testcases/malgo` case: that directory is swept by
# `zig-golden.sh`, `selfhost-golden.sh`, `cli-gate.sh`, and the Lean `lake
# test` corpus gates (`ir-invariants`/`zig-corpus`) as well as
# `scheme-golden.sh`, so a case there would force the Zig backend and the
# self-hosted evaluator (`runtime/malgo/compiler/`) to support
# `malgo_run_process` too, just to keep those gates green. Neither does --
# it's out of scope for both (see the plan doc / issue #416): nix-config
# doesn't run Zig-compiled binaries, and nothing exercises subprocess exec
# through Level 1 self-hosting. `test/testcases/scheme-only/` holds fixtures
# meant for this script alone.
#
# The interpreter (`malgo eval`) is the oracle everywhere else in this
# codebase, so it is here too: this script runs the same fixture through the
# interpreter and through `--target scheme` + `scheme --script`, and diffs
# their stdout byte-for-byte.
#
# Env knobs (all optional):
#   MALGO             path to the malgo executable (default: the Lean build)
#   SCHEME            path to the Chez Scheme executable (default: scheme)
#   COMPILE_TIMEOUT   seconds allowed for `malgo eval --target scheme` (default: 60)
#   CASE_TIMEOUT      seconds allowed for running either side (default: 10)
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

MALGO="${MALGO:-lean/.lake/build/bin/malgo}"
SCHEME="${SCHEME:-scheme}"
COMPILE_TIMEOUT="${COMPILE_TIMEOUT:-60}"
CASE_TIMEOUT="${CASE_TIMEOUT:-10}"

if ! command -v "$SCHEME" >/dev/null 2>&1; then
  echo "$SCHEME not found on PATH (set SCHEME, or run 'mise install' / activate mise)." >&2
  exit 1
fi

if [ ! -x "$MALGO" ]; then
  echo "malgo executable not found at '$MALGO' (set MALGO, or run 'lake build' in lean/)." >&2
  exit 1
fi

SRC="test/testcases/scheme-only/RunProcess.mlg"

# Same workspace seeding as scheme-golden.sh/zig-golden.sh.
for module in Builtin Prelude Either; do
  "$MALGO" eval "runtime/malgo/$module.mlg" >/dev/null 2>&1
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

echo "=== interpreter (oracle) ==="
if ! interp_out="$(timeout "$CASE_TIMEOUT" "$MALGO" eval "$SRC" 2>"$WORK/interp.err")"; then
  echo "FAIL: interpreter run failed:" >&2
  cat "$WORK/interp.err" >&2
  exit 1
fi
printf '%s' "$interp_out" >"$WORK/interp.out"
cat "$WORK/interp.out"

echo "=== scheme backend ==="
if ! timeout "$COMPILE_TIMEOUT" "$MALGO" eval --target scheme "$SRC" >"$WORK/main.scm" 2>"$WORK/compile.err"; then
  echo "FAIL: --target scheme compilation failed:" >&2
  cat "$WORK/compile.err" >&2
  exit 1
fi
if ! scheme_out="$(timeout "$CASE_TIMEOUT" "$SCHEME" --script "$WORK/main.scm" 2>"$WORK/scheme.err")"; then
  echo "FAIL: scheme run failed:" >&2
  cat "$WORK/scheme.err" >&2
  exit 1
fi
printf '%s' "$scheme_out" >"$WORK/scheme.out"
cat "$WORK/scheme.out"

if ! diff -u "$WORK/interp.out" "$WORK/scheme.out"; then
  echo "FAIL: interpreter and scheme backend disagree on $SRC" >&2
  exit 1
fi

echo "=== scheme-process-check OK ==="
