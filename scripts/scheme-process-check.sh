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

# An embedded NUL byte in an argument is expected to fail loudly (exit 1,
# same stderr message) on *both* backends -- the guard lives once, in
# Prelude.mlg's `joinWithNul`/`containsNul`, ahead of either backend's own
# `malgo_run_process` dispatch. A nonzero exit here is the pass condition,
# not a script failure, so this doesn't reuse the "capture stdout, exit 0"
# pattern above.
NUL_SRC="test/testcases/scheme-only/RunProcessNulGuard.mlg"
echo "=== NUL-byte guard: interpreter ==="
interp_nul_out="$(timeout "$CASE_TIMEOUT" "$MALGO" eval "$NUL_SRC" 2>&1)"
interp_nul_exit=$?
echo "$interp_nul_out"
echo "exit: $interp_nul_exit"

echo "=== NUL-byte guard: scheme backend ==="
if ! timeout "$COMPILE_TIMEOUT" "$MALGO" eval --target scheme "$NUL_SRC" >"$WORK/nul.scm" 2>"$WORK/nul.compile.err"; then
  echo "FAIL: --target scheme compilation failed for $NUL_SRC:" >&2
  cat "$WORK/nul.compile.err" >&2
  exit 1
fi
scheme_nul_out="$(timeout "$CASE_TIMEOUT" "$SCHEME" --script "$WORK/nul.scm" 2>&1)"
scheme_nul_exit=$?
echo "$scheme_nul_out"
echo "exit: $scheme_nul_exit"

if [ "$interp_nul_exit" -ne 1 ] || [ "$scheme_nul_exit" -ne 1 ]; then
  echo "FAIL: NUL-byte guard should exit 1 on both backends (got interpreter=$interp_nul_exit, scheme=$scheme_nul_exit)" >&2
  exit 1
fi
if [ "$interp_nul_out" != "$scheme_nul_out" ]; then
  echo "FAIL: NUL-byte guard message differs between backends" >&2
  diff <(echo "$interp_nul_out") <(echo "$scheme_nul_out") >&2
  exit 1
fi

# `exit`/`exec` as `cmd` are shell builtins, not real executables -- only
# meaningful for the Scheme backend, which runs `cmd` through /bin/sh -c.
# The interpreter's IO.Process.output spawns a real executable via argv, so
# "exit" there is just a nonexistent command, not a comparable scenario;
# this fixture is Scheme-only and asserts the real exit code directly
# instead of diffing against the interpreter.
BUILTIN_SRC="test/testcases/scheme-only/RunProcessShellBuiltin.mlg"
echo "=== shell-builtin cmd (Scheme-only): scheme backend ==="
if ! timeout "$COMPILE_TIMEOUT" "$MALGO" eval --target scheme "$BUILTIN_SRC" >"$WORK/builtin.scm" 2>"$WORK/builtin.compile.err"; then
  echo "FAIL: --target scheme compilation failed for $BUILTIN_SRC:" >&2
  cat "$WORK/builtin.compile.err" >&2
  exit 1
fi
if ! builtin_out="$(timeout "$CASE_TIMEOUT" "$SCHEME" --script "$WORK/builtin.scm" 2>"$WORK/builtin.run.err")"; then
  echo "FAIL: scheme run failed for $BUILTIN_SRC:" >&2
  cat "$WORK/builtin.run.err" >&2
  exit 1
fi
echo "$builtin_out"
if [ "$builtin_out" != "code=7|out=|err=" ]; then
  echo "FAIL: expected code=7|out=|err=, got: $builtin_out" >&2
  exit 1
fi

echo "=== scheme-process-check OK ==="
