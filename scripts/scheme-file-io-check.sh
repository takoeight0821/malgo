#!/usr/bin/env bash
# File-I/O regression gate for the Scheme backend.
#
# Deliberately NOT a `test/testcases/malgo` case, same reasoning as
# scheme-process-check.sh: that directory is swept by zig-golden.sh/
# selfhost-golden.sh/cli-gate.sh/the Lean corpus gates too, and this fixture
# only needs to run on the interpreter (oracle) and the Scheme backend.
# `test/testcases/scheme-only/` holds fixtures meant for scripts like this
# one alone.
#
# Unlike scheme-process-check.sh's fixtures, this one doesn't diff stdout
# between backends -- ReadWriteFileEdgeCases.mlg prints its own "ok"/"FAIL"
# assertions, so a real mismatch shows up as a FAIL line printed on stdout,
# not just a stdout diff. This script's job is to run it on both backends
# and make sure it exits cleanly with zero FAIL lines on each.
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

SRC="test/testcases/scheme-only/ReadWriteFileEdgeCases.mlg"

for module in Builtin Prelude Either; do
  "$MALGO" eval "runtime/malgo/$module.mlg" >/dev/null 2>&1
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

check_no_fail() {
  local label="$1" out="$2"
  if grep -q '^FAIL' <<<"$out"; then
    echo "FAIL: $label reported an assertion failure:" >&2
    echo "$out" >&2
    exit 1
  fi
}

echo "=== interpreter (oracle) ==="
if ! interp_out="$(timeout "$CASE_TIMEOUT" "$MALGO" eval "$SRC" 2>"$WORK/interp.err")"; then
  echo "FAIL: interpreter run failed:" >&2
  cat "$WORK/interp.err" >&2
  exit 1
fi
echo "$interp_out"
check_no_fail "interpreter" "$interp_out"

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
echo "$scheme_out"
check_no_fail "scheme backend" "$scheme_out"

if [ "$interp_out" != "$scheme_out" ]; then
  echo "FAIL: interpreter and scheme backend disagree on $SRC" >&2
  diff <(echo "$interp_out") <(echo "$scheme_out") >&2
  exit 1
fi

echo "=== scheme-file-io-check OK ==="
