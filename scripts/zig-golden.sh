#!/usr/bin/env bash
# Golden-parity harness for the Zig backend: compiles every testcase with a
# `.golden/Malgo.Sequent.Eval/<Case>/golden` file via `malgo compile` (the
# real user-facing path, exercising the zig toolchain end to end) and diffs
# its stdout against the interpreter's golden output byte-for-byte.
#
# Env knobs (all optional):
#   MALGO             path to the malgo executable (default: cabal-built one)
#   ZIG_BIN_DIR       directory containing the zig binary, prepended to PATH
#   COMPILE_TIMEOUT   seconds allowed for `malgo compile` (default: 60)
#   CASE_TIMEOUT      seconds allowed for running the compiled binary (default: 10)
#   MAX_FAILURES      stop after this many failures (default: unlimited)
#   KEEP_WORK         if set, do not delete the working directory on exit
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

if [ -z "${MALGO:-}" ]; then
  # `cabal list-bin` resolves the exact path regardless of platform triple
  # (aarch64-osx, x86_64-linux, ...) or GHC/package version, unlike a
  # hardcoded dist-newstyle path.
  MALGO="$(cabal list-bin exe:malgo 2>/dev/null || true)"
fi
COMPILE_TIMEOUT="${COMPILE_TIMEOUT:-60}"
CASE_TIMEOUT="${CASE_TIMEOUT:-10}"
MAX_FAILURES="${MAX_FAILURES:-999999}"

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

# Bare-name imports (e.g. `import Builtin` inside Prelude.mlg) resolve only
# by searching the .malgo-work workspace mirror (see
# Malgo.Module.searchAndRegister), which starts out empty on a fresh
# checkout. Seed it by compiling each runtime module as an entry point once,
# before any testcase transitively bare-imports them.
for module in Builtin Prelude Either; do
  "$MALGO" eval "runtime/malgo/$module.mlg" >/dev/null 2>&1
done

WORK="$(mktemp -d)"
cleanup() {
  if [ -z "${KEEP_WORK:-}" ]; then
    rm -rf "$WORK"
  else
    echo "Work directory kept at: $WORK"
  fi
}
trap cleanup EXIT

GOLDEN_ROOT=".golden/Malgo.Sequent.Eval"
TESTCASE_DIR="test/testcases/malgo"

pass=0
compile_fail=0
run_fail=0
mismatch=0
timeout_fail=0
total_failures=0

declare -a compile_fail_names run_fail_names mismatch_names timeout_names

for dir in "$GOLDEN_ROOT"/*/; do
  case=$(basename "$dir")
  src="$TESTCASE_DIR/$case.mlg"
  if [ ! -f "$src" ]; then
    continue
  fi
  golden="$dir/golden"
  if [ ! -f "$golden" ]; then
    continue
  fi

  out_bin="$WORK/$case"
  compile_log="$WORK/$case.compile.log"
  if ! timeout "$COMPILE_TIMEOUT" "$MALGO" compile "$src" -o "$out_bin" >"$compile_log" 2>&1; then
    compile_fail=$((compile_fail + 1))
    compile_fail_names+=("$case")
    total_failures=$((total_failures + 1))
  else
    actual_out="$WORK/$case.out"
    # Run as a plain statement (not `if ! pipeline`) so `$?` right after
    # reflects the pipeline's actual last-command exit status, including
    # `timeout`'s 124 on expiry -- `if ! pipeline; then ...; $?` would
    # instead capture the negated condition's status, which is always 0.
    printf 'Hello\n' | timeout "$CASE_TIMEOUT" "$out_bin" >"$actual_out" 2>"$WORK/$case.run.log"
    run_exit=$?
    if [ "$run_exit" -eq 124 ]; then
      timeout_fail=$((timeout_fail + 1))
      timeout_names+=("$case")
      total_failures=$((total_failures + 1))
    elif [ "$run_exit" -ne 0 ]; then
      run_fail=$((run_fail + 1))
      run_fail_names+=("$case")
      total_failures=$((total_failures + 1))
    elif cmp -s "$actual_out" "$golden"; then
      pass=$((pass + 1))
    else
      mismatch=$((mismatch + 1))
      mismatch_names+=("$case")
      total_failures=$((total_failures + 1))
    fi
  fi

  if [ "$total_failures" -ge "$MAX_FAILURES" ]; then
    echo "Stopping early: reached MAX_FAILURES=$MAX_FAILURES"
    break
  fi
done

total=$((pass + compile_fail + run_fail + mismatch + timeout_fail))
echo ""
echo "=== zig-golden results: $pass/$total passed ==="
echo "compile-fail: $compile_fail ${compile_fail_names[*]:-}"
echo "run-fail:     $run_fail ${run_fail_names[*]:-}"
echo "mismatch:     $mismatch ${mismatch_names[*]:-}"
echo "timeout:      $timeout_fail ${timeout_names[*]:-}"

if [ "$total" -eq 0 ]; then
  echo "No golden+testcase pairs were found under $GOLDEN_ROOT / $TESTCASE_DIR -- treating this as failure, not success." >&2
  exit 1
elif [ "$pass" -eq "$total" ]; then
  exit 0
else
  exit 1
fi
