#!/usr/bin/env bash
# Golden-parity harness for the Chez Scheme backend: compiles every testcase
# with a `.golden/Malgo.Sequent.Eval/<Case>/golden` file via
# `malgo eval --target scheme` and runs the result with `scheme --script`,
# diffing its stdout against the interpreter's golden output byte-for-byte.
#
# Unlike scripts/zig-golden.sh, there is no leak check: Chez Scheme is a
# garbage-collected runtime, not the Zig backend's reference-counted one.
#
# Env knobs (all optional):
#   MALGO             path to the malgo executable (default: the Lean build)
#   SCHEME            path to the Chez Scheme executable (default: scheme)
#   COMPILE_TIMEOUT   seconds allowed for `malgo eval --target scheme` (default: 60)
#   CASE_TIMEOUT      seconds allowed for running the compiled script (default: 10)
#   MAX_FAILURES      stop after this many failures (default: unlimited)
#   KEEP_WORK         if set, do not delete the working directory on exit
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

MALGO="${MALGO:-lean/.lake/build/bin/malgo}"
SCHEME="${SCHEME:-scheme}"
COMPILE_TIMEOUT="${COMPILE_TIMEOUT:-60}"
CASE_TIMEOUT="${CASE_TIMEOUT:-10}"
MAX_FAILURES="${MAX_FAILURES:-999999}"

if ! command -v "$SCHEME" >/dev/null 2>&1; then
  echo "$SCHEME not found on PATH (set SCHEME, or run 'mise install' / activate mise)." >&2
  exit 1
fi

if [ ! -x "$MALGO" ]; then
  echo "malgo executable not found at '$MALGO' (set MALGO, or run 'lake build' in lean/)." >&2
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

# TaggedRecordCrossModuleDef.mlg defines no `main` -- it's a library
# companion imported (unqualified) by TaggedRecordCrossModuleUse.mlg, which
# defines its own `main`, so this file can't have one too. The primary Lean
# interpreter treats a missing main as a silent no-op (empty golden), but
# this backend errors on it, so it's excluded here the same way
# lean/Test/Main.lean's evalHarnessUnsupported excludes its companion.
# TaggedRecordDiamondDef.mlg/TaggedRecordDiamondMid.mlg are the same shape,
# one hop further (see evalHarnessUnsupported's doc comment).
SKIP_CASES=("TaggedRecordCrossModuleDef" "TaggedRecordDiamondDef" "TaggedRecordDiamondMid")

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
  skip=0
  for skip_case in "${SKIP_CASES[@]}"; do
    [ "$case" = "$skip_case" ] && skip=1
  done
  if [ "$skip" -eq 1 ]; then
    continue
  fi
  golden="$dir/golden"
  if [ ! -f "$golden" ]; then
    continue
  fi

  out_scm="$WORK/$case.scm"
  compile_log="$WORK/$case.compile.log"
  if ! timeout "$COMPILE_TIMEOUT" "$MALGO" eval --target scheme "$src" >"$out_scm" 2>"$compile_log"; then
    compile_fail=$((compile_fail + 1))
    compile_fail_names+=("$case")
    total_failures=$((total_failures + 1))
  else
    actual_out="$WORK/$case.out"
    # Plain statement (not `if ! pipeline`) so `$?` right after reflects the
    # pipeline's actual last-command exit status, including `timeout`'s 124
    # on expiry -- see the identical comment in zig-golden.sh.
    printf 'Hello\n' | timeout "$CASE_TIMEOUT" "$SCHEME" --script "$out_scm" >"$actual_out" 2>"$WORK/$case.run.log"
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
echo "=== scheme-golden results: $pass/$total passed ==="
echo "compile-fail: $compile_fail ${compile_fail_names[*]:-}"
echo "run-fail:     $run_fail ${run_fail_names[*]:-}"
echo "mismatch:     $mismatch ${mismatch_names[*]:-}"
echo "timeout:      $timeout_fail ${timeout_names[*]:-}"

# Panic gate (regression test for malgo#426/#452, not part of the golden
# sweep above -- `Panic`/`CondPanic`/`PanicNamedImport` are excluded from
# `.golden/Malgo.Sequent.Eval/*` by lean/Test/Main.lean's
# evalHarnessUnsupported precisely because they never return normally, so
# the sweep above -- which discovers cases by listing that directory --
# never runs them, leaving this backend's own panic path (`Backend/
# Scheme.lean` compiles `malgo_panic` to `(error 'panic <msg>)`, which Chez
# reports as "Exception in panic: <msg>" on stderr and exits nonzero)
# entirely unverified. Mirrors `PanicGate` in lean/Test/Main.lean and the
# self-hosted panic gate in scripts/selfhost-golden.sh.
panic_fail=0
panic_scenarios=(
  $'Panic\001before panic\001malgo#426 regression check'
  $'CondPanic\001before cond\001no branch'
  $'PanicNamedImport\001before panic\001malgo#452 named-import regression check'
)
for entry in "${panic_scenarios[@]}"; do
  case_name=$(cut -d $'\001' -f1 <<< "$entry")
  expected_stdout=$(cut -d $'\001' -f2 <<< "$entry")
  expected_message=$(cut -d $'\001' -f3 <<< "$entry")
  src="$TESTCASE_DIR/$case_name.mlg"
  out_scm="$WORK/panic-$case_name.scm"
  if ! timeout "$COMPILE_TIMEOUT" "$MALGO" eval --target scheme "$src" \
       >"$out_scm" 2>"$WORK/panic-$case_name.compile.log"; then
    echo "panic gate FAIL: $case_name failed to compile"
    panic_fail=1
    continue
  fi
  out="$WORK/panic-$case_name.out"
  err="$WORK/panic-$case_name.err"
  printf 'Hello\n' | timeout "$CASE_TIMEOUT" "$SCHEME" --script "$out_scm" >"$out" 2>"$err"
  run_exit=$?
  actual_out="$(cat "$out")"
  if [ "$run_exit" -eq 0 ]; then
    echo "panic gate FAIL: $case_name exited 0, expected nonzero"
    panic_fail=1
  elif [ "$actual_out" != "$expected_stdout" ]; then
    echo "panic gate FAIL: $case_name stdout was '$actual_out', expected '$expected_stdout'"
    panic_fail=1
  elif ! grep -qF "panic: $expected_message" "$err"; then
    echo "panic gate FAIL: $case_name stderr missing 'panic: $expected_message' (got: $(cat "$err"))"
    panic_fail=1
  else
    echo "panic gate ok: $case_name"
  fi
done
if [ "$panic_fail" -eq 0 ]; then
  echo "=== scheme panic-gate: ${#panic_scenarios[@]}/${#panic_scenarios[@]} passed ==="
else
  echo "=== scheme panic-gate: FAILED ==="
fi

if [ "$total" -eq 0 ]; then
  echo "No golden+testcase pairs were found under $GOLDEN_ROOT / $TESTCASE_DIR -- treating this as failure, not success." >&2
  exit 1
elif [ "$pass" -eq "$total" ] && [ "$panic_fail" -eq 0 ]; then
  exit 0
else
  exit 1
fi
