#!/usr/bin/env bash
# End-to-end gate for the `malgo` CLI binary.
#
# `lake test` (and the Haskell hspec suite) call library functions in
# process. Nothing else drives the actual executable across the corpus:
# argument handling, workspace seeding, stdin plumbing and exit codes are
# only exercised here. `scripts/lean-parity.sh` used to cover this as a
# side effect of comparing the two implementations; this keeps the coverage
# once there is only one implementation left to compare against itself.
#
# Four checks:
#   eval      73 testcases, stdout must equal .golden/Malgo.Sequent.Eval/<case>
#   bigstep   the same 73 under --eval-mode bigstep
#   error     every test/testcases/malgo/error/*.mlg.expect fixture must make
#             `malgo eval --infer` exit nonzero (message text is deliberately
#             not compared -- see that directory's README.md)
#   infer-ok  the opposite polarity check: each case listed in
#             INFER_OK_CASES below must make `malgo eval --infer` exit ZERO.
#             These exercise Query.Engine.buildDepsEnv's real strict fold
#             directly through the CLI binary -- the Lean test suite's own
#             infer gate uses a separate, deliberately lenient fold
#             (`buildDepsEnvLenient` in lean/Test/Main.lean) that never
#             touches the code path these guard (see #429 and that
#             directory's README.md).
#
# Env knobs (all optional):
#   MALGO             path to the malgo executable (default: the Lean build)
#   MALGO_WORK_DIR    workspace mirror (default: .malgo-work)
#   CASE_TIMEOUT      seconds per case (default: 60)
#   MODES             space-separated subset of "eval bigstep error infer-ok"
set -u

# Cases that must make `malgo eval --infer` exit 0 -- see the "infer-ok"
# check above. All three import Builtin AND Prelude directly by relative
# path while also inheriting Builtin transitively through Prelude's own
# bare `import Builtin`, so `deps` legitimately lists the identical
# Builtin.mlg under two different `ModuleName` aliases; buildDepsEnv must
# not treat that as a duplicate-export collision (#429).
INFER_OK_CASES=(
  "test/testcases/malgo/error/BuiltinPreludeDiamond.mlg"
  "test/testcases/malgo/error/ConstructorArity.mlg"
  "test/testcases/malgo/error/StringPatIsNotSupported.mlg"
)

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

MALGO="${MALGO:-lean/.lake/build/bin/malgo}"
export MALGO_WORK_DIR="${MALGO_WORK_DIR:-.malgo-work}"
CASE_TIMEOUT="${CASE_TIMEOUT:-60}"
MODES="${MODES:-eval bigstep error infer-ok}"

if [ ! -x "$MALGO" ]; then
  echo "malgo executable not found at '$MALGO' (set MALGO, or run 'lake build' in lean/)." >&2
  exit 1
fi

# Bare-name imports (e.g. `import Builtin` inside Prelude.mlg) resolve only
# through the workspace mirror, which is empty on a fresh checkout. Seeding
# is checked, not silenced: a failure here otherwise surfaces as every case
# failing with an unrelated missing-artifact error.
for module in Builtin Prelude Either; do
  if ! "$MALGO" eval "runtime/malgo/$module.mlg" >/dev/null 2>&1; then
    echo "workspace seeding failed on runtime/malgo/$module.mlg" >&2
    exit 1
  fi
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

total_fail=0

run_eval_mode() {
  local label="$1" extra="$2"
  local pass=0 fail=0
  local -a failed=()
  for golden in .golden/Malgo.Sequent.Eval/*/golden; do
    local case src
    case="$(basename "$(dirname "$golden")")"
    src="test/testcases/malgo/$case.mlg"
    [ -f "$src" ] || continue
    # Every golden was produced with "Hello\n" on stdin (see TestUtils).
    if printf 'Hello\n' | timeout "$CASE_TIMEOUT" "$MALGO" eval $extra "$src" \
         >"$WORK/$case.out" 2>"$WORK/$case.err" && cmp -s "$WORK/$case.out" "$golden"; then
      pass=$((pass + 1))
    else
      fail=$((fail + 1)); failed+=("$case")
    fi
  done
  echo "=== $label: $pass/$((pass + fail)) passed ==="
  [ "$fail" -eq 0 ] || echo "  failed: ${failed[*]}"
  total_fail=$((total_fail + fail))
}

run_error_mode() {
  local pass=0 fail=0
  local -a failed=()
  for expect in test/testcases/malgo/error/*.mlg.expect; do
    [ -e "$expect" ] || continue
    local case src
    case="$(basename "$expect" .mlg.expect)"
    src="test/testcases/malgo/error/$case.mlg"
    timeout "$CASE_TIMEOUT" "$MALGO" eval --infer "$src" >/dev/null 2>&1
    if [ $? -ne 0 ]; then
      pass=$((pass + 1))
    else
      # Exiting 0 on an error fixture is the failure: the pass named in the
      # .expect sidecar was supposed to reject this program.
      fail=$((fail + 1)); failed+=("$case(exit 0, expected $(cat "$expect"))")
    fi
  done
  echo "=== error: $pass/$((pass + fail)) passed ==="
  [ "$fail" -eq 0 ] || echo "  failed: ${failed[*]}"
  total_fail=$((total_fail + fail))
}

run_infer_ok_mode() {
  local pass=0 fail=0
  local -a failed=()
  for src in "${INFER_OK_CASES[@]}"; do
    if [ ! -f "$src" ]; then
      echo "infer-ok case not found: $src" >&2
      fail=$((fail + 1)); failed+=("$src(missing)")
      continue
    fi
    if timeout "$CASE_TIMEOUT" "$MALGO" eval --infer "$src" >/dev/null 2>&1; then
      pass=$((pass + 1))
    else
      fail=$((fail + 1)); failed+=("$src(nonzero exit)")
    fi
  done
  echo "=== infer-ok: $pass/$((pass + fail)) passed ==="
  [ "$fail" -eq 0 ] || echo "  failed: ${failed[*]}"
  total_fail=$((total_fail + fail))
}

for mode in $MODES; do
  case "$mode" in
    eval) run_eval_mode eval "" ;;
    bigstep) run_eval_mode bigstep "--eval-mode bigstep" ;;
    error) run_error_mode ;;
    infer-ok) run_infer_ok_mode ;;
    *) echo "unknown mode: $mode" >&2; exit 2 ;;
  esac
done

if [ "$total_fail" -gt 0 ]; then
  echo "=== cli-gate: $total_fail failure(s) ==="
  exit 1
fi
echo "=== cli-gate: all modes passed ==="
