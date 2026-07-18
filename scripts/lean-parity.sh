#!/usr/bin/env bash
# Cross-implementation parity harness: runs the Haskell and Lean `malgo`
# binaries over the same testcases and diffs their output. Unlike the
# golden-file gates, this catches divergence introduced on EITHER side after
# the goldens were last regenerated -- it's the check that a change to one
# implementation didn't silently drift from the other.
#
# Modes (space-separated list, `--mode m1 m2 ...`; default: all of eval,
# bigstep, fingerprint):
#   eval         `<impl> eval SOURCE` with stdin "Hello\n"; stdout + exit
#                code must match exactly (hard gate). stderr is reported but
#                not compared -- error message text is not required to match.
#   bigstep      same as eval, with `--eval-mode bigstep`.
#   fingerprint  `<impl> dump --stage flat-fingerprint|join-fingerprint
#                SOURCE`; output must match exactly (format-immune IR
#                counts, so this is robust to uniq-numbering/formatting
#                differences that a golden-file diff would flag).
#   scheme, zig, error -- not yet implemented; each prints a note and is
#                skipped (not a failure), pending M3/M4/M5.
#
# Env knobs (all optional):
#   HS_MALGO       path to the Haskell malgo executable (default: `cabal
#                  list-bin exe:malgo`)
#   LEAN_MALGO     path to the Lean malgo executable (default:
#                  lean/.lake/build/bin/malgo)
#   CASE_TIMEOUT   seconds allowed per case per implementation (default: 10)
#   MAX_FAILURES   stop after this many failing cases (default: unlimited)
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

MODES=()
while [ $# -gt 0 ]; do
  case "$1" in
    --mode)
      shift
      while [ $# -gt 0 ] && [[ "$1" != --* ]]; do
        MODES+=("$1")
        shift
      done
      ;;
    *)
      echo "unknown argument: $1" >&2
      exit 2
      ;;
  esac
done
if [ "${#MODES[@]}" -eq 0 ]; then
  MODES=(eval bigstep fingerprint)
fi

if [ -z "${HS_MALGO:-}" ]; then
  HS_MALGO="$(cabal list-bin exe:malgo 2>/dev/null || true)"
fi
LEAN_MALGO="${LEAN_MALGO:-lean/.lake/build/bin/malgo}"
CASE_TIMEOUT="${CASE_TIMEOUT:-10}"
MAX_FAILURES="${MAX_FAILURES:-999999}"

if [ -z "$HS_MALGO" ] || [ ! -x "$HS_MALGO" ]; then
  echo "Haskell malgo executable not found (set HS_MALGO or run 'cabal build exe:malgo')." >&2
  exit 1
fi
if [ ! -x "$LEAN_MALGO" ]; then
  echo "Lean malgo executable not found at $LEAN_MALGO (set LEAN_MALGO or build 'lake build malgo' in lean/)." >&2
  exit 1
fi

TESTCASE_DIR="test/testcases/malgo"

# Seed each implementation's workspace mirror so bare-name imports
# (`import Builtin`) resolve (see Malgo.Module.searchAndRegister and its
# Lean port) -- same protocol as scripts/zig-golden.sh.
seed_workspace() {
  local malgo="$1" work_dir="$2"
  for module in Builtin Prelude Either; do
    MALGO_WORK_DIR="$work_dir" "$malgo" eval "runtime/malgo/$module.mlg" >/dev/null 2>&1
  done
}
rm -rf .malgo-work .malgo-work-lean
seed_workspace "$HS_MALGO" .malgo-work
seed_workspace "$LEAN_MALGO" .malgo-work-lean

WORK="$(mktemp -d)"
cleanup() { rm -rf "$WORK"; }
trap cleanup EXIT

total_pass=0
total_fail=0

run_eval_mode() {
  local mode_name="$1" extra_flag="$2"
  local pass=0 fail=0
  local -a fail_names=()
  for src in "$TESTCASE_DIR"/*.mlg; do
    case=$(basename "$src" .mlg)
    hs_out="$WORK/$case.hs.out"
    lean_out="$WORK/$case.lean.out"
    printf 'Hello\n' | MALGO_WORK_DIR=.malgo-work timeout "$CASE_TIMEOUT" \
      "$HS_MALGO" eval $extra_flag "$src" >"$hs_out" 2>"$WORK/$case.hs.err"
    hs_exit=$?
    printf 'Hello\n' | MALGO_WORK_DIR=.malgo-work-lean timeout "$CASE_TIMEOUT" \
      "$LEAN_MALGO" eval $extra_flag "$src" >"$lean_out" 2>"$WORK/$case.lean.err"
    lean_exit=$?
    if [ "$hs_exit" -eq "$lean_exit" ] && cmp -s "$hs_out" "$lean_out"; then
      pass=$((pass + 1))
    else
      fail=$((fail + 1))
      fail_names+=("$case")
    fi
    if [ "$((total_fail + fail))" -ge "$MAX_FAILURES" ]; then
      echo "Stopping early: reached MAX_FAILURES=$MAX_FAILURES"
      break
    fi
  done
  echo "=== $mode_name: $pass/$((pass + fail)) passed ==="
  if [ "$fail" -gt 0 ]; then
    echo "  mismatch: ${fail_names[*]}"
  fi
  total_pass=$((total_pass + pass))
  total_fail=$((total_fail + fail))
}

run_fingerprint_mode() {
  local pass=0 fail=0
  local -a fail_names=()
  for stage in flat-fingerprint join-fingerprint; do
    for src in "$TESTCASE_DIR"/*.mlg; do
      case=$(basename "$src" .mlg)
      hs_out=$(MALGO_WORK_DIR=.malgo-work timeout "$CASE_TIMEOUT" "$HS_MALGO" dump --stage "$stage" "$src" 2>/dev/null)
      lean_out=$(MALGO_WORK_DIR=.malgo-work-lean timeout "$CASE_TIMEOUT" "$LEAN_MALGO" dump --stage "$stage" "$src" 2>/dev/null)
      if [ "$hs_out" = "$lean_out" ]; then
        pass=$((pass + 1))
      else
        fail=$((fail + 1))
        fail_names+=("$stage/$case")
      fi
      if [ "$((total_fail + fail))" -ge "$MAX_FAILURES" ]; then
        echo "Stopping early: reached MAX_FAILURES=$MAX_FAILURES"
        break 2
      fi
    done
  done
  echo "=== fingerprint: $pass/$((pass + fail)) passed ==="
  if [ "$fail" -gt 0 ]; then
    echo "  mismatch: ${fail_names[*]}"
  fi
  total_pass=$((total_pass + pass))
  total_fail=$((total_fail + fail))
}

for mode in "${MODES[@]}"; do
  case "$mode" in
    eval) run_eval_mode eval "" ;;
    bigstep) run_eval_mode bigstep "--eval-mode bigstep" ;;
    fingerprint) run_fingerprint_mode ;;
    scheme|zig|error)
      echo "=== $mode: not yet implemented, skipping ==="
      ;;
    *)
      echo "unknown mode: $mode" >&2
      exit 2
      ;;
  esac
done

echo ""
echo "=== lean-parity total: $total_pass/$((total_pass + total_fail)) passed ==="
[ "$total_fail" -eq 0 ]
