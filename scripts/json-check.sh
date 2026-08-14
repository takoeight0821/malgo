#!/usr/bin/env bash
# JSON parser/merge/serializer regression gate (runtime/malgo/Json.mlg).
#
# Deliberately NOT `test/testcases/malgo` cases, same reasoning as
# scheme-process-check.sh/scheme-file-io-check.sh: any corpus fixture that
# imports Builtin.mlg + Prelude.mlg directly *and* a third relative-path-
# importing runtime module (Json.mlg) hits a pre-existing `--infer`
# module-diamond bug in the query engine's dependency resolver. See the
# comment at the top of each fixture for details.
#
# Each fixture prints its own "ok"/"FAIL" assertions, so a mismatch shows
# up as a FAIL line on stdout, not just a diff. This script runs each one
# on both the interpreter and the Scheme backend and checks both.
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

SRCS=(
  "test/testcases/scheme-only/JsonQueryLikeJq.mlg"
  "test/testcases/scheme-only/JsonMergeSerialize.mlg"
)

for module in Builtin Prelude Json; do
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

for SRC in "${SRCS[@]}"; do
  echo "=== $SRC: interpreter (oracle) ==="
  if ! interp_out="$(timeout "$CASE_TIMEOUT" "$MALGO" eval "$SRC" 2>"$WORK/interp.err")"; then
    echo "FAIL: interpreter run failed:" >&2
    cat "$WORK/interp.err" >&2
    exit 1
  fi
  echo "$interp_out"
  check_no_fail "interpreter ($SRC)" "$interp_out"

  echo "=== $SRC: scheme backend ==="
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
  check_no_fail "scheme backend ($SRC)" "$scheme_out"

  if [ "$interp_out" != "$scheme_out" ]; then
    echo "FAIL: interpreter and scheme backend disagree on $SRC" >&2
    diff <(echo "$interp_out") <(echo "$scheme_out") >&2
    exit 1
  fi
done

echo "=== json-check OK ==="
