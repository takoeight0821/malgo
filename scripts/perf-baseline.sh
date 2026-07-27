#!/usr/bin/env bash
# perf-baseline.sh — deterministic performance baseline for the Zig backend (#385).
#
# #385 requires that every before/after claim carry `MALGO_RC_STATS` counters,
# because "wall-clock alone will not survive review". The counters are
# deterministic and machine-independent: given the same compiler and the same
# fixture they do not vary between runs or between machines. Wall clock does, so
# it is recorded for information and never gated.
#
# The counters are always-on in the runtime (`g_total_allocs` at runtime.zig:175,
# `g_dispatches` at :606, ...); only the reporting is env-gated. So an
# instrumented run and a timed run measure the same binary and there is no
# observer cost to subtract.
#
# Gates are DIRECTIONAL, not exact-match: an optimization must not turn the gate
# red until the JSON is re-committed. `--update` rewrites the baseline, and that
# diff appearing in a PR *is* the before/after claim #385 asks for.
#
#   total_allocs      <= baseline   (gated; fewer allocations is the goal)
#   dispatches        <= baseline   (gated; fewer trampoline dispatches is the goal)
#   force_depth_max   == baseline   (gated; an invariant claim, not a budget: #382
#                                    declined to lower Force to a continuation
#                                    edge on the strength of depth 1. A rise is
#                                    a design regression; a fall means the
#                                    workload changed. Either wants a human.)
#   reuse_hits        reported, NOT gated
#   reuse_rate        reported, NOT gated
#
# reuse_hits deliberately is not a gate, which cost one false regression to
# learn. Interning small integers (#385) cut fib-deep's total_allocs by 5.8% and
# its reuse_hits by 10% at the same time: interned ints are immortal, so they are
# neither allocated nor available as reuse tokens. Reuse fell because there was
# less left to reuse, which is the optimization working, not regressing. Both
# reuse figures are ratios whose denominator legitimately moves, so only
# total_allocs -- the quantity actually being minimized -- can be gated
# absolutely. #354 work should read reuse_rate, and should hold total_allocs
# fixed while doing so.
#
# Usage:
#   scripts/perf-baseline.sh [--tier=LIST] [--update] [--timing]
#
#   --tier=LIST   comma-separated: fib-shallow,fib-deep,selfhost-l1,selfhost-l2,
#                 or `all`. Default: fib-shallow,fib-deep (the cheap tiers).
#   --update      rewrite the baseline JSON from this run instead of comparing.
#   --timing      also measure wall clock with hyperfine (local only; never CI).
#
# Env knobs:
#   MALGO             path to the malgo executable (default: the Lean build)
#   ZIG_BIN_DIR       directory containing the zig binary, prepended to PATH
#   BASELINE          baseline JSON path (default: bench/perf-baseline.json)
#   COMPILE_TIMEOUT   seconds for `malgo compile` (default: 600)
#   CASE_TIMEOUT      seconds for running a compiled binary (default: 900)
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

MALGO="${MALGO:-lean/.lake/build/bin/malgo}"
BASELINE="${BASELINE:-bench/perf-baseline.json}"
COMPILE_TIMEOUT="${COMPILE_TIMEOUT:-600}"
CASE_TIMEOUT="${CASE_TIMEOUT:-900}"

TIERS="fib-shallow,fib-deep"
UPDATE=0
TIMING=0

for arg in "$@"; do
  case "$arg" in
    --tier=*)  TIERS="${arg#--tier=}" ;;
    --update)  UPDATE=1 ;;
    --timing)  TIMING=1 ;;
    -h|--help) sed -n '2,48p' "$0"; exit 0 ;;
    *) echo "unknown argument '$arg' (see --help)" >&2; exit 2 ;;
  esac
done

if [ "$TIERS" = "all" ]; then
  TIERS="fib-shallow,fib-deep,selfhost-l1,selfhost-l2"
fi

if [ -n "${ZIG_BIN_DIR:-}" ]; then
  export PATH="$ZIG_BIN_DIR:$PATH"
fi

for tool in jq zig; do
  command -v "$tool" >/dev/null 2>&1 || {
    echo "$tool not found on PATH (run 'mise install' / activate mise)." >&2
    exit 1
  }
done

if [ ! -x "$MALGO" ]; then
  echo "malgo executable not found at '$MALGO' (set MALGO, or run 'lake build' in lean/)." >&2
  exit 1
fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Bare-name imports inside Prelude.mlg resolve only via the .malgo-work mirror,
# which is empty on a fresh checkout. Same seeding as zig-golden.sh.
for module in Builtin Prelude Either; do
  "$MALGO" eval "runtime/malgo/$module.mlg" >/dev/null 2>&1
done

# ---------------------------------------------------------------------------
# Counter capture
# ---------------------------------------------------------------------------

# Parses the runtime's stats line out of a stderr capture. The line goes to
# stderr alongside MALGO-LEAK:, so anchor on the prefix. A missing or
# unparseable line is a hard failure, never "unchanged" -- runtime.zig:787 is a
# fixed 160-byte buffer whose bufPrint error is swallowed into
# "MALGO-STATS: ?", so a fifth counter would silently degrade to `?` and a
# lenient parser would read that as no change.
parse_stats() {
  local stats_file="$1" label="$2"
  local line
  line="$(grep '^MALGO-STATS:' "$stats_file" | tail -n 1)"
  if [ -z "$line" ]; then
    echo "FAIL($label): no MALGO-STATS line on stderr" >&2
    return 1
  fi
  case "$line" in
    *'?'*) echo "FAIL($label): truncated stats line ($line) -- widen runtime.zig's bufPrint buffer" >&2; return 1 ;;
  esac
  local ta rh di fd
  ta="$(printf '%s\n' "$line" | sed -n 's/.*total_allocs=\([0-9]*\).*/\1/p')"
  rh="$(printf '%s\n' "$line" | sed -n 's/.*reuse_hits=\([0-9]*\).*/\1/p')"
  di="$(printf '%s\n' "$line" | sed -n 's/.*dispatches=\([0-9]*\).*/\1/p')"
  fd="$(printf '%s\n' "$line" | sed -n 's/.*force_depth_max=\([0-9]*\).*/\1/p')"
  for v in "$ta" "$rh" "$di" "$fd"; do
    if [ -z "$v" ]; then
      echo "FAIL($label): could not parse all four counters from: $line" >&2
      return 1
    fi
  done
  printf '%s %s %s %s\n' "$ta" "$rh" "$di" "$fd"
}

# Runs one compiled binary under MALGO_RC_STATS and echoes its four counters.
# Extra args after the binary are passed through (Level 2 needs them).
measure_binary() {
  local label="$1" expected="$2" bin="$3"; shift 3
  local out status
  out="$(printf 'Hello\n' | MALGO_RC_STATS=1 timeout "$CASE_TIMEOUT" "$bin" "$@" 2>"$WORK/stats.$label")"
  status=$?
  if [ "$status" -eq 124 ]; then
    echo "FAIL($label): timed out after ${CASE_TIMEOUT}s" >&2
    return 1
  fi
  if [ "$status" -ne 0 ]; then
    echo "FAIL($label): exited $status (83 = the runtime's leak gate, 139 = SIGSEGV)" >&2
    return 1
  fi
  if [ -n "$expected" ] && [ "$out" != "$expected" ]; then
    echo "FAIL($label): expected '$expected', got '$out'" >&2
    return 1
  fi
  parse_stats "$WORK/stats.$label" "$label"
}

compile_fixture() {
  local label="$1" src="$2" bin="$3"
  echo "  compiling $src (--opt release-fast)" >&2
  if ! timeout "$COMPILE_TIMEOUT" "$MALGO" compile "$src" -o "$bin" --opt release-fast >/dev/null; then
    echo "FAIL($label): malgo compile failed" >&2
    return 1
  fi
}

# The Level 1 evaluator: Main.mlg compiled release-fast. Both selfhost tiers
# need it, and it is the expensive step, so build it at most once.
ensure_selfhost_evaluator() {
  [ -x "$WORK/malgoc" ] && return 0
  echo "  precompiling the self-hosted compiler's modules" >&2
  local f
  for f in runtime/malgo/Builtin.mlg runtime/malgo/Prelude.mlg runtime/malgo/Either.mlg \
           runtime/malgo/compiler/AST.mlg runtime/malgo/compiler/Token.mlg \
           runtime/malgo/compiler/Diagnostic.mlg runtime/malgo/compiler/Lexer.mlg \
           runtime/malgo/compiler/Parser.mlg runtime/malgo/compiler/Value.mlg \
           runtime/malgo/compiler/Eval.mlg runtime/malgo/compiler/FunIR.mlg \
           runtime/malgo/compiler/Rename.mlg runtime/malgo/compiler/ToFun.mlg \
           runtime/malgo/compiler/Main.mlg; do
    if ! timeout "$COMPILE_TIMEOUT" "$MALGO" eval "$f" </dev/null >/dev/null; then
      echo "FAIL: precompile failed on $f" >&2
      return 1
    fi
  done
  compile_fixture selfhost runtime/malgo/compiler/Main.mlg "$WORK/malgoc"
}

# Echoes "<tier> <total_allocs> <reuse_hits> <dispatches> <force_depth_max>".
run_tier() {
  local tier="$1" counters
  echo "=== $tier ===" >&2
  case "$tier" in
    fib-shallow)
      compile_fixture "$tier" bench/fixtures/BenchFib.mlg "$WORK/fib" || return 1
      counters="$(measure_binary "$tier" 610 "$WORK/fib")" || return 1
      ;;
    fib-deep)
      compile_fixture "$tier" bench/fixtures/BenchFibDeep.mlg "$WORK/fibdeep" || return 1
      counters="$(measure_binary "$tier" 75025 "$WORK/fibdeep")" || return 1
      ;;
    selfhost-l1)
      ensure_selfhost_evaluator || return 1
      counters="$(measure_binary "$tier" 8 "$WORK/malgoc" test/testcases/malgo/Fib.mlg)" || return 1
      ;;
    selfhost-l2)
      # Serial, single-case, deliberately NOT selfhost-level2.sh: that runs five
      # cases in parallel and discards each case's stderr, so its numbers are
      # neither isolated nor capturable.
      ensure_selfhost_evaluator || return 1
      counters="$(measure_binary "$tier" 8 "$WORK/malgoc" runtime/malgo/compiler/Main.mlg test/testcases/malgo/Fib.mlg)" || return 1
      ;;
    *)
      echo "unknown tier '$tier' (fib-shallow|fib-deep|selfhost-l1|selfhost-l2|all)" >&2
      return 1
      ;;
  esac
  printf '%s %s\n' "$tier" "$counters"
}

# ---------------------------------------------------------------------------
# Run the requested tiers
# ---------------------------------------------------------------------------

RESULTS="$WORK/results"
: >"$RESULTS"
failed=0

old_ifs="$IFS"; IFS=','
for tier in $TIERS; do
  IFS="$old_ifs"
  if ! run_tier "$tier" >>"$RESULTS"; then
    failed=1
    [ "$UPDATE" -eq 1 ] && { echo "refusing to --update from a failed run" >&2; exit 1; }
  fi
  IFS=','
done
IFS="$old_ifs"

# ---------------------------------------------------------------------------
# Compare or update
# ---------------------------------------------------------------------------

results_to_json() {
  jq -n --rawfile raw "$RESULTS" --arg commit "$(git rev-parse --short HEAD 2>/dev/null || echo unknown)" \
        --arg zig "$(zig version 2>/dev/null || echo unknown)" '
    def rows: $raw | split("\n") | map(select(length > 0) | split(" "));
    {
      "$comment": "LIVE Zig-backend perf baseline for #385. Unrelated to bench/baseline-*.json, which are 2026-03 GHC *build*-time records. Regenerate with `mise run perf-baseline -- --update`; the diff of this file IS the before/after claim #385 requires. Gates are directional -- see scripts/perf-baseline.sh.",
      compiler: { commit: $commit, zig: $zig },
      tiers: (rows | map({
        key: .[0],
        value: {
          counters: {
            total_allocs: (.[1] | tonumber),
            reuse_hits: (.[2] | tonumber),
            dispatches: (.[3] | tonumber),
            force_depth_max: (.[4] | tonumber)
          },
          derived: {
            reuse_rate: (if (.[1] | tonumber) > 0
                         then (((.[2] | tonumber) / (.[1] | tonumber)) * 10000 | round) / 10000
                         else 0 end)
          }
        }
      }) | from_entries)
    }'
}

if [ "$UPDATE" -eq 1 ]; then
  # Merge, so updating a cheap tier does not drop an expensive tier's row.
  # jq's `*` on two objects merges recursively, which is exactly the semantics
  # wanted here: every key a fresh row supplies wins, absent tiers survive.
  if [ -f "$BASELINE" ]; then
    results_to_json >"$WORK/new.json" || exit 1
    jq -s '.[0] * .[1]' "$BASELINE" "$WORK/new.json" >"$WORK/merged.json" || exit 1
    mv "$WORK/merged.json" "$BASELINE"
  else
    results_to_json >"$BASELINE" || exit 1
  fi
  echo "updated $BASELINE:"
  jq . "$BASELINE"
  exit 0
fi

if [ ! -f "$BASELINE" ]; then
  echo "NOTICE: no baseline at $BASELINE -- skipping the perf gate."
  echo "        seed it with: mise run perf-baseline -- --tier=all --update"
  cat "$RESULTS"
  exit "$failed"
fi

regressions=0
while read -r tier ta rh di fd; do
  [ -n "$tier" ] || continue
  base="$(jq -r --arg t "$tier" '.tiers[$t] // empty' "$BASELINE")"
  if [ -z "$base" ]; then
    echo "NOTICE($tier): no baseline row -- skipping (add it with --update)"
    continue
  fi
  b_ta="$(printf '%s' "$base" | jq -r '.counters.total_allocs')"
  b_rh="$(printf '%s' "$base" | jq -r '.counters.reuse_hits')"
  b_di="$(printf '%s' "$base" | jq -r '.counters.dispatches')"
  b_fd="$(printf '%s' "$base" | jq -r '.counters.force_depth_max')"

  echo "--- $tier ---"
  printf '  total_allocs    %12s  (baseline %12s)\n' "$ta" "$b_ta"
  printf '  reuse_hits      %12s  (baseline %12s)\n' "$rh" "$b_rh"
  printf '  dispatches      %12s  (baseline %12s)\n' "$di" "$b_di"
  printf '  force_depth_max %12s  (baseline %12s)\n' "$fd" "$b_fd"

  [ "$ta" -le "$b_ta" ] || { echo "  REGRESSION: total_allocs rose by $((ta - b_ta))"; regressions=1; }
  [ "$di" -le "$b_di" ] || { echo "  REGRESSION: dispatches rose by $((di - b_di))"; regressions=1; }
  [ "$fd" -eq "$b_fd" ] || { echo "  REGRESSION: force_depth_max changed ($b_fd -> $fd) -- see #382"; regressions=1; }

  # Not a gate -- see the header. Reported so #354 work can watch it, and so a
  # reuse drop that is NOT explained by an allocation drop still gets noticed.
  if [ "$rh" -lt "$b_rh" ]; then
    if [ "$ta" -lt "$b_ta" ]; then
      echo "  note: reuse_hits fell by $((b_rh - rh)), alongside $((b_ta - ta)) fewer allocations (less left to reuse)"
    else
      echo "  WARNING: reuse_hits fell by $((b_rh - rh)) with no drop in total_allocs -- reuse itself got worse (#354)"
    fi
  fi

  if [ "$ta" -lt "$b_ta" ] || [ "$di" -lt "$b_di" ]; then
    echo "  IMPROVED -- re-run with --update to record it"
  fi
done <"$RESULTS"

# ---------------------------------------------------------------------------
# Wall clock (informational, local only)
# ---------------------------------------------------------------------------

if [ "$TIMING" -eq 1 ]; then
  if command -v hyperfine >/dev/null 2>&1; then
    echo "=== wall clock ($(uname -s) $(uname -m)) -- informational, never gated ==="
    if [ -x "$WORK/fibdeep" ]; then
      hyperfine --warmup 1 --runs 5 -n fib-deep "$WORK/fibdeep"
    fi
    if [ -x "$WORK/malgoc" ]; then
      hyperfine --warmup 1 --runs 3 -i \
        -n selfhost-l1 "$WORK/malgoc test/testcases/malgo/Fib.mlg" \
        -n selfhost-l2 "$WORK/malgoc runtime/malgo/compiler/Main.mlg test/testcases/malgo/Fib.mlg"
    fi
  else
    echo "NOTICE: hyperfine not on PATH -- skipping timings." >&2
  fi
fi

if [ "$regressions" -ne 0 ] || [ "$failed" -ne 0 ]; then
  echo "=== perf baseline FAILED ==="
  exit 1
fi
echo "=== perf baseline OK ==="
