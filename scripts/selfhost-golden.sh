#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT"

MALGO=${MALGO:-cabal exec malgo --}
SCHEME=${SCHEME:-scheme}
CASE_TIMEOUT=${CASE_TIMEOUT:-20}
PRECOMPILE_TIMEOUT=${PRECOMPILE_TIMEOUT:-180}
KEEP_WORK=${KEEP_WORK:-0}
PARALLEL_JOBS=${PARALLEL_JOBS:-$(nproc 2>/dev/null || echo 4)}
MALGO_WORK_DIR=${MALGO_WORK_DIR:-.malgo-work}

timestamp() {
  date '+%Y-%m-%d %H:%M:%S'
}

log() {
  printf '[%s] %s\n' "$(timestamp)" "$*"
}

if [[ "$KEEP_WORK" != "1" ]]; then
  log "cleaning work directory: $MALGO_WORK_DIR"
  rm -rf "$MALGO_WORK_DIR"
fi

if [[ "$MALGO" == *cabal* ]]; then
  log "building malgo executable"
  cabal build exe:malgo >/dev/null
  log "build complete"
fi

precompile=(
  runtime/malgo/Builtin.mlg
  runtime/malgo/Prelude.mlg
  runtime/malgo/Either.mlg
  runtime/malgo/compiler/AST.mlg
  runtime/malgo/compiler/Token.mlg
  runtime/malgo/compiler/Diagnostic.mlg
  runtime/malgo/compiler/Lexer.mlg
  runtime/malgo/compiler/Parser.mlg
  runtime/malgo/compiler/Value.mlg
  runtime/malgo/compiler/Eval.mlg
  runtime/malgo/compiler/FunIR.mlg
  runtime/malgo/compiler/Rename.mlg
  runtime/malgo/compiler/ToFun.mlg
  runtime/malgo/compiler/Main.mlg
)

for file in "${precompile[@]}"; do
  start=$SECONDS
  log "precompile start: $file"
  if ! timeout "$PRECOMPILE_TIMEOUT" $MALGO eval "$file" </dev/null >/dev/null; then
    log "precompile failed: $file"
    exit 1
  fi
  elapsed=$((SECONDS - start))
  log "precompile done: $file (${elapsed}s)"
done

log "precompile phase complete (${#precompile[@]} files)"

SCHEME_MAIN="$MALGO_WORK_DIR/main.scm"
mkdir -p "$MALGO_WORK_DIR"
log "compiling Main.mlg to Scheme"
if ! $MALGO eval --target scheme runtime/malgo/compiler/Main.mlg > "$SCHEME_MAIN"; then
  log "Scheme compilation failed"
  exit 1
fi
log "Scheme compilation done"

mapfile -t cases < <(find .golden/Malgo.Sequent.Eval -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort)
total_cases=${#cases[@]}
log "starting golden checks: ${total_cases} cases (parallelism: ${PARALLEL_JOBS})"

result_dir=$(mktemp -d)
trap 'rm -rf "$result_dir"' EXIT

# Run each test case in a background subshell, throttled to PARALLEL_JOBS
# concurrent workers. Results are written to per-case temp files and
# aggregated after all workers finish.
active_jobs=0
for i in "${!cases[@]}"; do
  dir="${cases[$i]}"
  index=$((i + 1))
  src="test/testcases/malgo/$dir.mlg"
  expected=".golden/Malgo.Sequent.Eval/$dir/golden"
  result_file="$result_dir/$i"

  if [[ ! -f "$src" ]]; then
    echo "skip" > "$result_file"
    continue
  fi

  (
    case_start=$SECONDS
    log "[$index/$total_cases] start: $dir"
    out=$(mktemp)
    err=$(mktemp)
    if printf 'Hello\n' | timeout "$CASE_TIMEOUT" $SCHEME --script "$SCHEME_MAIN" "$src" >"$out" 2>"$err"; then
      case_elapsed=$((SECONDS - case_start))
      if cmp -s "$out" "$expected"; then
        log "[$index/$total_cases] pass: $dir (${case_elapsed}s)"
        echo "pass" > "$result_file"
      else
        log "[$index/$total_cases] fail(mismatch): $dir (${case_elapsed}s)"
        first=$(head -n 1 "$out")
        [[ -n "$first" ]] || first="<empty>"
        printf '%s\001mismatch\001%s\n' "$dir" "$first" > "$result_file"
      fi
    else
      status=$?
      case_elapsed=$((SECONDS - case_start))
      if [[ $status -eq 124 ]]; then
        log "[$index/$total_cases] fail(timeout): $dir (${case_elapsed}s)"
        printf '%s\001timeout\001<timeout>\n' "$dir" > "$result_file"
      else
        first=$(head -n 1 "$err")
        [[ -n "$first" ]] || first=$(head -n 1 "$out")
        [[ -n "$first" ]] || first="<empty>"
        log "[$index/$total_cases] fail(error $status): $dir (${case_elapsed}s)"
        printf '%s\001error\001%s\n' "$dir" "$first" > "$result_file"
      fi
    fi
    rm -f "$out" "$err"
  ) &
  active_jobs=$((active_jobs + 1))
  if [[ $active_jobs -ge $PARALLEL_JOBS ]]; then
    wait -n 2>/dev/null || wait
    active_jobs=$((active_jobs - 1))
  fi
done
wait

log "golden checks complete"

pass=0
fail=0
timeout_count=0
printed=0
max_print=${MAX_FAILURES:-40}

for i in "${!cases[@]}"; do
  result_file="$result_dir/$i"
  [[ -f "$result_file" ]] || continue
  result=$(< "$result_file")
  case "$result" in
    pass)
      pass=$((pass + 1))
      ;;
    skip)
      ;;
    *)
      fail=$((fail + 1))
      dir=$(cut -d $'\001' -f1 <<< "$result")
      kind=$(cut -d $'\001' -f2 <<< "$result")
      msg=$(cut -d $'\001' -f3 <<< "$result")
      if [[ "$kind" == "timeout" ]]; then
        timeout_count=$((timeout_count + 1))
        reason="TIMEOUT :: <timeout>"
      elif [[ "$kind" == "mismatch" ]]; then
        reason="MISMATCH :: $msg"
      else
        reason="ERR :: $msg"
      fi
      if [[ $printed -lt $max_print ]]; then
        printf '%s :: %s\n' "$dir" "$reason"
        printed=$((printed + 1))
      fi
      ;;
  esac
done

printf 'PASS: %s\nFAIL: %s\nTIMEOUT: %s\n' "$pass" "$fail" "$timeout_count"
[[ $fail -eq 0 ]]
