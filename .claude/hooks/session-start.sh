#!/bin/bash
# SessionStart hook: makes `lake build`/`lake test`/`mise run ...` work in a
# fresh Claude Code on the web session without manual setup.
#
# Heavy, environment-wide toolchain provisioning (elan, the pinned Lean
# toolchain, mise, zig, chezscheme) belongs in the cloud environment's
# *Setup script* instead, because that step is cached across sessions for
# ~7 days -- see docs/claude-code-web.md. This hook only does the
# repo-aware, per-session work: exporting PATH for tools the setup script
# installed, reconciling mise's pinned tools against this checkout, and
# compiling it (a fresh clone has no .lake build cache, so that's
# unavoidable each session).
set -uo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# $CLAUDE_ENV_FILE/$CLAUDE_PROJECT_DIR are always set alongside
# $CLAUDE_CODE_REMOTE=true in a normal cloud session, but default them
# explicitly (matching the CLAUDE_CODE_REMOTE check above) so a harness
# variant that doesn't set them yet gets this diagnostic under `set -u`
# instead of an "unbound variable" crash.
env_file="${CLAUDE_ENV_FILE:-}"
project_dir="${CLAUDE_PROJECT_DIR:-}"
if [ -z "$env_file" ] || [ -z "$project_dir" ]; then
  echo "session-start hook: CLAUDE_ENV_FILE or CLAUDE_PROJECT_DIR is not set; skipping." >&2
  exit 0
fi

# Tools installed by the environment's Setup script live under these
# locations regardless of which session VM you land on (see
# docs/claude-code-web.md for the setup script that puts them there).
# Written once with a dedupe check: this hook can fire more than once per
# session (resume/compact), and $CLAUDE_ENV_FILE persists across firings.
path_line='export PATH="$HOME/.elan/bin:$HOME/.local/bin:$HOME/.local/share/mise/shims:$PATH"'
grep -qF "$path_line" "$env_file" 2>/dev/null || echo "$path_line" >> "$env_file"
eval "$path_line"

echo "== malgo web session setup =="
elan_path="$(command -v elan || true)"
lean_path="$(command -v lean || true)"
lake_path="$(command -v lake || true)"
mise_path="$(command -v mise || true)"
zig_path="$(command -v zig || true)"
for pair in "elan=$elan_path" "lean=$lean_path" "lake=$lake_path" "mise=$mise_path" "zig=$zig_path"; do
  name="${pair%%=*}"
  path="${pair#*=}"
  if [ -n "$path" ]; then
    echo "  $name: $path"
  else
    echo "  $name: NOT FOUND"
  fi
done

if [ -z "$elan_path" ]; then
  echo "elan is missing -- the environment's Setup script did not run or failed." >&2
  echo "See docs/claude-code-web.md to configure it." >&2
  exit 0
fi

if [ -n "$mise_path" ]; then
  mise_log="$(mise trust --yes "$project_dir/mise.toml" 2>&1)" || {
    echo "mise trust failed:" >&2
    echo "$mise_log" >&2
  }
  # Repo-aware reconciliation: (re-)installs mise.toml's pinned zig/chezscheme
  # from the actual checkout, a no-op if the Setup script's own hardcoded
  # pre-fetch (docs/claude-code-web.md) already matches -- and self-heals if
  # it drifted out of sync, at the cost of a network fetch instead of the
  # Setup script's cached one.
  mise_log="$(cd "$project_dir" && mise install 2>&1)" || {
    echo "mise install reported problems:" >&2
    echo "$mise_log" >&2
  }
fi

echo "Building the compiler (lake build)..."
build_output="$(cd "$project_dir/lean" && lake build 2>&1)"
build_status=$?
echo "$build_output"
if [ "$build_status" -ne 0 ]; then
  echo "lake build failed (exit $build_status)." >&2
  if echo "$build_output" | grep -qE 'CONNECT tunnel failed|error during download'; then
    echo "That looks like a blocked network fetch, not a compile error --" >&2
    echo "see docs/claude-code-web.md for the domains this repo's toolchain needs" >&2
    echo "(release.lean-lang.org, elan.lean-lang.org, ziglang.org, mise.jdx.dev)." >&2
  else
    echo "That doesn't match the known network-access failure signature --" >&2
    echo "check the build output above for a real compile error or regression." >&2
  fi
fi

exit 0
