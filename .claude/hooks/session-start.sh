#!/bin/bash
# SessionStart hook: makes `lake build`/`lake test`/`mise run ...` work in a
# fresh Claude Code on the web session without manual setup.
#
# Heavy, environment-wide toolchain provisioning (elan, the pinned Lean
# toolchain, mise, zig, chezscheme) belongs in the cloud environment's
# *Setup script* instead, because that step is cached across sessions for
# ~7 days -- see docs/agents/web-environment.md. This hook only does the
# repo-aware, per-session work: exporting PATH for tools the setup script
# installed, trusting mise.toml, and compiling this checkout (a fresh clone
# has no .lake build cache, so this is unavoidable each session).
set -uo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Tools installed by the environment's Setup script live under these
# locations regardless of which session VM you land on (see
# docs/agents/web-environment.md for the setup script that puts them there).
for line in \
  'export PATH="$HOME/.elan/bin:$PATH"' \
  'export PATH="$HOME/.local/bin:$HOME/.local/share/mise/shims:$PATH"'
do
  echo "$line" >> "$CLAUDE_ENV_FILE"
  eval "$line"
done

echo "== malgo web session setup =="
for tool in elan lean lake mise zig; do
  if command -v "$tool" >/dev/null 2>&1; then
    echo "  $tool: $(command -v "$tool")"
  else
    echo "  $tool: NOT FOUND"
  fi
done

if ! command -v elan >/dev/null 2>&1; then
  echo "elan is missing -- the environment's Setup script did not run or failed." >&2
  echo "See docs/agents/web-environment.md to configure it." >&2
  exit 0
fi

if command -v mise >/dev/null 2>&1; then
  mise trust --yes "$CLAUDE_PROJECT_DIR/mise.toml" 2>&1 || true
fi

echo "Building the compiler (lake build)..."
if ! (cd "$CLAUDE_PROJECT_DIR/lean" && lake build); then
  echo "lake build failed -- likely missing network access to release.lean-lang.org" >&2
  echo "for the pinned Lean toolchain. See docs/agents/web-environment.md." >&2
fi

exit 0
