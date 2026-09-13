#!/usr/bin/env bash
# The Zig runtime's own unit tests.
#
# A script rather than a bare command in CI and the docs, because the two flags
# are both load-bearing and both easy to forget:
#
#   -lc     the runtime calls std.c.write/std.c.getenv directly (see
#           runtime.zig's module doc). macOS links libc unconditionally as part
#           of libSystem, so a missing -lc only fails on Linux.
#   -fllvm  generated code and the runtime's helpers use
#           `@call(.always_tail, ..)`, which Zig's self-hosted x86_64 backend
#           cannot emit -- and that backend is the default for Debug on x86_64,
#           so a missing -fllvm only fails on x86_64.
#
# Each of them fails on exactly one platform, which is how both were found by
# CI rather than locally. Keeping the invocation in one file is what stops the
# next flag from having to be remembered in three.
#
# Env knobs (all optional):
#   ZIG_BIN_DIR   directory containing the zig binary, prepended to PATH
set -u

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

if [ -n "${ZIG_BIN_DIR:-}" ]; then
  export PATH="$ZIG_BIN_DIR:$PATH"
fi

if ! command -v zig >/dev/null 2>&1; then
  echo "zig not found on PATH (set ZIG_BIN_DIR or run 'mise install' / activate mise)." >&2
  exit 1
fi

exec zig test -lc -fllvm runtime/zig/runtime.zig
