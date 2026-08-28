# Claude Code on the web

This repo's toolchain (elan/Lean, mise, Zig, Chez Scheme) pulls binaries
from several vendor origins that Claude Code on the web's **Trusted**
network access level does not allowlist by default. Without configuring
the cloud environment as described here, `lake build` fails partway
through with a network error rather than a missing-tool error, which is
easy to misdiagnose. Configuration lives in the environment dialog at
[claude.ai/code](https://claude.ai/code) (the cloud icon above the message
box, or **Cloud environments** in org admin settings for a shared
environment) — see [Configure cloud environments](https://code.claude.com/docs/en/cloud-environments).

## Network access

Elan's installer script is served from `raw.githubusercontent.com`
(Trusted by default), but the pinned Lean toolchain it downloads
afterwards comes from `release.lean-lang.org`, which is not. Verified by
running the installer under a Trusted-only policy: it succeeds, and the
first `lake build` then fails with `error during download ... CONNECT
tunnel failed, response 403` against that host specifically.

Set **Network access** to **Custom**, keep **Also include default list of
common package managers** checked, and add:

```
release.lean-lang.org
elan.lean-lang.org
ziglang.org
mise.jdx.dev
```

`release.lean-lang.org`/`elan.lean-lang.org` are required for any
`lake build`. `ziglang.org` is required for the Zig backend (`malgo
compile`, `zig-golden`, self-hosting) but not for the plain Lean test
suite (`lake test` only shells out to `zig` from the `malgo compile`
code path — see `lean/Malgo/Backend/Zig/Toolchain.lean` — not from the
golden/gate tests `lake test` itself runs). `mise.jdx.dev` is required to
install `mise` itself, which this repo's `mise.toml` uses to pin `zig`
and `chezscheme` versions.

Chez Scheme (`mise run scheme-golden`) is optional and not covered above:
`mise`'s `chezscheme` plugin builds it from source from a host we haven't
pinned down here. If you need the Scheme backend gates, use **Full**
network access instead of Custom for this environment.

## Environment variables

Optional, for parity with `.devcontainer/post-create.sh`'s locale setup:

```
LANG=C.UTF-8
LC_ALL=C.UTF-8
LC_CTYPE=C.UTF-8
```

## Setup script

Paste into the environment dialog's **Setup script** field. This is
VM-level provisioning only (no dependency on the repo being cloned yet —
see [Setup scripts vs. SessionStart hooks](https://code.claude.com/docs/en/cloud-environments#setup-scripts-vs-sessionstart-hooks)),
so it's cached across sessions in this environment for about a week
instead of re-running every time. It installs elan, mise, and pre-fetches
the exact toolchain versions this repo pins — `lean/lean-toolchain` and
`mise.toml`'s `zig`/`chezscheme` — so update the hardcoded versions below
if those files change. If you forget: it isn't a correctness bug, only a
speed one. The SessionStart hook below reconciles against the actual
checkout every session (`lake build` auto-resolves the toolchain from
`lean/lean-toolchain`; `mise install` re-resolves `zig`/`chezscheme` from
`mise.toml`), so a stale pre-fetch here just means that reconciliation
falls back to a network fetch instead of using the cached one.

```bash
#!/bin/bash
set -uo pipefail
export DEBIAN_FRONTEND=noninteractive

# Headers Chez Scheme's from-source build needs (see mise.toml's chezscheme
# comment); harmless no-op if network access doesn't reach its build source.
# apt-get update and install are split so one broken/unreachable repo in
# update doesn't skip the install via a short-circuited &&.
apt-get update -qq >/dev/null 2>&1 || true
apt-get install -y -qq --no-install-recommends \
  build-essential libx11-dev libncurses-dev >/dev/null 2>&1 || true

# elan (Lean 4 toolchain manager) -- installer is GitHub-hosted, Trusted by
# default; the toolchain fetch that follows needs release.lean-lang.org
# (see "Network access" above).
if ! command -v elan >/dev/null 2>&1; then
  curl --retry 3 --retry-connrefused -fsSf \
    https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
    | sh -s -- -y --default-toolchain none || true
fi
export PATH="$HOME/.elan/bin:$PATH"
# Keep in sync with lean/lean-toolchain.
elan toolchain install leanprover/lean4:v4.32.0 || true

# mise (pins zig/chezscheme for this repo's tasks; see mise.toml).
if ! command -v mise >/dev/null 2>&1; then
  curl --retry 3 -fsSL https://mise.jdx.dev/install.sh | sh || true
fi
export PATH="$HOME/.local/bin:$HOME/.local/share/mise/shims:$PATH"
# Keep in sync with mise.toml's [tools] versions.
mise use -g zig@0.16.0 || true
mise use -g chezscheme@10.4.1 || true
```

This deliberately doesn't try to persist `PATH` for later shells (e.g. via
`/etc/profile.d`): Claude Code's own commands run as non-login shells that
wouldn't source it anyway, and it's not the mechanism that actually
matters here — the SessionStart hook below exports `PATH` through
`$CLAUDE_ENV_FILE`, which is guaranteed to reach every command Claude
runs.

## SessionStart hook

`.claude/settings.json` registers `.claude/hooks/session-start.sh` with
`"matcher": "startup|resume"`, so it runs once per fresh cloud session (and
on resume), not on every `/clear`/compact. It's a no-op locally since it
checks `CLAUDE_CODE_REMOTE`. Unlike the setup script above, a fresh clone
has no `.lake` build cache, so it re-runs `lake build` every session —
this is the one setup step per-session latency can't avoid. Besides
exporting `PATH` for what the setup script installed, it also runs `mise
install` from the checkout so `zig`/`chezscheme` get reconciled against
`mise.toml` even if the setup script's own pre-fetch drifted out of sync
(see the note above). It does not install elan or the Lean toolchain
itself — if it reports `elan: NOT FOUND`, the setup script above hasn't
run or failed. If `lake build` fails, it greps the build output for the
network-fetch error signature (`CONNECT tunnel failed` / `error during
download`) before blaming network access, so an unrelated compile error
isn't misdiagnosed as a missing domain.

Running the hook (synchronous) adds the fresh `lake build`'s time to every
session start, in exchange for `mise run test`/`lake test`/lint working
immediately once the session opens. If that startup latency isn't
acceptable, change the hook's `SessionStart` registration to async
(`echo '{"async": true, "asyncTimeout": 300000}'` as the hook's first line
of output) so Claude Code launches immediately and the build finishes in
the background — at the cost of Claude possibly running `lake build`/`lake
test` itself before that background build finishes.
