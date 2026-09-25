---
name: dep-upgrade
description: >
  Detect outdated dependencies, upgrade them safely, verify the build, and create a PR.
  Covers the Lean toolchain (lean/lean-toolchain), GitHub Actions (pinned SHA versions),
  mise toolchain versions (zig/go/chezscheme pins), and Nix flake inputs. Includes security advisory checks,
  release maturity verification (avoid bleeding-edge releases), and supply chain attack
  detection.
  This skill enforces explicit user-approval gates before modifying files and before
  creating PRs — these gates are mandatory even under Auto Mode.
  Use this skill whenever the user mentions upgrading, updating, or bumping dependencies,
  packages, actions, toolchains, or flake inputs — even if they say something casual like
  "update deps" or "are my packages outdated?". Also trigger when the user asks about
  Renovate/Dependabot-like workflows, version pinning, dependency auditing, or
  security scanning.
---

# Dependency Upgrade Skill

Detect outdated dependencies, verify security and maturity, upgrade safely, and create a PR.

## Overview

The malgo project has four categories of dependencies:

| Category | Files | How to check | How to update |
|----------|-------|--------------|---------------|
| Lean toolchain | `lean/lean-toolchain` (Lake has no package dependencies) | Lean 4 releases on GitHub | Edit the pin, then `mise run build && mise run test` |
| GitHub Actions | `.github/workflows/*.yml` | `gh api` to check latest releases | Update SHA + version comment |
| mise toolchain | `mise.toml` | `mise outdated` | `mise use <tool>@<version>` |
| Nix flake inputs | `flake.lock` | `nix flake update --dry-run` | `nix flake update` |

## Prerequisites

Before starting any dependency work, run `mise trust` (required in fresh environments /
subagents). Renovate (`renovate.json`) already opens routine update PRs; this skill is
for audited, batched upgrades beyond those.

## Step-by-step workflow

### 1. Detect outdated dependencies

Run detection for each category. Present results as a consolidated table.

#### Lean toolchain

Compare `lean/lean-toolchain` against the latest `leanprover/lean4` release
(`gh api repos/leanprover/lean4/releases/latest --jq .tag_name`).

#### GitHub Actions

For each `uses:` line in `.github/workflows/*.yml` that pins to a SHA:
1. Extract the owner/repo and current SHA/tag
2. Use `gh api repos/{owner}/{repo}/releases/latest` to find the latest release
3. Use `gh api repos/{owner}/{repo}/git/ref/tags/{tag}` to get the SHA for the new tag
4. Flag any actions where the pinned version is behind latest

#### mise

```bash
mise outdated
```

`zig`, `go` and `chezscheme` are pinned deliberately; the comment above each pin in
`mise.toml` names the golden sweep to re-run after bumping it. Flag Zig minor bumps
separately — they break `std`.

#### Nix flake

```bash
nix flake update --dry-run
```

### 2. Security and maturity verification

Every upgrade candidate must pass these checks before being applied.

#### 2a. Security advisory check

For **GitHub Actions**, check:
- GitHub Security Advisories for the action's repository
- Whether the repository has had any recent suspicious commits or ownership changes

```bash
gh api repos/{owner}/{repo}/security-advisories --jq '.[].summary'
```

For **Nix flake inputs**, check the upstream project's security advisories.

#### 2b. Supply chain attack indicators

Before adopting any new version, look for these red flags:

**For GitHub Actions:**
- **Repository transfer**: Check if the action's repository was recently transferred
  to a different owner. `gh api repos/{owner}/{repo}` and look at `created_at` vs
  the account's age.
- **Force-pushed tags**: If a tag's SHA differs from what release notes reference,
  the tag may have been moved. Always resolve the SHA yourself:
  ```bash
  gh api repos/{owner}/{repo}/git/ref/tags/{tag} --jq '.object.sha'
  ```
- **Commit verification**: Prefer actions whose release commits are signed.
  ```bash
  gh api repos/{owner}/{repo}/git/commits/{sha} --jq '.verification.verified'
  ```

**For all categories:**
- If anything looks off, **stop and report to the user** rather than proceeding.

#### 2c. Release maturity check

Avoid bleeding-edge releases. Apply these minimum age thresholds:

| Risk level | Minimum age since release |
|------------|--------------------------|
| Patch (x.y.Z) | 3 days |
| Minor (x.Y.0) | 1 week |
| Major (X.0.0) | 2 weeks |

To check release age:

**GitHub Actions:**
```bash
gh api repos/{owner}/{repo}/releases/latest --jq '.published_at'
```

If a release is younger than the threshold, flag it and suggest waiting or pinning
to the previous stable version.

### 3. Present findings and get approval

Show the user a comprehensive summary table:

```
Category        | Package/Action          | Current   | Latest    | Risk   | Security | Age     | Action
----------------|-------------------------|-----------|-----------|--------|----------|---------|--------
Lean            | leanprover/lean4        | v4.32.0   | v4.33.0   | medium | clean    | 2 weeks | upgrade
mise            | zig                     | 0.16.0    | 0.16.1    | low    | clean    | 3 weeks | upgrade
GitHub Actions  | actions/checkout        | v7.0.1    | v7.0.2    | low    | clean    | 1 month | upgrade
GitHub Actions  | some/action             | v1.2.0    | v1.2.1    | low    | ⚠ tag moved | 2 days | HOLD
```

Risk levels:
- **low**: patch version bump
- **medium**: minor version bump
- **high**: major version bump

Suggest a default scope (typically all low+medium with clean security and sufficient
age, holding anything flagged), but **do not apply anything yet** — proceed to Gate A.

### 3.5. Gate A — Confirmation before applying changes (HARD STOP)

**This is a mandatory stop. Do not proceed past this gate without explicit user
approval, even when running under Auto Mode.** Auto Mode's "execute immediately"
default does not override this gate — dependency upgrades are not "low-risk routine
work" and require human review.

What to do at this gate:

1. After presenting the table from §3, **stop and wait** for the user to say which
   candidates to apply. Acceptable approval signals are explicit phrases like
   "apply", "go ahead", "approve", "proceed", or an enumerated subset of candidates.
   Silence or ambiguity is **not** approval.
2. If the user's intent is unclear (e.g., they say "looks good" without specifying
   scope), use `AskUserQuestion` to confirm the exact set to apply.
3. Confirm the scope before any file modification: which version bumps, which
   Action SHA updates, and whether to also push / open a PR later.
4. Do **not** edit `lean/lean-toolchain`, `mise.toml`, workflow files, or `flake.lock`,
   or create a branch, until approval is given.

Once approval is received, proceed to §4.

### 4. Apply upgrades

#### Lean toolchain

Edit `lean/lean-toolchain`, then `mise run build` (elan fetches the new toolchain).

#### GitHub Actions

For each action to update:
1. Look up the new release tag and its commit SHA via `gh api`
2. Verify the SHA matches what the release references
3. Replace the old SHA with the new one
4. Update the version comment (e.g., `# v6.0.1` → `# v6.0.2`)

Format: `uses: owner/repo@<full-sha> # v<tag>`

#### mise

Update `mise.toml` directly. For tools pinned to `"latest"`, no change is needed —
they auto-resolve. For pinned tools (`zig`, `go`, `chezscheme`), update the version string,
run `mise install`, then run the golden sweep named in the pin's comment.

#### Nix flake

```bash
nix flake update
```

### 5. Verify

Run the project's standard verification:

```bash
mise run build && mise run test
```

If tests fail:
- Check whether golden outputs legitimately changed (`mise run test -- --update`, then review the diff)
- Check for API changes in upgraded packages
- Report failures to the user before proceeding

### 5.5. Gate B — Confirmation before pushing & creating the PR (HARD STOP)

**This is a second mandatory stop. Do not push the branch or create a PR without
explicit user approval, even when running under Auto Mode.** Pushing and opening
a PR are externally visible actions and must not be taken on assumption.

What to do at this gate:

1. It is fine to create local commits on a feature branch (so the user can review
   `git log` / `git diff`), but **do not** run `git push` or `gh pr create` yet.
2. Show the user:
   - The branch name you intend to push
   - The list of commits (`git log --oneline master..HEAD`)
   - The drafted PR title and body (paste it inline so they can edit it)
3. Wait for an explicit go-ahead like "push", "open the PR", "create PR",
   "looks good — ship it". A vague "thanks" is not approval.
4. If the user wants edits to the PR description, branch name, or commits, apply
   them and re-confirm before proceeding.
5. Only after approval, run `git push -u origin <branch>` and `gh pr create`.

### 6. Create PR

Create a branch and PR using `gh`:

- Branch name: `chore/deps-upgrade-YYYY-MM-DD`
- Commit message format (Conventional Commits):
  - Upgrades: `chore(deps): upgrade dependencies`
  - Security fixes: `fix(deps): upgrade <pkg> to fix <advisory>`
- PR body should list:
  - Dependencies upgraded (with version changes)
  - Security notes (any advisories addressed, any flags encountered)
  - Verification results

Follow the project's Conventional Commits format.

## Important notes

- **Always stop for explicit user approval at the two gates** (Gate A in §3.5
  before applying changes, Gate B in §5.5 before pushing & creating the PR).
  These gates are mandatory **even under Auto Mode** — the "execute immediately"
  default does not extend to dependency upgrades or PR creation. Treat silence,
  vague acknowledgement ("ok", "thanks"), or implicit consent as **not approved**
  and ask again with `AskUserQuestion`.
- **Always verify SHA hashes** for GitHub Actions — don't just trust tag names, as tags
  can be moved. Use `gh api` to resolve the actual commit SHA for a tag.
- **When in doubt, hold** — it is always safer to skip a suspicious upgrade and report
  findings to the user than to apply it.
