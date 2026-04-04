---
name: dep-upgrade
description: >
  Detect outdated dependencies, upgrade them safely, verify the build, and create a PR.
  Covers Haskell (cabal/package.yaml/freeze file), GitHub Actions (pinned SHA versions),
  mise toolchain versions, and Nix flake inputs. Includes security advisory checks,
  release maturity verification (avoid bleeding-edge releases), supply chain attack
  detection, and unnecessary dependency removal.
  Use this skill whenever the user mentions upgrading, updating, or bumping dependencies,
  packages, actions, toolchains, or flake inputs — even if they say something casual like
  "update deps" or "are my packages outdated?". Also trigger when the user asks about
  Dependabot-like workflows, version pinning, freeze file maintenance, dependency auditing,
  security scanning, or trimming unused packages.
---

# Dependency Upgrade Skill

Detect outdated dependencies, verify security and maturity, remove unnecessary packages,
upgrade safely, and create a PR.

## Overview

The malgo project has four categories of dependencies:

| Category | Files | How to check | How to update |
|----------|-------|--------------|---------------|
| Haskell packages | `package.yaml`, `cabal.project.freeze` | `cabal outdated --freeze-file` | Edit freeze / regenerate |
| GitHub Actions | `.github/workflows/*.yml` | `gh api` to check latest releases | Update SHA + version comment |
| mise toolchain | `mise.toml` | `mise outdated` | `mise use <tool>@<version>` |
| Nix flake inputs | `flake.lock` (if present at root) | `nix flake update --dry-run` | `nix flake update` |

## Prerequisites

Before starting any dependency work, run these setup commands:

```bash
mise trust    # Approve mise permissions (required in fresh environments / subagents)
cabal update  # Refresh the Hackage index — this updates the local index-state
```

`cabal update` is essential because `cabal outdated` compares against the local index.
Without it, you're comparing against a stale snapshot and will miss available updates.

## Step-by-step workflow

### 1. Audit unnecessary dependencies

Before upgrading anything, check whether each dependency in `package.yaml` is actually
used. Removing unneeded dependencies shrinks the attack surface and reduces build time.

#### How to detect unused Haskell dependencies

Check all dependency sections in `package.yaml`:
- Top-level `dependencies:` (library deps)
- `tests:` section `dependencies:` (test-only deps)
- `build-tools:` (build tools like `happy`, `ormolu`)

For each package, search the source code for imports from that package. Use these heuristics:

| Package | Expected import pattern |
|---------|----------------------|
| `aeson` | `import Data.Aeson` |
| `lens` | `import Control.Lens` |
| `megaparsec` | `import Text.Megaparsec` |
| `effectful` | `import Effectful` |
| (etc.) | Check Hackage for the package's module names |

```bash
grep -r "import.*ModuleName" src/ app/ test/
```

If zero hits are found for a dependency, it is a removal candidate. Some packages are
trickier — they may be used only as build-tool plugins (like `effectful-plugin`) or
provide orphan instances loaded implicitly. Flag these for the user to confirm.

For build-tools, check if the tool is actually invoked. For example, `happy` processes
`.y` files — if no `.y` files exist in the repo, it's likely unused. Similarly, check
test-only dependencies by searching only in `test/`.

#### Removal process

1. Remove the package from `package.yaml` `dependencies:` (or move to test-only if
   only used in tests)
2. Run `hpack && cabal build` to verify compilation
3. If build fails, check which modules need the package and decide:
   - If few usages exist, consider replacing with standard library equivalents
   - If heavily used, keep the dependency
4. Run `mise run test` to verify no runtime breakage

Present removal candidates to the user with a brief rationale before removing.

### 2. Detect outdated dependencies

Run detection for each category. Present results as a consolidated table.

#### Haskell

```bash
cabal outdated --freeze-file
```

This compares `cabal.project.freeze` against the Hackage index. Also check if the
`index-state` in `cabal.project.freeze` is stale (more than ~2 weeks old).

If `cabal outdated` is not available or errors, fall back to manually comparing
key packages in `package.yaml` against Hackage.

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

Pay attention to GHC version changes — major GHC upgrades (e.g., 9.12 → 9.14) are
breaking and should be flagged separately from patch updates.

#### Nix flake

Only if a `flake.nix` exists at the project root:
```bash
nix flake update --dry-run
```

### 3. Security and maturity verification

This is the most critical step. Do not skip it. Every upgrade candidate must pass
these checks before being applied.

#### 3a. Security advisory check

For **Haskell packages**, check:
- Haskell Security Advisory Database: search https://github.com/haskell/security-advisories
  for the package name using `gh api` or `gh search issues`
- Hackage package page changelog and metadata for security-related notes

```bash
gh search issues --repo haskell/security-advisories "<package-name>" --json title,url,state
```

For **GitHub Actions**, check:
- GitHub Security Advisories for the action's repository
- Whether the repository has had any recent suspicious commits or ownership changes

```bash
gh api repos/{owner}/{repo}/security-advisories --jq '.[].summary'
```

For **Nix flake inputs**, check the upstream project's security advisories.

#### 3b. Supply chain attack indicators

Before adopting any new version, look for these red flags:

**For Haskell packages:**
- **Maintainer change**: Compare the `maintainer` field between the current and new
  version on Hackage. A sudden change in maintainer is a yellow flag — investigate.
  ```
  curl -s https://hackage.haskell.org/package/<pkg>-<old-ver>/<pkg>.cabal | grep -i maintainer
  curl -s https://hackage.haskell.org/package/<pkg>-<new-ver>/<pkg>.cabal | grep -i maintainer
  ```
- **Unusual dependency additions**: If the new version adds dependencies that seem
  unrelated to the package's purpose (e.g., a parser library suddenly depending on
  `network` or `process`), flag it.
- **Upload timing**: A release published in the last 48 hours with no prior beta/RC
  and no changelog entry is suspicious. Prefer versions that have been on Hackage for
  at least 1-2 weeks.

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

#### 3c. Release maturity check

Avoid bleeding-edge releases. Apply these minimum age thresholds:

| Risk level | Minimum age since release |
|------------|--------------------------|
| Patch (x.y.Z) | 3 days |
| Minor (x.Y.0) | 1 week |
| Major (X.0.0) | 2 weeks |

To check release age:

**Hackage:**
```bash
curl -s "https://hackage.haskell.org/package/<pkg>/preferred.json"
```
Or check the upload timestamp on the Hackage package page.

**GitHub Actions:**
```bash
gh api repos/{owner}/{repo}/releases/latest --jq '.published_at'
```

If a release is younger than the threshold, flag it and suggest waiting or pinning
to the previous stable version.

### 4. Present findings and get approval

Show the user a comprehensive summary table:

```
Category        | Package/Action          | Current   | Latest    | Risk   | Security | Age     | Action
----------------|-------------------------|-----------|-----------|--------|----------|---------|--------
Haskell         | lens                    | 5.3.4     | 5.3.5     | low    | clean    | 3 weeks | upgrade
Haskell         | effectful               | 2.6.1.0   | 2.7.0.0   | medium | clean    | 2 weeks | upgrade
Haskell         | some-pkg                | 1.0.0     | 1.0.1     | low    | ⚠ new maintainer | 2 days | HOLD
GitHub Actions  | actions/checkout        | v6.0.1    | v6.0.2    | low    | clean    | 1 month | upgrade
Haskell         | unused-pkg              | 0.5.0     | -         | -      | -        | -       | REMOVE
```

Risk levels:
- **low**: patch version bump
- **medium**: minor version bump
- **high**: major version bump

Ask the user which upgrades to apply. Default to all low+medium with clean security
and sufficient age. Hold anything flagged.

### 5. Apply upgrades

#### Haskell freeze file update

1. Run `cabal update` (this refreshes the local Hackage index to the latest state)
2. Delete the old `index-state` line from `cabal.project.freeze`
3. Run `cabal freeze` to regenerate the freeze file (it will use the latest index-state)
4. If the freeze fails due to version conflicts, try relaxing constraints in
   `cabal.project` and report to the user

Important: after freezing, run `hpack && cabal build` to verify the build works.

#### GitHub Actions

For each action to update:
1. Look up the new release tag and its commit SHA via `gh api`
2. Verify the SHA matches what the release references
3. Replace the old SHA with the new one
4. Update the version comment (e.g., `# v6.0.1` → `# v6.0.2`)

Format: `uses: owner/repo@<full-sha> # v<tag>`

#### mise

Update `mise.toml` directly. For tools pinned to `"latest"`, no change is needed —
they auto-resolve. For tools with explicit versions (like GHC), update the version string.

After updating, run `mise install` to verify.

#### Nix flake

```bash
nix flake update
```

### 6. Verify

Run the project's standard verification:

```bash
mise run build && mise run test
```

If tests fail:
- Check if golden test outputs need resetting (`mise run reset` then re-run tests)
- Check for API changes in upgraded packages
- Report failures to the user before proceeding

### 7. Create PR

Create a branch and PR using `gh`:

- Branch name: `chore/deps-upgrade-YYYY-MM-DD`
- Commit message format (Conventional Commits):
  - Removals: `refactor(deps): remove unused <pkg>`
  - Upgrades: `chore(deps): upgrade dependencies`
  - Security fixes: `fix(deps): upgrade <pkg> to fix <advisory>`
- PR body should list:
  - Dependencies removed (with rationale)
  - Dependencies upgraded (with version changes)
  - Security notes (any advisories addressed, any flags encountered)
  - Verification results

Use separate commits for removals vs upgrades. Follow the project's Conventional
Commits format.

## Important notes

- **Never upgrade GHC major version without explicit user approval** — this can break
  the entire build and requires careful migration.
- **The `allow-newer` field in `cabal.project`** may need adjustment when upgrading.
  Currently it has `allow-newer: text`. Check if this is still needed after upgrading.
- **The existing CI workflow** (`haskell-deps-update.yml`) handles lower bound bumps
  automatically. This skill is for comprehensive upgrades beyond what that workflow covers.
- **Always verify SHA hashes** for GitHub Actions — don't just trust tag names, as tags
  can be moved. Use `gh api` to resolve the actual commit SHA for a tag.
- **When in doubt, hold** — it is always safer to skip a suspicious upgrade and report
  findings to the user than to apply it.
