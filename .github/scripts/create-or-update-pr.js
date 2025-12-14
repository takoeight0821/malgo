#!/usr/bin/env node

const { execSync } = require('child_process');
const fs = require('fs');

const args = process.argv.slice(2);
if (args.length !== 2) {
  console.error('Usage: create-or-update-pr.js <version> <releaseNotes>');
  process.exit(1);
}

const [version, releaseNotes] = args;
const branchName = `release/${version}`;
const baseBranch = 'master';

function exec(command, options = {}) {
  try {
    const result = execSync(command, {
      encoding: 'utf8',
      stdio: options.silent ? 'pipe' : 'inherit',
      ...options
    });
    return result ? result.trim() : '';
  } catch (error) {
    if (!options.ignoreError) throw error;
    return '';
  }
}

try {
  const remoteBranchExists = exec(
    `git ls-remote --heads origin ${branchName}`,
    { silent: true, ignoreError: true }
  );

  exec(`git checkout -b ${branchName}`);
  exec(`git commit --allow-empty -m "chore(release): prepare ${version}"`);

  const existingPr = exec(
    `gh pr list --head ${branchName} --base ${baseBranch} --json number --jq '.[0].number'`,
    { silent: true, ignoreError: true }
  );

  if (remoteBranchExists) {
    exec(`git push --force-with-lease origin ${branchName}`);
  } else {
    exec(`git push -u origin ${branchName}`);
  }

  const prBody = `## Release ${version}

This PR prepares the release of ${version}.

### Release notes

${releaseNotes}

---

**Note**: This PR was automatically generated.`;

  const tempBodyFile = '/tmp/pr-body.md';
  fs.writeFileSync(tempBodyFile, prBody);

  if (existingPr) {
    exec(`gh pr edit ${existingPr} --body-file ${tempBodyFile}`);
  } else {
    exec(`gh pr create --title "chore: release ${version}" --base ${baseBranch} --head ${branchName} --body-file ${tempBodyFile}`, { silent: true });
  }

  exec(`git checkout ${baseBranch}`);
} catch (error) {
  console.error('Error:', error.message);
  process.exit(1);
}