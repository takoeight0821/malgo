#!/usr/bin/env node

const args = process.argv.slice(2);
if (args.length !== 2) {
  console.error('Usage: compute-next-version.js <latestTag> <labelsCsv>');
  process.exit(1);
}

const [latestTag, labelsCsv] = args;

function parseSemver(tag) {
  const match = tag.match(/^v?(\d+)\.(\d+)\.(\d+)$/);
  if (!match) throw new Error(`Invalid semver tag: ${tag}`);
  return {
    major: parseInt(match[1], 10),
    minor: parseInt(match[2], 10),
    patch: parseInt(match[3], 10)
  };
}

function determineBumpType(labels) {
  const labelSet = new Set(labels.filter(l => l.trim()));
  if (labelSet.has('breaking')) return 'major';
  if (labelSet.has('feat')) return 'minor';
  if (labelSet.has('fix')) return 'patch';
  return null;
}

function bumpVersion(version, bumpType) {
  const newVersion = { ...version };
  switch (bumpType) {
    case 'major':
      newVersion.major += 1;
      newVersion.minor = 0;
      newVersion.patch = 0;
      break;
    case 'minor':
      newVersion.minor += 1;
      newVersion.patch = 0;
      break;
    case 'patch':
      newVersion.patch += 1;
      break;
  }
  return newVersion;
}

function formatTag(version) {
  return `v${version.major}.${version.minor}.${version.patch}`;
}

try {
  const currentVersion = parseSemver(latestTag);
  const labels = labelsCsv ? labelsCsv.split(',').map(l => l.trim()) : [];
  const bumpType = determineBumpType(labels);

  if (!bumpType) {
    console.error('ERROR: No version-related labels (breaking, feat, fix) found.');
    process.exit(1);
  }

  const nextVersion = bumpVersion(currentVersion, bumpType);
  const result = {
    nextTag: formatTag(nextVersion),
    bumpType,
    currentTag: latestTag,
    labels: labels.filter(l => l)
  };
  console.log(JSON.stringify(result, null, 2));
} catch (error) {
  console.error('Error:', error.message);
  process.exit(1);
}