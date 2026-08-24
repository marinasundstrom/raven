export function nextUnreleasedVersion(version) {
  const previewMatch = /^(.*-preview\.)(\d+)$/.exec(version);
  if (previewMatch) {
    return `${previewMatch[1]}${Number(previewMatch[2]) + 1}`;
  }

  const stableMatch = /^(\d+)\.(\d+)\.(\d+)$/.exec(version);
  if (stableMatch) {
    return `${stableMatch[1]}.${stableMatch[2]}.${Number(stableMatch[3]) + 1}-preview.1`;
  }

  throw new Error(
    `Version ${version} already has a release tag and its next preview cannot be inferred. ` +
    "Set RAVEN_SITE_VERSION explicitly.",
  );
}
