export function getDocumentedVersionProvenance(version, hasReleaseTag) {
  if (!/^\d+\.\d+\.\d+(?:-[0-9A-Za-z.-]+)?$/.test(version)) {
    throw new Error(`Invalid Raven documentation version: ${version}`);
  }

  return {
    version,
    status: hasReleaseTag ? "released" : "unreleased",
  };
}
