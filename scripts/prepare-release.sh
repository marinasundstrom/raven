#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VERSION="${1:-}"

if [[ -z "$VERSION" || "$VERSION" == v* || ! "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z.-]+)?$ ]]; then
  echo "Usage: scripts/prepare-release.sh <version-without-leading-v>" >&2
  exit 2
fi

worktree_state="$(git -C "$REPO_ROOT" status --porcelain --untracked-files=all)"
if [[ -n "$worktree_state" ]]; then
  echo "Start release preparation from a clean worktree:" >&2
  printf '%s\n' "$worktree_state" >&2
  exit 1
fi

if git -C "$REPO_ROOT" rev-parse --verify --quiet "refs/tags/v$VERSION" >/dev/null; then
  echo "Release tag v$VERSION already exists." >&2
  exit 1
fi

current_version="$(sed -n 's/.*"Raven.Sdk"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$REPO_ROOT/global.json")"
if [[ -z "$current_version" ]]; then
  echo "Could not determine the current Raven.Sdk version from global.json." >&2
  exit 1
fi
if [[ "$current_version" == "$VERSION" ]]; then
  echo "global.json already selects $VERSION; refusing an ambiguous second preparation." >&2
  exit 1
fi

while IFS= read -r file; do
  OLD_RELEASE_VERSION="$current_version" NEW_RELEASE_VERSION="$VERSION" \
    perl -pi -e 's/\Q$ENV{OLD_RELEASE_VERSION}\E/$ENV{NEW_RELEASE_VERSION}/g' "$REPO_ROOT/$file"
done < <(git -C "$REPO_ROOT" grep -Il -F "$current_version" -- ':!CHANGELOG.md')

release_date="$(date +%F)"
RELEASE_VERSION="$VERSION" RELEASE_DATE="$release_date" \
  perl -0pi -e 's/## Unreleased\n/## Unreleased\n\n## $ENV{RELEASE_VERSION} - $ENV{RELEASE_DATE}\n/' \
  "$REPO_ROOT/CHANGELOG.md"

"$REPO_ROOT/scripts/validate-release.sh" "$VERSION"

cat <<EOF

Prepared release references for $VERSION.

Next:
  1. Review every changed file and finish the $VERSION changelog entry.
  2. Commit every intended release change.
  3. Run scripts/validate-release.sh $VERSION --require-clean,
     scripts/test-release.sh, and scripts/package-nuget.sh $VERSION.
  4. Push the tested commit and wait for its ordinary CI checks.
  5. Tag that exact tested commit with v$VERSION and push the tag.
  6. Dispatch Distribution against v$VERSION with the desired publish switches.

The Distribution workflow repeats the release test gate on the tagged commit before packaging.
EOF
