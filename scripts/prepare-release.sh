#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REQUESTED_VERSION="${1:-}"

worktree_state="$(git -C "$REPO_ROOT" status --porcelain --untracked-files=all)"
if [[ -n "$worktree_state" ]]; then
  echo "Start release preparation from a clean worktree:" >&2
  printf '%s\n' "$worktree_state" >&2
  exit 1
fi

current_version="$(sed -n 's/.*"Raven.Sdk"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$REPO_ROOT/global.json")"
if [[ -z "$current_version" ]]; then
  echo "Could not determine the current Raven.Sdk version from global.json." >&2
  exit 1
fi

if [[ ! "$current_version" =~ ^0\.1\.0-preview\.([0-9]+)(\.[0-9]+)*$ ]]; then
  echo "Cannot infer the next preview from $current_version; decide the next release line explicitly." >&2
  exit 1
fi

next_preview_number="$((BASH_REMATCH[1] + 1))"
VERSION="0.1.0-preview.$next_preview_number"

if [[ -n "$REQUESTED_VERSION" && "$REQUESTED_VERSION" != "$VERSION" ]]; then
  echo "The next release after $current_version is $VERSION, not $REQUESTED_VERSION." >&2
  exit 2
fi

if git -C "$REPO_ROOT" rev-parse --verify --quiet "refs/tags/v$VERSION" >/dev/null; then
  echo "Release tag v$VERSION already exists." >&2
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
  3. Run scripts/validate-release.sh $VERSION --require-clean and
     scripts/package-nuget.sh $VERSION.
  4. Push the commit and wait for its Main CI full build/test gate.
  5. Tag that exact tested commit with v$VERSION and push the tag.
  6. Dispatch Distribution against v$VERSION with the desired publish switches.

The Distribution workflow requires a successful Main CI run for the tagged commit,
then repeats only release-specific package and distribution checks.
EOF
