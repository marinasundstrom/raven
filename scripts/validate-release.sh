#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VERSION="${1:-}"
shift || true

REQUIRE_CLEAN=0
REQUIRE_TAG=0
CHECK_NUGET_AVAILABLE=0

usage() {
  cat <<'EOF'
Usage: scripts/validate-release.sh <version> [options]

Options:
  --require-clean           Fail when the Git worktree is not clean.
  --require-tag             Require HEAD to be tagged exactly v<version>.
  --check-nuget-available   Fail when any lockstep package version already exists on NuGet.org.
EOF
}

if [[ -z "$VERSION" ]]; then
  usage >&2
  exit 2
fi

while [[ $# -gt 0 ]]; do
  case "$1" in
    --require-clean)
      REQUIRE_CLEAN=1
      ;;
    --require-tag)
      REQUIRE_TAG=1
      ;;
    --check-nuget-available)
      CHECK_NUGET_AVAILABLE=1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

if [[ "$VERSION" == v* || ! "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z.-]+)?$ ]]; then
  echo "Release version must be SemVer without a leading v: $VERSION" >&2
  exit 2
fi

assert_contains() {
  local file="$1"
  local expected="$2"
  if ! grep -Fq "$expected" "$REPO_ROOT/$file"; then
    echo "Release reference is missing or stale in $file: $expected" >&2
    exit 1
  fi
}

selected_sdk_version="$(sed -n 's/.*"Raven.Sdk"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$REPO_ROOT/global.json")"
if [[ "$selected_sdk_version" != "$VERSION" ]]; then
  echo "global.json selects Raven.Sdk $selected_sdk_version, expected $VERSION." >&2
  exit 1
fi

selected_web_sdk_version="$(sed -n 's/.*"Raven.Sdk.Web"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$REPO_ROOT/global.json")"
if [[ "$selected_web_sdk_version" != "$VERSION" ]]; then
  echo "global.json selects Raven.Sdk.Web $selected_web_sdk_version, expected $VERSION." >&2
  exit 1
fi

assert_contains ".github/workflows/installation.yml" "default: $VERSION"
assert_contains "README.md" "Raven.Sdk\` version \`$VERSION"
assert_contains "README.md" "scripts/package-nuget.sh $VERSION"
assert_contains "docs/getting-started.md" "releases/download/v$VERSION/install-raven.sh"
assert_contains "docs/getting-started.md" "\$version = \"$VERSION\""
assert_contains "docs/getting-started.md" "releases/download/v$VERSION/raven-vscode.vsix"
assert_contains "docs/compiler/distribution.md" "releases/download/v$VERSION/install-raven.sh"
assert_contains "docs/compiler/distribution.md" "\$version = \"$VERSION\""
assert_contains "docs/compiler/distribution.md" "releases/download/v$VERSION/raven-vscode.vsix"
assert_contains "docs/compiler/raven-vscode-extension.md" "releases/download/v$VERSION/raven-vscode.vsix"
assert_contains "samples/projects/runtime-async-net11/global.json" "\"Raven.Sdk\": \"$VERSION\""
assert_contains "CHANGELOG.md" "## $VERSION - "

if [[ "$REQUIRE_CLEAN" == "1" ]]; then
  worktree_state="$(git -C "$REPO_ROOT" status --porcelain --untracked-files=all)"
  if [[ -n "$worktree_state" ]]; then
    echo "Release validation requires a clean worktree:" >&2
    printf '%s\n' "$worktree_state" >&2
    exit 1
  fi
fi

if [[ "$REQUIRE_TAG" == "1" ]]; then
  expected_tag="v$VERSION"
  head_commit="$(git -C "$REPO_ROOT" rev-parse HEAD)"
  if ! tag_commit="$(git -C "$REPO_ROOT" rev-parse "refs/tags/$expected_tag^{commit}" 2>/dev/null)"; then
    echo "Required release tag does not exist: $expected_tag" >&2
    exit 1
  fi
  if [[ "$tag_commit" != "$head_commit" ]]; then
    echo "Tag $expected_tag points to $tag_commit, but the checked-out commit is $head_commit." >&2
    exit 1
  fi
fi

if [[ "$CHECK_NUGET_AVAILABLE" == "1" ]]; then
  package_ids=(
    raven.core
    raven.macros
    raven.codeanalysis
    raven.analyzers
    raven.sdk
    raven.sdk.web
    raven.templates
  )

  for package_id in "${package_ids[@]}"; do
    if ! response="$(curl --location --silent --show-error \
      --write-out $'\n%{http_code}' \
      "https://api.nuget.org/v3-flatcontainer/$package_id/index.json")"; then
      echo "Could not query NuGet.org for $package_id." >&2
      exit 1
    fi

    http_status="${response##*$'\n'}"
    versions="${response%$'\n'*}"
    if [[ "$http_status" == "404" ]]; then
      continue
    fi
    if [[ "$http_status" != "200" ]]; then
      echo "NuGet.org returned HTTP $http_status while checking $package_id." >&2
      exit 1
    fi
    if grep -Fq "\"$VERSION\"" <<<"$versions"; then
      echo "NuGet.org already contains $package_id $VERSION; release versions are immutable." >&2
      exit 1
    fi
  done
fi

echo "Release metadata is consistent for $VERSION at $(git -C "$REPO_ROOT" rev-parse HEAD)."
