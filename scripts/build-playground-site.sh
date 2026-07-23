#!/usr/bin/env bash
set -euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
playground_dir="$repository_root/src/Raven.Playground"
site_dir="$repository_root/_site/playground"
publish_dir="$(mktemp -d "${TMPDIR:-/tmp}/raven-playground-site.XXXXXX")"

cleanup() {
    rm -rf -- "$publish_dir"
}
trap cleanup EXIT

if [[ ! -d "$repository_root/_site" ]]; then
    echo "The documentation site must be built before adding the playground." >&2
    exit 1
fi

BUILD_CONFIG=Release "$repository_root/scripts/codex-build.sh"

dotnet publish "$playground_dir/Raven.Playground.csproj" \
    -c Release \
    -o "$publish_dir" \
    --no-restore \
    --property WarningLevel=0

rm -rf -- "$site_dir"
mkdir -p "$site_dir"
cp -R "$publish_dir/wwwroot/." "$site_dir/"

test -f "$site_dir/index.html"
test -d "$site_dir/_framework"
