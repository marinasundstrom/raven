#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
playground_dir="$repo_root/src/Raven.Playground"
publish_dir="$(mktemp -d "${TMPDIR:-/tmp}/raven-playground-smoke.XXXXXX")"

cleanup() {
    rm -rf "$publish_dir"
}
trap cleanup EXIT

dotnet publish "$playground_dir/Raven.Playground.csproj" \
    -c Release \
    -o "$publish_dir" \
    --property WarningLevel=0

if [[ ! -x "$playground_dir/node_modules/.bin/playwright" ]]; then
    npm --prefix "$playground_dir" ci --no-audit --no-fund
fi

"$playground_dir/node_modules/.bin/playwright" install chromium

npm --prefix "$playground_dir" run test:browser -- "$publish_dir/wwwroot"
