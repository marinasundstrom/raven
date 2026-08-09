#!/usr/bin/env bash
set -euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
project="$repository_root/samples/projects/macro-html-blazor/wasm/HtmlBlazorShowcase.Wasm.csproj"
site_dir="$repository_root/_site/experiments/html-macro"
publish_dir="$(mktemp -d "${TMPDIR:-/tmp}/raven-html-macro-site.XXXXXX")"

cleanup() {
    rm -rf -- "$publish_dir"
}
trap cleanup EXIT

if [[ ! -d "$repository_root/_site" ]]; then
    echo "The documentation site must be built before adding the component-template showcase." >&2
    exit 1
fi

BUILD_CONFIG=Release "$repository_root/scripts/codex-build.sh"

dotnet restore "$project"
dotnet publish "$project" \
    -c Release \
    -o "$publish_dir" \
    --no-restore \
    --property WarningLevel=0

rm -rf -- "$site_dir"
mkdir -p "$site_dir"
cp -R "$publish_dir/wwwroot/." "$site_dir/"

test -f "$site_dir/index.html"
test -d "$site_dir/_framework"
test -f "$site_dir/app.css"
grep -Fq '<script type="importmap">' "$site_dir/index.html"
find "$site_dir/_framework" -maxdepth 1 -name 'Raven.Core.*.wasm' -print -quit | grep -q .
find "$site_dir/_framework" -maxdepth 1 -name 'ExistingBlazorComponents.*.wasm' -print -quit | grep -q .
find "$site_dir/_framework" -maxdepth 1 -name 'HtmlBlazorMacros.*.wasm' -print -quit | grep -q .
