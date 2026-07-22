#!/usr/bin/env bash
set -euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
site_output="$repository_root/_site"
api_output="$repository_root/docs/api"

dotnet tool restore --tool-manifest "$repository_root/.config/dotnet-tools.json"

# DocFX preserves files from previous builds. Always clear this generated,
# repository-local directory so excluded development pages cannot leak into a
# later user-facing build.
if [[ -d "$site_output" ]]; then
    rm -rf -- "$site_output"
fi

if [[ -d "$api_output" ]]; then
    rm -rf -- "$api_output"
fi

# API metadata is generated separately. Existing source-comment warnings remain
# visible without weakening strict validation of the authored documentation.
dotnet docfx metadata "$repository_root/docs/docfx-metadata.json" \
    --property "WarningLevel=0;TargetFramework=net10.0"

if [[ "${1:-}" == "--serve" ]]; then
    dotnet docfx build "$repository_root/docs/docfx.json" --warningsAsErrors --serve
else
    dotnet docfx build "$repository_root/docs/docfx.json" --warningsAsErrors
fi
