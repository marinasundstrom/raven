#!/usr/bin/env bash
set -euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
site_output="$repository_root/_site"
api_output="$repository_root/docs/api"
core_api_output="$site_output/libraries/raven-core"
macros_api_output="$site_output/libraries/raven-macros"

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

# Build the compiler, Raven-authored libraries, and generated compiler sources
# through the same bootstrap sequence used by local compiler development.
BUILD_CONFIG=Debug "$repository_root/scripts/codex-build.sh"

dotnet build "$repository_root/src/RavenDoc/RavenDoc.csproj" \
    --framework net10.0 \
    --no-restore \
    --property WarningLevel=0

# RavenDoc sites remain independent static sites. They are written into the
# shared Pages artifact before DocFX runs; DocFX preserves unrelated output.
dotnet run --project "$repository_root/src/RavenDoc/RavenDoc.csproj" \
    --framework net10.0 \
    --no-build \
    -- \
    "$repository_root/src/Raven.Core/bin/Debug/net10.0/Raven.Core.dll" \
    --output "$core_api_output" \
    --framework net10.0 \
    --nav "Raven docs=https://marinasundstrom.github.io/raven/" \
    --nav "Raven.Macros API=https://marinasundstrom.github.io/raven/libraries/raven-macros/"

dotnet run --project "$repository_root/src/RavenDoc/RavenDoc.csproj" \
    --framework net10.0 \
    --no-build \
    -- \
    "$repository_root/src/Raven.Macros" \
    --output "$macros_api_output" \
    --framework net10.0 \
    --reference "$repository_root/src/Raven.CodeAnalysis/bin/Debug/net10.0/Raven.CodeAnalysis.dll" \
    --nav "Raven docs=https://marinasundstrom.github.io/raven/" \
    --nav "Raven.Core API=https://marinasundstrom.github.io/raven/libraries/raven-core/" \
    --nav "Syntax trees=https://marinasundstrom.github.io/raven/compiler/api/syntax-tree.html"

required_library_pages=(
    "$core_api_output/index.html"
    "$macros_api_output/index.html"
    "$macros_api_output/Raven/Macros/index.html"
    "$macros_api_output/Raven/Macros/macro_Quote.html"
    "$macros_api_output/Raven/Macros/macro_Compile.html"
)
for required_page in "${required_library_pages[@]}"; do
    if [[ ! -f "$required_page" ]]; then
        echo "RavenDoc did not generate required library page: $required_page" >&2
        exit 1
    fi
done

# API metadata is generated separately. Existing source-comment warnings remain
# visible without weakening strict validation of the authored documentation.
dotnet docfx metadata "$repository_root/docs/docfx-metadata.json" \
    --property "WarningLevel=0;TargetFrameworks=net10.0"

if [[ "${1:-}" == "--serve" ]]; then
    dotnet docfx build "$repository_root/docs/docfx.json" --warningsAsErrors --serve
else
    dotnet docfx build "$repository_root/docs/docfx.json" --warningsAsErrors
fi
