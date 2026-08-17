#!/usr/bin/env bash
set -euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
site_output="$repository_root/_site"
core_api_output="$site_output/libraries/raven-core"
macros_api_output="$site_output/libraries/raven-macros"

serve=false
no_build=false

for arg in "$@"; do
    case "$arg" in
        --serve)
            serve=true
            ;;
        --no-build)
            no_build=true
            ;;
        *)
            echo "Unknown argument: $arg" >&2
            exit 1
            ;;
    esac
done

dotnet tool restore --tool-manifest "$repository_root/.config/dotnet-tools.json"

# DocFX preserves files from previous builds. Always clear this generated,
# repository-local directory so excluded development pages cannot leak into a
# later user-facing build.
if [[ -d "$site_output" ]]; then
    rm -rf -- "$site_output"
fi

if [[ "$no_build" == false ]]; then
    # Build the compiler, Raven-authored libraries, and generated compiler sources
    # through the same bootstrap sequence used by local compiler development.
    BUILD_CONFIG=Debug "$repository_root/scripts/codex-build.sh"

    dotnet build "$repository_root/src/RavenDoc/RavenDoc.csproj" \
        --framework net10.0 \
        --no-restore \
        --property WarningLevel=0
fi

# RavenDoc sites remain independent static sites. They are written into the
# shared Pages artifact before DocFX runs; DocFX preserves unrelated output.
dotnet run --project "$repository_root/src/RavenDoc/RavenDoc.csproj" \
    --framework net10.0 \
    --no-build \
    -- \
    "$repository_root/src/Raven.Core/Raven.Core.rvnproj" \
    --output "$core_api_output" \
    --site-root "$site_output" \
    --framework net10.0 \
    --nav "Raven docs=https://marinasundstrom.github.io/raven/" \
    --nav "Raven.Macros API=https://marinasundstrom.github.io/raven/libraries/raven-macros/"

dotnet run --project "$repository_root/src/RavenDoc/RavenDoc.csproj" \
    --framework net10.0 \
    --no-build \
    -- \
    "$repository_root/src/Raven.Macros" \
    --output "$macros_api_output" \
    --site-root "$site_output" \
    --framework net10.0 \
    --reference "$repository_root/src/Raven.CodeAnalysis/bin/Debug/net10.0/Raven.CodeAnalysis.dll" \
    --nav "Raven docs=https://marinasundstrom.github.io/raven/" \
    --nav "Raven.Core API=https://marinasundstrom.github.io/raven/libraries/raven-core/"

required_library_pages=(
    "$core_api_output/index.html"
    "$macros_api_output/index.html"
    "$macros_api_output/Raven/Macros/index.html"
    "$macros_api_output/Raven/Macros/macro_Quote.html"
    "$macros_api_output/Raven/Macros/macro_Compile.html"
    "$macros_api_output/Raven/Macros/macro_EmbedFileContent.html"
    "$macros_api_output/Raven/Macros/macro_Sha256Digest.html"
)

for required_page in "${required_library_pages[@]}"; do
    if [[ ! -f "$required_page" ]]; then
        echo "RavenDoc did not generate required library page: $required_page" >&2
        exit 1
    fi
done

if ! grep -Fq \
    "github.com/marinasundstrom/raven/blob/main/src/Raven.Core/Option.rvn#L" \
    "$core_api_output/System/Option\`1/index.html"; then
    echo "RavenDoc did not preserve the Raven.Core source link." >&2
    exit 1
fi

if [[ "$serve" == true ]]; then
    dotnet docfx build "$repository_root/docs/docfx.json" --warningsAsErrors --serve
else
    dotnet docfx build "$repository_root/docs/docfx.json" --warningsAsErrors
fi