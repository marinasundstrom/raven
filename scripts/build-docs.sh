#!/usr/bin/env bash
set -euo pipefail

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
site_output="$repository_root/_site"
core_api_output="$site_output/libraries/raven-core"
macros_api_output="$site_output/libraries/raven-macros"
compiler_api_output="$site_output/libraries/raven-codeanalysis"

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

if [[ "$no_build" == false ]]; then
    # Build the compiler, Raven-authored libraries, and generated compiler sources
    # through the same bootstrap sequence used by local compiler development.
    BUILD_CONFIG=Debug "$repository_root/scripts/codex-build.sh"

    dotnet build "$repository_root/src/RavenDoc/RavenDoc.csproj" \
        --framework net10.0 \
        --no-restore \
        --property WarningLevel=0
fi

# One configuration publishes articles and library references with the same shell.
dotnet run --project "$repository_root/src/RavenDoc/RavenDoc.csproj" \
    --framework net10.0 --no-build -- --site "$repository_root/docs/ravendoc.json"

required_library_pages=(
    "$core_api_output/index.html"
    "$macros_api_output/index.html"
    "$compiler_api_output/index.html"
    "$compiler_api_output/Raven/CodeAnalysis/Compilation/index.html"
    "$compiler_api_output/Raven/CodeAnalysis/SemanticModel/index.html"
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
    python3 -m http.server 8080 --directory "$site_output"
fi
