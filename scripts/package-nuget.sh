#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VERSION="${1:-}"
OUTPUT_DIR="${RAVEN_NUGET_OUTPUT:-$ROOT_DIR/artifacts/packages}"

if [[ -z "$VERSION" ]]; then
  echo "Usage: scripts/package-nuget.sh <version>" >&2
  exit 1
fi

if [[ "$VERSION" == v* ]]; then
  echo "Package version must not include the leading v: $VERSION" >&2
  exit 1
fi

if [[ ! "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z.-]+)?$ ]]; then
  echo "Invalid package version: $VERSION" >&2
  exit 1
fi

mkdir -p "$OUTPUT_DIR"

COMMON_PROPERTIES=(
  "/property:WarningLevel=0"
  "/property:Version=$VERSION"
  "/property:PackageVersion=$VERSION"
  "/property:InformationalVersion=$VERSION"
  "/property:IncludeSourceRevisionInInformationalVersion=false"
)

"$ROOT_DIR/scripts/generate-compiler-sources.sh"

# Build both public API target assets, then bootstrap the distributed .NET 11
# compiler host without Raven.Core so Raven-authored libraries can be built
# from a clean checkout.
dotnet build "$ROOT_DIR/src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj" \
  -c Release \
  "${COMMON_PROPERTIES[@]}"

dotnet build "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" \
  -c Release \
  -f net11.0 \
  -p:UseRavenCoreReference=false \
  "${COMMON_PROPERTIES[@]}"

dotnet pack "$ROOT_DIR/src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj" \
  -c Release \
  --no-build \
  -o "$OUTPUT_DIR" \
  "${COMMON_PROPERTIES[@]}"

dotnet pack "$ROOT_DIR/src/Raven.Analyzers/Raven.Analyzers.csproj" \
  -c Release \
  -o "$OUTPUT_DIR" \
  "${COMMON_PROPERTIES[@]}"

# Build Raven-authored multi-targeted libraries one framework at a time. This
# avoids concurrently invoking two compiler hosts against shared project state.
for target_framework in net10.0 net11.0; do
  dotnet build "$ROOT_DIR/src/Raven.Core/Raven.Core.rvnproj" \
    -c Release \
    -f "$target_framework" \
    "${COMMON_PROPERTIES[@]}"
done

dotnet pack "$ROOT_DIR/src/Raven.Core/Raven.Core.rvnproj" \
  -c Release \
  --no-build \
  -o "$OUTPUT_DIR" \
  "${COMMON_PROPERTIES[@]}"

for target_framework in net10.0 net11.0; do
  dotnet build "$ROOT_DIR/src/Raven.Macros/Raven.Macros.rvnproj" \
    -c Release \
    -f "$target_framework" \
    -p:BuildProjectReferences=false \
    "${COMMON_PROPERTIES[@]}"
done

dotnet pack "$ROOT_DIR/src/Raven.Macros/Raven.Macros.rvnproj" \
  -c Release \
  --no-build \
  -o "$OUTPUT_DIR" \
  "${COMMON_PROPERTIES[@]}"

dotnet pack "$ROOT_DIR/sdk/Raven.Sdk/Raven.Sdk.csproj" \
  -c Release \
  -o "$OUTPUT_DIR" \
  "${COMMON_PROPERTIES[@]}"

TEMPLATE_STAGE_DIR="$(mktemp -d /tmp/raven-template-package.XXXXXX)"
cleanup_template_stage() {
  case "$TEMPLATE_STAGE_DIR" in
    /tmp/raven-template-package.*) rm -rf "$TEMPLATE_STAGE_DIR" ;;
    *) echo "Refusing to remove unexpected template staging path: $TEMPLATE_STAGE_DIR" >&2 ;;
  esac
}
trap cleanup_template_stage EXIT

cp -R "$ROOT_DIR/templates/Raven.Templates/content" "$TEMPLATE_STAGE_DIR/content"
while IFS= read -r project_file; do
  perl -pi -e "s/RavenSdkVersion/$VERSION/g" "$project_file"
done < <(find "$TEMPLATE_STAGE_DIR/content" -name '*.rvnproj' -type f -print)

dotnet pack "$ROOT_DIR/templates/Raven.Templates/Raven.Templates.csproj" \
  -c Release \
  -o "$OUTPUT_DIR" \
  "/property:RavenTemplateContentRoot=$TEMPLATE_STAGE_DIR/content" \
  "${COMMON_PROPERTIES[@]}"

if [[ "${RAVEN_SKIP_PACKAGE_VALIDATION:-0}" != "1" ]]; then
  "$ROOT_DIR/scripts/test-nuget-packages.sh" "$VERSION" "$OUTPUT_DIR"
fi

echo "$OUTPUT_DIR"
