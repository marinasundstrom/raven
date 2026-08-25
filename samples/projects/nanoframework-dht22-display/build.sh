#!/usr/bin/env bash
set -euo pipefail

SAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SAMPLE_DIR/../../.." && pwd)"
PROJECT="$SAMPLE_DIR/NanoFrameworkDht22Display.rvnproj"
PACKAGES_DIR="${NANOFRAMEWORK_PACKAGES_DIR:-$SAMPLE_DIR/.packages}"
OUTPUT_DIR="${OUTPUT_DIR:-$SAMPLE_DIR/artifacts}"
CONFIGURATION="${RAVEN_BUILD_CONFIGURATION:-Release}"
COMPILER_DLL="${RAVEN_COMPILER_DLL:-$REPO_ROOT/src/Raven.Compiler/bin/Debug/net10.0/rvnc.dll}"

if [[ ! -f "$COMPILER_DLL" ]]; then
  dotnet build "$REPO_ROOT/src/Raven.Compiler/Raven.Compiler.csproj" \
    --framework net10.0 \
    --property WarningLevel=0
fi

dotnet restore "$PROJECT" \
  --packages "$PACKAGES_DIR" \
  --property WarningLevel=0

dotnet build "$PROJECT" \
  --no-restore \
  --configuration "$CONFIGURATION" \
  --output "$OUTPUT_DIR" \
  --property:RavenBuildConfiguration="$CONFIGURATION" \
  --property:RavenCompilerHost="$COMPILER_DLL" \
  --property:NanoFrameworkPackagesDirectory="$PACKAGES_DIR" \
  --property:WarningLevel=0

echo "Managed assembly: $OUTPUT_DIR/NanoFrameworkDht22Display.dll"
echo "nanoFramework image: $OUTPUT_DIR/NanoFrameworkDht22Display.pe"
echo "Deployment image: $OUTPUT_DIR/NanoFrameworkDht22Display.bin"
