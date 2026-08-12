#!/usr/bin/env bash
set -euo pipefail

SAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SAMPLE_DIR/../../.." && pwd)"
PROJECT="$SAMPLE_DIR/NanoFrameworkTemperature.rvnproj"
PACKAGES_DIR="${NANOFRAMEWORK_PACKAGES_DIR:-$SAMPLE_DIR/.packages}"
OUTPUT_DIR="${OUTPUT_DIR:-$SAMPLE_DIR/artifacts}"
COMPILER_DLL="${RAVEN_COMPILER_DLL:-$REPO_ROOT/src/Raven.Compiler/bin/Debug/net10.0/rvnc.dll}"
MONO_COMMAND="${MONO_COMMAND:-mono}"

require_command() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "Required command '$1' was not found." >&2
    exit 1
  fi
}

package_file() {
  local package_id="$1"
  local package_version="$2"
  local relative_path="$3"
  local package_directory package_id_lower

  package_id_lower="$(printf '%s' "$package_id" | tr '[:upper:]' '[:lower:]')"
  package_directory="$PACKAGES_DIR/$package_id_lower/$package_version"
  if [[ ! -d "$package_directory" ]]; then
    echo "Package '$package_id' version '$package_version' was not restored under '$PACKAGES_DIR'." >&2
    exit 1
  fi

  local resolved_path="$package_directory/$relative_path"
  if [[ ! -f "$resolved_path" ]]; then
    echo "Expected package file '$resolved_path' was not found." >&2
    exit 1
  fi

  printf '%s\n' "$resolved_path"
}

require_command dotnet
require_command "$MONO_COMMAND"

mkdir -p "$PACKAGES_DIR" "$OUTPUT_DIR"
dotnet restore "$PROJECT" --packages "$PACKAGES_DIR" --property WarningLevel=0

if [[ ! -f "$COMPILER_DLL" ]]; then
  dotnet build "$REPO_ROOT/src/Raven.Compiler/Raven.Compiler.csproj" \
    --framework net10.0 \
    --property WarningLevel=0
fi
if [[ ! -f "$COMPILER_DLL" ]]; then
  echo "Raven compiler '$COMPILER_DLL' was not produced." >&2
  exit 1
fi

CORE_LIBRARY="$(package_file nanoFramework.CoreLibrary 2.0.0-preview.52 lib/netnano1.0/mscorlib.dll)"
DHT_LIBRARY="$(package_file nanoFramework.Iot.Device.Dhtxx 2.0.0-preview.109 lib/netnano1.0/Iot.Device.Dhtxx.dll)"
TEMPERATURE_LIBRARY="$(package_file nanoFramework.UnitsNet.Temperature 5.77.0-preview.16 lib/netnano1.0/nanoFramework.UnitsNet.Temperature.dll)"
GPIO_LIBRARY="$(package_file nanoFramework.System.Device.Gpio 2.0.0-preview.18 lib/netnano1.0/System.Device.Gpio.dll)"

METADATA_PROCESSOR_PACKAGE="$PACKAGES_DIR/nanoframework.tools.metadataprocessor.cli/4.0.0-preview.101"
METADATA_PROCESSOR="$METADATA_PROCESSOR_PACKAGE/content/MetadataProcessor/nanoFramework.Tools.MetadataProcessor.exe"
if [[ ! -f "$METADATA_PROCESSOR" ]]; then
  METADATA_PROCESSOR="$METADATA_PROCESSOR_PACKAGE/contentFiles/any/any/MetadataProcessor/nanoFramework.Tools.MetadataProcessor.exe"
fi
if [[ ! -f "$METADATA_PROCESSOR" ]]; then
  echo "The nanoFramework metadata processor was not found in '$METADATA_PROCESSOR_PACKAGE'." >&2
  exit 1
fi

MANAGED_OUTPUT="$OUTPUT_DIR/NanoFrameworkTemperature.dll"
NANO_OUTPUT="$OUTPUT_DIR/NanoFrameworkTemperature.pe"

dotnet build "$PROJECT" \
  --no-restore \
  --configuration Debug \
  --output "$OUTPUT_DIR" \
  --property:RavenBuildConfiguration=Debug \
  --property:RavenCompilerHost="$COMPILER_DLL" \
  --property:NanoFrameworkPackagesDirectory="$PACKAGES_DIR" \
  --property:NanoFrameworkPackageOnBuild=false \
  --property:WarningLevel=0

"$MONO_COMMAND" "$METADATA_PROCESSOR" \
  -loadhints mscorlib "$CORE_LIBRARY" \
  -loadhints Iot.Device.Dhtxx "$DHT_LIBRARY" \
  -loadhints nanoFramework.UnitsNet.Temperature "$TEMPERATURE_LIBRARY" \
  -loadhints System.Device.Gpio "$GPIO_LIBRARY" \
  -parse "$MANAGED_OUTPUT" \
  -compile "$NANO_OUTPUT" false

if [[ ! -f "$NANO_OUTPUT" ]]; then
  echo "The metadata processor did not produce '$NANO_OUTPUT'." >&2
  exit 1
fi

NANO_HEADER="$(od -An -N6 -tx1 "$NANO_OUTPUT" | tr -d ' \n')"
if [[ "$NANO_HEADER" != "4e464d524b32" ]]; then
  echo "Unexpected nanoFramework image header '$NANO_HEADER'." >&2
  exit 1
fi

echo "Managed assembly: $MANAGED_OUTPUT"
echo "nanoFramework image: $NANO_OUTPUT"
