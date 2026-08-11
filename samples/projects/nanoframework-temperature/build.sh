#!/usr/bin/env bash
set -euo pipefail

SAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SAMPLE_DIR/../../.." && pwd)"
PACKAGES_DIR="${NANOFRAMEWORK_PACKAGES_DIR:-$SAMPLE_DIR/.packages}"
OUTPUT_DIR="${OUTPUT_DIR:-$SAMPLE_DIR/artifacts}"
COMPILER_DLL="${RAVEN_COMPILER_DLL:-$REPO_ROOT/src/Raven.Compiler/bin/Debug/net10.0/rvnc.dll}"
NUGET_COMMAND="${NUGET_COMMAND:-nuget}"
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
  local package_directory

  package_directory="$(find "$PACKAGES_DIR" -mindepth 1 -maxdepth 1 -type d -iname "$package_id.$package_version" -print -quit)"
  if [[ -z "$package_directory" ]]; then
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
require_command "$NUGET_COMMAND"
require_command "$MONO_COMMAND"

mkdir -p "$PACKAGES_DIR" "$OUTPUT_DIR"
"$NUGET_COMMAND" restore "$SAMPLE_DIR/packages.config" -PackagesDirectory "$PACKAGES_DIR" -NonInteractive

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
DHT_LIBRARY="$(package_file nanoFramework.Iot.Device.Dhtxx 1.2.1016 lib/Iot.Device.Dhtxx.dll)"
TEMPERATURE_LIBRARY="$(package_file nanoFramework.UnitsNet.Temperature 5.76.15 lib/netnano1.0/nanoFramework.UnitsNet.Temperature.dll)"
HUMIDITY_LIBRARY="$(package_file nanoFramework.UnitsNet.RelativeHumidity 5.76.15 lib/netnano1.0/nanoFramework.UnitsNet.RelativeHumidity.dll)"
GPIO_LIBRARY="$(package_file nanoFramework.System.Device.Gpio 1.1.62 lib/System.Device.Gpio.dll)"
I2C_LIBRARY="$(package_file nanoFramework.System.Device.I2c 1.1.29 lib/System.Device.I2c.dll)"
MODEL_LIBRARY="$(package_file nanoFramework.System.Device.Model 1.2.862 lib/System.Device.Model.dll)"
NATIVE_LIBRARY="$(package_file nanoFramework.Runtime.Native 1.7.11 lib/nanoFramework.Runtime.Native.dll)"
EVENTS_LIBRARY="$(package_file nanoFramework.Runtime.Events 1.11.37 lib/nanoFramework.Runtime.Events.dll)"

METADATA_PROCESSOR_PACKAGE="$(find "$PACKAGES_DIR" -mindepth 1 -maxdepth 1 -type d -iname 'nanoFramework.Tools.MetadataProcessor.CLI.4.0.0-preview.101' -print -quit)"
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

dotnet "$COMPILER_DLL" \
  --no-framework-references \
  --target-core-library "$CORE_LIBRARY" \
  --refs "$DHT_LIBRARY" \
  --refs "$TEMPERATURE_LIBRARY" \
  --refs "$HUMIDITY_LIBRARY" \
  --refs "$GPIO_LIBRARY" \
  --refs "$I2C_LIBRARY" \
  --refs "$MODEL_LIBRARY" \
  --refs "$NATIVE_LIBRARY" \
  --refs "$EVENTS_LIBRARY" \
  --emit-core-types-only \
  --output-type console \
  --output "$MANAGED_OUTPUT" \
  "$SAMPLE_DIR/Program.rvn"

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
