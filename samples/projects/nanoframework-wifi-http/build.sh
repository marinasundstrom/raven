#!/usr/bin/env bash
set -euo pipefail

SAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SAMPLE_DIR/../../.." && pwd)"
PROJECT="$SAMPLE_DIR/NanoFrameworkWifiHttp.rvnproj"
PACKAGES_DIR="${NANOFRAMEWORK_PACKAGES_DIR:-$SAMPLE_DIR/.packages}"
OUTPUT_DIR="${OUTPUT_DIR:-$SAMPLE_DIR/artifacts}"
COMPILER_DLL="${RAVEN_COMPILER_DLL:-$REPO_ROOT/src/Raven.Compiler/bin/Debug/net10.0/rvnc.dll}"
MONO_COMMAND="${MONO_COMMAND:-mono}"
BOARD="pico-w"
LED_PIN=""

usage() {
  cat <<'EOF'
Usage: ./build.sh [--board pico-w|pico2-w] [--led-pin <gpio>]

Defaults to Pico W with an external LED on GP15. When RAVEN_WIFI_SSID and
RAVEN_WIFI_PASSWORD are set, their values replace the sample's nonfunctional
placeholder credentials at compile time.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --board)
      [[ $# -ge 2 ]] || { echo "Missing value for --board." >&2; exit 2; }
      BOARD="$2"
      shift 2
      ;;
    --led-pin)
      [[ $# -ge 2 ]] || { echo "Missing value for --led-pin." >&2; exit 2; }
      LED_PIN="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option '$1'." >&2
      usage >&2
      exit 2
      ;;
  esac
done

case "$BOARD" in
  pico-w|pico2-w) ;;
  *)
    echo "Unsupported board profile '$BOARD'. This sample requires a wireless Pico." >&2
    exit 2
    ;;
esac

if [[ -n "$LED_PIN" && ! "$LED_PIN" =~ ^[0-9]+$ ]]; then
  echo "--led-pin must be a non-negative GPIO number." >&2
  exit 2
fi

PROFILE_OUTPUT_DIR="$OUTPUT_DIR/$BOARD"

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
require_command python3

mkdir -p "$PACKAGES_DIR" "$PROFILE_OUTPUT_DIR"
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
CORE_LIBRARY_PE="$(package_file nanoFramework.CoreLibrary 2.0.0-preview.52 lib/netnano1.0/mscorlib.pe)"
EVENTS_LIBRARY="$(package_file nanoFramework.Runtime.Events 2.0.0-preview.13 lib/netnano1.0/nanoFramework.Runtime.Events.dll)"
EVENTS_LIBRARY_PE="$(package_file nanoFramework.Runtime.Events 2.0.0-preview.13 lib/netnano1.0/nanoFramework.Runtime.Events.pe)"
COLLECTIONS_LIBRARY="$(package_file nanoFramework.System.Collections 2.0.0-preview.15 lib/netnano1.0/nanoFramework.System.Collections.dll)"
COLLECTIONS_LIBRARY_PE="$(package_file nanoFramework.System.Collections 2.0.0-preview.15 lib/netnano1.0/nanoFramework.System.Collections.pe)"
TEXT_LIBRARY="$(package_file nanoFramework.System.Text 2.0.0-preview.23 lib/netnano1.0/nanoFramework.System.Text.dll)"
TEXT_LIBRARY_PE="$(package_file nanoFramework.System.Text 2.0.0-preview.23 lib/netnano1.0/nanoFramework.System.Text.pe)"
NET_LIBRARY="$(package_file nanoFramework.System.Net 2.0.0-preview.32 lib/netnano1.0/System.Net.dll)"
NET_LIBRARY_PE="$(package_file nanoFramework.System.Net 2.0.0-preview.32 lib/netnano1.0/System.Net.pe)"
THREADING_LIBRARY="$(package_file nanoFramework.System.Threading 2.0.0-preview.14 lib/netnano1.0/System.Threading.dll)"
THREADING_LIBRARY_PE="$(package_file nanoFramework.System.Threading 2.0.0-preview.14 lib/netnano1.0/System.Threading.pe)"
STREAMS_LIBRARY="$(package_file nanoFramework.System.IO.Streams 2.0.0-preview.22 lib/netnano1.0/System.IO.Streams.dll)"
STREAMS_LIBRARY_PE="$(package_file nanoFramework.System.IO.Streams 2.0.0-preview.22 lib/netnano1.0/System.IO.Streams.pe)"
GPIO_LIBRARY="$(package_file nanoFramework.System.Device.Gpio 2.0.0-preview.18 lib/netnano1.0/System.Device.Gpio.dll)"
GPIO_LIBRARY_PE="$(package_file nanoFramework.System.Device.Gpio 2.0.0-preview.18 lib/netnano1.0/System.Device.Gpio.pe)"
WIFI_LIBRARY="$(package_file nanoFramework.System.Device.Wifi 2.0.0-preview.32 lib/netnano1.0/System.Device.Wifi.dll)"
WIFI_LIBRARY_PE="$(package_file nanoFramework.System.Device.Wifi 2.0.0-preview.32 lib/netnano1.0/System.Device.Wifi.pe)"
HTTP_LIBRARY="$(package_file nanoFramework.System.Net.Http 2.0.0-preview.9 lib/System.Net.Http.dll)"
HTTP_LIBRARY_PE="$(package_file nanoFramework.System.Net.Http 2.0.0-preview.9 lib/System.Net.Http.pe)"

DOTNET_SDK_LINE="$(dotnet --list-sdks | tail -n 1)"
DOTNET_SDK_VERSION="${DOTNET_SDK_LINE%% *}"
DOTNET_SDK_ROOT="${DOTNET_SDK_LINE#*[}"
DOTNET_SDK_ROOT="${DOTNET_SDK_ROOT%]}"
CSC_DLL="$DOTNET_SDK_ROOT/$DOTNET_SDK_VERSION/Roslyn/bincore/csc.dll"
if [[ ! -f "$CSC_DLL" ]]; then
  echo "The C# compiler was not found at '$CSC_DLL'." >&2
  exit 1
fi

METADATA_PROCESSOR_PACKAGE="$PACKAGES_DIR/nanoframework.tools.metadataprocessor.cli/4.0.0-preview.101"
METADATA_PROCESSOR="$METADATA_PROCESSOR_PACKAGE/content/MetadataProcessor/nanoFramework.Tools.MetadataProcessor.exe"
if [[ ! -f "$METADATA_PROCESSOR" ]]; then
  METADATA_PROCESSOR="$METADATA_PROCESSOR_PACKAGE/contentFiles/any/any/MetadataProcessor/nanoFramework.Tools.MetadataProcessor.exe"
fi
if [[ ! -f "$METADATA_PROCESSOR" ]]; then
  echo "The nanoFramework metadata processor was not found in '$METADATA_PROCESSOR_PACKAGE'." >&2
  exit 1
fi

MANAGED_OUTPUT="$PROFILE_OUTPUT_DIR/NanoFrameworkWifiHttp.dll"
NANO_OUTPUT="$PROFILE_OUTPUT_DIR/NanoFrameworkWifiHttp.pe"
DEPLOYMENT_OUTPUT="$PROFILE_OUTPUT_DIR/NanoFrameworkWifiHttp.bin"
BRIDGE_MANAGED_OUTPUT="$PROFILE_OUTPUT_DIR/NanoFrameworkWifiHttp.Bridge.dll"
BRIDGE_NANO_OUTPUT="$PROFILE_OUTPUT_DIR/NanoFrameworkWifiHttp.Bridge.pe"

dotnet "$CSC_DLL" \
  -nologo \
  -noconfig \
  -nostdlib+ \
  -target:library \
  -define:DEBUG \
  -langversion:latest \
  -out:"$BRIDGE_MANAGED_OUTPUT" \
  -reference:"$CORE_LIBRARY" \
  -reference:"$EVENTS_LIBRARY" \
  -reference:"$COLLECTIONS_LIBRARY" \
  -reference:"$TEXT_LIBRARY" \
  -reference:"$THREADING_LIBRARY" \
  -reference:"$STREAMS_LIBRARY" \
  -reference:"$NET_LIBRARY" \
  -reference:"$WIFI_LIBRARY" \
  -reference:"$HTTP_LIBRARY" \
  "$SAMPLE_DIR/NetworkHttpBridge.cs"

BUILD_ARGUMENTS=(
  --no-restore
  --configuration Debug
  --output "$PROFILE_OUTPUT_DIR"
  --property:RavenBuildConfiguration=Debug
  --property:RavenCompilerHost="$COMPILER_DLL"
  --property:NanoFrameworkPackagesDirectory="$PACKAGES_DIR"
  --property:NanoFrameworkPackageOnBuild=false
  --property:NanoFrameworkBridgePath="$BRIDGE_MANAGED_OUTPUT"
  --property:WarningLevel=0
)

if [[ -n "$LED_PIN" ]]; then
  BUILD_ARGUMENTS+=(--property:RavenLedPin="$LED_PIN")
fi

EXTERNAL_CONSTANT_OVERRIDES="$(
  RAVEN_WIFI_SSID_VALUE="${RAVEN_WIFI_SSID:-}" \
  RAVEN_WIFI_PASSWORD_VALUE="${RAVEN_WIFI_PASSWORD:-}" \
    python3 -c 'import base64, json, os; values = {"Wifi_SSID": os.environ["RAVEN_WIFI_SSID_VALUE"], "Wifi_Password": os.environ["RAVEN_WIFI_PASSWORD_VALUE"]}; print(base64.b64encode(json.dumps(values, separators=(",", ":")).encode()).decode())'
)"

RavenExternalConstantOverrides="$EXTERNAL_CONSTANT_OVERRIDES" \
  dotnet build "$PROJECT" "${BUILD_ARGUMENTS[@]}"

"$MONO_COMMAND" "$METADATA_PROCESSOR" \
  -loadhints mscorlib "$CORE_LIBRARY" \
  -loadhints nanoFramework.Runtime.Events "$EVENTS_LIBRARY" \
  -loadhints nanoFramework.System.Collections "$COLLECTIONS_LIBRARY" \
  -loadhints nanoFramework.System.Text "$TEXT_LIBRARY" \
  -loadhints System.Net "$NET_LIBRARY" \
  -loadhints System.Threading "$THREADING_LIBRARY" \
  -loadhints System.IO.Streams "$STREAMS_LIBRARY" \
  -loadhints System.Device.Gpio "$GPIO_LIBRARY" \
  -loadhints System.Device.Wifi "$WIFI_LIBRARY" \
  -loadhints System.Net.Http "$HTTP_LIBRARY" \
  -parse "$BRIDGE_MANAGED_OUTPUT" \
  -compile "$BRIDGE_NANO_OUTPUT" false

if [[ ! -f "$BRIDGE_NANO_OUTPUT" ]]; then
  echo "The metadata processor did not produce '$BRIDGE_NANO_OUTPUT'." >&2
  exit 1
fi

BRIDGE_NANO_HEADER="$(od -An -N6 -tx1 "$BRIDGE_NANO_OUTPUT" | tr -d ' \n')"
if [[ "$BRIDGE_NANO_HEADER" != "4e464d524b32" ]]; then
  echo "Unexpected nanoFramework bridge image header '$BRIDGE_NANO_HEADER'." >&2
  exit 1
fi

"$MONO_COMMAND" "$METADATA_PROCESSOR" \
  -loadhints mscorlib "$CORE_LIBRARY" \
  -loadhints nanoFramework.Runtime.Events "$EVENTS_LIBRARY" \
  -loadhints nanoFramework.System.Collections "$COLLECTIONS_LIBRARY" \
  -loadhints nanoFramework.System.Text "$TEXT_LIBRARY" \
  -loadhints System.Net "$NET_LIBRARY" \
  -loadhints System.Threading "$THREADING_LIBRARY" \
  -loadhints System.IO.Streams "$STREAMS_LIBRARY" \
  -loadhints System.Device.Gpio "$GPIO_LIBRARY" \
  -loadhints System.Device.Wifi "$WIFI_LIBRARY" \
  -loadhints System.Net.Http "$HTTP_LIBRARY" \
  -loadhints NanoFrameworkWifiHttp.Bridge "$BRIDGE_MANAGED_OUTPUT" \
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

append_aligned_pe() {
  local source_path="$1"
  local source_size padding_size

  source_size="$(wc -c < "$source_path" | tr -d ' ')"
  padding_size=$(( (4 - (source_size % 4)) % 4 ))
  dd if="$source_path" bs=65536 status=none >> "$DEPLOYMENT_OUTPUT"
  if (( padding_size > 0 )); then
    dd if=/dev/zero bs=1 count="$padding_size" status=none >> "$DEPLOYMENT_OUTPUT"
  fi
}

: > "$DEPLOYMENT_OUTPUT"
append_aligned_pe "$CORE_LIBRARY_PE"
append_aligned_pe "$EVENTS_LIBRARY_PE"
append_aligned_pe "$COLLECTIONS_LIBRARY_PE"
append_aligned_pe "$TEXT_LIBRARY_PE"
append_aligned_pe "$NET_LIBRARY_PE"
append_aligned_pe "$THREADING_LIBRARY_PE"
append_aligned_pe "$STREAMS_LIBRARY_PE"
append_aligned_pe "$GPIO_LIBRARY_PE"
append_aligned_pe "$WIFI_LIBRARY_PE"
append_aligned_pe "$HTTP_LIBRARY_PE"
append_aligned_pe "$BRIDGE_NANO_OUTPUT"
append_aligned_pe "$NANO_OUTPUT"

echo "Board profile: $BOARD"
echo "Managed assembly: $MANAGED_OUTPUT"
echo "nanoFramework image: $NANO_OUTPUT"
echo "nanoFramework bridge: $BRIDGE_NANO_OUTPUT"
echo "Deployment image: $DEPLOYMENT_OUTPUT"
