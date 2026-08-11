#!/usr/bin/env bash
set -euo pipefail

SAMPLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SAMPLE_DIR/../../.." && pwd)"
PROJECT="$SAMPLE_DIR/GreenhouseMonitor.rvnproj"
RAVEN_CONFIGURATION="Debug"
RAVEN_COMPILER="$REPO_ROOT/src/Raven.Compiler/bin/$RAVEN_CONFIGURATION/net10.0/rvnc.dll"
RAVEN_CORE="$REPO_ROOT/src/Raven.Core/bin/$RAVEN_CONFIGURATION/net10.0/Raven.Core.dll"

host_rid() {
  case "$(uname -s)-$(uname -m)" in
    Darwin-arm64) echo "osx-arm64" ;;
    Darwin-x86_64) echo "osx-x64" ;;
    Linux-aarch64 | Linux-arm64) echo "linux-arm64" ;;
    Linux-x86_64) echo "linux-x64" ;;
    *) return 1 ;;
  esac
}

HOST_RID="$(host_rid || true)"
TARGET_RID="${1:-$HOST_RID}"

if [[ -z "$TARGET_RID" ]]; then
  echo "Could not infer a supported host runtime identifier; pass one explicitly." >&2
  echo "Usage: $0 [osx-arm64|osx-x64|linux-arm64|linux-x64]" >&2
  exit 1
fi

if [[ "${FORCE_REBUILD:-0}" == "1" || ! -f "$RAVEN_COMPILER" || ! -f "$RAVEN_CORE" ]]; then
  "$REPO_ROOT/scripts/codex-build.sh"
fi

OUTPUT_DIR="${OUTPUT_DIR:-$SAMPLE_DIR/artifacts/native-aot/$TARGET_RID}"

dotnet publish "$PROJECT" \
  --configuration Release \
  --runtime "$TARGET_RID" \
  --self-contained true \
  --output "$OUTPUT_DIR" \
  --property PublishAot=true \
  --property StripSymbols=true \
  --property RavenBuildConfiguration="$RAVEN_CONFIGURATION" \
  --property WarningLevel=0

EXECUTABLE="$OUTPUT_DIR/GreenhouseMonitor"
if [[ ! -x "$EXECUTABLE" ]]; then
  echo "Expected native executable '$EXECUTABLE' was not produced." >&2
  exit 1
fi

echo "Native AOT executable: $EXECUTABLE"

if [[ "${RUN:-0}" == "1" ]]; then
  if [[ "$TARGET_RID" != "$HOST_RID" ]]; then
    echo "Cannot run '$TARGET_RID' output on host '$HOST_RID'." >&2
    exit 1
  fi

  "$EXECUTABLE"
fi
