#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RID="${1:-}"
VERSION="${2:-0.1.0-dev}"
TFM="${RAVEN_PACKAGE_TFM:-net10.0}"
OUTPUT_DIR="${RAVEN_PACKAGE_OUTPUT:-$ROOT_DIR/artifacts/distribution}"

if [[ -z "$RID" ]]; then
  echo "Usage: scripts/package-sdk.sh <runtime-identifier> [version]" >&2
  exit 1
fi

STAGE_DIR="$OUTPUT_DIR/raven-sdk-$VERSION-$RID"
PUBLISH_DIR="$OUTPUT_DIR/.publish-$RID"

rm -rf "$STAGE_DIR" "$PUBLISH_DIR"
mkdir -p "$STAGE_DIR/bin" "$STAGE_DIR/tools/rvn" "$STAGE_DIR/tools/rvnc" \
  "$STAGE_DIR/tools/language-server" "$STAGE_DIR/sdk/build" "$PUBLISH_DIR"

dotnet publish "$ROOT_DIR/src/Raven/Raven.csproj" -c Release -f "$TFM" -r "$RID" \
  --self-contained false -o "$PUBLISH_DIR/rvn" /property:WarningLevel=0
dotnet publish "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" -c Release -f "$TFM" -r "$RID" \
  --self-contained false -o "$PUBLISH_DIR/rvnc" /property:WarningLevel=0
dotnet publish "$ROOT_DIR/src/Raven.LanguageServer/Raven.LanguageServer.csproj" -c Release -f "$TFM" -r "$RID" \
  --self-contained false -o "$PUBLISH_DIR/language-server" /property:WarningLevel=0

cp -R "$PUBLISH_DIR/rvn/." "$STAGE_DIR/tools/rvn/"
cp -R "$PUBLISH_DIR/rvnc/." "$STAGE_DIR/tools/rvnc/"
cp -R "$PUBLISH_DIR/language-server/." "$STAGE_DIR/tools/language-server/"
cp "$ROOT_DIR/build/Raven.Language.targets" "$STAGE_DIR/sdk/build/"
cp "$ROOT_DIR/build/Raven.MSBuild.props" "$STAGE_DIR/sdk/build/"
cp "$ROOT_DIR/build/Raven.MSBuild.targets" "$STAGE_DIR/sdk/build/"
cp "$ROOT_DIR/src/Raven.Core/bin/Release/$TFM/Raven.Core.dll" "$STAGE_DIR/sdk/"
printf '%s\n' "$VERSION" > "$STAGE_DIR/VERSION"

if [[ "$RID" == win-* ]]; then
  printf '@echo off\r\ndotnet "%%~dp0..\\tools\\rvn\\rvn.dll" %%*\r\n' > "$STAGE_DIR/bin/rvn.cmd"
  printf '@echo off\r\ndotnet "%%~dp0..\\tools\\rvnc\\rvnc.dll" %%*\r\n' > "$STAGE_DIR/bin/rvnc.cmd"
  printf '@echo off\r\ndotnet "%%~dp0..\\tools\\language-server\\Raven.LanguageServer.dll" %%*\r\n' > "$STAGE_DIR/bin/raven-language-server.cmd"
  (cd "$OUTPUT_DIR" && zip -qr "raven-sdk-$VERSION-$RID.zip" "$(basename "$STAGE_DIR")")
else
  cp "$ROOT_DIR/eng/distribution/rvn" "$STAGE_DIR/bin/rvn"
  cp "$ROOT_DIR/eng/distribution/rvnc" "$STAGE_DIR/bin/rvnc"
  cp "$ROOT_DIR/eng/distribution/raven-language-server" "$STAGE_DIR/bin/raven-language-server"
  chmod +x "$STAGE_DIR/bin/rvn" "$STAGE_DIR/bin/rvnc" "$STAGE_DIR/bin/raven-language-server"
  tar -C "$OUTPUT_DIR" -czf "$OUTPUT_DIR/raven-sdk-$VERSION-$RID.tar.gz" "$(basename "$STAGE_DIR")"
fi

echo "$STAGE_DIR"
