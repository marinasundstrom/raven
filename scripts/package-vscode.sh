#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXTENSION_DIR="$ROOT_DIR/src/Raven.VSCode"
OUTPUT_DIR="${RAVEN_PACKAGE_OUTPUT:-$ROOT_DIR/artifacts/distribution}"
TFM="${RAVEN_PACKAGE_TFM:-net10.0}"
SERVER_DIR="$EXTENSION_DIR/server"

rm -rf "$SERVER_DIR"
mkdir -p "$SERVER_DIR" "$OUTPUT_DIR"

dotnet publish "$ROOT_DIR/src/Raven.LanguageServer/Raven.LanguageServer.csproj" \
  -c Release -f "$TFM" --self-contained false -o "$SERVER_DIR" /property:WarningLevel=0

npm --prefix "$EXTENSION_DIR" ci
npm --prefix "$EXTENSION_DIR" run compile
(cd "$EXTENSION_DIR" && npm exec -- vsce package --out "$OUTPUT_DIR/raven-vscode.vsix")

echo "$OUTPUT_DIR/raven-vscode.vsix"
