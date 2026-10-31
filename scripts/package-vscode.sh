#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VERSION="${1:-}"
EXTENSION_DIR="$ROOT_DIR/src/Raven.VSCode"
OUTPUT_DIR="${RAVEN_PACKAGE_OUTPUT:-$ROOT_DIR/artifacts/distribution}"
TFM="net11.0"
# Optional native adapter selection is explicit; default Raven packages remain .NET-only.
METADATA_ARGS=()
if [[ -n "${RAVEN_NEOCLR_METADATA_PROJECT:-}" ]]; then
  if [[ ! -f "$RAVEN_NEOCLR_METADATA_PROJECT" ]]; then
    echo "RAVEN_NEOCLR_METADATA_PROJECT must name the metadata project." >&2
    exit 1
  fi
  METADATA_ARGS+=("-p:NeoClrMetadataProject=$RAVEN_NEOCLR_METADATA_PROJECT")
fi
SERVER_DIR="$EXTENSION_DIR/server"

rm -rf "$SERVER_DIR"
mkdir -p "$SERVER_DIR" "$OUTPUT_DIR"

"$ROOT_DIR/scripts/generate-compiler-sources.sh"

dotnet publish ${METADATA_ARGS[@]+"${METADATA_ARGS[@]}"} "$ROOT_DIR/src/Raven.LanguageServer/Raven.LanguageServer.csproj" \
  -c Release -f "$TFM" --self-contained false -o "$SERVER_DIR" /property:WarningLevel=0

npm --prefix "$EXTENSION_DIR" ci
node "$ROOT_DIR/scripts/test-raven-highlighting-sync.mjs"
npm --prefix "$EXTENSION_DIR" run package:extension
if [[ -n "$VERSION" ]]; then
  (cd "$EXTENSION_DIR" && npm exec -- vsce package "$VERSION" \
    --no-update-package-json --no-dependencies \
    --out "$OUTPUT_DIR/raven-vscode.vsix")
else
  (cd "$EXTENSION_DIR" && npm exec -- vsce package --no-dependencies \
    --out "$OUTPUT_DIR/raven-vscode.vsix")
fi

echo "$OUTPUT_DIR/raven-vscode.vsix"
