#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD_CONFIG="${BUILD_CONFIG:-Debug}"

echo "==> Restoring dependencies"
(cd "$ROOT_DIR" && dotnet restore)

echo "==> Running generators"
(cd "$ROOT_DIR/src/Raven.CodeAnalysis/Syntax" && dotnet run --no-restore --project ../../../tools/NodeGenerator -- -f)
(cd "$ROOT_DIR/src/Raven.CodeAnalysis" && dotnet run --no-restore --project ../../tools/BoundNodeGenerator -- -f)
(cd "$ROOT_DIR/src/Raven.CodeAnalysis" && dotnet run --no-restore --project ../../tools/DiagnosticsGenerator -- -f)

echo "==> Building Raven.CodeAnalysis"
dotnet build "$ROOT_DIR/src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj" -c "$BUILD_CONFIG" --no-restore --property WarningLevel=0

echo "==> Building Raven.Compiler without Raven.Core"
dotnet build "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" -c "$BUILD_CONFIG" --no-restore --property WarningLevel=0 -p:UseRavenCoreReference=false

echo "==> Emitting Raven.Core.dll via ravc"
RAVEN_CORE_OUT="$ROOT_DIR/src/Raven.Core/bin/$BUILD_CONFIG/net9.0/Raven.Core.dll"
mkdir -p "$(dirname "$RAVEN_CORE_OUT")"
RAVEN_CORE_SOURCES=()
while IFS= read -r source_file; do
  RAVEN_CORE_SOURCES+=("$source_file")
done < <(find "$ROOT_DIR/src/Raven.Core" -maxdepth 1 -name '*.rav' | sort)
if [ "${#RAVEN_CORE_SOURCES[@]}" -eq 0 ]; then
  echo "No Raven.Core sources found under src/Raven.Core"
  exit 1
fi
dotnet run --project "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" --no-restore -p:UseRavenCoreReference=false \
  -- --emit-core-types-only --framework net9.0 --output-type classlib \
  -o "$RAVEN_CORE_OUT" "${RAVEN_CORE_SOURCES[@]}"

echo "==> Building Raven.Compiler with Raven.Core"
dotnet build "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" -c "$BUILD_CONFIG" --no-restore --property WarningLevel=0

# echo "==> Building Raven solution"
# dotnet build "$ROOT_DIR" -c "$BUILD_CONFIG" --no-restore --property WarningLevel=0
