#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD_CONFIG="${BUILD_CONFIG:-Debug}"

echo "==> Running generators"
(cd "$ROOT_DIR/src/Raven.CodeAnalysis/Syntax" && dotnet run --project ../../../tools/NodeGenerator -- -f)
(cd "$ROOT_DIR/src/Raven.CodeAnalysis" && dotnet run --project ../../tools/BoundNodeGenerator -- -f)
(cd "$ROOT_DIR/src/Raven.CodeAnalysis" && dotnet run --project ../../tools/DiagnosticsGenerator -- -f)

echo "==> Building Raven.CodeAnalysis"
dotnet build "$ROOT_DIR/src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj" -c "$BUILD_CONFIG" --property WarningLevel=0

echo "==> Building Raven.Compiler without Raven.Core"
dotnet build "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" -c "$BUILD_CONFIG" --property WarningLevel=0 -p:UseRavenCoreReference=false

echo "==> Emitting Raven.Core.dll via ravc"
RAVEN_CORE_OUT="$ROOT_DIR/src/Raven.Core/bin/$BUILD_CONFIG/net9.0/Raven.Core.dll"
mkdir -p "$(dirname "$RAVEN_CORE_OUT")"
dotnet run --project "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" -p:UseRavenCoreReference=false \
  -- --emit-core-types-only --framework net9.0 --output-type classlib \
  -o "$RAVEN_CORE_OUT" "$ROOT_DIR/src/Raven.Core/Option.rav" "$ROOT_DIR/src/Raven.Core/Result.rav"

echo "==> Building Raven.Compiler with Raven.Core"
dotnet build "$ROOT_DIR/src/Raven.Compiler/Raven.Compiler.csproj" -c "$BUILD_CONFIG" --property WarningLevel=0
