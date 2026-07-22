#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

(cd "$ROOT_DIR/src/Raven.CodeAnalysis/Syntax" && \
  dotnet run --project "$ROOT_DIR/tools/NodeGenerator/NodeGenerator.csproj" -- -f)
(cd "$ROOT_DIR/src/Raven.CodeAnalysis" && \
  dotnet run --project "$ROOT_DIR/tools/BoundNodeGenerator/BoundNodeGenerator.csproj" -- -f)
(cd "$ROOT_DIR/src/Raven.CodeAnalysis" && \
  dotnet run --project "$ROOT_DIR/tools/DiagnosticsGenerator/DiagnosticsGenerator.csproj" -- -f)
