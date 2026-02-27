#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Baseline excludes runtime/emission-heavy CodeGen + Samples tests.
dotnet test "$ROOT_DIR/Raven.sln" /property:WarningLevel=0 \
  --filter "FullyQualifiedName!~Raven.CodeAnalysis.Tests.CodeGen&FullyQualifiedName!~Raven.CodeAnalysis.Tests.Samples"
