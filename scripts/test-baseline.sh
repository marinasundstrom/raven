#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Baseline excludes runtime/emission-heavy CodeGen + Samples tests.
# Use substring matching on fully-qualified test names to catch both
# namespace- and class-based naming (for example RuntimeAsyncCodeGenTests
# under Raven.CodeAnalysis.Tests).
dotnet test "$ROOT_DIR/Raven.sln" /property:WarningLevel=0 \
  --filter "FullyQualifiedName!~CodeGen&FullyQualifiedName!~Samples"
