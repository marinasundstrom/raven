#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Runtime/emission-heavy tests are isolated in a dedicated pass.
# Raven.CodeAnalysis.Tests already runs with xUnit collection parallelization disabled.
dotnet test "$ROOT_DIR/test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj" /property:WarningLevel=0 \
  --filter "(FullyQualifiedName~CodeGen|FullyQualifiedName~Samples)&FullyQualifiedName!~CodeGen.Development"
