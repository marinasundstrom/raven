#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROJECT="$ROOT_DIR/test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj"

build_codegen_inclusion_filter() {
  local filter="FullyQualifiedName~CodeGen"

  while IFS= read -r class_name; do
    [[ -z "$class_name" ]] && continue
    filter+="|FullyQualifiedName~$class_name"
  done < <(
    find "$ROOT_DIR/test/Raven.CodeAnalysis.Tests/CodeGen" -name '*.cs' -print0 |
      xargs -0 sed -nE 's/^[[:space:]]*(public|internal)[[:space:]]+((sealed|abstract|static|partial)[[:space:]]+)*class[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\4/p' |
      sort -u
  )

  printf '%s' "$filter"
}

# Runtime/emission-heavy tests are isolated in a dedicated pass.
# Raven.CodeAnalysis.Tests already runs with xUnit collection parallelization disabled.
dotnet test "$PROJECT" /property:WarningLevel=0 \
  --filter "($(build_codegen_inclusion_filter)|FullyQualifiedName~Sample)&FullyQualifiedName!~CodeGen.Development"
