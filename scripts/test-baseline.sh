#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CODE_ANALYSIS_TESTS="$ROOT_DIR/test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj"

build_codegen_exclusion_filter() {
  local filter=""

  while IFS= read -r class_name; do
    [[ -z "$class_name" ]] && continue
    filter+="&FullyQualifiedName!~$class_name"
  done < <(
    find "$ROOT_DIR/test/Raven.CodeAnalysis.Tests/CodeGen" -name '*.cs' -print0 |
      xargs -0 sed -nE 's/^[[:space:]]*(public|internal)[[:space:]]+((sealed|abstract|static|partial)[[:space:]]+)*class[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\4/p' |
      sort -u
  )

  printf '%s' "$filter"
}

# Baseline excludes runtime/emission-heavy CodeGen + Samples tests.
# Some CodeGen tests still use older Raven.CodeAnalysis.Tests namespaces,
# so derive the CodeGen exclusion set from the folder rather than relying
# only on fully-qualified namespace substrings.
common_filter="FullyQualifiedName!~Sample"
code_analysis_filter="$common_filter$(build_codegen_exclusion_filter)"

dotnet test "$CODE_ANALYSIS_TESTS" /property:WarningLevel=0 \
  --filter "$code_analysis_filter"

while IFS= read -r project; do
  [[ "$project" == "$CODE_ANALYSIS_TESTS" ]] && continue
  [[ "$project" == *Raven.CodeAnalysis.Samples.Tests.csproj ]] && continue

  dotnet test "$project" /property:WarningLevel=0 \
    --filter "$common_filter"
done < <(
  find "$ROOT_DIR/src" "$ROOT_DIR/test" \
    \( -name '*Tests.csproj' -o -name '*.Testing.csproj' \) |
    sort
)
