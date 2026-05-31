#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROJECT="$ROOT_DIR/test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj"

build_codegen_inclusion_filter() {
  local filter="FullyQualifiedName~CodeGen"

  while IFS= read -r class_name; do
    [[ -z "$class_name" ]] && continue
    filter+="|FullyQualifiedName~.$class_name."
  done < <(
    find "$ROOT_DIR/test/Raven.CodeAnalysis.Tests/CodeGen" -name '*.cs' -print0 |
      xargs -0 sed -nE 's/^[[:space:]]*(public|internal)[[:space:]]+((sealed|abstract|static|partial)[[:space:]]+)*class[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\4/p' |
      sort -u
  )

  printf '%s' "$filter"
}

build_additional_isolated_filter() {
  local filter=""
  local names=(
    "MsBuildSampleProjectCompilationTests"
    "ProjectDocumentationEmissionTests"
    "ProjectFileTargetFrameworkAttributeTests"
    "RavenProjectOutputDeterminismTests"
    "NuGetHarness_AvaloniaRefAssemblySignature_EmitsWithoutRuntimeTypeLoadCrash"
    "NuGetHarness_AvaloniaConstructedGenericContainerMember_Emits"
    "StaticFactoryMethod_UsesCanonicalSourceMethodForEmission"
    "OpenProject_RavenMacroProjectReference_WithObservableReplacement_EmitsExpandedSetter"
  )

  for name in "${names[@]}"; do
    filter+="|FullyQualifiedName~$name"
  done

  printf '%s' "$filter"
}

build_stale_runtime_exclusion_filter() {
  local filter=""
  local names=(
    "AsyncEntryPointBridgeTests"
    "AsyncPropagateCodeGenTests"
    "AsyncTryAwaitCodeGenTests"
    "ByRefCodeGenTests"
    "ExpressionBodyCodeGenTests"
    "FunctionExpressionCodeGenTests"
    "GenericInvocationCodeGenTests"
    "MacroCodeGenTests"
    "MemberBindingCodeGenTests"
    "MsBuildSampleProjectCompilationTests"
    "PdbSequencePointTests"
    "PrimaryConstructorParameterCodeGenTests"
    "ProjectFileNuGetReferenceTests"
    "PropertyTests"
    "RuntimeAsyncCodeGenTests"
    "RuntimeSymbolResolverTests"
    "TrailingBlockCodeGenTests"
    "TryExpressionCodeGenTests"
    "TypeOfExpressionCodeGenTests"
    "TypeResolutionPrecedenceTests"
    "UnionCodeGenTests"
  )

  for name in "${names[@]}"; do
    filter+="&FullyQualifiedName!~.$name."
  done

  printf '%s' "$filter"
}

# Runtime/emission-heavy tests are isolated in a dedicated pass.
# Raven.CodeAnalysis.Tests already runs with xUnit collection parallelization disabled.
dotnet test "$PROJECT" /property:WarningLevel=0 --blame-hang-timeout 300s --blame-hang-dump-type none \
  --filter "($(build_codegen_inclusion_filter)$(build_additional_isolated_filter)|FullyQualifiedName~Sample)&FullyQualifiedName!~CodeGen.Development$(build_stale_runtime_exclusion_filter)"
