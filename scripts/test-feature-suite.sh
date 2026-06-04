#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROJECT="$ROOT_DIR/test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj"

if [[ $# -eq 0 || "${1:-}" == "--list" ]]; then
  cat <<'EOF'
Usage: scripts/test-feature-suite.sh <suite> [--runtime]

Available suites:
  overload-resolution
  functions-async
  patterns
  extensions
  partials
  macros
  imports-and-namespaces
  framework-and-targeting

Default mode runs the fast syntax/semantic surface for the suite and excludes
CodeGen, sample, and known runtime/project-heavy integration tests.

Use --runtime to run the suite's runtime/emission-heavy overlap explicitly.
EOF
  exit 0
fi

suite="$1"
mode="${2:-fast}"
filter=""

if [[ "$mode" != "fast" && "$mode" != "--runtime" ]]; then
  echo "Unknown option: $mode" >&2
  echo "Run scripts/test-feature-suite.sh --list for usage." >&2
  exit 1
fi

build_runtime_heavy_filter() {
  local filter="FullyQualifiedName~CodeGen"
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

  while IFS= read -r class_name; do
    [[ -z "$class_name" ]] && continue
    filter+="|FullyQualifiedName~.$class_name."
  done < <(
    find "$ROOT_DIR/test/Raven.CodeAnalysis.Tests/CodeGen" -name '*.cs' -print0 |
      xargs -0 sed -nE 's/^[[:space:]]*(public|internal)[[:space:]]+((sealed|abstract|static|partial)[[:space:]]+)*class[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\4/p' |
      sort -u
  )

  for name in "${names[@]}"; do
    filter+="|FullyQualifiedName~$name"
  done

  filter+="|FullyQualifiedName~Sample"

  printf '%s' "$filter"
}

build_fast_exclusion_filter() {
  local filter="FullyQualifiedName!~Sample"
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

  while IFS= read -r class_name; do
    [[ -z "$class_name" ]] && continue
    filter+="&FullyQualifiedName!~.$class_name."
  done < <(
    find "$ROOT_DIR/test/Raven.CodeAnalysis.Tests/CodeGen" -name '*.cs' -print0 |
      xargs -0 sed -nE 's/^[[:space:]]*(public|internal)[[:space:]]+((sealed|abstract|static|partial)[[:space:]]+)*class[[:space:]]+([A-Za-z_][A-Za-z0-9_]*).*/\4/p' |
      sort -u
  )

  for name in "${names[@]}"; do
    filter+="&FullyQualifiedName!~$name"
  done

  printf '%s' "$filter"
}

case "$suite" in
  overload-resolution)
    filter="(FullyQualifiedName~.MethodOverloadTests.|FullyQualifiedName~.OverloadResolverTests.|FullyQualifiedName~.NamedArgumentTests.|FullyQualifiedName~.OptionalParameterSemanticTests.|FullyQualifiedName~.ParamsParameterSemanticTests.|FullyQualifiedName~.GenericMethodTests.|FullyQualifiedName~.InvocationOperatorTests.|FullyQualifiedName~.MethodReferenceSemanticTests.|FullyQualifiedName~.InvocationDiagnosticsTests.|FullyQualifiedName~.MethodReferenceDiagnosticsTests.|FullyQualifiedName~.TargetTypedExpressionTests.|FullyQualifiedName~.NullableTypeTests.|FullyQualifiedName~.FunctionExpressionInferenceTests.|FullyQualifiedName~.ExtensionMethodSemanticTests.|FullyQualifiedName~.GenericInvocationCodeGenTests.|FullyQualifiedName~.MethodReferenceCodeGenTests.|FullyQualifiedName~.RuntimeSymbolResolverTests.|FullyQualifiedName~.FunctionExpressionCodeGenTests.|FullyQualifiedName~.ConversionOperatorCodeGenTests.|FullyQualifiedName~.ByRefCodeGenTests.)"
    ;;
  functions-async)
    filter="(FullyQualifiedName~.AwaitExpressionBindingTests.|FullyQualifiedName~.AsyncFunctionExpressionTests.|FullyQualifiedName~.AsyncMethodTests.|FullyQualifiedName~.AsyncLowererTests.|FullyQualifiedName~.AsyncEntryPointBridgeTests.|FullyQualifiedName~.AsyncFunctionExpressionStateMachineTests.|FullyQualifiedName~.AsyncPropagateCodeGenTests.|FullyQualifiedName~.AsyncTryAwaitCodeGenTests.|FullyQualifiedName~.AwaitReturnThrowExpressionCodeGenTests.|FullyQualifiedName~.RuntimeAsyncCodeGenTests.)"
    ;;
  patterns)
    filter="(FullyQualifiedName~.PatternSyntaxParserTests.|FullyQualifiedName~.PositionalPatternSyntaxTests.|FullyQualifiedName~.MatchExpressionSyntaxTests.|FullyQualifiedName~.MatchStatementSyntaxTests.|FullyQualifiedName~.IsPatternExpressionTests.|FullyQualifiedName~.IsPatternSemanticTests.|FullyQualifiedName~.MatchExpressionTests.|FullyQualifiedName~.MatchStatementTests.|FullyQualifiedName~.PatternAssignmentSemanticTests.|FullyQualifiedName~.MatchExpressionCodeGenTests.|FullyQualifiedName~.MatchReturnCodeGenTests.|FullyQualifiedName~.MatchStatementCodeGenTests.|FullyQualifiedName~.PatternLocalCodeGenTests.|FullyQualifiedName~.PatternVariableTests.|FullyQualifiedName~.PatternWholeDesignationCodeGenTests.|FullyQualifiedName~.PositionalPatternCodeGenTests.)"
    ;;
  extensions)
    filter="(FullyQualifiedName~.ExtensionDeclarationSemanticTests.|FullyQualifiedName~.ExtensionInferenceInstrumentationTests.|FullyQualifiedName~.ExtensionMemberLookupApiTests.|FullyQualifiedName~.ExtensionMethodSemanticTests.|FullyQualifiedName~.ExtensionPropertySemanticTests.|FullyQualifiedName~.MetadataExtensionMethodSemanticTests.|FullyQualifiedName~.MetadataStaticExtensionMemberSemanticTests.|FullyQualifiedName~.StaticExtensionMemberSemanticTests.)"
    ;;
  partials)
    filter="(FullyQualifiedName~.PartialClassTests.|FullyQualifiedName~.PartialClassDiagnosticsTests.|FullyQualifiedName~.PartialTypeTests.|FullyQualifiedName~.PartialMethodTests.|FullyQualifiedName~.PartialPropertyTests.|FullyQualifiedName~.PartialEventTests.|FullyQualifiedName~.PartialMemberModifierTests.|FullyQualifiedName~.PartialClassCodeGenTests.)"
    ;;
  macros)
    filter="(FullyQualifiedName~.FreestandingMacroParsingTests.|FullyQualifiedName~.FreestandingMacroSemanticTests.|FullyQualifiedName~.MacroAttributeSemanticTests.|FullyQualifiedName~.MacroExpandedDocumentTests.|FullyQualifiedName~.FreestandingMacroCodeGenTests.|FullyQualifiedName~.MacroCodeGenTests.)"
    ;;
  imports-and-namespaces)
    filter="(FullyQualifiedName~.NamespaceDirectiveSyntaxTests.|FullyQualifiedName~.NamespaceDirectiveTests.|FullyQualifiedName~.NamespaceResolutionTest.|FullyQualifiedName~.ImportResolutionTest.|FullyQualifiedName~.AliasResolutionTest.|FullyQualifiedName~.EscapedIdentifierSemanticTests.|FullyQualifiedName~.TypeSyntaxDirectResolutionTests.|FullyQualifiedName~.MultiFileCompilationTests.|FullyQualifiedName~.FileScopedNamespaceCodeGenTests.)"
    ;;
  framework-and-targeting)
    filter="(FullyQualifiedName~.ReferenceAssemblyPathsTests.|FullyQualifiedName~.TargetFrameworkMonikerTests.|FullyQualifiedName~.TargetFrameworkResolverTests.)"
    ;;
  *)
    echo "Unknown suite: $suite" >&2
    echo "Run scripts/test-feature-suite.sh --list to see available suites." >&2
    exit 1
    ;;
esac

if [[ "$mode" == "--runtime" ]]; then
  filter="($filter)&($(build_runtime_heavy_filter))&FullyQualifiedName!~CodeGen.Development"
  dotnet test "$PROJECT" /property:WarningLevel=0 --blame-hang-timeout 300s --blame-hang-dump-type none --filter "$filter"
else
  filter="($filter)&$(build_fast_exclusion_filter)"
  dotnet test "$PROJECT" /property:WarningLevel=0 --blame-hang-timeout 60s --blame-hang-dump-type none --filter "$filter"
fi
