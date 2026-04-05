#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROJECT="$ROOT_DIR/test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj"

if [[ $# -eq 0 || "${1:-}" == "--list" ]]; then
  cat <<'EOF'
Usage: scripts/test-feature-suite.sh <suite>

Available suites:
  overload-resolution
  functions-async
  patterns
  extensions
  partials
  macros
  imports-and-namespaces
  framework-and-targeting
EOF
  exit 0
fi

suite="$1"
filter=""

case "$suite" in
  overload-resolution)
    filter="(FullyQualifiedName~.MethodOverloadTests.|FullyQualifiedName~.OverloadResolverTests.|FullyQualifiedName~.NamedArgumentTests.|FullyQualifiedName~.OptionalParameterSemanticTests.|FullyQualifiedName~.ParamsParameterSemanticTests.|FullyQualifiedName~.GenericMethodTests.|FullyQualifiedName~.InvocationOperatorTests.|FullyQualifiedName~.MethodReferenceSemanticTests.|FullyQualifiedName~.InvocationDiagnosticsTests.|FullyQualifiedName~.MethodReferenceDiagnosticsTests.|FullyQualifiedName~.TargetTypedExpressionTests.|FullyQualifiedName~.NullableTypeTests.|FullyQualifiedName~.FunctionExpressionInferenceTests.|FullyQualifiedName~.ExtensionMethodSemanticTests.|FullyQualifiedName~.GenericInvocationCodeGenTests.|FullyQualifiedName~.MethodReferenceCodeGenTests.|FullyQualifiedName~.RuntimeSymbolResolverTests.|FullyQualifiedName~.FunctionExpressionCodeGenTests.|FullyQualifiedName~.ConversionOperatorCodeGenTests.|FullyQualifiedName~.ByRefCodeGenTests.)"
    ;;
  functions-async)
    filter="(FullyQualifiedName~.AwaitExpressionBindingTests.|FullyQualifiedName~.AsyncFunctionExpressionTests.|FullyQualifiedName~.AsyncMethodTests.|FullyQualifiedName~.AsyncLowererTests.|FullyQualifiedName~.AsyncEntryPointBridgeTests.|FullyQualifiedName~.AsyncFunctionExpressionStateMachineTests.|FullyQualifiedName~.AsyncPropagateCodeGenTests.|FullyQualifiedName~.AsyncTryAwaitCodeGenTests.|FullyQualifiedName~.AwaitReturnThrowExpressionCodeGenTests.|FullyQualifiedName~.RuntimeAsyncCodeGenTests.)"
    ;;
  patterns)
    filter="(FullyQualifiedName~.PatternSyntaxParserTests.|FullyQualifiedName~.PositionalPatternSyntaxTests.|FullyQualifiedName~.MatchExpressionSyntaxTests.|FullyQualifiedName~.MatchStatementSyntaxTests.|FullyQualifiedName~.IsPatternExpressionTests.|FullyQualifiedName~.IsPatternSemanticTests.|FullyQualifiedName~.MatchExpressionTests.|FullyQualifiedName~.MatchStatementTests.|FullyQualifiedName~.PatternAssignmentSemanticTests.|FullyQualifiedName~.MatchExhaustivenessAnalyzerTests.|FullyQualifiedName~.MatchExpressionCodeGenTests.|FullyQualifiedName~.MatchReturnCodeGenTests.|FullyQualifiedName~.MatchStatementCodeGenTests.|FullyQualifiedName~.PatternLocalCodeGenTests.|FullyQualifiedName~.PatternVariableTests.|FullyQualifiedName~.PatternWholeDesignationCodeGenTests.|FullyQualifiedName~.PositionalPatternCodeGenTests.)"
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

dotnet test "$PROJECT" /property:WarningLevel=0 --filter "$filter"
