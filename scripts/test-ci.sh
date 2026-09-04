#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPILER_TESTS="$REPO_ROOT/test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj"
CORE_TESTS="$REPO_ROOT/test/Raven.Core.Tests/Raven.Core.Tests.csproj"
LANGUAGE_SERVER_TESTS="$REPO_ROOT/test/Raven.LanguageServer.Tests/Raven.LanguageServer.Tests.csproj"

# Main CI is a bounded integration gate. It proves that generated compiler
# sources, the compiler toolchain, core contracts, incremental diagnostics, and
# language-server presentation still work together. Exhaustive baseline,
# runtime/emit, process, project-system, sample, LSP integration, and perf
# coverage stays opt-in through the dedicated scripts documented in the test
# ledger.
"$REPO_ROOT/scripts/codex-build.sh"

compiler_contract_filter='(
FullyQualifiedName~.SyntaxTreeContractTests.|
FullyQualifiedName~.ParserNewlineTests.|
FullyQualifiedName~.PatternSyntaxParserTests.|
FullyQualifiedName~.PropagationExpressionTests.|
FullyQualifiedName~.SemanticModelCachingTests.|
FullyQualifiedName~.SemanticModelDiagnosticCachingTests.|
FullyQualifiedName~.IncrementalBinderLifecycleTests.|
FullyQualifiedName~.MethodOverloadTests.|
FullyQualifiedName~.ImportBindingSemanticTests.|
FullyQualifiedName~.CompilerDiagnosticsLookupTests.|
FullyQualifiedName~.AnalyzerDiagnosticIdUniquenessTests.|
FullyQualifiedName~.PdbSequencePointTests.AbstractMethod_DoesNotShiftLaterSequencePointsToEarlierMethods|
FullyQualifiedName~.CompletionExistingBehaviorTests.
)'
compiler_contract_filter="${compiler_contract_filter//$'\n'/}"

dotnet test "$COMPILER_TESTS" \
  -m:1 \
  /property:WarningLevel=0 \
  --blame-hang-timeout 120s \
  --blame-hang-dump-type none \
  --filter "$compiler_contract_filter"

dotnet test "$CORE_TESTS" \
  -m:1 \
  /property:WarningLevel=0 \
  --blame-hang-timeout 120s \
  --blame-hang-dump-type none

dotnet test "$LANGUAGE_SERVER_TESTS" \
  -m:1 \
  /property:WarningLevel=0 \
  --blame-hang-timeout 120s \
  --blame-hang-dump-type none
