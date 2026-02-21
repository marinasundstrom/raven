using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MatchExhaustivenessAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void MatchExpression_MissingSingleCase_ReportsDiagnostic()
    {
        const string code = """
let result = true match {
    true => 1
}
""";

        var verifier = CreateAnalyzerVerifier<MatchExhaustivenessAnalyzer>(
            code,
            [
                new DiagnosticResult(MatchExhaustivenessAnalyzer.MissingCaseDiagnosticId)
                    .WithLocation(1, 19)
                    .WithArguments("false")
            ],
            [CompilerDiagnostics.MatchExpressionNotExhaustive.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_DiscardArmCanBeSpecific_ReportsDiagnostic()
    {
        const string code = """
val value: State = .Idle

val result = value match {
    .Idle => 0
    .Running => 1
    _ => 2
}

enum State {
    Idle,
    Running,
    Done
}
""";

        var verifier = CreateAnalyzerVerifier<MatchExhaustivenessAnalyzer>(
            code,
            [
                new DiagnosticResult(MatchExhaustivenessAnalyzer.DiscardCaseDiagnosticId)
                    .WithLocation(12, 5)
                    .WithArguments("Done")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_DuCaseSuggestion_OmitsGenericTypeArguments()
    {
        const string code = """
val result: Result<int, string> = Ok(42)

val text = result match {
    Ok(val value) => value.ToString()
}

union Result<T, E> {
    Ok(value: T)
    Error(message: E)
}
""";

        var verifier = CreateAnalyzerVerifier<MatchExhaustivenessAnalyzer>(
            code,
            [
                new DiagnosticResult(MatchExhaustivenessAnalyzer.MissingCaseDiagnosticId)
                    .WithLocation(3, 19)
                    .WithArguments("Error")
            ],
            [CompilerDiagnostics.MatchExpressionNotExhaustive.Id]);

        verifier.Verify();
    }
}
