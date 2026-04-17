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
    public void MatchExpression_DiscardArmCanBeSpecific_DoesNotReportDiagnostic()
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
            []);

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

    [Fact]
    public void MatchExpression_SealedHierarchyWithOpenIntermediate_SuggestsIntermediateCase()
    {
        const string code = """
import System.*

val expr: Expr = Add(Lit(1), Lit(2))

val result = expr match {
    Lit(val value) => value
    Add(val left, val right) => 0
    Sub(val left, val right) => 0
}

sealed record Expr
record Lit(Value: int) : Expr
abstract record BinaryExpr(Left: Expr, Right: Expr) : Expr
record Add(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
record Sub(Left: Expr, Right: Expr) : BinaryExpr(Left, Right)
""";

        var verifier = CreateAnalyzerVerifier<MatchExhaustivenessAnalyzer>(
            code,
            [
                new DiagnosticResult(MatchExhaustivenessAnalyzer.MissingCaseDiagnosticId)
                    .WithLocation(5, 19)
                    .WithArguments("BinaryExpr")
            ],
            [CompilerDiagnostics.MatchExpressionNotExhaustive.Id]);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_StructUnionMissingInactiveState_SuggestsNull()
    {
        const string code = """
union struct Result<T, E> {
    Ok(value: T)
    Error(message: E)
}

val value: Result<int, string> = Ok(42)

val text = value match {
    Ok(val payload) => payload.ToString()
    Error(val message) => message
}
""";

        var verifier = CreateAnalyzerVerifier<MatchExhaustivenessAnalyzer>(
            code,
            [
                new DiagnosticResult(MatchExhaustivenessAnalyzer.MissingCaseDiagnosticId)
                    .WithLocation(8, 18)
                    .WithArguments("null")
            ],
            [CompilerDiagnostics.MatchExpressionNotExhaustive.Id]);

        verifier.Verify();
    }
}
