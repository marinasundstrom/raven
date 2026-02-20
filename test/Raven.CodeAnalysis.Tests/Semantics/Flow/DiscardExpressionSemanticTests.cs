using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class DiscardExpressionSemanticTests : DiagnosticTestBase
{
    [Fact]
    public void DiscardExpression_ReportsDiagnostic()
    {
        const string source = "_ + 2";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.DiscardExpressionNotAllowed.Id)
                    .WithSpan(1, 1, 1, 2)
            ]);

        verifier.Verify();
    }
}
