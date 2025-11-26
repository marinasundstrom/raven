using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AssignmentExpressionSemanticTests : DiagnosticTestBase
{
    [Fact]
    public void AssignmentExpression_UsedAsValue_ReportsDiagnostic()
    {
        const string source = """
var x = 0
var y = (x = 1)
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.AssignmentExpressionMustBeStatement.Id)
                    .WithSpan(2, 11, 2, 15)
            ]);

        verifier.Verify();
    }
}
