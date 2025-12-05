using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TypeOfExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void TypeOf_WithMissingType_ReportsSingleDiagnostic()
    {
        const string testCode = """
let t = typeof(System.DoesNotExist)
let members = t.GetMembers()
""";

        var verifier = CreateVerifier(testCode, expectedDiagnostics: new[]
        {
            new DiagnosticResult("RAV0103")
                .WithSeverity(DiagnosticSeverity.Error)
                .WithLocation(1, 16)
                .WithArguments("System.DoesNotExist"),
        });

        verifier.Verify();
    }
}
