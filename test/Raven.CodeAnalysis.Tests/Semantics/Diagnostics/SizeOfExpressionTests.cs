using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SizeOfExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void SizeOf_WithQualifiedFrameworkType_BindsSuccessfully()
    {
        const string testCode = """
val size = sizeof(System.Int32)
""";

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }

    [Fact]
    public void SizeOf_WithMissingType_ReportsSingleDiagnostic()
    {
        const string testCode = """
val size = sizeof(System.DoesNotExist)
""";

        var verifier = CreateVerifier(testCode, expectedDiagnostics: new[]
        {
            new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                .WithSeverity(DiagnosticSeverity.Error)
                .WithLocation(1, 19)
                .WithArguments("System.DoesNotExist"),
        });

        verifier.Verify();
    }
}
