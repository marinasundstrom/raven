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
val t = typeof(System.DoesNotExist)
val members = t.GetMembers()
""";

        var verifier = CreateVerifier(testCode, expectedDiagnostics: new[]
        {
            new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                .WithSeverity(DiagnosticSeverity.Error)
                .WithLocation(1, 16)
                .WithArguments("System.DoesNotExist"),
        });

        verifier.Verify();
    }

    [Fact]
    public void TypeOf_WithMissingType_ShortCircuitsDependentExpressions()
    {
        const string testCode = """
import System.Console.*

val t = typeof(System.DoesNotExist)
WriteLine(t)

val members = t.GetMembers()
for member in members {
    WriteLine(member.Name)
}
""";

        var verifier = CreateVerifier(testCode, expectedDiagnostics: new[]
        {
            new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id)
                .WithSeverity(DiagnosticSeverity.Error)
                .WithLocation(3, 16)
                .WithArguments("System.DoesNotExist"),
            new DiagnosticResult(CompilerDiagnostics.CallIsAmbiguous.Id)
                .WithSeverity(DiagnosticSeverity.Error)
                .WithSpan(4, 1, 4, 13)
                .WithArguments("static WriteLine(value: bool) → ()", "static WriteLine(value: char) → ()"),
        });

        verifier.Verify();
    }

    [Fact]
    public void TypeOf_WithQualifiedFrameworkType_BindsSuccessfully()
    {
        const string testCode = """
import System.*

val t = typeof(System.String)
val members = t.GetMembers(.Public)
""";

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void TypeOf_WithOpenGenericFrameworkType_BindsSuccessfully()
    {
        const string testCode = """
import System.*

val t = typeof(System.Collections.Generic.Dictionary<,>)
val name = t.Name
""";

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

}
