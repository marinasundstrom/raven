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

    [Fact]
    public void TypeOf_WithMissingType_ShortCircuitsDependentExpressions()
    {
        const string testCode = """
import System.Console.*

let t = typeof(System.DoesNotExist)
WriteLine(t)

let members = t.GetMembers()
for member in members {
    WriteLine(member.Name)
}
""";

        var verifier = CreateVerifier(testCode, expectedDiagnostics: new[]
        {
            new DiagnosticResult("RAV0103")
                .WithSeverity(DiagnosticSeverity.Error)
                .WithLocation(3, 16)
                .WithArguments("System.DoesNotExist"),
        });

        verifier.Verify();
    }

    [Fact]
    public void TypeOf_WithQualifiedFrameworkType_BindsSuccessfully()
    {
        const string testCode = """
import System.*

let t = typeof(System.String)
let members = t.GetMembers(.Public)
""";

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
