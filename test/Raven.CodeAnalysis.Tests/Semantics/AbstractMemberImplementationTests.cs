using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AbstractMemberImplementationTests : DiagnosticTestBase
{
    [Fact]
    public void DerivedClassWithoutAbstractMember_ReportsWarning()
    {
        const string source = """
open abstract class Base {
    public abstract Bar()
}

class Derived : Base {
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.TypeDoesNotImplementAbstractMember.Id)
                    .WithSpan(5, 7, 5, 14)
                    .WithArguments("Derived", "Bar()", "Base")
            ]);

        verifier.Verify();
    }
}
