using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ClassModifierDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void OpenModifierOnAbstractClass_ReportsWarning()
    {
        const string source = """
open abstract class Base {}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0333")
                    .WithSeverity(DiagnosticSeverity.Warning)
                    .WithSpan(1, 1, 1, 5)
                    .WithArguments("Base")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void OpenModifierOnNestedAbstractClass_ReportsWarning()
    {
        const string source = """
class Outer {
    open abstract class Inner {}
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult("RAV0333")
                    .WithSeverity(DiagnosticSeverity.Warning)
                    .WithSpan(2, 5, 2, 9)
                    .WithArguments("Inner")
            ]);

        verifier.Verify();
    }
}
