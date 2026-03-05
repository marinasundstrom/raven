using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class MemberNameDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void MethodNameMatchingContainingType_ReportsDiagnostic()
    {
        const string source = """
class Foo {
    func Foo() -> int => 0
}
""";

        CreateVerifier(
            source,
            [new DiagnosticResult("RAV0113").WithAnySpan().WithArguments("Foo", "Foo")])
            .Verify();
    }

    [Fact]
    public void PropertyNameMatchingContainingType_ReportsDiagnostic()
    {
        const string source = """
class Foo {
    val Foo: int => 1
}
""";

        CreateVerifier(
            source,
            [new DiagnosticResult("RAV0113").WithAnySpan().WithArguments("Foo", "Foo")])
            .Verify();
    }

    [Fact]
    public void NestedTypeUsesImmediateContainingTypeForRule()
    {
        const string source = """
class Outer {
    class Inner {
        func Outer() -> int => 0
        func Inner() -> int => 1
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0113").WithAnySpan().WithArguments("Inner", "Inner")]);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructorPromotedPropertyNameMatchingContainingType_ReportsDiagnostic()
    {
        const string source = """
class Foo(val Foo: int)
""";

        CreateVerifier(
            source,
            [new DiagnosticResult("RAV0113").WithAnySpan().WithArguments("Foo", "Foo")])
            .Verify();
    }
}
