using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AccessibilityDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void PrivateConstructorInvocation_ReportsRAV0500()
    {
        const string source = """
class Container {
    private init() {}
}

let instance = Container();
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0500").WithSpan(5, 16, 5, 25).WithArguments("constructor", ".ctor")]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateFieldAccess_ReportsRAV0500()
    {
        const string source = """
class Container {
    private var secret: int;

    public init() {
        self.secret = 42;
    }
}

let instance = Container();
let value = instance.secret;
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0500").WithSpan(10, 22, 10, 28).WithArguments("field", "secret")]);

        verifier.Verify();
    }
}
