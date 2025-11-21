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

    [Fact]
    public void PublicMethodReturningInternalType_ReportsRAV0501()
    {
        const string source = """
public class Container {
    public static ParseNumber(str: string) -> Result<int> {
        return .Ok(0);
    }
}

union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithSpan(2, 47, 2, 58).WithArguments("return", "Result<int>", "method", "Container.ParseNumber")]);

        verifier.Verify();
    }

    [Fact]
    public void PublicMethodParameterWithInternalType_ReportsRAV0501()
    {
        const string source = """
internal class Hidden {}

public class Exposer {
    public Call(value: Hidden) -> int {
        return 0;
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0501").WithSpan(4, 24, 4, 30).WithArguments("parameter 'value'", "Hidden", "method", "Exposer.Call")]);

        verifier.Verify();
    }
}
