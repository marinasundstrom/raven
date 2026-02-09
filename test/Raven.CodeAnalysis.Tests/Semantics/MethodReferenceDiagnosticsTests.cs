using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MethodReferenceDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void MethodGroupWithoutDelegateContext_ReportsRAV2201()
    {
        const string source = """
import System.*

class Logger {
    static Log(value: string) -> unit {}
    static Log(value: int) -> unit {}
}

val callback = Logger.Log
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV2201").WithSpan(8, 16, 8, 26).WithArguments("Logger.Log(string)")]);

        verifier.Verify();
    }

    [Fact]
    public void MethodGroupConversionAmbiguous_SelectsBestOverload()
    {
        const string source = """
import System.*

class Logger {
    static Log(value: int) -> unit {}
    static Log(value: double) -> unit {}
}

val callback: System.Action<int> = Logger.Log
""";

        var verifier = CreateVerifier(source);

        verifier.Verify();
    }

    [Fact]
    public void MethodGroupConversionIncompatible_AllowsExplicitDelegate()
    {
        const string source = """
import System.*

class Logger {
    static Log(value: int) -> unit {}
}

val callback: System.Action<string> = Logger.Log
""";

        var verifier = CreateVerifier(source);

        verifier.Verify();
    }
}
