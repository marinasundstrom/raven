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

let callback = Logger.Log
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV2201").WithSpan(6, 16, 6, 25).WithArguments("Logger.Log(string)")]);

        verifier.Verify();
    }

    [Fact]
    public void MethodGroupConversionAmbiguous_ReportsRAV2202()
    {
        const string source = """
import System.*

class Logger {
    static Log(value: int) -> unit {}
    static Log(value: double) -> unit {}
}

let callback: System.Action<int> = Logger.Log
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV2202").WithSpan(6, 32, 6, 41).WithArguments("Logger.Log(int)")]);

        verifier.Verify();
    }

    [Fact]
    public void MethodGroupConversionIncompatible_ReportsRAV2203()
    {
        const string source = """
import System.*

class Logger {
    static Log(value: int) -> unit {}
}

let callback: System.Action<string> = Logger.Log
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV2203").WithSpan(5, 35, 5, 44).WithArguments("Logger.Log(int)", "System.Action<string>")]);

        verifier.Verify();
    }
}
