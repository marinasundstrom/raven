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
    public static Log(value: string) -> unit {}
    public static Log(value: int) -> unit {}
}

val callback = Logger.Log
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV2201").WithSpan(8, 16, 8, 26).WithArguments("Logger.Log(string)")]);

        verifier.Verify();
    }

    [Fact]
    public void MethodGroupConversionAmbiguous_ReportsDiagnostic()
    {
        const string source = """
import System.*

class Logger {
    public static Log(value: int) -> unit {}
    public static Log(value: double) -> unit {}
}

val callback: System.Action<int> = Logger.Log
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV2202").WithSpan(8, 36, 8, 46).WithArguments("Logger.Log(int)")]);

        verifier.Verify();
    }

    [Fact]
    public void MethodGroupConversionIncompatible_ReportsDiagnostic()
    {
        const string source = """
import System.*

class Logger {
    public static Log(value: int) -> unit {}
}

val callback: System.Action<string> = Logger.Log
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV2203").WithSpan(7, 39, 7, 49).WithArguments("Logger.Log(int)", "string -> ()")]);

        verifier.Verify();
    }
}
