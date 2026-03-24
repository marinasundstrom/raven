using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class VarCanBeValAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void VarLocal_NotReassigned_ReportsDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var count = 0
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(VarCanBeValAnalyzer.DiagnosticId)
                    .WithLocation(3, 9)
                    .WithArguments("count")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_Reassigned_NoDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var count = 0
        count = 1
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarGlobal_NotReassigned_ReportsDiagnostic()
    {
        const string code = "var count = 0";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(VarCanBeValAnalyzer.DiagnosticId)
                    .WithLocation(1, 1)
                    .WithArguments("count")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarGlobal_Reassigned_NoDiagnostic()
    {
        const string code = """
var count = 0
count = 1
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_ReassignedByTupleDeconstruction_NoDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var a = 1, b = 2
        (b, a) = (a, b)
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_ReassignedBySequenceDeconstruction_NoDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var first = 0, second = 0, third = 0
        [first, second, third] = [1, 2, 3]
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_CapturedByBlockLambda_NoDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var count = 0
        val read = func () -> int {
            return count
        }

        Consume(read())
    }

    private func Consume(value: int) -> unit { }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_PassedAsOutArgument_NoDiagnostic()
    {
        const string code = """
class C {
    public func M(arg: string) -> unit {
        var total = 0

        int.TryParse(arg, out total)
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeValAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
