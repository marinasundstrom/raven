using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class MissingReturnDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void NonUnitFunction_WithEmptyBody_ReportsMissingReturn()
    {
        var code = """
func Main() -> int {
}
""";

        CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.NotAllCodePathsReturnAValue.Id).WithSpan(1, 6, 1, 10)
            ]).Verify();
    }

    [Fact]
    public void NonUnitFunction_WithReachableEndPoint_ReportsMissingReturn()
    {
        var code = """
func Main() -> int {
    if true {
        return 1
    }
}
""";

        CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.NotAllCodePathsReturnAValue.Id).WithSpan(1, 6, 1, 10)
            ]).Verify();
    }

    [Fact]
    public void UnitFunction_WithEmptyBody_DoesNotReportMissingReturn()
    {
        var code = """
func Main() -> () {
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void NonUnitFunction_WithNonTerminatingLoop_DoesNotReportMissingReturn()
    {
        var code = """
func Main() -> int {
    loop {
    }
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void NonUnitFunction_WithBreakableLoop_ReportsMissingReturn()
    {
        var code = """
func Main() -> int {
    loop {
        break
    }
}
""";

        CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.NotAllCodePathsReturnAValue.Id).WithSpan(1, 6, 1, 10)
            ]).Verify();
    }

    [Fact]
    public void NonUnitFunction_WithReturnInsideUnsafeBlock_DoesNotReportMissingReturn()
    {
        var code = """
unsafe func Main() -> int {
    unsafe {
        return 1
    }
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void NonUnitFunction_WithCompletingFinally_PreservesAbruptTryExit()
    {
        var code = """
func Main() -> int {
    try {
        return 1
    }
    finally {
        let cleanup = 0
    }
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void NonUnitFunction_WithAbruptFinally_DoesNotReportMissingReturn()
    {
        var code = """
func Main() -> int {
    try {
        let value = 0
    }
    finally {
        return 1
    }
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void NonUnitFunction_WithBodyError_StillReportsMissingReturn()
    {
        var code = """
func Main(flag: bool) -> int {
    let value: int = "not an int"
    if flag {
        return 1
    }
}
""";

        var result = CreateVerifier(code).GetResult();
        var diagnostics = result.Compilation.GetDiagnostics();

        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NotAllCodePathsReturnAValue);
        Assert.Contains(
            diagnostics,
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignFromTypeToType);
    }
}
