using System.Linq;

using Raven.CodeAnalysis.Testing;

namespace Raven.Core.Tests;

public sealed class CarrierPropagationDiagnosticsTests : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void OptionPropagation_IntoResultReturn_ReportsErrors()
    {
        const string code = """
import System.*

func test() -> Result<int, string> {
    val r = test2()?
    return .Ok(r)
}

func test2() -> Option<int> {
    return .Some(1)
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0023")
                    .WithAnySpan()
                    .WithArguments("?", "Option<int>"),
                new DiagnosticResult("RAV1503")
                    .WithAnySpan()
                    .WithArguments("Option<int>", "Result<int, string>")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ResultPropagation_ErrorTypeMismatch_ReportsError()
    {
        const string code = """
import System.*

func test() -> Result<int, int> {
    val r = test2()?
    return .Ok(r)
}

func test2() -> Result<int, string> {
    return .Error("bad")
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1503")
                    .WithAnySpan()
                    .WithArguments("string", "int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void OptionCarrierConditionalAccess_IntoResultReturn_ReportsError()
    {
        const string code = """
import System.*

class Box {
    public val Value: int = 42
}

func map(opt: Option<Box>) -> Result<int, string> {
    return opt?.Value
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1503")
                    .WithAnySpan()
                    .WithArguments("Option<int>", "Result<int, string>")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ResultCarrierConditionalAccess_ErrorTypeMismatch_ReportsError()
    {
        const string code = """
import System.*

class Box {
    public val Value: int = 42
}

func map(res: Result<Box, string>) -> Result<int, int> {
    return res?.Value
}
""";

        var result = CreateVerifier(code).GetResult();
        var diagnostics = result.Compilation.GetDiagnostics().AsEnumerable().ToArray();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV1503");
    }

    [Fact]
    public void ResultPropagation_ErrorTypeConvertible_DoesNotReportDiagnostic()
    {
        const string code = """
import System.*

func test() -> Result<int, object> {
    val r = test2()?
    return .Ok(r)
}

func test2() -> Result<int, string> {
    return .Error("bad")
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void AsyncResultPropagation_ErrorTypeMismatch_ReportsError()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

async func test() -> Task<Result<int, int>> {
    await Task.FromResult(0)
    val r = test2()?
    return .Ok(r)
}

func test2() -> Result<int, string> {
    return .Error("bad")
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1503")
                    .WithAnySpan()
                    .WithArguments("string", "int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void AsyncOptionPropagation_IntoTaskOption_DoesNotReportDiagnostic()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

async func test() -> Task<Option<int>> {
    await Task.FromResult(0)
    val r = test2()?
    return .Some(r)
}

func test2() -> Option<int> {
    return .Some(1)
}
""";

        CreateVerifier(code).Verify();
    }
}
