using System.Linq;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ObsoleteMemberDiagnosticsTests : CompilationTestBase
{
    [Theory]
    [InlineData("val IsCompleted: bool => Services.Ready(1)")]
    [InlineData("val IsCompleted: bool = Services.Ready(1)")]
    public void PropertyCallingAttributedFunctionInLaterNamespace_PreservesDiagnostic(string property)
    {
        var source = $$"""
namespace Consumer {
    class Operation {
        {{property}}
    }
}
namespace Services {
    import System.*

    [Obsolete("Use NewMethod")]
    func Ready(value: long) -> bool => true
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics().AsEnumerable().ToArray();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
        var diagnostic = Assert.Single(diagnostics.Where(d => d.Id == "RAV0505"));
        Assert.Contains("Use NewMethod", diagnostic.GetMessage());
    }

    [Fact]
    public void CallingObsoleteMethod_ReportsWarningDiagnostic()
    {
        const string source = """
import System.*

class Api
{
    [Obsolete("Use NewMethod")]
    public static func Old() -> int { return 1 }
}

class Consumer
{
    public func Run() -> int { return Api.Old() }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var obsoleteDiagnostics = compilation.GetDiagnostics().AsEnumerable().Where(d => d.Id == "RAV0505").ToArray();

        var diagnostic = Assert.Single(obsoleteDiagnostics);
        Assert.Equal(DiagnosticSeverity.Warning, diagnostic.Severity);
        Assert.Contains("obsolete", diagnostic.GetMessage());
        Assert.Contains("Use NewMethod", diagnostic.GetMessage());
    }

    [Fact]
    public void CallingObsoleteErrorMethod_ReportsErrorDiagnostic()
    {
        const string source = """
import System.*

class Api
{
    [Obsolete("Removed", true)]
    public static func Old() -> int { return 1 }
}

class Consumer
{
    public func Run() -> int { return Api.Old() }
}
""";

        var (compilation, _) = CreateCompilation(source);
        var obsoleteDiagnostics = compilation.GetDiagnostics().AsEnumerable().Where(d => d.Id == "RAV0505").ToArray();

        var diagnostic = Assert.Single(obsoleteDiagnostics);
        Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
        Assert.Contains("obsolete", diagnostic.GetMessage());
        Assert.Contains("Removed", diagnostic.GetMessage());
    }
}
