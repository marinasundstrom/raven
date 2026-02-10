using System.Linq;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ObsoleteMemberDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void CallingObsoleteMethod_ReportsWarningDiagnostic()
    {
        const string source = """
import System.*

class Api
{
    [Obsolete("Use NewMethod")]
    public static Old() -> int { return 1 }
}

class Consumer
{
    public Run() -> int { return Api.Old() }
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
    public static Old() -> int { return 1 }
}

class Consumer
{
    public Run() -> int { return Api.Old() }
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
