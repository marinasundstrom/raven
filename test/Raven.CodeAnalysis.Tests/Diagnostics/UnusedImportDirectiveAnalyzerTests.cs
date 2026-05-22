using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public sealed class UnusedImportDirectiveAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ReportsCompilationUnitNamespaceImport_WhenNoMemberFromImportedNamespaceIsUsed()
    {
        const string code = """
import System.Text.*
import System.*

val text: String = "ok"
""";

        var verifier = CreateAnalyzerVerifier<UnusedImportDirectiveAnalyzer>(
            code,
            [
                new DiagnosticResult(UnusedImportDirectiveAnalyzer.DiagnosticId)
                    .WithLocation(1, 8)
                    .WithArguments("System.Text")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void DoesNotReportCompilationUnitNamespaceImport_WhenTypeFromImportedNamespaceIsUsed()
    {
        const string code = """
import System.*

val text: String = "ok"
""";

        var verifier = CreateAnalyzerVerifier<UnusedImportDirectiveAnalyzer>(code);

        verifier.Verify();
    }

    [Fact]
    public void DoesNotReportCompilationUnitNamespaceImport_WhenNestedNamespaceIsUsedByQualifiedName()
    {
        const string code = """
import System.*

val options: Text.Json.JsonSerializerOptions = Text.Json.JsonSerializerOptions()
""";

        var verifier = CreateAnalyzerVerifier<UnusedImportDirectiveAnalyzer>(code);

        verifier.Verify();
    }

    [Fact]
    public void DoesNotReportNamespaceImport_WhenImportedTypeIsUsedUnqualified()
    {
        const string code = """
import System.Text.Json.*

val options = JsonSerializerOptions()
val json = JsonSerializer.Serialize(options)
""";

        var verifier = CreateAnalyzerVerifier<UnusedImportDirectiveAnalyzer>(code);

        verifier.Verify();
    }

    [Fact]
    public void ReportsNamespaceScopedImport_WhenNoMemberFromImportedNamespaceIsUsedInNamespace()
    {
        const string code = """
namespace Sample {
    import System.Text.*
    import System.*

    func Main() {
        val text: String = "ok"
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedImportDirectiveAnalyzer>(
            code,
            [
                new DiagnosticResult(UnusedImportDirectiveAnalyzer.DiagnosticId)
                    .WithLocation(2, 12)
                    .WithArguments("System.Text")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void DoesNotReportOuterNamespaceImport_WhenUsedInsideNestedNamespace()
    {
        const string code = """
namespace Outer {
    import System.*

    namespace Inner {
        func Main() {
            val text: String = "ok"
        }
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedImportDirectiveAnalyzer>(code);

        verifier.Verify();
    }

    [Fact]
    public void ReportsImport_WhenOnlyFullyQualifiedNameUsesThatNamespace()
    {
        const string code = """
import System.*

val text: System.String = "ok"
""";

        var verifier = CreateAnalyzerVerifier<UnusedImportDirectiveAnalyzer>(
            code,
            [
                new DiagnosticResult(UnusedImportDirectiveAnalyzer.DiagnosticId)
                    .WithLocation(1, 8)
                    .WithArguments("System")
            ]);

        verifier.Verify();
    }
}
