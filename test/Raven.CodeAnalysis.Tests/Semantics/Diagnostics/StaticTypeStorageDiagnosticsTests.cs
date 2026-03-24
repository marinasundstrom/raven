using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StaticTypeStorageDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void LocalDeclaration_WithStaticType_ReportsDiagnostic()
    {
        var code = """
import System.IO.*

func Main() {
    val file: File = null
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.StaticTypeCannotBeUsedAsStorageType.Id).WithAnySpan().WithArguments("File")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void PrimaryConstructorPromotedParameter_WithStaticType_ReportsDiagnostic()
    {
        var code = """
import System.IO.*

class SessionsStorage(
    private val file: File
) {
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(CompilerDiagnostics.StaticTypeCannotBeUsedAsStorageType.Id).WithAnySpan().WithArguments("File")
            ]);

        verifier.Verify();
    }
}
