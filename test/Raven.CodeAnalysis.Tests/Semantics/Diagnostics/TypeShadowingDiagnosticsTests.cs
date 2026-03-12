using System;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TypeShadowingDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void RedeclaringMetadataTypeInSameNamespace_IsCurrentlyAllowed()
    {
        const string source = """
namespace System {
    class String {}
}
""";

        var verifier = CreateVerifier(
            source,
            Array.Empty<DiagnosticResult>(),
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ShadowingMetadataTypeAcrossNamespaces_ReportsConversionDiagnostic()
    {
        const string source = """
import System.*

class Exception {
    public init() {}
}

val local: Exception = Exception()
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id).WithSpan(7, 24, 7, 35).WithArguments("Exception", "Exception")],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
