using System;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TypeShadowingDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void RedeclaringMetadataTypeInSameNamespace_ReportsTypeAlreadyDefined()
    {
        const string source = """
namespace System {
    class String {}
}
""";

        var verifier = CreateVerifier(
            source,
            [new DiagnosticResult("RAV0600").WithSpan(2, 11, 2, 17).WithArguments("String")],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }

    [Fact]
    public void ShadowingMetadataTypeAcrossNamespaces_AllowsLocalBinding()
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
            Array.Empty<DiagnosticResult>(),
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }
}
