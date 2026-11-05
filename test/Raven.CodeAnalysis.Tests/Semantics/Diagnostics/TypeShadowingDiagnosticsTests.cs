using System;

using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

[Trait("Target", "DotNet")]
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
    public void LocalTypeShadowsImportedMetadataTypeInConstructorExpression()
    {
        const string source = """
import System.*

class Exception {
    public init() {}
}

let local: Exception = Exception()
""";

        var verifier = CreateVerifier(
            source,
            Array.Empty<DiagnosticResult>(),
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
