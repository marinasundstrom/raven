using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class UseDeclarationTests : DiagnosticTestBase
{
    [Fact]
    public void UseDeclaration_InferredType_DisposableInitializer_NoDiagnostics()
    {
        const string source = """
import System.*

class Foo : IDisposable {
    public init() {}
    public Dispose() -> unit {}
}

use foo = Foo()
""";

        var verifier = CreateVerifier(source);
        verifier.Verify();
    }

    [Fact]
    public void UseDeclaration_InferredType_NonDisposableInitializer_ReportsDiagnostic()
    {
        const string source = """
class Foo {
    public init() {}
}

use foo = Foo()
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(5, 11, 5, 16)
                    .WithArguments("Foo", "IDisposable")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void UseDeclaration_AnnotatedType_NonDisposableLocalType_ReportsDiagnostic()
    {
        const string source = """
import System.*

class Foo : IDisposable {
    public init() {}
    public Dispose() -> unit {}
}

use foo: object = Foo()
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(8, 5, 8, 8)
                    .WithArguments("object", "IDisposable")
            ]);

        verifier.Verify();
    }
}
