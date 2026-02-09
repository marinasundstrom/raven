using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class UsingDeclarationTests : DiagnosticTestBase
{
    [Fact]
    public void UsingDeclaration_InferredType_DisposableInitializer_NoDiagnostics()
    {
        const string source = """
import System.*

class Foo : IDisposable {
    public init() {}
    public Dispose() -> unit {}
}

using val foo = Foo()
""";

        var verifier = CreateVerifier(source);
        verifier.Verify();
    }

    [Fact]
    public void UsingDeclaration_InferredType_NonDisposableInitializer_ReportsDiagnostic()
    {
        const string source = """
class Foo {
    public init() {}
}

using val foo = Foo()
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(5, 17, 5, 22)
                    .WithArguments("Foo", "IDisposable")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void UsingDeclaration_AnnotatedType_NonDisposableLocalType_ReportsDiagnostic()
    {
        const string source = """
import System.*

class Foo : IDisposable {
    public init() {}
    public Dispose() -> unit {}
}

using val foo: object = Foo()
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(8, 11, 8, 14)
                    .WithArguments("object", "IDisposable")
            ]);

        verifier.Verify();
    }
}
