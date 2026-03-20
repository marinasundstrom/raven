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
    public func Dispose() -> unit {}
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
    public func Dispose() -> unit {}
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

    [Fact]
    public void UseDeclaration_FixedInitializer_DoesNotRequireDisposableShape()
    {
        const string source = """
class Test {
    unsafe static func Run() {
        var value = 0
        use pointer: *int = fixed &value
        *pointer = 1
    }
}
""";

        var verifier = CreateVerifier(source);
        verifier.Verify();
    }

    [Fact]
    public void FixedExpression_OutsideUseInitializer_ReportsDiagnostic()
    {
        const string source = """
class Test {
    unsafe static func Run() {
        var value = 0
        val pointer: *int = fixed &value
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.FixedExpressionRequiresUseInitializer.Id)
                    .WithSpan(4, 29, 4, 41)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void FixedExpression_WithoutExplicitAddressOf_ReportsDiagnostic()
    {
        const string source = """
class Test {
    unsafe static func Run() {
        var value = 0
        use pointer: *int = fixed value
    }
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.FixedExpressionRequiresAddressOfOperand.Id)
                    .WithSpan(4, 35, 4, 40)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void UseDeclaration_WithInBlock_ScopesResourceToNestedBlock()
    {
        const string source = """
import System.*

class Foo : IDisposable {
    public var Value: int = 0
    public init() {}
    public func Run() -> int { self.Value }
    public func Dispose() -> unit {}
}

func test() -> int {
    use obj = Foo { Value = 2 } in {
        return obj.Run()
    }
}
""";

        var verifier = CreateVerifier(source);
        verifier.Verify();
    }
}
