using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

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
class Test2 {
    public init() {}
}

use test = Test2()
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(5, 5, 5, 9)
                    .WithArguments("Test2", "IDisposable")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void UseDeclaration_InferredType_NullableDisposableInitializer_ReportsDiagnosticOnLocalName()
    {
        const string source = """
import System.*

func Test2() -> IDisposable? {
    return null
}

func test() {
    use test = Test2()
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(8, 9, 8, 13)
                    .WithArguments("IDisposable?", "IDisposable")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void UseDeclaration_InferredType_NonDisposableInitializer_ReportsDiagnosticAndKeepsLocalUsable()
    {
        const string source = """
class Foo {
    public init() {}
}

func test() {
    use foo = Foo()
    foo
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(6, 9, 6, 12)
                    .WithArguments("Foo", "IDisposable")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void UseDeclaration_InferredType_NonDisposableInitializer_GetSymbolInfoForLaterReferenceReturnsLocal()
    {
        const string source = """
class Foo {
    public init() {}
}

func test() {
    use foo = Foo()
    foo
}
""";

        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "invalid-use-declaration",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(syntaxTree);
        var reference = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "foo");
        var declarator = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(candidate => candidate.Identifier.ValueText == "foo");

        var symbolInfo = model.GetSymbolInfo(reference);
        var declaredSymbol = model.GetDeclaredSymbol(declarator);

        Assert.NotNull(symbolInfo.Symbol);
        Assert.Equal("foo", symbolInfo.Symbol.Name);
        Assert.NotNull(declaredSymbol);
        Assert.Equal("foo", declaredSymbol.Name);
        Assert.True(model.TryGetAvailableLocalDeclarationSymbol(
            declarator,
            out var availableLocal,
            allowBindingFallback: false));
        Assert.NotNull(availableLocal);
        Assert.Equal("foo", availableLocal.Name);
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
    public void UseDeclaration_AnnotatedNullableDisposableType_ReportsDiagnostic()
    {
        const string source = """
import System.*

class Foo : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
}

func test() {
    use foo: IDisposable? = Foo()
    foo
}
""";

        var verifier = CreateVerifier(
            source,
            [
                new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id)
                    .WithSpan(9, 9, 9, 12)
                    .WithArguments("IDisposable?", "IDisposable")
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
    use obj = Foo with { Value = 2 } in {
        return obj.Run()
    }
}
""";

        var verifier = CreateVerifier(source);
        verifier.Verify();
    }
}
