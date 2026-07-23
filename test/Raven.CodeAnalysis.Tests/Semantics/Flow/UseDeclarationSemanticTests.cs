using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class UseDeclarationSemanticTests : DiagnosticTestBase
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
    public void UseDeclaration_WithDisposableInitializer_GetSymbolInfoForLaterReferenceReturnsLocal()
    {
        const string source = """
import System.*

class App : IDisposable {
    public init() {}
    public func Dispose() -> unit {}
    public func MapGet(route: string) -> unit {}
}

class Builder {
    public func Build() -> App {
        App()
    }
}

func configure(builder: Builder) {
    use app = builder.Build()
    app.MapGet("/")
}
""";

        var verifier = CreateVerifier(source);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var syntaxTree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(syntaxTree);
        var root = syntaxTree.GetRoot();
        var declarator = root
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(candidate => candidate.Identifier.ValueText == "app");
        var laterReference = root
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "app");

        var declaredSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));
        var referencedSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetSymbolInfo(laterReference).Symbol);

        Assert.True(SymbolEqualityComparer.Default.Equals(declaredSymbol, referencedSymbol));
        Assert.Equal("App", declaredSymbol.Type.Name);
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
