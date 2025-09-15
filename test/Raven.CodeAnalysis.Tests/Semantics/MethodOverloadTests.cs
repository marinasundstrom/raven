using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MethodOverloadTests : CompilationTestBase
{
    [Fact]
    public void Overloads_DifferOnlyByNullableReferenceType_AreRejected()
    {
        var source = """
        class C {
            f(x: string) -> int { 0 }
            f(x: string?) -> int { 1 }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var methods = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        _ = model.GetDeclaredSymbol(methods[0]);
        _ = model.GetDeclaredSymbol(methods[1]);

        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.TypeAlreadyDefinesMember, diagnostic.Descriptor);
    }

    [Fact]
    public void Overloads_WithNullableValueType_AreAllowed()
    {
        var source = """
        class C {
            f(x: int) -> int { 0 }
            f(x: int?) -> int { 1 }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var methods = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        _ = model.GetDeclaredSymbol(methods[0]);
        _ = model.GetDeclaredSymbol(methods[1]);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void UnionArgument_UsesCommonDenominatorForOverloadResolution()
    {
        var source = """
        open class Base {}
        class D1 : Base {}
        class D2 : Base {}
        class C {
            static m(x: Base) -> int { 0 }
            static m(x: object) -> int { 1 }
            test(u: D1 | D2) -> int {
                return m(u);
            }
        }
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var symbol = (IMethodSymbol)model.GetSymbolInfo(invocation).Symbol!;

        Assert.Equal("Base", symbol.Parameters[0].Type.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }
}
