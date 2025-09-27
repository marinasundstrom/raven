using System.Linq;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class EscapedIdentifierSemanticTests : CompilationTestBase
{
    [Fact]
    public void EscapedClassIdentifier_UsesUnescapedSymbolName()
    {
        var source = """
            class @int {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var classDecl = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var symbol = (INamedTypeSymbol)model.GetDeclaredSymbol(classDecl)!;

        Assert.Equal("int", symbol.Name);
    }

    [Fact]
    public void EscapedMemberAndLocalIdentifiers_UseUnescapedSymbolNames()
    {
        var source = """
            class Container
            {
                static @int(@return: int) -> int
                {
                    let @class = @return;
                    return @class;
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);

        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        Assert.Equal("int", methodSymbol.Name);

        var parameterSyntax = method.ParameterList.Parameters.Single();
        var parameterSymbol = (IParameterSymbol)model.GetDeclaredSymbol(parameterSyntax)!;
        Assert.Equal("return", parameterSymbol.Name);

        var declarator = method.Body!.DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var localSymbol = (ILocalSymbol)model.GetDeclaredSymbol(declarator)!;
        Assert.Equal("class", localSymbol.Name);
    }
}
