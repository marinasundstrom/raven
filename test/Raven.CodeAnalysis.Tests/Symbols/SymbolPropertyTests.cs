using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class SymbolPropertyTests : CompilationTestBase
{
    [Fact]
    public void CanBeReferencedByName_IsTrueForNamedSymbols()
    {
        const string source = """
class Widget {
    val field = 1

    M(value: int) -> int {
        let local = value
        return local
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var fieldDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Parent?.Parent is FieldDeclarationSyntax);
        var localDeclarator = root.DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Parent?.Parent is LocalDeclarationStatementSyntax);
        var methodSyntax = root.DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var parameterSyntax = methodSyntax.ParameterList.Parameters.Single();

        var fieldSymbol = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(fieldDeclarator));
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodSyntax));
        var parameterSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameterSyntax));
        var localSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(localDeclarator));

        Assert.True(fieldSymbol.CanBeReferencedByName);
        Assert.True(methodSymbol.CanBeReferencedByName);
        Assert.True(parameterSymbol.CanBeReferencedByName);
        Assert.True(localSymbol.CanBeReferencedByName);

        Assert.False(fieldSymbol.IsImplicitlyDeclared);
        Assert.False(methodSymbol.IsImplicitlyDeclared);
        Assert.False(parameterSymbol.IsImplicitlyDeclared);
        Assert.False(localSymbol.IsImplicitlyDeclared);
    }
}
