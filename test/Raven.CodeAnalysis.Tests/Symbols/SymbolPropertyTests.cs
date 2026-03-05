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
    field stored: int = 1

    func M(value: int) -> int {
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
            .Single(declarator => declarator.Parent?.Parent is FieldDeclarationSyntax or ConstDeclarationSyntax);
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

    [Fact]
    public void ConstructorAndPrimaryConstructorParameterSymbols_UseIdentifierLocation()
    {
        const string source = """
class Foo(name: string) {
    init(value: int) {
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();

        var typeDecl = root.DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var primaryParameter = typeDecl.ParameterList!.Parameters.Single();
        var primarySymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(primaryParameter));

        var ctorDecl = root.DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var ctorParameter = ctorDecl.ParameterList.Parameters.Single();
        var ctorSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(ctorParameter));

        Assert.Equal(primaryParameter.Identifier.Span, primarySymbol.Locations[0].SourceSpan);
        Assert.Equal(ctorParameter.Identifier.Span, ctorSymbol.Locations[0].SourceSpan);
    }
}
