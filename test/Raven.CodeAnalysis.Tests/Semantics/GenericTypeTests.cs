using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class GenericTypeTests : CompilationTestBase
{
    [Fact]
    public void GenericClass_ExposesTypeParametersAndArguments()
    {
        var source = """
            class Box<T>
            {
                public Value: T { get; }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var classSymbol = (INamedTypeSymbol)model.GetDeclaredSymbol(classDeclaration)!;

        Assert.True(classSymbol.IsGenericType);
        Assert.Equal(1, classSymbol.Arity);
        Assert.Equal("T", classSymbol.TypeParameters[0].Name);
        Assert.Same(classSymbol.TypeParameters[0], classSymbol.TypeArguments[0]);

        var propertySyntax = classDeclaration.Members.OfType<PropertyDeclarationSyntax>().Single();
        var propertySymbol = (IPropertySymbol)model.GetDeclaredSymbol(propertySyntax)!;

        Assert.Same(classSymbol.TypeParameters[0], propertySymbol.Type);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericClassDeclaration_Compilation_ExposesTypeParameters()
    {
        var source = """
            class Box<T>
            {
                public Value: T { get; }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);

        compilation.GetSemanticModel(tree);

        var classSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Box"));

        Assert.True(classSymbol.IsGenericType);
        Assert.Equal(1, classSymbol.Arity);
        Assert.Equal("T", classSymbol.TypeParameters[0].Name);
        Assert.Same(classSymbol.TypeParameters[0], classSymbol.TypeArguments[0]);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericClass_WithTypeParameterConstraints_RecordsConstraintKind()
    {
        var source = """
            interface IFoo {}

            class Box<T: class, IFoo>
            {
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);

        compilation.GetSemanticModel(tree);

        var classSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Box"));

        var typeParameter = Assert.Single(classSymbol.TypeParameters);

        Assert.Equal(
            TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.TypeConstraint,
            typeParameter.ConstraintKind);
        Assert.Empty(compilation.GetDiagnostics());
    }
}

