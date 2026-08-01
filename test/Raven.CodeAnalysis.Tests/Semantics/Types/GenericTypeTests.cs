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
                val Value: T {
                    get
                }
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
        Assert.Equal(TypeParameterOwnerKind.Type, classSymbol.TypeParameters[0].OwnerKind);
        Assert.Same(classSymbol, classSymbol.TypeParameters[0].DeclaringTypeParameterOwner);
        Assert.Null(classSymbol.TypeParameters[0].DeclaringMethodParameterOwner);
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
                val Value: T {
                    get
                }
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
        Assert.Equal(TypeParameterOwnerKind.Type, classSymbol.TypeParameters[0].OwnerKind);
        Assert.Same(classSymbol, classSymbol.TypeParameters[0].DeclaringTypeParameterOwner);
        Assert.Null(classSymbol.TypeParameters[0].DeclaringMethodParameterOwner);
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

    [Fact]
    public void InterfaceTypeParameters_ReportDeclaredVariance()
    {
        var source = """
            interface Mapper<in TSource, out TResult> {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);

        compilation.GetSemanticModel(tree);

        var mapper = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Mapper"));

        Assert.Equal(VarianceKind.In, mapper.TypeParameters[0].Variance);
        Assert.Equal(VarianceKind.Out, mapper.TypeParameters[1].Variance);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NestedGenericType_CanReferenceOuterTypeParameters()
    {
        var source = """
            class Outer<A>
            {
                class Inner<B>
                {
                    val value: A
                    val b: B
                }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);

        compilation.GetSemanticModel(tree);

        var outer = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Outer"));
        var inner = Assert.IsAssignableFrom<INamedTypeSymbol>(
            Assert.Single(outer.GetMembers("Inner")));

        var valueField = Assert.IsAssignableFrom<IPropertySymbol>(
            Assert.Single(inner.GetMembers("value")));
        var bField = Assert.IsAssignableFrom<IPropertySymbol>(
            Assert.Single(inner.GetMembers("b")));

        Assert.Same(outer.TypeParameters[0], valueField.Type);
        Assert.Same(inner.TypeParameters[0], bField.Type);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void ConstructedNestedGenericType_PreservesOuterAndInnerOwnership()
    {
        var source = """
            class Outer<A>
            {
                class Inner<B>
                {
                    val outer: A
                    val inner: B
                }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var outerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            model.GetDeclaredSymbol(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().First()));
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var outer = Assert.IsAssignableFrom<INamedTypeSymbol>(outerDefinition.Construct(stringType));
        var innerDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(outer.LookupType("Inner"));
        var innerTypeParameter = Assert.Single(innerDefinition.TypeParameters);
        var inner = Assert.IsAssignableFrom<INamedTypeSymbol>(innerDefinition.Construct(intType));
        var outerProperty = Assert.Single(inner.GetMembers("outer").OfType<IPropertySymbol>());
        var innerProperty = Assert.Single(inner.GetMembers("inner").OfType<IPropertySymbol>());

        Assert.True(SymbolEqualityComparer.Default.Equals(outer, innerDefinition.ContainingType));
        Assert.True(SymbolEqualityComparer.Default.Equals(
            innerDefinition.OriginalDefinition,
            innerTypeParameter.DeclaringTypeParameterOwner));
        Assert.True(SymbolEqualityComparer.Default.Equals(outer, inner.ContainingType));
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, outerProperty.Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, innerProperty.Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(inner, outerProperty.ContainingType));
        Assert.True(SymbolEqualityComparer.Default.Equals(inner, innerProperty.ContainingType));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void LookupType_PrefersNonGenericTypeWhenSameNameExistsWithDifferentArities()
    {
        var source = """
            class Test {}
            class Test<T> {}
            class Test<T, U> {}
            """;

        var (compilation, _) = CreateCompilation(source);
        compilation.GetSemanticModel(compilation.SyntaxTrees.Single());

        var resolved = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("Test"));

        Assert.Equal(0, resolved.Arity);
        Assert.Equal("Test", resolved.MetadataName);
    }

    [Fact]
    public void LookupType_ReturnsGenericTypeWhenItIsTheOnlyMatchingArity()
    {
        var source = """
            class List<T> {
                init() {}
            }
            """;

        var (compilation, _) = CreateCompilation(source);
        compilation.GetSemanticModel(compilation.SyntaxTrees.Single());

        var resolved = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.SourceGlobalNamespace.LookupType("List"));

        Assert.Equal(1, resolved.Arity);
        Assert.Equal("List`1", resolved.MetadataName);
    }

    [Fact]
    public void LookupType_ReturnsNullWhenOnlyMultipleGenericAritiesExist()
    {
        var source = """
            class Test<T> {}
            class Test<T, U> {}
            """;

        var (compilation, _) = CreateCompilation(source);
        compilation.GetSemanticModel(compilation.SyntaxTrees.Single());

        Assert.Null(compilation.SourceGlobalNamespace.LookupType("Test"));
    }
}
