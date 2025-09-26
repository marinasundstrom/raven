using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class SemanticFactsTests
{
    [Fact]
    public void IsDerivedFrom_ReturnsTrueForDirectBaseType()
    {
        var source = "class Base {} class Derived : Base {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var derived = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Derived")!;
        var baseType = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Base")!;

        Assert.True(SemanticFacts.IsDerivedFrom(derived, baseType));
        Assert.False(SemanticFacts.IsDerivedFrom(baseType, derived));
    }

    [Fact]
    public void IsDerivedFrom_ReturnsTrueForTypeParameterConstraint()
    {
        var source = "class Base {} class Container<T> where T : Base {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Container`1")!;
        var typeParameter = container.TypeParameters[0];
        var baseType = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Base")!;

        Assert.True(SemanticFacts.IsDerivedFrom(typeParameter, baseType));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForClassImplementation()
    {
        var source = "interface IMarker {} class Implementation : IMarker {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var implementation = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Implementation")!;
        var marker = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IMarker")!;

        Assert.True(SemanticFacts.ImplementsInterface(implementation, marker));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForTypeParameterConstraint()
    {
        var source = "interface IMarker {} class Container<T> where T : IMarker {}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var container = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Container`1")!;
        var typeParameter = container.TypeParameters[0];
        var marker = (INamedTypeSymbol)compilation.GetTypeByMetadataName("IMarker")!;

        Assert.True(SemanticFacts.ImplementsInterface(typeParameter, marker));
    }

    [Fact]
    public void ImplementsInterface_ReturnsTrueForArrayInterfaces()
    {
        var tree = SyntaxTree.ParseText("class C {}");
        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var arrayType = compilation.CreateArrayTypeSymbol(intType);
        var enumerable = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Collections.IEnumerable")!;

        Assert.True(SemanticFacts.ImplementsInterface(arrayType, enumerable));
    }
}
