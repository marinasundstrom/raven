using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public class ConstructedNamedTypeSymbolTests
{
    [Fact]
    public void LookupType_SubstitutesOuterTypeArguments()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var listOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        var enumeratorType = Assert.IsAssignableFrom<INamedTypeSymbol>(listOfInt.LookupType("Enumerator"));

        Assert.Equal("Enumerator", enumeratorType.Name);
        Assert.Equal(1, enumeratorType.Arity);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, enumeratorType.TypeArguments[0]));
    }

    [Fact]
    public void IsMemberDefined_ReturnsSubstitutedMember()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var listDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.Collections.Generic.List`1"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var listOfInt = Assert.IsAssignableFrom<INamedTypeSymbol>(listDefinition.Construct(intType));

        Assert.True(listOfInt.IsMemberDefined("Add", out var symbol));
        var addMethod = Assert.IsAssignableFrom<IMethodSymbol>(symbol);

        Assert.Single(addMethod.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, addMethod.Parameters[0].Type));
    }

    [Fact]
    public void TupleElements_AreSubstitutedFromDefinition()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var tupleDefinition = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.GetTypeByMetadataName("System.ValueTuple`2"));

        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var tuple = Assert.IsAssignableFrom<INamedTypeSymbol>(tupleDefinition.Construct(intType, stringType));

        Assert.True(SymbolEqualityComparer.Default.Equals(tuple, tuple.UnderlyingTupleType));

        var elements = tuple.TupleElements;
        Assert.Equal(2, elements.Length);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, elements[0].Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(stringType, elements[1].Type));
    }
}
