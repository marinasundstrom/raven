using System;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics;

[Trait("Target", "Shared")]
public sealed class SymbolContractNamingTests
{
    [Theory]
    [InlineData("Symbol")]
    [InlineData("NamespaceOrTypeSymbol")]
    [InlineData("NamespaceSymbol")]
    [InlineData("LambdaSymbol")]
    [InlineData("LabelSymbol")]
    [InlineData("AliasSymbol")]
    [InlineData("MethodSymbol")]
    [InlineData("MacroSymbol")]
    [InlineData("MacroDeclarationSymbol")]
    [InlineData("ParameterSymbol")]
    [InlineData("FieldSymbol")]
    [InlineData("PropertySymbol")]
    [InlineData("EventSymbol")]
    [InlineData("TypeSymbol")]
    [InlineData("IntersectionTypeSymbol")]
    [InlineData("StructuralUnionTypeSymbol")]
    [InlineData("FunctionReturnTypeSymbol")]
    [InlineData("NamedTypeSymbol")]
    [InlineData("ArrayTypeSymbol")]
    [InlineData("PointerTypeSymbol")]
    [InlineData("AddressTypeSymbol")]
    [InlineData("TupleTypeSymbol")]
    [InlineData("UnionSymbol")]
    [InlineData("UnionCaseTypeSymbol")]
    [InlineData("TypeParameterSymbol")]
    [InlineData("LocalSymbol")]
    [InlineData("ErrorSymbol")]
    [InlineData("ErrorTypeSymbol")]
    [InlineData("AssemblySymbol")]
    [InlineData("ModuleSymbol")]
    public void SymbolContractsAreInterfacesWithoutIPrefix(string name)
    {
        var assembly = typeof(Symbol).Assembly;
        var contract = assembly.GetType("Raven.CodeAnalysis." + name, throwOnError: true)!;
        Assert.True(contract.IsInterface);
        Assert.Null(assembly.GetType("Raven.CodeAnalysis.I" + name));
        Assert.True(typeof(Symbol).IsAssignableFrom(contract));
    }

    [Fact]
    public void GeneratedVisitorsPreserveIntersectionVisitName()
    {
        var method = typeof(SymbolVisitor).GetMethod("VisitIntersectionType")!;
        Assert.NotNull(method);
        Assert.Equal(typeof(IntersectionTypeSymbol), Assert.Single(method.GetParameters()).ParameterType);
        var generic = typeof(SymbolVisitor<int>).GetMethod("VisitIntersectionType")!;
        Assert.NotNull(generic);
        Assert.Equal(typeof(IntersectionTypeSymbol), Assert.Single(generic.GetParameters()).ParameterType);
    }
}
