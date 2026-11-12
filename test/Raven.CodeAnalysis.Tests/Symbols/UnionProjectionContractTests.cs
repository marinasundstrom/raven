using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class UnionProjectionContractTests
{
    [Fact]
    public void ImportedCaseMapsByNameRatherThanCaseOrdinal()
    {
        var first = Parameter("T", 0);
        var second = Parameter("E", 1);
        var variant = Case(first, second);
        var argument = Argument("Error");
        Assert.True(UnionFacts.TryProjectCaseTypeParameterFromUnionArguments(variant, Parameter("E", 0),
            [first, second], [Argument("Value"), argument], out var mapped));
        Assert.Same(argument, mapped);
    }

    [Fact]
    public void DeclarationMappingTakesPrecedenceOverMatchingName()
    {
        var first = Parameter("T", 0);
        var second = Parameter("E", 1);
        var parameter = Parameter("T", 0);
        var variant = MappedCase([parameter], parameter, second, first, second);
        var argument = Argument("Error");
        Assert.True(UnionFacts.TryProjectCaseTypeParameterFromUnionArguments(variant, parameter,
            [first, second], [Argument("Value"), argument], out var mapped));
        Assert.Same(argument, mapped);
    }

    [Fact]
    public void ReboundCaseParameterUsesDeclarationOrdinalMapping()
    {
        var unionParameter = Parameter("T", 0);
        var declarationParameter = Parameter("Declared", 0);
        var variant = MappedCase([declarationParameter], declarationParameter, unionParameter, unionParameter);
        var argument = Argument("Value");
        Assert.True(UnionFacts.TryProjectCaseTypeParameterFromUnionArguments(variant, Parameter("Rebound", 0),
            [unionParameter], [argument], out var mapped));
        Assert.Same(argument, mapped);
    }

    [Fact]
    public void ConstructedCaseUsesOriginalDefinitionsMapping()
    {
        var unionParameter = Parameter("T", 0);
        var parameter = Parameter("Renamed", 0);
        var definition = MappedCase([parameter], parameter, unionParameter, unionParameter);
        var constructed = Case(unionParameter);
        Set(constructed, "get_OriginalDefinition", definition);
        var argument = Argument("Value");
        Assert.True(UnionFacts.TryProjectCaseTypeParameterFromUnionArguments(constructed, parameter,
            [unionParameter], [argument], out var mapped));
        Assert.Same(argument, mapped);
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(2)]
    public void UnmappedCaseParameterFallsBackToNameEvenWithInvalidOrdinal(int ordinal)
    {
        var unionParameter = Parameter("T", 0);
        var declarationParameter = Parameter("Other", 0);
        var variant = MappedCase([declarationParameter], declarationParameter, unionParameter, unionParameter);
        var argument = Argument("Value");
        Assert.True(UnionFacts.TryProjectCaseTypeParameterFromUnionArguments(variant, Parameter("T", ordinal),
            [unionParameter], [argument], out var mapped));
        Assert.Same(argument, mapped);
    }

    [Fact]
    public void UnionParameterLookupFallsBackToNameAfterIdentityMismatch()
    {
        var unionParameter = Parameter("T", 0);
        var variant = Case(unionParameter);
        var argument = Argument("Value");
        Assert.True(UnionFacts.TryProjectCaseTypeParameterFromUnionArguments(variant, Parameter("T", 0),
            [Parameter("T", 9)], [argument], out var mapped));
        Assert.Same(argument, mapped);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    [InlineData(3)]
    [InlineData(4)]
    public void FailedProjectionRetainsInputParameter(int failure)
    {
        var first = Parameter("T", 0);
        var second = Parameter("E", 1);
        var parameter = Parameter(failure == 4 ? "Missing" : "E", 0);
        var variant = Case(first, second);
        ImmutableArray<TypeParameterSymbol> parameters = failure switch
        {
            0 => default,
            3 => [first],
            _ => [first, second]
        };
        ImmutableArray<TypeSymbol> arguments = failure switch
        {
            1 => [],
            2 => [Argument("OnlyFirst")],
            _ => [Argument("First"), Argument("Second")]
        };
        Assert.False(UnionFacts.TryProjectCaseTypeParameterFromUnionArguments(variant, parameter,
            parameters, arguments, out var mapped));
        Assert.Same(parameter, mapped);
    }

    private static UnionCaseTypeSymbol Case(params TypeParameterSymbol[] parameters)
    {
        var variant = Make<UnionCaseTypeSymbol>(SymbolKind.Type, "Case");
        SetUnion(variant, parameters);
        return variant;
    }

    private static MappedUnionCase MappedCase(ImmutableArray<TypeParameterSymbol> caseParameters,
        TypeParameterSymbol mappedParameter, TypeParameterSymbol target, params TypeParameterSymbol[] unionParameters)
    {
        var variant = Make<MappedUnionCase>(SymbolKind.Type, "Case");
        SetUnion(variant, unionParameters);
        Set(variant, "get_TypeParameters", caseParameters);
        Set(variant, "TryGetProjectedUnionTypeParameter", (Func<object?[]?, object?>)(arguments =>
        {
            var success = ReferenceEquals(arguments![0], mappedParameter);
            arguments[1] = success ? target : null;
            return success;
        }));
        return variant;
    }

    private static void SetUnion(object variant, TypeParameterSymbol[] parameters)
    {
        var union = Make<UnionSymbol>(SymbolKind.Type, "Choice");
        Set(union, "get_TypeParameters", parameters.ToImmutableArray());
        Set(variant, "get_Union", union);
    }

    private static TypeSymbol Argument(string name) => Make<TypeSymbol>(SymbolKind.Type, name);

    internal interface MappedUnionCase : UnionCaseTypeSymbol, UnionCaseTypeParameterMapping { }
}
