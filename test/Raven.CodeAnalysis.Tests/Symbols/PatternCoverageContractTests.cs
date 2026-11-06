using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class PatternCoverageContractTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void BooleanCoverageRequiresBothValuesAndStopsWhenComplete(bool first)
    {
        var target = Alias(Alias(Boolean()));
        Assert.True(TypeCoverageHelper.RequiresLiteralCoverage(target));
        Assert.False(TypeCoverageHelper.LiteralsCoverType(target, [null, first, "true", first]));
        Assert.True(TypeCoverageHelper.LiteralsCoverType(target, Complete()));

        IEnumerable<object?> Complete()
        {
            yield return first;
            yield return !first;
            throw new InvalidOperationException("Both alternatives were already covered.");
        }
    }

    [Fact]
    public void NonBooleanTypesDoNotEnumerateTheConstants()
    {
        var type = Named("A", "Example.Text");
        Assert.False(TypeCoverageHelper.RequiresLiteralCoverage(type));
        Assert.False(TypeCoverageHelper.LiteralsCoverType(type, Unexpected()));
        static IEnumerable<object?> Unexpected()
        {
            yield return Throw();
            static object Throw() => throw new InvalidOperationException();
        }
    }

    [Fact]
    public void LiteralMembershipUsesTheUnderlyingTypeAndHandlesNull()
    {
        var number = Named("A", "Example.Number");
        var nullType = Make<TypeSymbol>(SymbolKind.Type, "null", TypeKind.Null);
        Assert.True(TypeCoverageHelper.LiteralBelongsToType(new Literal(Alias(number), 42), Alias(number)));
        Assert.False(TypeCoverageHelper.LiteralBelongsToType(new Literal(number, 42), Boolean()));
        Assert.True(TypeCoverageHelper.LiteralBelongsToType(new Literal(number, null), nullType));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ParameterPatternMatchingChecksConstraintsInEitherPosition(bool reverse)
    {
        var parameter = Parameter("T", 0);
        Set(parameter, "get_ConstraintKind", TypeParameterConstraintKind.ReferenceType);
        var type = Named("A", "Example.Text");
        Set(type, "get_IsRefLikeType", false);
        Set(type, "get_IsReferenceType", true);
        bool Matches() => reverse
            ? TypeCoverageHelper.CanTypeParameterMatchPattern(Alias(type), Alias(parameter))
            : TypeCoverageHelper.CanTypeParameterMatchPattern(Alias(parameter), Alias(type));
        Assert.True(Matches());
        Set(type, "get_IsReferenceType", false);
        Assert.False(Matches());
        Assert.False(TypeCoverageHelper.CanTypeParameterMatchPattern(type, type));
    }

    [Fact]
    public void CyclicAliasCannotEstablishBooleanCoverage()
    {
        var first = Alias(Boolean());
        var second = Alias(first);
        Set(first, "get_UnderlyingSymbol", second);
        ((IdentityProxy)(object)first).ReadOnce.Add("get_UnderlyingSymbol");
        Assert.False(TypeCoverageHelper.RequiresLiteralCoverage(first));
    }

    private static TypeSymbol Boolean()
    {
        var type = Named("A", "System.Boolean");
        Set(type, "get_SpecialType", SpecialType.System_Boolean);
        return type;
    }

    private static TypeSymbol Alias(TypeSymbol target)
    {
        var alias = Make<TypeSymbol>(SymbolKind.Type, "Alias");
        Set(alias, "get_IsAlias", true);
        Set(alias, "get_UnderlyingSymbol", target);
        return alias;
    }

    private sealed record Literal(TypeSymbol UnderlyingType, object? ConstantValue) : ILiteralTypeInfo;
}
