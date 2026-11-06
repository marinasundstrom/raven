using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class SimpleConstraintContractTests
{
    [Theory]
    [InlineData(TypeParameterConstraintKind.None, false, false, false, false)]
    [InlineData(TypeParameterConstraintKind.ReferenceType, true, false, true, false)]
    [InlineData(TypeParameterConstraintKind.ValueType, false, true, true, true)]
    [InlineData(TypeParameterConstraintKind.NotNull, false, false, true, false)]
    [InlineData(TypeParameterConstraintKind.Constructor, false, false, false, true)]
    [InlineData(TypeParameterConstraintKind.ReferenceType | TypeParameterConstraintKind.Constructor, true, false, true, true)]
    [InlineData(TypeParameterConstraintKind.AllowByRefLike, false, false, false, false)]
    public void TypeParameterConstraintsUseTheirDeclaredGuarantees(TypeParameterConstraintKind flags,
        bool reference, bool value, bool notNull, bool constructor)
    {
        var parameter = Make<TypeParameterSymbol>(("get_TypeKind", TypeKind.TypeParameter),
            ("get_IsReferenceType", false), ("get_IsValueType", false), ("get_IsNullable", false),
            ("get_ConstraintKind", flags));
        Assert.Equal(reference, SemanticFacts.SatisfiesReferenceTypeConstraint(parameter));
        Assert.Equal(value, SemanticFacts.SatisfiesValueTypeConstraint(parameter));
        Assert.Equal(notNull, SemanticFacts.SatisfiesNotNullConstraint(parameter));
        Assert.Equal(constructor, SemanticFacts.SatisfiesConstructorConstraint(parameter));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NullableValueTypesHaveConstructorsButDoNotSatisfyStructOrNotNull(bool nullable)
    {
        var type = Make<NamedTypeSymbol>(("get_TypeKind", nullable ? TypeKind.Nullable : TypeKind.Struct),
            ("get_IsReferenceType", false), ("get_IsValueType", true), ("get_IsNullable", nullable));
        Assert.False(SemanticFacts.SatisfiesReferenceTypeConstraint(type));
        Assert.Equal(!nullable, SemanticFacts.SatisfiesValueTypeConstraint(type));
        Assert.Equal(!nullable, SemanticFacts.SatisfiesNotNullConstraint(type));
        Assert.True(SemanticFacts.SatisfiesConstructorConstraint(type));
    }

    [Theory]
    [InlineData(false, false, false)]
    [InlineData(false, true, true)]
    [InlineData(true, true, false)]
    public void ClassesRequireAnAccessibleParameterlessInstanceConstructor(bool isAbstract, bool addPublic, bool expected)
    {
        var constructors = ImmutableArray.CreateBuilder<MethodSymbol>();
        constructors.Add(Constructor(Accessibility.Private, []));
        constructors.Add(Constructor(Accessibility.Public, [Make<ParameterSymbol>()]));
        if (addPublic)
            constructors.Add(Constructor(Accessibility.Public, []));
        var type = Make<NamedTypeSymbol>(("get_IsValueType", false), ("get_IsAbstract", isAbstract),
            ("get_InstanceConstructors", constructors.ToImmutable()));
        Assert.Equal(expected, SemanticFacts.SatisfiesConstructorConstraint(type));
    }

    private static MethodSymbol Constructor(Accessibility accessibility, ImmutableArray<ParameterSymbol> parameters)
        => Make<MethodSymbol>(("get_DeclaredAccessibility", accessibility), ("get_Parameters", parameters));

    private static T Make<T>(params (string Member, object Value)[] members) where T : class
    {
        var symbol = DispatchProxy.Create<T, ConstraintSymbol>();
        var proxy = (ConstraintSymbol)(object)symbol;
        foreach (var (member, value) in members)
            proxy.Values.Add(member, value);
        return symbol;
    }

    public class ConstraintSymbol : DispatchProxy
    {
        internal Dictionary<string, object> Values { get; } = new();
        protected override object Invoke(MethodInfo? method, object?[]? arguments) => Values[method!.Name];
    }
}
