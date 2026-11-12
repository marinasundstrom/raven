using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class LiteralTypeContractTests
{
    [Theory]
    [InlineData("text", "\"text\"")]
    [InlineData('x', "'x'")]
    [InlineData(true, "true")]
    [InlineData(false, "false")]
    [InlineData(42, "42")]
    [InlineData(null, "")]
    public void DisplayAndConstantIdentityArePreserved(object? value, string display)
    {
        var underlying = Underlying(value);
        var owner = Make<AssemblySymbol>(SymbolKind.Assembly, "Owner");
        var literal = new LiteralTypeSymbol(underlying, value, owner);
        Assert.Same(underlying, literal.UnderlyingType);
        Assert.Same(value, literal.ConstantValue);
        Assert.Equal(display, literal.Name);
        Assert.Equal(display, literal.MetadataName);
        Assert.Same(owner, literal.ContainingSymbol);
        Assert.Equal(underlying.TypeKind, literal.TypeKind);
        Assert.Equal(underlying.IsValueType, literal.IsValueType);
        Assert.Equal(underlying.IsReferenceType, literal.IsReferenceType);
        Assert.Equal(SpecialType.None, literal.SpecialType);
        Assert.Null(literal.OriginalDefinition);
        Assert.Empty(literal.GetAttributes());
        Assert.Null(literal.GetDocumentationComment());
    }

    [Fact]
    public void NullableConstantIsAnnotatedInMetadata()
    {
        var context = new NullabilityInfoContext();
        var property = typeof(LiteralTypeSymbol).GetProperty(nameof(LiteralTypeSymbol.ConstantValue))!;
        Assert.Equal(NullabilityState.Nullable, context.Create(property).ReadState);
        var constructor = Assert.Single(typeof(LiteralTypeSymbol).GetConstructors());
        Assert.Equal(NullabilityState.Nullable, context.Create(constructor.GetParameters()[1]).ReadState);
    }

    [Fact]
    public void ValueEqualityIncludesSemanticTypeAndConstant()
    {
        var owner = Make<AssemblySymbol>(SymbolKind.Assembly, "Owner");
        var first = new LiteralTypeSymbol(Underlying(42), 42, owner);
        var second = new LiteralTypeSymbol(Underlying(42), 42, owner);
        Assert.NotSame(first.UnderlyingType, second.UnderlyingType);
        Assert.True(first.Equals((object)second));
        Assert.True(((IEquatable<LiteralTypeSymbol>)first).Equals(second));
        Assert.True(first == second);
        Assert.False(first != second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.Single(new HashSet<LiteralTypeSymbol> { first, second });
        Assert.False(first == new LiteralTypeSymbol(Underlying(43), 43, owner));
        Assert.False(first == new LiteralTypeSymbol(Underlying("42"), "42", owner));
        Assert.False(first == null);
        Assert.True(first != null);
        Assert.True((LiteralTypeSymbol?)null == null);
    }

    [Fact]
    public void MembersAndBaseTypeForwardToUnderlyingType()
    {
        var underlying = Underlying(42);
        var baseType = Underlying("base");
        var nested = Underlying("nested");
        var member = Make<FieldSymbol>(SymbolKind.Field, "Member");
        var members = ImmutableArray.Create<Symbol>(member);
        Set(underlying, "get_BaseType", baseType);
        Set(underlying, "get_Interfaces", ImmutableArray.Create(baseType));
        Set(underlying, "get_AllInterfaces", ImmutableArray.Create(baseType));
        Set(underlying, "GetMembers", members);
        Set(underlying, "LookupType", nested);
        Set(underlying, "IsMemberDefined", (Func<object?[]?, object?>)(arguments =>
        {
            arguments![1] = member;
            return true;
        }));
        var literal = new LiteralTypeSymbol(underlying, 42, Make<AssemblySymbol>(SymbolKind.Assembly, "Owner"));
        Assert.Same(baseType, literal.BaseType);
        Assert.Equal(members, literal.GetMembers());
        Assert.Equal(members, literal.GetMembers("Member"));
        Assert.Same(nested, literal.LookupType("Nested"));
        Assert.Same(baseType, Assert.Single(literal.Interfaces));
        Assert.Same(baseType, Assert.Single(literal.AllInterfaces));
        Assert.True(literal.IsMemberDefined("Member", out var found));
        Assert.Same(member, found);
    }

    [Fact]
    public void VisitorDefaultsReceiveTheLiteral()
    {
        var literal = new LiteralTypeSymbol(Underlying(42), 42, Make<AssemblySymbol>(SymbolKind.Assembly, "Owner"));
        var visitor = new RecordingVisitor();
        literal.Accept(visitor);
        Assert.Same(literal, visitor.Seen);
    }

    private sealed class RecordingVisitor : SymbolVisitor
    {
        public Symbol? Seen { get; private set; }
        public override void DefaultVisit(Symbol symbol) => Seen = symbol;
    }

    private static NamedTypeSymbol Underlying(object? value)
    {
        var special = value switch
        {
            string => SpecialType.System_String,
            char => SpecialType.System_Char,
            bool => SpecialType.System_Boolean,
            null => SpecialType.System_Object,
            _ => SpecialType.System_Int32
        };
        var reference = value is string or null;
        var result = Make<NamedTypeSymbol>(SymbolKind.Type, special.ToString(), reference ? TypeKind.Class : TypeKind.Struct);
        Set(result, "get_SpecialType", special);
        Set(result, "get_IsValueType", !reference);
        Set(result, "get_IsReferenceType", reference);
        return result;
    }
}
