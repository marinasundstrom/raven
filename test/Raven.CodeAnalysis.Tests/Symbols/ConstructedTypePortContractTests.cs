using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class ConstructedTypePortContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MemberCachingUsesProviderDeclarationState(bool mutable)
    {
        var definition = Make<SourceNamedTypeIdentity>(SymbolKind.Type, "Definition");
        Set(definition, "get_HasMutableMemberShape", mutable);
        Set(definition, "get_ContainingAssembly", null);
        var first = Make<Symbol>(SymbolKind.Label, "Member");
        var second = Make<Symbol>(SymbolKind.Label, "Member");
        ImmutableArray<Symbol> members = [first];
        Set(definition, "GetMembers", (Func<object?[]?, object?>)(_ => members));
        var constructed = new ConstructedNamedTypeSymbol(definition, []);
        Assert.Same(first, Assert.Single(constructed.GetMembers()));
        Assert.Same(first, Assert.Single(constructed.GetMembers("Member")));
        members = [second];
        Assert.Same(mutable ? second : first, Assert.Single(constructed.GetMembers()));
        Assert.Same(mutable ? second : first, Assert.Single(constructed.GetMembers("Member")));
    }

    [Fact]
    public void ConstructionSubstitutesParametersAndPreservesUnchangedTypes()
    {
        var (definition, parameter, argument) = Generic();
        var constructed = new ConstructedNamedTypeSymbol(definition, [argument]);
        Assert.Same(definition, constructed.ConstructedFrom);
        Assert.Equal(new[] { argument }, constructed.TypeArguments);
        Assert.Same(argument, constructed.Substitute(parameter));
        Assert.Same(argument, constructed.Substitute(argument));
        Assert.Equal(new[] { argument }, constructed.GetAllTypeArguments());
        Assert.Same(parameter, Assert.Single(constructed.TypeParameters));
        Assert.Empty(constructed.GetMembers());
    }

    [Fact]
    public void FieldMethodPropertyAndEventProjectionsShareTheConstructedOwner()
    {
        var (definition, parameter, argument) = Generic();
        var field = Make<FieldSymbol>(SymbolKind.Field, "Field");
        var method = Make<MethodSymbol>(SymbolKind.Method, "Method");
        var property = Make<PropertySymbol>(SymbolKind.Property, "Property");
        var @event = Make<EventSymbol>(SymbolKind.Event, "Event");
        foreach (var member in new Symbol[] { field, method, property, @event })
        {
            Set(member, "get_ContainingSymbol", definition);
            Set(member, "get_ContainingType", definition);
            Set(member, "get_ContainingNamespace", null);
            Set(member, "get_Type", parameter);
        }
        Set(method, "get_ReturnType", parameter);
        Set(method, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(method, "get_Parameters", ImmutableArray<ParameterSymbol>.Empty);
        Set(method, "get_DeclaredFunctionType", null);
        Set(property, "get_GetMethod", null);
        Set(property, "get_SetMethod", null);
        Set(@event, "get_AddMethod", null);
        Set(@event, "get_RemoveMethod", null);
        Members(definition, [field, method, property, @event]);
        var constructed = new ConstructedNamedTypeSymbol(definition, [argument]);
        var projected = constructed.GetMembers();
        Assert.Equal(4, projected.Length);
        Assert.All(projected, member => Assert.Same(constructed, member.ContainingType));
        var projectedField = Assert.IsAssignableFrom<FieldSymbol>(projected[0]);
        var projectedMethod = Assert.IsAssignableFrom<MethodSymbol>(projected[1]);
        var projectedProperty = Assert.IsAssignableFrom<PropertySymbol>(projected[2]);
        var projectedEvent = Assert.IsAssignableFrom<EventSymbol>(projected[3]);
        Assert.Same(argument, projectedField.Type);
        Assert.Same(argument, projectedMethod.ReturnType);
        Assert.Same(argument, projectedProperty.Type);
        Assert.Same(argument, projectedEvent.Type);
        Assert.Empty(projectedMethod.Parameters);
        Assert.Null(projectedProperty.GetMethod);
        Assert.Null(projectedProperty.SetMethod);
        Assert.Null(projectedEvent.AddMethod);
        Assert.Null(projectedEvent.RemoveMethod);
        Assert.Same(projectedField, Assert.Single(constructed.GetMembers("Field")));
        Assert.True(constructed.IsMemberDefined("Field", out var found));
        Assert.Same(projectedField, found);
        Assert.Same(projectedField, constructed.GetMembers()[0]);
    }

    [Fact]
    public void ConstructedMethodSubstitutesReturnTypeAndRejectsWrongArity()
    {
        var (_, parameter, argument) = Generic();
        var method = Make<MethodSymbol>(SymbolKind.Method, "Method");
        Set(method, "get_TypeParameters", ImmutableArray.Create(parameter));
        Set(method, "get_ReturnType", parameter);
        Set(method, "get_ContainingType", null);
        Set(method, "get_ContainingSymbol", null);
        var valueParameter = Make<ParameterSymbol>(SymbolKind.Parameter, "value");
        Set(valueParameter, "get_Type", parameter);
        Set(valueParameter, "get_RefKind", RefKind.Ref);
        Set(valueParameter, "get_IsVarParams", false);
        Set(method, "get_Parameters", ImmutableArray.Create(valueParameter));
        Set(method, "get_DeclaredFunctionType", null);
        var constructed = new ConstructedMethodSymbol(method, [argument]);
        Assert.Same(argument, constructed.ReturnType);
        Assert.Equal(new[] { argument }, constructed.TypeArguments);
        var projectedParameter = Assert.Single(constructed.Parameters);
        Assert.Same(argument, projectedParameter.Type);
        Assert.Same(constructed, projectedParameter.ContainingSymbol);
        Assert.Equal(RefKind.Ref, projectedParameter.RefKind);
        Assert.False(projectedParameter.IsVarParams);
        Assert.Throws<ArgumentException>(() => new ConstructedMethodSymbol(method, []));
    }

    [Fact]
    public void NestedTypeMembersInheritTheConstructedOuterSubstitution()
    {
        var (definition, parameter, argument) = Generic();
        var nested = Named("Types", "Example.Nested");
        Set(nested, "get_ConstructedFrom", null);
        Set(nested, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(nested, "get_ContainingAssembly", null);
        Set(nested, "get_ContainingType", definition);
        Set(nested, "get_ContainingSymbol", definition);
        var field = Make<FieldSymbol>(SymbolKind.Field, "Value");
        Set(field, "get_Type", parameter);
        Members(nested, [field]);
        var outer = new ConstructedNamedTypeSymbol(definition, [argument]);
        var projected = Assert.IsAssignableFrom<NamedTypeSymbol>(outer.Substitute(nested));
        Assert.Same(outer, projected.ContainingType);
        Assert.Same(argument, Assert.IsAssignableFrom<FieldSymbol>(Assert.Single(projected.GetMembers("Value"))).Type);
    }

    [Fact]
    public void ArrayAndReferenceSubstitutionPreserveShapeAndUnchangedIdentity()
    {
        var (definition, parameter, argument) = Generic();
        var constructed = new ConstructedNamedTypeSymbol(definition, [argument]);
        var array = new ArrayTypeSymbolImplementation(null, parameter, null, null, null, [], rank: 2);
        var projected = Assert.IsAssignableFrom<ArrayTypeSymbol>(constructed.Substitute(array));
        Assert.Same(argument, projected.ElementType);
        Assert.Equal(2, projected.Rank);
        Assert.Same(projected, constructed.Substitute(projected));
        var reference = new RefTypeSymbol(parameter);
        var projectedReference = Assert.IsType<RefTypeSymbol>(constructed.Substitute(reference));
        Assert.Same(argument, projectedReference.ElementType);
    }

    private static (NamedTypeSymbol definition, TypeParameterSymbol parameter, TypeSymbol argument) Generic()
    {
        var parameter = Parameter("T", 0);
        Set(parameter, "get_ConstraintTypes", ImmutableArray<TypeSymbol>.Empty);
        ((IdentityProxy)parameter).ReadOnce.Remove("get_ConstraintTypes");
        var definition = Named("Types", "Example.Generic");
        Set(definition, "get_TypeParameters", ImmutableArray.Create(parameter));
        Set(definition, "get_ContainingAssembly", null);
        Set(definition, "get_ConstructedFrom", null);
        Set(definition, "get_Arity", 1);
        Set(definition, "get_BaseType", null);
        Members(definition, []);
        var argument = Named("Types", "Example.Argument");
        Set(argument, "get_ConstructedFrom", null);
        return (definition, parameter, argument);
    }

    private static void Members(NamedTypeSymbol type, ImmutableArray<Symbol> members)
        => Set(type, "GetMembers", (Func<object?[]?, object?>)(args => args!.Length == 0
            ? members : members.Where(member => member.Name == (string)args[0]!).ToImmutableArray()));
}
