using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class ConditionalAttributeFactsContractTests
{
    [Fact]
    public void TypeConditionsPreserveOwnThenInheritedOrderAndAnyDefinedSymbolEnables()
    {
        var parent = Type("Example.Base");
        Set(parent, "GetAttributes", ImmutableArray.Create(Conditional("BASE")));
        var child = Type("Example.Child");
        Set(child, "get_BaseType", parent);
        Set(child, "GetAttributes", ImmutableArray.Create(Conditional("LOCAL"), Conditional("OTHER")));
        Assert.Equal(new[] { "LOCAL", "OTHER", "BASE" }, ConditionalAttributeFacts.GetConditions(child));
        Assert.True(ConditionalAttributeFacts.IsOmitted(child, ["base"]));
        Assert.False(ConditionalAttributeFacts.IsOmitted(child, ["BASE"]));
        Assert.False(ConditionalAttributeFacts.IsOmitted(Type("Plain"), []));
    }

    [Fact]
    public void ResolvedOverrideIdentityTakesPrecedenceOverSearchingBaseMembers()
    {
        var parent = Method();
        Set(parent, "GetAttributes", ImmutableArray.Create(Conditional("PARENT")));
        var method = Make<ResolvedMethod>(SymbolKind.Method, "Call");
        Set(method, "get_IsOverride", true);
        Set(method, "get_OverriddenMethod", parent);
        Set(method, "GetAttributes", ImmutableArray.Create(Conditional("LOCAL")));
        Assert.Equal(new[] { "LOCAL", "PARENT" }, ConditionalAttributeFacts.GetConditions(method));
        Assert.True(ConditionalAttributeFacts.HasConditions(method));
    }

    [Fact]
    public void OverrideFallbackFindsMatchingVirtualBaseMethod()
    {
        var parent = Method();
        Set(parent, "get_IsVirtual", true);
        Set(parent, "GetAttributes", ImmutableArray.Create(Conditional("BASE")));
        var baseType = Type("Base");
        Set(baseType, "GetMembers", ImmutableArray.Create<Symbol>(parent));
        var owner = Type("Owner");
        Set(owner, "get_BaseType", baseType);
        var method = Method();
        Set(method, "get_IsOverride", true);
        Set(method, "get_ContainingType", owner);
        Assert.Equal(new[] { "BASE" }, ConditionalAttributeFacts.GetConditions(method));
    }

    [Theory]
    [InlineData("")]
    [InlineData("1BAD")]
    [InlineData("BAD-NAME")]
    public void InvalidConditionIdentifiersAreRejected(string condition)
        => Assert.Equal("A conditional symbol must be a nonempty identifier", ConditionalAttributeFacts.GetDeclarationError(Method(), Conditional(condition)));

    [Fact]
    public void DeclarationRulesCoverValidMethodsReturnTypesOutParametersAndAttributeClasses()
    {
        Assert.Null(ConditionalAttributeFacts.GetDeclarationError(Method(), Conditional("TRACE")));
        var method = Method();
        Set(method, "get_IsOverride", true);
        Assert.Contains("non-override", ConditionalAttributeFacts.GetDeclarationError(method, Conditional("TRACE"))!);
        method = Method();
        Set(method, "get_ReturnType", Type("Int32"));
        Assert.Contains("return unit", ConditionalAttributeFacts.GetDeclarationError(method, Conditional("TRACE"))!);
        method = Method();
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "result");
        Set(parameter, "get_RefKind", RefKind.Out);
        Set(method, "get_Parameters", ImmutableArray.Create(parameter));
        Assert.Contains("out parameters", ConditionalAttributeFacts.GetDeclarationError(method, Conditional("TRACE"))!);
        var attributeClass = Type("MyAttribute");
        Set(attributeClass, "get_BaseType", Type("System.Attribute"));
        Assert.Null(ConditionalAttributeFacts.GetDeclarationError(attributeClass, Conditional("TRACE")));
        Assert.Contains("attribute class", ConditionalAttributeFacts.GetDeclarationError(Type("Plain"), Conditional("TRACE"))!);
    }

    [Fact]
    public void AttributeOmissionUsesDeclarationTreeSymbolsAndRequiresSourceReference()
    {
        var attributeClass = Type("Example.TagAttribute");
        Set(attributeClass, "GetAttributes", ImmutableArray.Create(Conditional("ENABLED")));
        var ctor = Method();
        foreach (var enabled in new[] { false, true })
        {
            var options = new ParseOptions().WithPreprocessorSymbols(enabled ? ["ENABLED"] : []);
            var syntax = SyntaxTree.ParseText("[Tag] class C {}", options).GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();
            var attribute = new AttributeData(attributeClass, ctor, [], [], syntax.GetReference());
            Assert.Equal(!enabled, ConditionalAttributeFacts.IsAttributeOmitted(attribute));
        }
        Assert.False(ConditionalAttributeFacts.IsAttributeOmitted(new AttributeData(attributeClass, ctor, [], [], null)));
    }

    internal interface ResolvedMethod : MethodSymbol, MethodOverrideInfo { }

    private static AttributeData Conditional(string condition)
        => new(Type("System.Diagnostics.ConditionalAttribute"), Make<MethodSymbol>(SymbolKind.Method, ".ctor"),
            [TypedConstant.CreatePrimitive(null, condition)], [], null);

    private static NamedTypeSymbol Type(string name)
    {
        var type = Named("Runtime", name);
        Set(type, "get_BaseType", null);
        Set(type, "GetAttributes", ImmutableArray<AttributeData>.Empty);
        Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        return type;
    }

    private static MethodSymbol Method()
    {
        var method = Make<MethodSymbol>(SymbolKind.Method, "Call");
        var result = Type("Unit");
        Set(result, "get_SpecialType", SpecialType.System_Unit);
        Set(method, "get_ReturnType", result);
        Set(method, "get_IsOverride", false);
        Set(method, "get_Arity", 0);
        Set(method, "get_MethodKind", MethodKind.Ordinary);
        Set(method, "get_Parameters", ImmutableArray<ParameterSymbol>.Empty);
        Set(method, "get_ContainingType", Type("Owner"));
        Set(method, "get_ExplicitInterfaceImplementations", ImmutableArray<MethodSymbol>.Empty);
        Set(method, "GetAttributes", ImmutableArray<AttributeData>.Empty);
        return method;
    }
}
