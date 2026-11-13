using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroDeclarationContractTests
{
    [Theory]
    [InlineData(typeof(MacroAliasAttribute), AttributeTargets.Class | AttributeTargets.Method, false, false)]
    [InlineData(typeof(MacroCarrierAttribute), AttributeTargets.Class | AttributeTargets.Method, false, false)]
    [InlineData(typeof(LocalMacroAttribute), AttributeTargets.Class, false, false)]
    [InlineData(typeof(RavenCompilerPluginAttribute), AttributeTargets.Assembly, true, true)]
    public void AttributeUsagePreservesTargetsAndInheritance(Type type, AttributeTargets targets, bool multiple, bool inherited)
    {
        var usage = Assert.IsType<AttributeUsageAttribute>(Attribute.GetCustomAttribute(type, typeof(AttributeUsageAttribute)));
        Assert.Equal(targets, usage.ValidOn);
        Assert.Equal(multiple, usage.AllowMultiple);
        Assert.Equal(inherited, usage.Inherited);
        Assert.True(type.IsSealed);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("  ")]
    public void AliasRejectsMissingNames(string? name)
    {
        Assert.Equal("alias", Assert.Throws<ArgumentException>(() => new MacroAliasAttribute(name!)).ParamName);
    }

    [Fact]
    public void AttributeValuesAndConstructorDefaultsArePreserved()
    {
        Assert.Equal(" Alias ", new MacroAliasAttribute(" Alias ").Alias);
        Assert.Null(new RavenCompilerPluginAttribute().ExportedType);
        Assert.Equal(typeof(Macro), new RavenCompilerPluginAttribute(typeof(Macro)).ExportedType);
        Assert.Equal("exportedType", Assert.Throws<ArgumentNullException>(() => new RavenCompilerPluginAttribute(null!)).ParamName);
        var carrier = new MacroCarrierAttribute(MacroCarrierKinds.TokenTree | MacroCarrierKinds.Parenthesized);
        Assert.Equal(MacroBodyRequirement.Default, carrier.BodyRequirement);
        Assert.Equal(MacroCarrierKinds.TokenTree | MacroCarrierKinds.Parenthesized, carrier.CarrierKinds);
        Assert.Equal(MacroBodyRequirement.None, new MacroCarrierAttribute(MacroCarrierKinds.Declaration, MacroBodyRequirement.None).BodyRequirement);
    }

    [Theory]
    [InlineData(MacroBodyRequirement.None, false)]
    [InlineData(MacroBodyRequirement.Default, true)]
    public void DefinitionDescriptorPreservesSnapshotAndValueSemantics(MacroBodyRequirement requirement, bool hasBody)
    {
        var definition = new Macro();
        var parameter = new MacroParameterDescriptor("value", typeof(int), MacroParameterRole.Value, 0, true, null);
        var parameters = ImmutableArray.Create(parameter);
        MacroDefinitionDescriptor Create(IMacroDefinition macro) => new(macro, MacroApplicationKind.Freestanding,
            MacroInvocationTargets.Expression, typeof(int), MacroCarrierKinds.Parenthesized,
            requirement, MacroTarget.None, parameters, true, false);
        var first = Create(definition);
        var second = Create(definition);
        Assert.Equal(first, second);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.NotEqual(first, Create(new Macro()));
        Assert.Same(definition, first.Definition);
        Assert.Same(parameter, Assert.Single(first.Parameters));
        Assert.Equal(typeof(int), first.ExpressionResultType);
        Assert.Equal(hasBody, first.HasTokenBody);
        Assert.True(first.AcceptsArguments);
        Assert.False(first.HasDeclarationInput);
        foreach (var property in typeof(MacroDefinitionDescriptor).GetProperties())
            Assert.Null(property.SetMethod);
    }

    private sealed class Macro : IMacroDefinition { }
}
