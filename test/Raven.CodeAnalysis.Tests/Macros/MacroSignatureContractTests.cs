using System;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroSignatureContractTests
{
    [Fact]
    public void ParameterRetainsNormalizedFieldsAndOptionalDisplay()
    {
        var parameter = CreateParameter();
        Assert.Equal("count", parameter.Name);
        Assert.Equal("int", parameter.TypeDisplayName);
        Assert.Equal(MacroParameterRole.Value, parameter.Role);
        Assert.Equal(MacroParameterSource.Value, parameter.Source);
        Assert.Equal(2, parameter.Ordinal);
        Assert.False(parameter.IsRequired);
        Assert.Equal(42, parameter.DefaultValue);
        Assert.Null(parameter.DefaultValueDisplay);
        Assert.Equal("forty-two", CreateParameter("forty-two").DefaultValueDisplay);
    }

    [Fact]
    public void ParameterEqualityIncludesDefaultDisplayAndStoredValue()
    {
        var first = CreateParameter();
        var second = CreateParameter();
        Assert.Equal(first, second);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.NotEqual(first, CreateParameter("42"));
        Assert.NotEqual(first, new MacroSignatureParameter("count", "int", MacroParameterRole.Value, MacroParameterSource.Value, 2, false, 43));
        var value = new object();
        var left = new MacroSignatureParameter("item", "object", MacroParameterRole.Value, MacroParameterSource.Value, 0, true, value);
        var right = new MacroSignatureParameter("item", "object", MacroParameterRole.Value, MacroParameterSource.Value, 0, true, value);
        Assert.Equal(left, right);
        Assert.NotEqual(left, new MacroSignatureParameter("item", "object", MacroParameterRole.Value, MacroParameterSource.Value, 0, true, new object()));
    }

    [Theory]
    [InlineData(-1, false)]
    [InlineData(0, true)]
    [InlineData(5, false)]
    public void SignaturePreservesActiveParameterWithoutClamping(int active, bool body)
    {
        var parameter = CreateParameter();
        var parameters = ImmutableArray.Create(parameter);
        var first = new MacroSignatureHelp("run", MacroKind.Freestanding, parameters, active, body);
        var second = new MacroSignatureHelp("run", MacroKind.Freestanding, parameters, active, body);
        Assert.Equal("run", first.Name);
        Assert.Equal(MacroKind.Freestanding, first.Kind);
        Assert.Equal(active, first.ActiveParameter);
        Assert.Equal(body, first.HasTokenTreeBody);
        Assert.Same(parameter, Assert.Single(first.Parameters));
        Assert.Equal(first, second);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.NotEqual(first, new MacroSignatureHelp("run", MacroKind.Freestanding, parameters, active + 1, body));
        Assert.NotEqual(first, new MacroSignatureHelp("run", MacroKind.Freestanding, parameters, active, !body));
    }

    [Fact]
    public void DefaultAndEmptyParameterArraysRemainDistinct()
    {
        var absent = new MacroSignatureHelp("run", MacroKind.AttachedDeclaration, default, -1, false);
        var empty = new MacroSignatureHelp("run", MacroKind.AttachedDeclaration, ImmutableArray<MacroSignatureParameter>.Empty, -1, false);
        Assert.True(absent.Parameters.IsDefault);
        Assert.False(empty.Parameters.IsDefault);
        Assert.Empty(empty.Parameters);
        Assert.NotEqual(absent, empty);
    }

    [Theory]
    [InlineData(typeof(MacroSignatureHelp))]
    [InlineData(typeof(MacroSignatureParameter))]
    public void PublicPropertiesRemainGetterOnly(Type type)
    {
        Assert.True(type.IsSealed);
        Assert.Empty(type.GetConstructors());
        foreach (var property in type.GetProperties())
            Assert.Null(property.SetMethod);
    }

    private static MacroSignatureParameter CreateParameter(string? display = null)
        => new("count", "int", MacroParameterRole.Value, MacroParameterSource.Value, 2, false, 42, display);
}
