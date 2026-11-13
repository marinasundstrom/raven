using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroDefinitionContractTests
{
    private sealed class WidgetMacro : IMacroDefinition { }
    private sealed class WidgetMacro<T> : IMacroDefinition { }
    private sealed class Macro : IMacroDefinition { }
    private sealed class Ordinary : IMacroDefinition { }

    [Fact]
    public void DefaultIdentity_StripsGenericArityAndNonemptyMacroSuffix()
    {
        Assert.Equal("Widget", ((IMacroDefinition)new WidgetMacro()).Name);
        Assert.Equal("Widget", ((IMacroDefinition)new WidgetMacro<int>()).Name);
        Assert.Equal("Macro", ((IMacroDefinition)new Macro()).Name);
        Assert.Equal("Ordinary", ((IMacroDefinition)new Ordinary()).Name);
        Assert.Equal(typeof(WidgetMacro).Namespace, ((IMacroDefinition)new WidgetMacro()).Namespace);
    }

    [Fact]
    public void DefaultCapabilities_RetainExpressionCompatibility()
    {
        IMacroDefinition definition = new WidgetMacro();
        Assert.Null(definition.Alias);
        Assert.Null(definition.Documentation);
        Assert.Equal(DocumentationFormat.Markdown, definition.DocumentationFormat);
        Assert.Equal(MacroInvocationTargets.Expression, definition.InvocationTargets);
        Assert.Null(definition.ExpressionResultType);
        Assert.Equal(MacroCarrierKinds.Default, definition.CarrierKinds);
        Assert.Equal(MacroBodyRequirement.Default, definition.BodyRequirement);
        Assert.False(definition.AcceptsArguments);
    }

    [Fact]
    public void CarrierEnums_PreserveAbiValuesAndFlags()
    {
        Assert.Equal(15, (int)(MacroCarrierKinds.Parenthesized | MacroCarrierKinds.ExpressionHeader |
            MacroCarrierKinds.TokenTree | MacroCarrierKinds.Declaration));
        Assert.True(typeof(MacroCarrierKinds).IsDefined(typeof(FlagsAttribute), false));
        Assert.Equal(new[] { 0, 1, 2, 3 }, Enum.GetValues<MacroBodyRequirement>().Select(x => (int)x));
    }
}
