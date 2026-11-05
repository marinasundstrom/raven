namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class CompilerOptionValueTests
{
    [Fact]
    public void AsyncInvestigationPreservesDefaultsAndIndependentCopies()
    {
        var disabled = AsyncInvestigationOptions.Disabled;
        Assert.Same(disabled, AsyncInvestigationOptions.Disabled);
        Assert.False(disabled.IsEnabled);
        Assert.Equal("Step14", disabled.StepLabel);
        Assert.Equal(AsyncInvestigationPointerLabelScope.FieldOnly, disabled.PointerLabelScope);
        var enabled = AsyncInvestigationOptions.Enable("Step42", AsyncInvestigationPointerLabelScope.IncludeAsyncMethodName);
        Assert.True(enabled.IsEnabled);
        Assert.Equal("Step42", enabled.StepLabel);
        Assert.Equal(AsyncInvestigationPointerLabelScope.IncludeAsyncMethodName, enabled.PointerLabelScope);
        var changed = enabled.WithStepLabel("Step43").WithPointerLabelScope(AsyncInvestigationPointerLabelScope.FieldOnly);
        Assert.Equal("Step43", changed.StepLabel);
        Assert.Equal("Step42", enabled.StepLabel);
        Assert.Equal("Step42", enabled.WithStepLabel(null).StepLabel);
        Assert.False(disabled.WithStepLabel("Step15").IsEnabled);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("  ")]
    public void MissingInvestigationLabelsUseDefault(string? label)
    {
        Assert.Equal("Step14", AsyncInvestigationOptions.Enable(label).StepLabel);
        if (label is not null)
            Assert.Equal("Step14", AsyncInvestigationOptions.Enable("Custom").WithStepLabel(label).StepLabel);
    }

    [Fact]
    public void InvestigationConfigurationHasRecordValueEquality()
    {
        var value = AsyncInvestigationOptions.Enable("Step42");
        var equal = AsyncInvestigationOptions.Enable(new string("Step42".ToCharArray()));
        Assert.NotSame(value, equal);
        Assert.Equal(value, equal);
        Assert.Equal(value.GetHashCode(), equal.GetHashCode());
        Assert.NotEqual(value, value.WithStepLabel("Step43"));
        Assert.NotEqual(value, value.WithPointerLabelScope(AsyncInvestigationPointerLabelScope.IncludeAsyncMethodName));
        Assert.NotEqual(value, AsyncInvestigationOptions.Disabled.WithStepLabel("Step42"));
        Assert.Single(new HashSet<AsyncInvestigationOptions> { value, equal });
    }

    [Theory]
    [InlineData("default", ReturnedValueHandlingMode.Off)]
    [InlineData("none", ReturnedValueHandlingMode.Off)]
    [InlineData("off", ReturnedValueHandlingMode.Off)]
    [InlineData("disabled", ReturnedValueHandlingMode.Off)]
    [InlineData("disable", ReturnedValueHandlingMode.Off)]
    [InlineData("false", ReturnedValueHandlingMode.Off)]
    [InlineData("full", ReturnedValueHandlingMode.Full)]
    [InlineData("enabled", ReturnedValueHandlingMode.Full)]
    [InlineData("enable", ReturnedValueHandlingMode.Full)]
    [InlineData("true", ReturnedValueHandlingMode.Full)]
    public void ReturnedValueModeAcceptsAliasesIgnoringCaseAndWhitespace(string text, ReturnedValueHandlingMode expected)
    {
        Assert.True(ReturnedValueHandlingOptions.TryParse("  " + text.ToUpperInvariant() + " ", out var mode));
        Assert.Equal(expected, mode);
        Assert.Equal(expected == ReturnedValueHandlingMode.Full ? "full" : "off", ReturnedValueHandlingOptions.ToProjectFileValue(mode));
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData(" ")]
    [InlineData("unknown")]
    public void InvalidReturnedValueModesFailAndResetOutput(string? text)
    {
        var mode = ReturnedValueHandlingMode.Full;
        Assert.False(ReturnedValueHandlingOptions.TryParse(text, out mode));
        Assert.Equal(ReturnedValueHandlingMode.Off, mode);
    }

    [Fact]
    public void UnknownModeFormatsAsOff()
        => Assert.Equal("off", ReturnedValueHandlingOptions.ToProjectFileValue((ReturnedValueHandlingMode)123));
}
