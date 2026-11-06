using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "DotNet")]
public sealed class ParameterPatternFactsContractTests
{
    [Theory]
    [InlineData("(x, y)", "(x, y)")]
    [InlineData("[head, ..tail]", "[head, ..tail]")]
    [InlineData("(x, [..items])", "(x, [..items])")]
    [InlineData("(x, _)", "(x, _)")]
    [InlineData("(x /* implementation comment */, y)", "(x, y)")]
    [InlineData("_", "_")]
    [InlineData("(@class, value)", "(@class, value)")]
    [InlineData("Row(let x)", "Row(let x)")]
    [InlineData("{Value: let x}", "{ Value: let x }")]
    public void DecodedPatternsRetainNormalizedDisplay(string text, string expected)
    {
        var pattern = Assert.IsAssignableFrom<PatternSyntax>(ParameterPatternFacts.Decode(1, text));
        Assert.Equal(expected, ParameterPatternFacts.GetDisplayText(pattern));
        Assert.Equal(expected, ParameterPatternFacts.GetDisplayText(ParameterPatternFacts.Decode(1, expected)!));
    }

    [Theory]
    [InlineData(2, "(x, y)")]
    [InlineData(1, "(x,")]
    [InlineData(1, "(x, y) extra")]
    [InlineData(1, "")]
    [InlineData(1, null)]
    [InlineData(1, "   ")]
    public void UnsupportedOrMalformedMetadataHasNoPattern(int version, string? text)
        => Assert.Null(ParameterPatternFacts.Decode(version, text));

    [Fact]
    public void OversizedOrDeepMetadataIsRejectedBeforeParsing()
    {
        Assert.Null(ParameterPatternFacts.Decode(1, new string('x', 16385)));
        Assert.Null(ParameterPatternFacts.Decode(1, new string('(', 129) + "x" + new string(')', 129)));
    }

    [Theory]
    [InlineData("(left, right)", "(left, right)")]
    [InlineData("_", "_")]
    [InlineData("value", null)]
    public void SourceParametersRetainPatternsAndDiscardFallback(string declaration, string? expected)
    {
        var tree = SyntaxTree.ParseText($"func F({declaration}: int) {{ }}");
        var syntax = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var parameter = DispatchProxy.Create<Symbol, SourceSymbolProxy>();
        ((SourceSymbolProxy)parameter).References = ImmutableArray.Create(syntax.GetReference());
        var pattern = ParameterPatternFacts.GetSourcePattern(parameter);
        if (expected is null) Assert.Null(pattern);
        else Assert.Equal(expected, ParameterPatternFacts.GetDisplayText(Assert.IsAssignableFrom<PatternSyntax>(pattern)));
    }

    [Fact]
    public void MissingSourceReferenceHasNoPattern()
    {
        var parameter = DispatchProxy.Create<Symbol, SourceSymbolProxy>();
        Assert.Null(ParameterPatternFacts.GetSourcePattern(parameter));
    }

    [Fact]
    public void OptionalDefaultMarkerRetainsSingletonReferenceSemantics()
    {
        var marker = OptionNoneParameterDefaultValue.Instance;
        Assert.Same(marker, OptionNoneParameterDefaultValue.Instance);
        Assert.Equal(".None", marker.ToString());
        Assert.Empty(typeof(OptionNoneParameterDefaultValue).GetConstructors());
        Assert.True(typeof(OptionNoneParameterDefaultValue).IsSealed);
    }

    public class SourceSymbolProxy : DispatchProxy
    {
        public ImmutableArray<SyntaxReference> References = [];
        protected override object? Invoke(MethodInfo? method, object?[]? arguments)
            => method!.Name == "get_DeclaringSyntaxReferences" ? References : throw new InvalidOperationException(method.Name);
    }
}
