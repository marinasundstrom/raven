using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class ParseOptionsContractTests
{
    [Fact]
    public void Defaults_PreserveOrdinaryDotNetParsingOptions()
    {
        var options = new ParseOptions();

        Assert.True(options.EnableCharKeyword);
        Assert.True(options.DocumentationMode);
        Assert.Equal(DocumentationFormat.Markdown, options.DocumentationFormat);
        Assert.Equal(SourceCodeKind.Regular, options.Kind);
        Assert.False(options.ThrowOnIncrementalParseFallback);
        Assert.Empty(options.Errors);
        Assert.Empty(options.Features);
        Assert.Empty(options.PreprocessorSymbolNames);
    }

    [Fact]
    public void Symbols_AreTrimmedDistinctAndSortedOrdinally()
    {
        var names = new[] { " z ", "A", "", " ", "A", "a", "z", null! };
        var options = new ParseOptions().WithPreprocessorSymbols(names);
        names[1] = "mutated";

        Assert.Equal(new[] { "A", "a", "z" }, options.PreprocessorSymbolNames);
        Assert.Empty(options.WithPreprocessorSymbols(null!).PreprocessorSymbolNames);
    }

    [Fact]
    public void Snapshot_IsolatesMutableFeaturesAndSymbols()
    {
        var features = new Dictionary<string, string> { ["feature"] = "enabled" };
        var names = new[] { "B", " A " };
        var options = new ParseOptions { Features = features, PreprocessorSymbolNames = names };

        var snapshot = options.Snapshot();
        features["feature"] = "changed";
        features["new"] = "enabled";
        names[0] = "changed";

        Assert.Equal("enabled", snapshot.Features["feature"]);
        Assert.Single(snapshot.Features);
        Assert.Equal(new[] { "A", "B" }, snapshot.PreprocessorSymbolNames);
        Assert.False(options.IsEquivalentTo(snapshot));
    }

    [Fact]
    public void CopyingSymbols_PreservesAllOtherOptions()
    {
        var options = new ParseOptions(false, ImmutableArray<Diagnostic>.Empty,
            ImmutableDictionary<string, string>.Empty.Add("feature", "enabled"),
            SourceCodeKind.Script, new[] { "A" }, DocumentationFormat.Xml)
        {
            EnableCharKeyword = false,
            ThrowOnIncrementalParseFallback = true
        };

        var snapshot = options.Snapshot();
        Assert.True(options.IsEquivalentTo(snapshot));
        Assert.True(snapshot.IsEquivalentTo(options));
        var changed = snapshot.WithPreprocessorSymbols(new[] { " b ", "B", "b" });

        Assert.Equal(new[] { "B", "b" }, changed.PreprocessorSymbolNames);
        Assert.Equal(new[] { "A" }, snapshot.PreprocessorSymbolNames);
        Assert.False(changed.EnableCharKeyword);
        Assert.False(changed.DocumentationMode);
        Assert.Equal(DocumentationFormat.Xml, changed.DocumentationFormat);
        Assert.Equal(SourceCodeKind.Script, changed.Kind);
        Assert.True(changed.ThrowOnIncrementalParseFallback);
        Assert.Same(snapshot.Features, changed.Features);
        Assert.Equal(snapshot.Errors, changed.Errors);
        Assert.False(snapshot.IsEquivalentTo(changed));
    }

    [Fact]
    public void Equivalence_UsesOrdinalValuesAndIgnoresInsertionOrder()
    {
        var options = new ParseOptions { Features = new Dictionary<string, string> { ["a"] = "one", ["b"] = "two" } };
        var reordered = new ParseOptions { Features = new Dictionary<string, string> { ["b"] = "two", ["a"] = "one" } };
        var changedValue = new ParseOptions { Features = new Dictionary<string, string> { ["a"] = "ONE", ["b"] = "two" } };

        Assert.True(options.IsEquivalentTo(reordered));
        Assert.True(reordered.IsEquivalentTo(options));
        Assert.False(options.IsEquivalentTo(changedValue));
    }
}
