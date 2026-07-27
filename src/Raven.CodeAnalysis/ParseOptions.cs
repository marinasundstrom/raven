using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class ParseOptions
{
    public ParseOptions()
    {
        DocumentationMode = true;
        DocumentationFormat = DocumentationFormat.Markdown;
        Errors = ImmutableArray<Diagnostic>.Empty;
        Features = ImmutableDictionary<string, string>.Empty;
        PreprocessorSymbolNames = Array.Empty<string>();
    }

    public ParseOptions(
        bool documentationMode,
        ImmutableArray<Diagnostic> errors,
        IReadOnlyDictionary<string, string> features,
        SourceCodeKind kind,
        IEnumerable<string> preprocessorSymbolNames,
        DocumentationFormat documentationFormat = DocumentationFormat.Markdown)
    {
        DocumentationMode = documentationMode;
        Errors = errors;
        Features = features;
        Kind = kind;
        PreprocessorSymbolNames = NormalizePreprocessorSymbolNames(preprocessorSymbolNames);
        DocumentationFormat = documentationFormat;
    }

    public bool DocumentationMode { get; init; }

    public DocumentationFormat DocumentationFormat { get; init; }

    public ImmutableArray<Diagnostic> Errors { get; }

    public IReadOnlyDictionary<string, string> Features { get; init; }

    public SourceCodeKind Kind { get; init; }

    public IEnumerable<string> PreprocessorSymbolNames { get; init; }

    public ParseOptions WithPreprocessorSymbols(IEnumerable<string> preprocessorSymbolNames)
        => new(
            DocumentationMode,
            Errors,
            Features,
            Kind,
            preprocessorSymbolNames,
            DocumentationFormat);

    internal ParseOptions Snapshot()
        => new(
            DocumentationMode,
            Errors,
            Features.ToImmutableDictionary(StringComparer.Ordinal),
            Kind,
            PreprocessorSymbolNames,
            DocumentationFormat);

    internal bool IsEquivalentTo(ParseOptions other)
        => DocumentationMode == other.DocumentationMode &&
           DocumentationFormat == other.DocumentationFormat &&
           Kind == other.Kind &&
           Errors.SequenceEqual(other.Errors) &&
           PreprocessorSymbolNames.SequenceEqual(other.PreprocessorSymbolNames, StringComparer.Ordinal) &&
           Features.Count == other.Features.Count &&
           Features.All(pair =>
               other.Features.TryGetValue(pair.Key, out var value) &&
               string.Equals(pair.Value, value, StringComparison.Ordinal));

    private static ImmutableArray<string> NormalizePreprocessorSymbolNames(IEnumerable<string>? names)
    {
        if (names is null)
            return ImmutableArray<string>.Empty;

        return names
            .Where(static name => !string.IsNullOrWhiteSpace(name))
            .Select(static name => name.Trim())
            .Distinct(StringComparer.Ordinal)
            .OrderBy(static name => name, StringComparer.Ordinal)
            .ToImmutableArray();
    }
}

public enum SourceCodeKind
{
    Regular = 0,
    Script = 1,
    Interactive = 2
}
