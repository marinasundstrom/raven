using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class ParseOptions
{
    public ParseOptions()
    {
        DocumentationMode = true;
        DocumentationFormat = DocumentationFormat.Markdown;
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
        PreprocessorSymbolNames = preprocessorSymbolNames;
        DocumentationFormat = documentationFormat;
    }

    public bool DocumentationMode { get; set; }

    public DocumentationFormat DocumentationFormat { get; set; }

    public ImmutableArray<Diagnostic> Errors { get; }

    public IReadOnlyDictionary<string, string> Features { get; set; }

    public SourceCodeKind Kind { get; set; }

    public IEnumerable<string> PreprocessorSymbolNames { get; set; }
}

public enum SourceCodeKind
{
    Regular = 0,
    Script = 1,
    Interactive = 2
}