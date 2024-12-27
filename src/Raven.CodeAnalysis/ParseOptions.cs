using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class ParseOptions
{
    public ParseOptions()
    {

    }
    
    public ParseOptions(bool documentationMode, ImmutableArray<Diagnostic> errors, IReadOnlyDictionary<string, string> features, SourceCodeKind kind, IEnumerable<string> preprocessorSymbolNames)
    {
        DocumentationMode = documentationMode;
        Errors = errors;
        Features = features;
        Kind = kind;
        PreprocessorSymbolNames = preprocessorSymbolNames;
    }

    public bool DocumentationMode { get; set; }
    
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
