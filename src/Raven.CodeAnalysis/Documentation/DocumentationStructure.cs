using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Documentation;

public sealed record DocumentationStructure(
    DocumentationFormat SourceFormat,
    string Summary,
    string Body,
    string AdditionalBody,
    string? Returns,
    string? Value,
    string? Remarks,
    string? Example,
    string? InheritDocReference,
    ImmutableArray<DocumentationEntry> TypeParameters,
    ImmutableArray<DocumentationEntry> Parameters,
    ImmutableArray<DocumentationEntry> Exceptions,
    ImmutableArray<DocumentationEntry> See,
    ImmutableArray<DocumentationEntry> SeeAlso);
