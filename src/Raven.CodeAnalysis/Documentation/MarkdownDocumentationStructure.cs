using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Documentation;

public sealed record MarkdownDocumentationStructure(
    string Summary,
    string Body,
    string AdditionalBody,
    string? Returns,
    string? Value,
    string? Remarks,
    string? Example,
    string? InheritDocReference,
    ImmutableArray<MarkdownDocumentationEntry> TypeParameters,
    ImmutableArray<MarkdownDocumentationEntry> Parameters,
    ImmutableArray<MarkdownDocumentationEntry> Exceptions,
    ImmutableArray<MarkdownDocumentationEntry> See,
    ImmutableArray<MarkdownDocumentationEntry> SeeAlso);
