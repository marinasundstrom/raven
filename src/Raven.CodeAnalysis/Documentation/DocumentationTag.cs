namespace Raven.CodeAnalysis.Documentation;

public sealed record DocumentationTag(
    DocumentationTagKind Kind,
    string Name,
    string? Argument,
    string Content);
