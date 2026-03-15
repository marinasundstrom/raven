namespace Raven.CodeAnalysis;

public sealed record ProjectDocumentationOptions(
    bool GenerateXmlDocumentation = false,
    bool GenerateMarkdownDocumentation = false,
    string? XmlDocumentationFile = null,
    string? MarkdownDocumentationOutputPath = null);
