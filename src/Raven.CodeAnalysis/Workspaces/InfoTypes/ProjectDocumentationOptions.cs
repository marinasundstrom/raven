namespace Raven.CodeAnalysis;

public sealed record ProjectDocumentationOptions(
    bool GenerateXmlDocumentation = false,
    bool GenerateMarkdownDocumentation = false,
    bool GenerateXmlDocumentationFromMarkdownComments = false,
    string? XmlDocumentationFile = null,
    string? MarkdownDocumentationOutputPath = null);
