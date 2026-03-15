using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;

namespace Raven;

internal static class DocumentationEmitter
{
    public static string WriteDocumentation(
        Compilation compilation,
        DocumentationFormat format,
        string outputPath,
        bool includeMarkdownWhenEmittingXml = true)
        => ExternalDocumentationEmitter.WriteDocumentation(compilation, format, outputPath, includeMarkdownWhenEmittingXml);
}
