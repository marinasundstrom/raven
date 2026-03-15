using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;

namespace Raven;

internal static class DocumentationEmitter
{
    public static string WriteDocumentation(Compilation compilation, DocumentationFormat format, string outputPath)
        => ExternalDocumentationEmitter.WriteDocumentation(compilation, format, outputPath);
}
