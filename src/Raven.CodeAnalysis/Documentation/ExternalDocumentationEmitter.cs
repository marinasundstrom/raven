using System.Security;
using System.Text;
using System.Text.Json;
using System.Xml.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Documentation;

internal static class ExternalDocumentationEmitter
{
    private const string InvariantLocale = "invariant";
    private const string SymbolsPath = "symbols";

    public static string WriteDocumentation(
        Compilation compilation,
        DocumentationFormat format,
        string outputPath,
        bool includeMarkdownWhenEmittingXml = true)
    {
        return format == DocumentationFormat.Markdown
            ? WriteMarkdownDocumentation(compilation, outputPath)
            : WriteXmlDocumentation(compilation, outputPath, includeMarkdownWhenEmittingXml);
    }

    public static string WriteMarkdownDocumentation(Compilation compilation, string outputPath)
    {
        var docsRoot = NormalizeMarkdownOutputPath(outputPath);
        var entries = Collect(compilation).ToList();

        if (Directory.Exists(docsRoot))
            Directory.Delete(docsRoot, recursive: true);

        Directory.CreateDirectory(docsRoot);
        var symbolsRoot = Path.Combine(docsRoot, InvariantLocale, SymbolsPath);
        Directory.CreateDirectory(symbolsRoot);

        foreach (var entry in entries)
        {
            var memberDirectory = Path.Combine(symbolsRoot, entry.MemberId[0].ToString());
            Directory.CreateDirectory(memberDirectory);
            var filePath = Path.Combine(memberDirectory, DocumentationCommentIdBuilder.GetMarkdownPathHash(entry.MemberId) + ".md");
            var markdown = MarkdownDocumentationFrontMatter.Write(
                entry.Comment.Content,
                ("xref", entry.MemberId));
            File.WriteAllText(filePath, markdown);
        }

        var manifest = new
        {
            formatVersion = 1,
            assemblyName = compilation.AssemblyName,
            documentationFormat = "markdown",
            idFormat = "doc-comment-id",
            defaultLocale = InvariantLocale,
            locales = new[] { InvariantLocale },
            symbolsPath = SymbolsPath
        };

        File.WriteAllText(
            Path.Combine(docsRoot, "manifest.json"),
            JsonSerializer.Serialize(manifest, new JsonSerializerOptions { WriteIndented = true }));

        return docsRoot;
    }

    public static string WriteXmlDocumentation(Compilation compilation, string outputPath, bool includeMarkdownWhenEmittingXml = true)
    {
        var xmlPath = NormalizeXmlOutputPath(outputPath);
        var directory = Path.GetDirectoryName(xmlPath);
        if (!string.IsNullOrEmpty(directory))
            Directory.CreateDirectory(directory);

        var entries = Collect(compilation, includeMarkdownWhenEmittingXml)
            .OrderBy(static entry => entry.MemberId, StringComparer.Ordinal)
            .ToList();

        var builder = new StringBuilder();
        builder.AppendLine("<?xml version=\"1.0\"?>");
        builder.AppendLine("<doc>");
        builder.AppendLine("  <assembly>");
        builder.Append("    <name>");
        builder.Append(SecurityElement.Escape(compilation.AssemblyName));
        builder.AppendLine("</name>");
        builder.AppendLine("  </assembly>");
        builder.AppendLine("  <members>");

        foreach (var entry in entries)
        {
            builder.Append("    <member name=\"");
            builder.Append(SecurityElement.Escape(entry.MemberId));
            builder.AppendLine("\">");
            AppendXmlMemberBody(builder, entry.Comment);
            builder.AppendLine("    </member>");
        }

        builder.AppendLine("  </members>");
        builder.AppendLine("</doc>");
        File.WriteAllText(xmlPath, builder.ToString());

        return xmlPath;
    }

    private static IEnumerable<DocumentationEntry> Collect(Compilation compilation, bool includeMarkdown = true)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        foreach (var symbol in EnumerateDocumentableSymbols(compilation.Assembly.GlobalNamespace))
        {
            if (symbol.IsImplicitlyDeclared)
                continue;

            var comment = symbol.GetDocumentationComment();
            if (comment is null || string.IsNullOrWhiteSpace(comment.Content))
                continue;

            if (!includeMarkdown &&
                comment.Format == DocumentationFormat.Markdown &&
                !LooksLikeXmlDocumentation(comment.Content))
                continue;

            if (!DocumentationCommentIdBuilder.TryGetMemberId(symbol, out var memberId))
                continue;

            if (!seen.Add(memberId))
                continue;

            yield return new DocumentationEntry(memberId, comment);
        }
    }

    private static IEnumerable<ISymbol> EnumerateDocumentableSymbols(INamespaceSymbol namespaceSymbol)
    {
        foreach (var member in namespaceSymbol.GetMembers())
        {
            switch (member)
            {
                case INamespaceSymbol nestedNamespace:
                    foreach (var nested in EnumerateDocumentableSymbols(nestedNamespace))
                        yield return nested;
                    break;
                case INamedTypeSymbol namedType:
                    foreach (var nested in EnumerateDocumentableSymbols(namedType))
                        yield return nested;
                    break;
            }
        }
    }

    private static IEnumerable<ISymbol> EnumerateDocumentableSymbols(INamedTypeSymbol typeSymbol)
    {
        yield return typeSymbol;

        foreach (var member in typeSymbol.GetMembers())
        {
            if (member is INamedTypeSymbol nestedType)
            {
                foreach (var nested in EnumerateDocumentableSymbols(nestedType))
                    yield return nested;
                continue;
            }

            if (member is IMethodSymbol { AssociatedSymbol: not null })
                continue;

            if (member is not IMethodSymbol and not IPropertySymbol and not IFieldSymbol and not IEventSymbol)
                continue;

            yield return member;
        }
    }

    private static void AppendXmlMemberBody(StringBuilder builder, DocumentationComment comment)
    {
        var body = comment.Content.Trim();
        if (string.IsNullOrWhiteSpace(body))
        {
            builder.AppendLine("      <summary />");
            return;
        }

        if (comment.Format == DocumentationFormat.Xml && TryNormalizeXmlFragment(body, out var fragment))
        {
            foreach (var line in fragment!.Split(Environment.NewLine))
            {
                builder.Append("      ");
                builder.AppendLine(line);
            }

            return;
        }

        if (comment.Format == DocumentationFormat.Markdown)
        {
            AppendMarkdownAsXml(builder, comment);
            return;
        }

        builder.Append("      <summary>");
        builder.Append(SecurityElement.Escape(body));
        builder.AppendLine("</summary>");
    }

    private static void AppendMarkdownAsXml(StringBuilder builder, DocumentationComment comment)
    {
        var structure = DocumentationStructureExtractor.Extract(comment);

        AppendXmlElement(builder, "summary", MarkdownDocumentationTextConverter.ToPlainText(structure.Summary));

        foreach (var entry in structure.TypeParameters)
            AppendXmlElement(builder, "typeparam", MarkdownDocumentationTextConverter.ToPlainText(entry.Content), ("name", entry.Name));

        foreach (var entry in structure.Parameters)
            AppendXmlElement(builder, "param", MarkdownDocumentationTextConverter.ToPlainText(entry.Content), ("name", entry.Name));

        AppendXmlElement(builder, "returns", MarkdownDocumentationTextConverter.ToPlainText(structure.Returns));
        AppendXmlElement(builder, "value", MarkdownDocumentationTextConverter.ToPlainText(structure.Value));
        AppendXmlElement(builder, "remarks", MarkdownDocumentationTextConverter.ToPlainText(structure.Remarks));
        AppendXmlElement(builder, "example", MarkdownDocumentationTextConverter.ToPlainText(structure.Example));

        foreach (var entry in structure.Exceptions)
            AppendXmlElement(builder, "exception", MarkdownDocumentationTextConverter.ToPlainText(entry.Content), ("cref", entry.Reference));

        foreach (var entry in structure.See)
            AppendXmlElement(builder, "see", MarkdownDocumentationTextConverter.ToPlainText(entry.Content), ("cref", entry.Reference));

        foreach (var entry in structure.SeeAlso)
            AppendXmlElement(builder, "seealso", MarkdownDocumentationTextConverter.ToPlainText(entry.Content), ("cref", entry.Reference));

        if (!string.IsNullOrWhiteSpace(structure.InheritDocReference))
            AppendXmlElement(builder, "inheritdoc", attributes: [("cref", structure.InheritDocReference)]);
    }

    private static void AppendXmlElement(
        StringBuilder builder,
        string elementName,
        string? content = null,
        params (string Name, string? Value)[] attributes)
    {
        var hasAnyAttributes = attributes.Any(static attribute => !string.IsNullOrWhiteSpace(attribute.Name) && !string.IsNullOrWhiteSpace(attribute.Value));
        var normalizedContent = string.IsNullOrWhiteSpace(content) ? null : content.Trim();

        if (normalizedContent is null && !hasAnyAttributes)
            return;

        builder.Append("      <");
        builder.Append(elementName);

        foreach (var (name, value) in attributes)
        {
            if (string.IsNullOrWhiteSpace(name) || string.IsNullOrWhiteSpace(value))
                continue;

            builder.Append(' ');
            builder.Append(name);
            builder.Append("=\"");
            builder.Append(SecurityElement.Escape(value));
            builder.Append('"');
        }

        if (normalizedContent is null)
        {
            builder.AppendLine(" />");
            return;
        }

        builder.Append('>');
        builder.Append(SecurityElement.Escape(normalizedContent));
        builder.Append("</");
        builder.Append(elementName);
        builder.AppendLine(">");
    }

    private static bool TryNormalizeXmlFragment(string content, out string? fragment)
    {
        fragment = null;

        try
        {
            var wrapper = XElement.Parse("<root>" + content + "</root>");
            fragment = string.Join(
                Environment.NewLine,
                wrapper.Nodes().Select(static node => node.ToString(SaveOptions.DisableFormatting)));
            return true;
        }
        catch
        {
            return false;
        }
    }

    private static bool LooksLikeXmlDocumentation(string content)
    {
        if (string.IsNullOrWhiteSpace(content))
            return false;

        return content.TrimStart().StartsWith("<", StringComparison.Ordinal);
    }

    private static string NormalizeMarkdownOutputPath(string outputPath)
    {
        if (string.IsNullOrWhiteSpace(outputPath))
            throw new ArgumentException("Output path is required.", nameof(outputPath));

        if (outputPath.EndsWith(".docs", StringComparison.OrdinalIgnoreCase))
            return outputPath;

        return Path.HasExtension(outputPath)
            ? Path.ChangeExtension(outputPath, ".docs")
            : outputPath;
    }

    private static string NormalizeXmlOutputPath(string outputPath)
    {
        if (string.IsNullOrWhiteSpace(outputPath))
            throw new ArgumentException("Output path is required.", nameof(outputPath));

        return Path.HasExtension(outputPath)
            ? Path.ChangeExtension(outputPath, ".xml")
            : outputPath + ".xml";
    }

    private sealed record DocumentationEntry(string MemberId, DocumentationComment Comment);
}
