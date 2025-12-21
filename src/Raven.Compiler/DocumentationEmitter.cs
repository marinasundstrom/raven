using System.Linq;
using System.Security;
using System.Text;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

namespace Raven;

internal static class DocumentationEmitter
{
    public static void WriteDocumentation(IEnumerable<Document> documents, DocumentationFormat format, string outputPath)
    {
        var entries = Collect(documents, format).ToList();
        var directory = Path.GetDirectoryName(outputPath);
        if (!string.IsNullOrEmpty(directory))
            Directory.CreateDirectory(directory);

        var content = format == DocumentationFormat.Markdown
            ? RenderMarkdown(entries)
            : RenderXml(entries);

        File.WriteAllText(outputPath, content);
    }

    private static IEnumerable<DocumentationEntry> Collect(IEnumerable<Document> documents, DocumentationFormat format)
    {
        foreach (var document in documents)
        {
            var syntaxTree = document.GetSyntaxTreeAsync().Result;
            if (syntaxTree is null)
                continue;

            var root = syntaxTree.GetRoot();
            foreach (var trivia in root.DescendantTrivia())
            {
                if (!DocumentationComment.TryParse(trivia, format, out var comment))
                    continue;

                var span = trivia.GetLocation().GetLineSpan();
                var position = span.StartLinePosition;
                yield return new DocumentationEntry(document.FilePath ?? document.Name, position.Line + 1, comment!.Content);
            }
        }
    }

    private static string RenderMarkdown(IEnumerable<DocumentationEntry> entries)
    {
        var builder = new StringBuilder();
        builder.AppendLine("# Raven documentation comments");
        builder.AppendLine();

        foreach (var group in entries.GroupBy(e => e.FilePath).OrderBy(g => g.Key, StringComparer.OrdinalIgnoreCase))
        {
            builder.AppendLine($"## {group.Key}");
            builder.AppendLine();

            foreach (var entry in group.OrderBy(e => e.LineNumber))
            {
                builder.AppendLine($"- Line {entry.LineNumber}: {entry.Content}");
            }

            builder.AppendLine();
        }

        return builder.ToString().TrimEnd();
    }

    private static string RenderXml(IEnumerable<DocumentationEntry> entries)
    {
        var builder = new StringBuilder();
        builder.AppendLine("<documentation>");

        foreach (var entry in entries.OrderBy(e => e.FilePath, StringComparer.OrdinalIgnoreCase).ThenBy(e => e.LineNumber))
        {
            builder.Append("  <comment file=\"");
            builder.Append(SecurityElement.Escape(entry.FilePath));
            builder.Append("\" line=\"");
            builder.Append(entry.LineNumber);
            builder.Append("\">");
            builder.Append(SecurityElement.Escape(entry.Content));
            builder.AppendLine("</comment>");
        }

        builder.AppendLine("</documentation>");
        return builder.ToString();
    }

    private sealed record DocumentationEntry(string FilePath, int LineNumber, string Content);
}
