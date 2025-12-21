using System.Globalization;
using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Documentation;

public sealed class DocumentationComment
{
    private DocumentationComment(DocumentationFormat format, string rawText, string content, bool isMultiline)
    {
        Format = format;
        RawText = rawText;
        Content = content;
        IsMultiline = isMultiline;
    }

    public DocumentationFormat Format { get; }

    public string RawText { get; }

    public string Content { get; }

    public bool IsMultiline { get; }

    public static bool TryParse(SyntaxTrivia trivia, DocumentationFormat format, out DocumentationComment? comment)
    {
        comment = null;

        if (!IsDocumentationTrivia(trivia.Kind))
            return false;

        var raw = trivia.Text;
        var isMultiline = trivia.Kind == SyntaxKind.MultiLineDocumentationCommentTrivia;
        var content = Normalize(raw, isMultiline);

        comment = new DocumentationComment(format, raw, content, isMultiline);
        return true;
    }

    private static bool IsDocumentationTrivia(SyntaxKind kind) => kind is SyntaxKind.SingleLineDocumentationCommentTrivia or SyntaxKind.MultiLineDocumentationCommentTrivia;

    private static string Normalize(string rawText, bool isMultiline)
    {
        if (string.IsNullOrEmpty(rawText))
            return string.Empty;

        if (!isMultiline)
        {
            return TrimSingleLine(rawText);
        }

        return TrimMultiLine(rawText);
    }

    private static string TrimSingleLine(string rawText)
    {
        // Split conservatively to preserve intentional blank lines.
        var lines = rawText.Replace("\r\n", "\n", StringComparison.Ordinal).Split('\n');
        var builder = new StringBuilder();

        foreach (var line in lines)
        {
            var trimmed = RemovePrefix(line, "///");
            builder.AppendLine(trimmed);
        }

        return builder.ToString().TrimEnd();
    }

    private static string TrimMultiLine(string rawText)
    {
        var content = rawText;

        if (content.StartsWith("/*", true, CultureInfo.InvariantCulture))
        {
            content = content.Substring(2);
        }

        if (content.EndsWith("*/", true, CultureInfo.InvariantCulture))
        {
            content = content.Substring(0, content.Length - 2);
        }

        // Drop the leading '*' often present in block doc comments.
        var lines = content.Replace("\r\n", "\n", StringComparison.Ordinal).Split('\n');
        var builder = new StringBuilder();

        foreach (var line in lines)
        {
            var trimmed = RemovePrefix(line, "*");
            builder.AppendLine(trimmed.TrimEnd());
        }

        return builder.ToString().TrimEnd();
    }

    private static string RemovePrefix(string text, string prefix)
    {
        if (text.StartsWith(prefix, StringComparison.Ordinal))
            return text.Substring(prefix.Length).TrimStart();

        return text.TrimStart();
    }
}
