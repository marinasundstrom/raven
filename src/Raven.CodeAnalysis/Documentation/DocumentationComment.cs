using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using System.Linq;

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

    internal static DocumentationComment Merge(DocumentationFormat format, IReadOnlyList<DocumentationComment> comments)
    {
        if (comments.Count == 0)
            throw new ArgumentException("At least one comment is required to merge", nameof(comments));

        if (comments.Count == 1)
            return comments[0];

        var raw = string.Join(Environment.NewLine, comments.Select(static c => c.RawText));
        var content = string.Join(Environment.NewLine, comments.Select(static c => c.Content));
        var isMultiline = comments.Any(static c => c.IsMultiline);

        return new DocumentationComment(format, raw, content, isMultiline);
    }

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

        return TrimCommentLine(rawText);
    }

    private static string TrimCommentLine(string rawText)
    {
        var lines = rawText.Replace("\r\n", "\n", StringComparison.Ordinal).Split('\n');

        // 1) Strip "///" but keep indentation for now (we need it to compute common indent)
        var stripped = new List<string>(lines.Length);
        foreach (var line in lines)
            stripped.Add(RemovePrefix(line, "///"));

        // 2) Remove common indentation (spaces/tabs) across non-blank lines
        stripped = TrimCommonIndentation(stripped, tabSize: 4);

        // 3) Rebuild preserving blank lines
        var builder = new StringBuilder();
        foreach (var line in stripped)
            builder.AppendLine(line);

        return builder.ToString().TrimEnd();
    }

    private static List<string> TrimCommonIndentation(List<string> lines, int tabSize)
    {
        // Find minimal indent (in columns) across non-empty lines
        var minIndent = int.MaxValue;

        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line))
                continue;

            var indentCols = GetIndentColumns(line, tabSize);
            if (indentCols < minIndent)
                minIndent = indentCols;
        }

        if (minIndent == int.MaxValue || minIndent == 0)
            return lines;

        // Remove that indent from every line (in columns, respecting tab stops)
        for (int i = 0; i < lines.Count; i++)
            lines[i] = RemoveIndentColumns(lines[i], minIndent, tabSize);

        return lines;
    }

    private static int GetIndentColumns(string line, int tabSize)
    {
        int col = 0;

        for (int i = 0; i < line.Length; i++)
        {
            char ch = line[i];
            if (ch == ' ')
            {
                col++;
            }
            else if (ch == '\t')
            {
                // Advance to next tab stop
                col = ((col / tabSize) + 1) * tabSize;
            }
            else
            {
                break;
            }
        }

        return col;
    }

    private static string RemoveIndentColumns(string line, int columnsToRemove, int tabSize)
    {
        int col = 0;
        int i = 0;

        while (i < line.Length && col < columnsToRemove)
        {
            char ch = line[i];
            if (ch == ' ')
            {
                col++;
                i++;
            }
            else if (ch == '\t')
            {
                int next = ((col / tabSize) + 1) * tabSize;

                // If this tab would overshoot the target, we need to “partially remove” it:
                // replace the tab with spaces representing the remaining indent.
                if (next > columnsToRemove)
                {
                    int remaining = columnsToRemove - col;
                    // Keep the tab’s leftover width as spaces at the front of the remainder
                    // (so relative alignment remains stable).
                    var rest = line.Substring(i + 1);
                    return new string(' ', (next - columnsToRemove)) + rest;
                }

                col = next;
                i++;
            }
            else
            {
                break;
            }
        }

        return (i == 0) ? line : line.Substring(i);
    }

    private static string RemovePrefix(string text, string prefix)
    {
        if (text.StartsWith(prefix, StringComparison.Ordinal))
            return text.Substring(prefix.Length);

        if (text.StartsWith(prefix + " ", StringComparison.Ordinal))
            return text.Substring((prefix + " ").Length);

        return text.TrimStart();
    }
}
