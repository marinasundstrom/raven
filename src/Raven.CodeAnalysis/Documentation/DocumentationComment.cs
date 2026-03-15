using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Documentation;

public sealed class DocumentationComment
{
    private DocumentationComment(
        DocumentationFormat format,
        string rawText,
        string content,
        string body,
        ImmutableArray<DocumentationTag> tags,
        bool isMultiline)
    {
        Format = format;
        RawText = rawText;
        Content = content;
        Body = body;
        Tags = tags;
        IsMultiline = isMultiline;
    }

    public DocumentationFormat Format { get; }

    public string RawText { get; }

    public string Content { get; }

    public string Body { get; }

    public ImmutableArray<DocumentationTag> Tags { get; }

    public bool IsMultiline { get; }

    internal static DocumentationComment Create(
        DocumentationFormat format,
        string content,
        string? rawText = null)
    {
        var normalizedContent = content ?? string.Empty;
        var normalizedRaw = rawText ?? normalizedContent;
        var isMultiline = normalizedContent.AsSpan().IndexOfAny('\n', '\r') >= 0;
        ParseStructuredParts(format, normalizedContent, out var body, out var tags);
        return new DocumentationComment(format, normalizedRaw, normalizedContent, body, tags, isMultiline);
    }

    internal static DocumentationComment Merge(DocumentationFormat format, IReadOnlyList<DocumentationComment> comments)
    {
        if (comments.Count == 0)
            throw new ArgumentException("At least one comment is required to merge", nameof(comments));

        if (comments.Count == 1)
            return comments[0];

        var raw = string.Join(Environment.NewLine, comments.Select(static c => c.RawText));
        var content = string.Join(Environment.NewLine, comments.Select(static c => c.Content));
        var body = string.Join(Environment.NewLine, comments.Select(static c => c.Body));
        var tags = comments.SelectMany(static c => c.Tags).ToImmutableArray();
        var isMultiline = comments.Any(static c => c.IsMultiline);

        return new DocumentationComment(format, raw, content, body, tags, isMultiline);
    }

    public static bool TryParse(SyntaxTrivia trivia, DocumentationFormat format, out DocumentationComment? comment)
    {
        comment = null;

        if (!IsDocumentationTrivia(trivia.Kind))
            return false;

        var raw = trivia.Text;
        // Doc comments are represented by a single trivia kind; treat a block as multiline if it spans multiple lines.
        var isMultiline = raw.AsSpan().IndexOfAny('\n', '\r') >= 0;
        var content = Normalize(raw);
        ParseStructuredParts(format, content, out var body, out var tags);
        comment = new DocumentationComment(format, raw, content, body, tags, isMultiline);
        return true;
    }

    private static void ParseStructuredParts(
        DocumentationFormat format,
        string content,
        out string body,
        out ImmutableArray<DocumentationTag> tags)
    {
        if (format != DocumentationFormat.Markdown || string.IsNullOrWhiteSpace(content))
        {
            body = content;
            tags = ImmutableArray<DocumentationTag>.Empty;
            return;
        }

        ParseMarkdownTags(content, out body, out tags);
    }

    private static bool IsDocumentationTrivia(SyntaxKind kind) => kind is SyntaxKind.DocumentationCommentTrivia;

    private static string Normalize(string rawText)
    {
        if (string.IsNullOrEmpty(rawText))
            return string.Empty;

        return TrimCommentLine(rawText);
    }

    private static string TrimCommentLine(string rawText)
    {
        var lines = rawText.Replace("\r\n", "\n", StringComparison.Ordinal).Split('\n');

        // 1) Strip indentation before "///" (if any), then strip the "///" marker.
        // We keep indentation *after* the marker for now (we need it to compute common indent).
        var stripped = new List<string>(lines.Length);
        foreach (var line in lines)
            stripped.Add(RemoveDocLinePrefix(line, "///"));

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

    private static string RemoveDocLinePrefix(string line, string prefix)
    {
        if (string.IsNullOrEmpty(line))
            return string.Empty;

        // Remove any indentation before the documentation marker.
        int i = 0;
        while (i < line.Length)
        {
            char ch = line[i];
            if (ch == ' ' || ch == '\t')
            {
                i++;
                continue;
            }

            break;
        }

        // If the line is whitespace-only, keep it empty.
        if (i >= line.Length)
            return string.Empty;

        // If we don't have the marker, just trim leading whitespace.
        if (!line.AsSpan(i).StartsWith(prefix.AsSpan(), StringComparison.Ordinal))
            return line.Substring(i);

        i += prefix.Length;

        // Allow one optional space after the marker.
        if (i < line.Length && line[i] == ' ')
            i++;

        return (i >= line.Length) ? string.Empty : line.Substring(i);
    }

    private static string RemovePrefix(string text, string prefix)
    {
        if (text.StartsWith(prefix, StringComparison.Ordinal))
            return text.Substring(prefix.Length);

        if (text.StartsWith(prefix + " ", StringComparison.Ordinal))
            return text.Substring((prefix + " ").Length);

        return text;
    }

    private static void ParseMarkdownTags(
        string content,
        out string body,
        out ImmutableArray<DocumentationTag> tags)
    {
        var normalized = content.Replace("\r\n", "\n", StringComparison.Ordinal);
        var lines = normalized.Split('\n');
        var bodyLines = new List<string>();
        var tagBuilder = ImmutableArray.CreateBuilder<DocumentationTag>();
        DocumentationTagBuilder? currentTag = null;

        foreach (var line in lines)
        {
            if (TryParseTagHeader(line, out var header))
            {
                if (currentTag is not null)
                    tagBuilder.Add(currentTag.Build());

                currentTag = header;
                continue;
            }

            if (currentTag is not null)
            {
                currentTag.AppendLine(line);
            }
            else
            {
                bodyLines.Add(line);
            }
        }

        if (currentTag is not null)
            tagBuilder.Add(currentTag.Build());

        body = TrimBlankLines(string.Join("\n", bodyLines));
        tags = tagBuilder.ToImmutable();
    }

    private static bool TryParseTagHeader(string line, out DocumentationTagBuilder? tag)
    {
        tag = null;
        if (string.IsNullOrWhiteSpace(line))
            return false;

        var match = Regex.Match(line, @"^\s*@(?<name>[A-Za-z][A-Za-z0-9]*)\b(?<rest>.*)$");
        if (!match.Success)
            return false;

        var name = match.Groups["name"].Value;
        var rest = match.Groups["rest"].Value.Trim();
        var kind = GetTagKind(name);
        string? argument = null;
        var content = rest;

        if (kind is DocumentationTagKind.Param or DocumentationTagKind.TypeParam or DocumentationTagKind.Exception or DocumentationTagKind.See or DocumentationTagKind.SeeAlso)
        {
            SplitArgument(rest, out argument, out content);
        }

        tag = new DocumentationTagBuilder(kind, name, argument, content);
        return true;
    }

    private static DocumentationTagKind GetTagKind(string name)
    {
        return name switch
        {
            "param" => DocumentationTagKind.Param,
            "typeparam" => DocumentationTagKind.TypeParam,
            "returns" => DocumentationTagKind.Returns,
            "value" => DocumentationTagKind.Value,
            "remarks" => DocumentationTagKind.Remarks,
            "example" => DocumentationTagKind.Example,
            "exception" => DocumentationTagKind.Exception,
            "see" => DocumentationTagKind.See,
            "seealso" => DocumentationTagKind.SeeAlso,
            "inheritdoc" => DocumentationTagKind.InheritDoc,
            _ => DocumentationTagKind.Unknown
        };
    }

    private static void SplitArgument(string rest, out string? argument, out string content)
    {
        argument = null;
        content = string.Empty;

        if (string.IsNullOrWhiteSpace(rest))
            return;

        var separatorIndex = rest.IndexOfAny([' ', '\t']);
        if (separatorIndex < 0)
        {
            argument = rest;
            return;
        }

        argument = rest[..separatorIndex].Trim();
        content = rest[(separatorIndex + 1)..].Trim();
    }

    private static string TrimBlankLines(string text)
    {
        if (string.IsNullOrEmpty(text))
            return string.Empty;

        var lines = text.Replace("\r\n", "\n", StringComparison.Ordinal).Split('\n').ToList();
        while (lines.Count > 0 && string.IsNullOrWhiteSpace(lines[0]))
            lines.RemoveAt(0);
        while (lines.Count > 0 && string.IsNullOrWhiteSpace(lines[^1]))
            lines.RemoveAt(lines.Count - 1);
        return string.Join("\n", lines);
    }

    private sealed class DocumentationTagBuilder
    {
        private readonly StringBuilder _contentBuilder = new();

        public DocumentationTagBuilder(DocumentationTagKind kind, string name, string? argument, string firstLineContent)
        {
            Kind = kind;
            Name = name;
            Argument = string.IsNullOrWhiteSpace(argument) ? null : argument;
            if (!string.IsNullOrWhiteSpace(firstLineContent))
                _contentBuilder.Append(firstLineContent.Trim());
        }

        public DocumentationTagKind Kind { get; }
        public string Name { get; }
        public string? Argument { get; }

        public void AppendLine(string line)
        {
            if (_contentBuilder.Length > 0)
                _contentBuilder.Append('\n');

            _contentBuilder.Append(line);
        }

        public DocumentationTag Build()
            => new(Kind, Name, Argument, TrimBlankLines(_contentBuilder.ToString()));
    }
}
