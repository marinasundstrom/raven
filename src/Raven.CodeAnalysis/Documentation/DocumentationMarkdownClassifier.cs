using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Documentation;

/// <summary>Classifies Markdown constructs within a Raven documentation comment.</summary>
public static class DocumentationMarkdownClassifier
{
    /// <summary>Classifies Markdown constructs in <paramref name="trivia"/>.</summary>
    /// <param name="trivia">A documentation-comment trivia value.</param>
    /// <returns>Non-overlapping classified spans in source order.</returns>
    public static ImmutableArray<DocumentationMarkdownClassification> Classify(SyntaxTrivia trivia)
    {
        if (trivia.Kind != SyntaxKind.DocumentationCommentTrivia)
            return ImmutableArray<DocumentationMarkdownClassification>.Empty;

        var builder = ImmutableArray.CreateBuilder<DocumentationMarkdownClassification>();
        var text = trivia.Text;
        var lineStart = 0;
        char fenceCharacter = '\0';
        var fenceLength = 0;

        while (lineStart <= text.Length)
        {
            var lineEnd = text.IndexOf('\n', lineStart);
            if (lineEnd < 0)
                lineEnd = text.Length;

            var contentStart = GetContentStart(text, lineStart, lineEnd);
            var contentEnd = lineEnd > contentStart && text[lineEnd - 1] == '\r' ? lineEnd - 1 : lineEnd;

            if (contentStart < contentEnd)
            {
                var firstContent = SkipWhitespace(text, contentStart, contentEnd);
                var markerLength = GetFenceMarkerLength(text, firstContent, contentEnd);

                if (markerLength >= 3)
                {
                    var markerCharacter = text[firstContent];
                    if (fenceCharacter == '\0')
                    {
                        fenceCharacter = markerCharacter;
                        fenceLength = markerLength;
                    }
                    else if (markerCharacter == fenceCharacter && markerLength >= fenceLength)
                    {
                        fenceCharacter = '\0';
                        fenceLength = 0;
                    }

                    Add(builder, trivia, firstContent, markerLength, DocumentationMarkdownKind.Code);
                }
                else if (fenceCharacter != '\0')
                {
                    Add(builder, trivia, contentStart, contentEnd - contentStart, DocumentationMarkdownKind.Code);
                }
                else
                {
                    ClassifyLine(builder, trivia, text, contentStart, contentEnd);
                }
            }

            if (lineEnd == text.Length)
                break;

            lineStart = lineEnd + 1;
        }

        return builder
            .OrderBy(static classification => classification.Span.Start)
            .ToImmutableArray();
    }

    private static void ClassifyLine(
        ImmutableArray<DocumentationMarkdownClassification>.Builder builder,
        SyntaxTrivia trivia,
        string text,
        int contentStart,
        int contentEnd)
    {
        var firstContent = SkipWhitespace(text, contentStart, contentEnd);
        ClassifyHeading(builder, trivia, text, firstContent, contentEnd);
        ClassifyTag(builder, trivia, text, firstContent, contentEnd);

        var occupied = new List<(int Start, int End)>();
        ClassifyLinks(builder, occupied, trivia, text, contentStart, contentEnd);
        ClassifyCodeSpans(builder, occupied, trivia, text, contentStart, contentEnd);
    }

    private static void ClassifyHeading(
        ImmutableArray<DocumentationMarkdownClassification>.Builder builder,
        SyntaxTrivia trivia,
        string text,
        int start,
        int end)
    {
        var index = start;
        while (index < end && text[index] == '#')
            index++;

        var length = index - start;
        if (length is >= 1 and <= 6 && index < end && char.IsWhiteSpace(text[index]))
            Add(builder, trivia, start, length, DocumentationMarkdownKind.Heading);
    }

    private static void ClassifyTag(
        ImmutableArray<DocumentationMarkdownClassification>.Builder builder,
        SyntaxTrivia trivia,
        string text,
        int start,
        int end)
    {
        if (start >= end || text[start] != '@' || start + 1 >= end || !char.IsLetter(text[start + 1]))
            return;

        var index = start + 2;
        while (index < end && char.IsLetterOrDigit(text[index]))
            index++;

        Add(builder, trivia, start, index - start, DocumentationMarkdownKind.Tag);
    }

    private static void ClassifyCodeSpans(
        ImmutableArray<DocumentationMarkdownClassification>.Builder builder,
        List<(int Start, int End)> occupied,
        SyntaxTrivia trivia,
        string text,
        int start,
        int end)
    {
        var index = start;
        while (index < end)
        {
            if (text[index] != '`')
            {
                index++;
                continue;
            }

            var delimiterLength = CountRun(text, index, end, '`');
            var closing = FindRun(text, index + delimiterLength, end, '`', delimiterLength);
            if (closing < 0)
            {
                index += delimiterLength;
                continue;
            }

            var spanEnd = closing + delimiterLength;
            if (!occupied.Any(span => index < span.End && spanEnd > span.Start))
            {
                Add(builder, trivia, index, spanEnd - index, DocumentationMarkdownKind.Code);
                occupied.Add((index, spanEnd));
            }

            index = spanEnd;
        }
    }

    private static void ClassifyLinks(
        ImmutableArray<DocumentationMarkdownClassification>.Builder builder,
        List<(int Start, int End)> occupied,
        SyntaxTrivia trivia,
        string text,
        int start,
        int end)
    {
        var index = start;
        while (index < end)
        {
            var openBracket = text.IndexOf('[', index, end - index);
            if (openBracket < 0)
                return;

            var closeBracket = text.IndexOf(']', openBracket + 1, end - openBracket - 1);
            if (closeBracket < 0 || closeBracket + 1 >= end || text[closeBracket + 1] != '(')
            {
                index = openBracket + 1;
                continue;
            }

            var closeParenthesis = text.IndexOf(')', closeBracket + 2, end - closeBracket - 2);
            if (closeParenthesis < 0)
                return;

            var spanEnd = closeParenthesis + 1;
            if (!occupied.Any(span => openBracket < span.End && spanEnd > span.Start))
            {
                Add(builder, trivia, openBracket, spanEnd - openBracket, DocumentationMarkdownKind.Link);
                occupied.Add((openBracket, spanEnd));
            }

            index = spanEnd;
        }
    }

    private static int GetContentStart(string text, int lineStart, int lineEnd)
    {
        var index = SkipWhitespace(text, lineStart, lineEnd);
        if (index + 2 < lineEnd && text.AsSpan(index, 3).SequenceEqual("///"))
            index += 3;

        if (index < lineEnd && text[index] == ' ')
            index++;

        return index;
    }

    private static int SkipWhitespace(string text, int start, int end)
    {
        while (start < end && text[start] is ' ' or '\t')
            start++;

        return start;
    }

    private static int GetFenceMarkerLength(string text, int start, int end)
    {
        if (start >= end || text[start] is not ('`' or '~'))
            return 0;

        return CountRun(text, start, end, text[start]);
    }

    private static int CountRun(string text, int start, int end, char character)
    {
        var index = start;
        while (index < end && text[index] == character)
            index++;

        return index - start;
    }

    private static int FindRun(string text, int start, int end, char character, int requiredLength)
    {
        for (var index = start; index < end;)
        {
            if (text[index] != character)
            {
                index++;
                continue;
            }

            var length = CountRun(text, index, end, character);
            if (length == requiredLength)
                return index;

            index += length;
        }

        return -1;
    }

    private static void Add(
        ImmutableArray<DocumentationMarkdownClassification>.Builder builder,
        SyntaxTrivia trivia,
        int relativeStart,
        int length,
        DocumentationMarkdownKind kind)
    {
        if (length > 0)
            builder.Add(new DocumentationMarkdownClassification(
                new TextSpan(trivia.Span.Start + relativeStart, length),
                kind));
    }
}

/// <summary>Identifies a Markdown construct within a documentation comment.</summary>
public enum DocumentationMarkdownKind
{
    /// <summary>A Raven documentation tag.</summary>
    Tag,

    /// <summary>A Markdown heading marker.</summary>
    Heading,

    /// <summary>An inline code span, fenced-code marker, or fenced-code line.</summary>
    Code,

    /// <summary>A Markdown inline link.</summary>
    Link
}

/// <summary>Associates a source span with a Markdown documentation construct.</summary>
/// <param name="Span">The absolute source span.</param>
/// <param name="Kind">The classified Markdown construct.</param>
public readonly record struct DocumentationMarkdownClassification(TextSpan Span, DocumentationMarkdownKind Kind);
