using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Documentation;

internal static class DocumentationCommentUtilities
{
    public static ImmutableArray<DocumentationComment> GetDocumentationComments(
        IEnumerable<SyntaxReference> declaringSyntaxReferences,
        ParseOptions options)
    {
        var builder = ImmutableArray.CreateBuilder<DocumentationComment>();

        foreach (var reference in declaringSyntaxReferences)
        {
            var syntax = reference.GetSyntax();
            var format = syntax.SyntaxTree?.Options?.DocumentationFormat ?? options.DocumentationFormat;

            if (TryGetDocumentationComment(syntax, format, out var comment, out _) && comment is not null)
                builder.Add(comment);
        }

        return builder.ToImmutable();
    }

    public static DocumentationComment? GetMergedDocumentationComment(
        IEnumerable<SyntaxReference> declaringSyntaxReferences,
        ParseOptions options)
    {
        var comments = GetDocumentationComments(declaringSyntaxReferences, options);
        if (comments.IsDefaultOrEmpty)
            return null;

        if (comments.Length == 1)
            return comments[0];

        // Prefer the format from the first declaration; all declarations currently share the same parse options.
        return DocumentationComment.Merge(comments[0].Format, comments);
    }

    public static bool TryGetDocumentationComment(
        SyntaxNode syntax,
        DocumentationFormat format,
        out DocumentationComment? documentationComment)
    {
        return TryGetDocumentationComment(syntax, format, out documentationComment, out _);
    }

    public static bool TryGetDocumentationComment(
        SyntaxNode syntax,
        DocumentationFormat format,
        out DocumentationComment? documentationComment,
        out IReadOnlyList<SyntaxTrivia> documentationTrivia)
    {
        documentationComment = null;
        documentationTrivia = Array.Empty<SyntaxTrivia>();

        if (syntax is null)
            return false;

        var trivia = syntax.GetLeadingTrivia();
        if (trivia.Count == 0)
            return false;

        var collected = new List<DocumentationComment>();
        var collectedTrivia = new List<SyntaxTrivia>();

        for (var i = trivia.Count - 1; i >= 0; i--)
        {
            var current = trivia[i];

            if (DocumentationComment.TryParse(current, format, out var comment))
            {
                collected.Add(comment);
                collectedTrivia.Add(current);
                continue;
            }

            if (collected.Count == 0 && !IsSkippableLeadingTrivia(current))
                break;

            if (collected.Count > 0)
            {
                if (IsSkippableLeadingTrivia(current))
                    continue;

                break;
            }
        }

        if (collected.Count == 0)
            return false;

        collected.Reverse();
        collectedTrivia.Reverse();
        documentationTrivia = collectedTrivia;
        documentationComment = DocumentationComment.Merge(format, collected);
        return documentationComment is not null;
    }

    private static bool IsSkippableLeadingTrivia(SyntaxTrivia trivia)
    {
        return trivia.Kind is SyntaxKind.WhitespaceTrivia
            or SyntaxKind.EndOfLineTrivia
            or SyntaxKind.LineFeedTrivia
            or SyntaxKind.CarriageReturnTrivia
            or SyntaxKind.CarriageReturnLineFeedTrivia;
    }
}
