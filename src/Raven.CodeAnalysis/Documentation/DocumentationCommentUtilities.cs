using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Documentation;

internal static class DocumentationCommentUtilities
{
    public static DocumentationComment? FromSyntax(SyntaxNode syntax)
    {
        if (syntax is null)
            return null;

        var syntaxTree = syntax.SyntaxTree;
        var options = syntaxTree?.Options ?? new ParseOptions();

        if (!options.DocumentationMode)
            return null;

        var format = options.DocumentationFormat;
        var token = syntax.GetFirstToken(includeZeroWidth: true);

        if (token == default)
            return null;

        var leadingTrivia = token.LeadingTrivia;

        for (var i = leadingTrivia.Count - 1; i >= 0; i--)
        {
            var trivia = leadingTrivia[i];

            if (DocumentationComment.TryParse(trivia, format, out var comment))
                return comment;

            if (!IsIgnorableTrivia(trivia.Kind))
                break;
        }

        return null;
    }

    private static bool IsIgnorableTrivia(SyntaxKind kind) => kind switch
    {
        SyntaxKind.WhitespaceTrivia => true,
        SyntaxKind.TabTrivia => true,
        SyntaxKind.LineFeedTrivia => true,
        SyntaxKind.CarriageReturnTrivia => true,
        SyntaxKind.CarriageReturnLineFeedTrivia => true,
        SyntaxKind.EndOfLineTrivia => true,
        _ => false
    };
}
