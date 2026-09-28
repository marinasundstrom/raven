using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal static class ParameterPatternFacts
{
    internal const string AttributeMetadataName = "Raven.Runtime.CompilerServices.PatternParameterAttribute";
    internal const int MetadataVersion = 1;

    internal static PatternSyntax? GetSourcePattern(ISymbol parameter)
    {
        var syntax = parameter.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() as ParameterSyntax;
        return syntax?.Pattern ?? (syntax?.Identifier.IsKind(SyntaxKind.UnderscoreToken) == true
            ? SyntaxFactory.DiscardPattern()
            : null);
    }

    internal static string GetDisplayText(PatternSyntax pattern)
        => new RemoveTriviaRewriter().Visit(pattern)!.NormalizeWhitespace().ToString();

    // Metadata carries a versioned, trivia-free pattern, never a complete signature.
    // Reconstruct detached syntax without binding it in the consumer's scope.
    internal static PatternSyntax? Decode(int version, string? text)
    {
        if (version != MetadataVersion || string.IsNullOrWhiteSpace(text) || text.Length > 16384)
            return null;

        // Bound recursion before invoking the recursive parser on external metadata.
        var depth = 0;
        foreach (var ch in text)
        {
            if (ch is '(' or '[' or '{' && ++depth > 128)
                return null;
            if (ch is ')' or ']' or '}')
                depth--;
        }

        if (depth != 0)
            return null;

        var parser = new LanguageParser(null, new ParseOptions());
        var result = parser.ParseSyntaxWithDiagnostics(typeof(PatternSyntax), SourceText.From(text), 0,
            consumeFullText: true, parameterPattern: true);
        if (result is not { Diagnostics.Count: 0 } parsed || parsed.Root.CreateRed() is not PatternSyntax pattern)
            return null;
        return pattern.DescendantTokens().Any(token => token.IsMissing && token.Kind != SyntaxKind.None) ? null : pattern;
    }

    private sealed class RemoveTriviaRewriter : SyntaxRewriter
    {
        public override SyntaxToken VisitToken(SyntaxToken token)
            => token.WithLeadingTrivia(SyntaxTriviaList.Empty).WithTrailingTrivia(SyntaxTriviaList.Empty);
    }
}
