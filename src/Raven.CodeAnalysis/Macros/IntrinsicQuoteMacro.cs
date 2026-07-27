using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal sealed class IntrinsicQuoteMacro : ITokenTreeExpressionMacro
{
    private const string IncompleteQuoteCode = "QUOTE001";
    private const string ExpansionFailedCode = "QUOTE002";
    private const string MissingReferenceCode = "QUOTE003";
    private const string IncompleteSpliceCode = "QUOTE005";

    public static IntrinsicQuoteMacro Instance { get; } = new();

    private IntrinsicQuoteMacro()
    {
    }

    public string Name => "quote";

    public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
    {
        var splicePreparation = PrepareExpressionSplices(context);
        if (!splicePreparation.Diagnostics.IsEmpty ||
            !splicePreparation.MacroDiagnostics.IsEmpty)
        {
            return FreestandingMacroExpansionResult.FromDiagnostics(
                splicePreparation.Diagnostics,
                splicePreparation.MacroDiagnostics);
        }

        var fragment = context.ParseExpressionResult(splicePreparation.BodyText);
        if (!fragment.Diagnostics.IsEmpty)
            return FreestandingMacroExpansionResult.FromDiagnostics(fragment.Diagnostics);

        var missingTokens = fragment.Syntax
            .DescendantTokens()
            .Where(static token => token.IsMissing)
            .ToImmutableArray();
        if (fragment.Syntax.IsMissing || !missingTokens.IsEmpty)
        {
            var bodyPosition = missingTokens.IsEmpty
                ? 0
                : Math.Clamp(
                    missingTokens[0].SpanStart - context.BodySpan.Start,
                    0,
                    context.BodySpan.Length);
            return FreestandingMacroExpansionResult.FromDiagnostic(
                context.CreateBodyDiagnostic(
                    new TextSpan(bodyPosition, 0),
                    "Quoted expression is incomplete.",
                    code: IncompleteQuoteCode));
        }

        if (context.Compilation.GetTypeByMetadataName(
                "Raven.CodeAnalysis.Syntax.SyntaxFactory") is null)
        {
            return Error(
                context,
                "Expression quotes require a runtime reference to Raven.CodeAnalysis.",
                code: MissingReferenceCode);
        }

        var quotedSyntax = RedistributePlaceholderTrivia(
            fragment.Syntax,
            splicePreparation.SourceByPlaceholderName.Keys);
        var expansionText = RavenQuoter.Quote(quotedSyntax, new RavenQuoterOptions
        {
            GenerateUsingDirectives = false,
            UseStaticSyntaxFactoryImport = false,
            FullyQualifyNames = true,
            IncludeTrivia = true,
            NormalizeWhitespace = false,
            NodeSourceOverride = node =>
                node is IdentifierNameSyntax identifier &&
                splicePreparation.SourceByPlaceholderName.TryGetValue(
                    identifier.Identifier.ValueText,
                    out var source)
                    ? source
                    : null
        });
        var parser = new Syntax.InternalSyntax.Parser.LanguageParser(
            context.Syntax.SyntaxTree?.FilePath,
            context.Syntax.SyntaxTree?.Options ?? new ParseOptions());
        var expansion = parser.ParseSyntaxWithDiagnostics(
            typeof(ExpressionSyntax),
            SourceText.From(expansionText),
            position: 0,
            consumeFullText: true);

        if (expansion is null)
        {
            return Error(
                context,
                "The compiler could not construct the quoted expression.",
                code: ExpansionFailedCode);
        }

        var expansionResult = expansion.Value;
        var expansionExpression = expansionResult.Root.CreateRed() as ExpressionSyntax;
        if (expansionResult.Diagnostics.Count > 0 ||
            expansionResult.Root.IsMissing ||
            expansionExpression is null)
        {
            return Error(
                context,
                "The compiler could not construct the quoted expression.",
                code: ExpansionFailedCode);
        }

        return FreestandingMacroExpansionResult.FromExpression(expansionExpression);
    }

    private static ExpressionSyntax RedistributePlaceholderTrivia(
        ExpressionSyntax syntax,
        IEnumerable<string> placeholderNames)
    {
        var names = placeholderNames.ToImmutableHashSet(StringComparer.Ordinal);
        if (names.IsEmpty)
            return syntax;

        var tokens = syntax.DescendantTokens().ToArray();
        var replacements = new Dictionary<SyntaxToken, SyntaxToken>();

        SyntaxToken GetReplacement(int index)
            => replacements.TryGetValue(tokens[index], out var replacement)
                ? replacement
                : tokens[index];

        for (var index = 0; index < tokens.Length; index++)
        {
            var token = tokens[index];
            if (token.Kind != SyntaxKind.IdentifierToken ||
                !names.Contains(token.ValueText))
            {
                continue;
            }

            var replacement = GetReplacement(index);
            if (replacement.HasLeadingTrivia && index > 0)
            {
                var previous = GetReplacement(index - 1);
                replacements[tokens[index - 1]] = previous.WithTrailingTrivia(
                    previous.TrailingTrivia.Concat(replacement.LeadingTrivia));
                replacement = replacement.WithLeadingTrivia(SyntaxTriviaList.Empty);
            }

            if (replacement.HasTrailingTrivia && index < tokens.Length - 1)
            {
                var next = GetReplacement(index + 1);
                replacements[tokens[index + 1]] = next.WithLeadingTrivia(
                    replacement.TrailingTrivia.Concat(next.LeadingTrivia));
                replacement = replacement.WithTrailingTrivia(SyntaxTriviaList.Empty);
            }

            replacements[token] = replacement;
        }

        return (ExpressionSyntax)syntax.ReplaceTokens(
            replacements.Keys,
            (original, _) => replacements[original]);
    }

    private static SplicePreparation PrepareExpressionSplices(TokenTreeMacroContext context)
    {
        var bodyText = context.GetBodyText();
        var transformedBody = bodyText.ToCharArray();
        var sourceByPlaceholderName = ImmutableDictionary.CreateBuilder<string, string>(
            StringComparer.Ordinal);
        var diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();
        var macroDiagnostics = ImmutableArray.CreateBuilder<MacroExpansionDiagnostic>();
        var stream = context.CreateTokenStream();
        var placeholderIndex = 0;

        while (!stream.IsEndOfFile)
        {
            var hashToken = stream.ReadToken();
            if (hashToken.Kind != SyntaxKind.HashToken ||
                stream.PeekToken().Kind != SyntaxKind.OpenParenToken ||
                hashToken.Span.End != stream.PeekToken().SpanStart)
            {
                continue;
            }

            var openParenToken = stream.ReadToken();
            var depth = 1;
            SyntaxToken closeParenToken = default;

            while (!stream.IsEndOfFile)
            {
                var token = stream.ReadToken();
                if (token.Kind == SyntaxKind.OpenParenToken)
                {
                    depth++;
                }
                else if (token.Kind == SyntaxKind.CloseParenToken && --depth == 0)
                {
                    closeParenToken = token;
                    break;
                }
            }

            if (closeParenToken.Kind == SyntaxKind.None)
            {
                break;
            }

            var expressionSpan = TextSpan.FromBounds(
                openParenToken.Span.End,
                closeParenToken.SpanStart);
            if (string.IsNullOrWhiteSpace(
                    bodyText.Substring(expressionSpan.Start, expressionSpan.Length)))
            {
                macroDiagnostics.Add(context.CreateBodyDiagnostic(
                    expressionSpan,
                    "Expression splice is incomplete.",
                    code: IncompleteSpliceCode));
                continue;
            }

            var spliceExpression = context.ParseExpressionResult(expressionSpan);
            diagnostics.AddRange(spliceExpression.Diagnostics);
            if (!spliceExpression.Diagnostics.IsEmpty)
                continue;

            var missingTokens = spliceExpression.Syntax
                .DescendantTokens()
                .Where(static token => token.IsMissing)
                .ToImmutableArray();
            if (spliceExpression.Syntax.IsMissing || !missingTokens.IsEmpty)
            {
                var position = missingTokens.IsEmpty
                    ? expressionSpan.Start
                    : Math.Clamp(
                        missingTokens[0].SpanStart - context.BodySpan.Start,
                        expressionSpan.Start,
                        expressionSpan.End);
                macroDiagnostics.Add(context.CreateBodyDiagnostic(
                    new TextSpan(position, 0),
                    "Expression splice is incomplete.",
                    code: IncompleteSpliceCode));
                continue;
            }

            var placeholderSpan = TextSpan.FromBounds(
                hashToken.SpanStart,
                closeParenToken.Span.End);
            var placeholderName = CreatePlaceholderName(
                placeholderSpan.Length,
                placeholderIndex++,
                bodyText,
                sourceByPlaceholderName);
            placeholderName.AsSpan().CopyTo(
                transformedBody.AsSpan(placeholderSpan.Start, placeholderSpan.Length));

            sourceByPlaceholderName.Add(
                placeholderName,
                bodyText.Substring(expressionSpan.Start, expressionSpan.Length));
        }

        return new SplicePreparation(
            new string(transformedBody),
            sourceByPlaceholderName.ToImmutable(),
            diagnostics.ToImmutable(),
            macroDiagnostics.ToImmutable());
    }

    private static string CreatePlaceholderName(
        int length,
        int index,
        string bodyText,
        ImmutableDictionary<string, string>.Builder existingPlaceholders)
    {
        const string alphabet = "_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

        while (true)
        {
            var value = index++;
            var characters = new char[length];
            characters[0] = 'q';
            for (var position = length - 1; position > 0; position--)
            {
                characters[position] = alphabet[value % alphabet.Length];
                value /= alphabet.Length;
            }

            var placeholder = new string(characters);
            if (!bodyText.Contains(placeholder, StringComparison.Ordinal) &&
                !existingPlaceholders.ContainsKey(placeholder))
            {
                return placeholder;
            }
        }
    }

    private static FreestandingMacroExpansionResult Error(
        TokenTreeMacroContext context,
        string message,
        string code)
        => FreestandingMacroExpansionResult.FromDiagnostic(
            context.CreateDiagnostic(message, code: code));

    private sealed record SplicePreparation(
        string BodyText,
        ImmutableDictionary<string, string> SourceByPlaceholderName,
        ImmutableArray<Diagnostic> Diagnostics,
        ImmutableArray<MacroExpansionDiagnostic> MacroDiagnostics);
}
