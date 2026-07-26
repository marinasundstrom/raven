using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

internal sealed class IntrinsicQuoteMacro : ITokenTreeExpressionMacro
{
    public static IntrinsicQuoteMacro Instance { get; } = new();

    private IntrinsicQuoteMacro()
    {
    }

    public string Name => "quote";

    public MacroTarget Targets => MacroTarget.None;

    public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
    {
        var fragment = context.ParseExpressionResult();
        if (!fragment.Diagnostics.IsEmpty)
        {
            return new FreestandingMacroExpansionResult
            {
                Diagnostics = fragment.Diagnostics
            };
        }

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
            return new FreestandingMacroExpansionResult
            {
                MacroDiagnostics =
                [
                    context.CreateBodyDiagnostic(
                        new TextSpan(bodyPosition, 0),
                        "Quoted expression is incomplete.",
                        code: "QUOTE001")
                ]
            };
        }

        if (context.Compilation.GetTypeByMetadataName(
                "Raven.CodeAnalysis.Syntax.SyntaxFactory") is null)
        {
            return Error(
                context,
                "Expression quotes require a runtime reference to Raven.CodeAnalysis.",
                code: "QUOTE003");
        }

        var expansionText = RavenQuoter.Quote(fragment.Syntax, new RavenQuoterOptions
        {
            GenerateUsingDirectives = false,
            UseStaticSyntaxFactoryImport = false,
            FullyQualifyNames = true,
            IncludeTrivia = true,
            NormalizeWhitespace = false
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
                code: "QUOTE002");
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
                code: "QUOTE002");
        }

        return new FreestandingMacroExpansionResult
        {
            Expression = expansionExpression
        };
    }

    private static FreestandingMacroExpansionResult Error(
        TokenTreeMacroContext context,
        string message,
        string code)
        => new()
        {
            MacroDiagnostics =
            [
                context.CreateDiagnostic(message, code: code)
            ]
        };
}
