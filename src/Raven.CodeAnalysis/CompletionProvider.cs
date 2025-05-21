using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class CompletionProvider
{
    public static IEnumerable<CompletionItem> GetCompletions(SyntaxToken token, SemanticModel model, int position)
    {
        var binder = model.Compilation.GetBinder(token.Parent);
        var completions = new List<CompletionItem>();

        // Handle member access expressions like Console.Wri
        var memberAccess = token.GetAncestor<MemberAccessExpressionSyntax>();
        if (memberAccess is not null)
        {
            var dotToken = memberAccess.OperatorToken;

            if (position >= dotToken.End)
            {
                var symbolInfo = model.GetSymbolInfo(memberAccess.Expression);
                if (symbolInfo.Symbol is INamedTypeSymbol typeSymbol)
                {
                    var prefix = memberAccess.Name.Identifier.Text;
                    completions.AddRange(
                        typeSymbol.GetMembers()
                            .Where(m => m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                            .Select(m => new CompletionItem(m.Name, m.Name))
                    );
                }

                return completions;
            }
        }

        // Top-level completions (e.g. in global scope or statement block)
        if (token.Parent is CompilationUnitSyntax || token.Parent is BlockSyntax)
        {
            completions.AddRange(binder.LookupAvailableSymbols()
                .Select(sym => new CompletionItem(sym.Name, sym.Name)));
        }

        return completions;
    }
}
