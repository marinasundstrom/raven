using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class CompletionProvider
{
    public static IEnumerable<CompletionItem> GetCompletions(SyntaxToken token, SemanticModel model, int position)
    {
        var binder = model.Compilation.GetBinder(token.Parent);
        var completions = new List<CompletionItem>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        // Member access: Console.Wri
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

                    foreach (var member in typeSymbol.GetMembers()
                                 .Where(x => x.DeclaredAccessibility == Accessibility.Public)
                                 .Where(m => m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
                    {
                        if (member is IMethodSymbol method && method.ContainingSymbol is IPropertySymbol) //& method.MethodKind == MethodKind.PropertyGet or MethodKind.PropertySet)
                            continue;

                        var insertText = member is IMethodSymbol ? member.Name + "()" : member.Name;
                        if (seen.Add(member.Name))
                            completions.Add(new CompletionItem(member.Name, insertText));
                    }
                }

                return completions;
            }
        }
        
        // Basic prefix from current token
        var tokenText = token.Text;

        // Language keywords
        var keywords = new[] { "if", "else", "while", "for", "return", "let", "var", "new", "true", "false", "null" };
        foreach (var keyword in keywords.Where(m => m.StartsWith(tokenText, StringComparison.OrdinalIgnoreCase)))
        {
            if (seen.Add(keyword))
                completions.Add(new CompletionItem(keyword, keyword));
        }

        if (token.Parent is IdentifierNameSyntax { Parent: BlockSyntax or ExpressionStatementSyntax })
        {
            // Visible symbols (locals, globals, parameters, etc.)
            foreach (var symbol in binder.LookupAvailableSymbols())
            {
                if (symbol.DeclaredAccessibility != Accessibility.Public &&
                    symbol.DeclaredAccessibility != Accessibility.NotApplicable)
                    continue;

                if (symbol is IMethodSymbol { IsConstructor: true })
                    continue;

                if (symbol is IMethodSymbol && symbol.ContainingSymbol is IPropertySymbol)
                    continue;

                if (!token.IsKind(SyntaxKind.DotToken) &&
                    !symbol.Name.StartsWith(tokenText, StringComparison.OrdinalIgnoreCase))
                    continue;

                var insertText = symbol is IMethodSymbol ? symbol.Name + "()" : symbol.Name;
                if (seen.Add(symbol.Name))
                    completions.Add(new CompletionItem(symbol.Name, insertText));
            }
        }

        return completions;
    }
}