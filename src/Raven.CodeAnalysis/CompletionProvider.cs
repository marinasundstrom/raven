using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class CompletionProvider
{
    public static IEnumerable<CompletionItem> GetCompletions(SyntaxToken token, SemanticModel model, int position)
    {
        var binder = model.Compilation.GetBinder(token.Parent);
        var completions = new List<CompletionItem>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        var tokenText = token.Text;
        var replacementSpan = new TextSpan(token.Position, tokenText.Length);

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
                    var nameSpan = memberAccess.Name.Identifier.Span;

                    foreach (var member in typeSymbol.GetMembers()
                                 .Where(x => x.DeclaredAccessibility == Accessibility.Public)
                                 .Where(m => m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
                    {
                        if (member is IMethodSymbol method && method.ContainingSymbol is IPropertySymbol)
                            continue;

                        var insertText = member is IMethodSymbol ? member.Name + "()" : member.Name;

                        var cursorOffset = member switch
                        {
                            IMethodSymbol => insertText.Length - 1,
                            IPropertySymbol => insertText.Length - 1,
                            ITypeSymbol => insertText.Length - 1,
                            _ => (int?)null
                        };
                        
                        if (seen.Add(member.Name))
                        {
                            completions.Add(new CompletionItem(
                                DisplayText: member.Name,
                                InsertionText: insertText,
                                ReplacementSpan: nameSpan,
                                CursorOffset: cursorOffset,
                                Description: member.ToDisplayString()
                            ));
                        }
                    }
                }

                return completions;
            }
        }

        // Language keywords
        var keywords = new[] { "if", "else", "while", "for", "return", "let", "var", "new", "true", "false", "null" };
        foreach (var keyword in keywords.Where(m => m.StartsWith(tokenText, StringComparison.OrdinalIgnoreCase)))
        {
            if (seen.Add(keyword))
            {
                completions.Add(new CompletionItem(
                    DisplayText: keyword,
                    InsertionText: keyword,
                    ReplacementSpan: replacementSpan
                ));
            }
        }

        // Visible symbols (locals, globals, etc.)
        if (token.Parent is IdentifierNameSyntax { Parent: BlockSyntax or ExpressionStatementSyntax })
        {
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
                
                var cursorOffset = symbol switch
                {
                    IMethodSymbol => insertText.Length - 1,
                    ITypeSymbol => insertText.Length - 1,
                    _ => (int?)null
                };
                
                if (seen.Add(symbol.Name))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: symbol.Name,
                        InsertionText: insertText,
                        ReplacementSpan: replacementSpan,
                        CursorOffset: cursorOffset,
                        Description: symbol.ToDisplayString()
                    ));
                }
            }
        }

        return completions;
    }
}