using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class CompletionProvider
{
    public static IEnumerable<CompletionItem> GetCompletions(SyntaxToken token, SemanticModel model, int position)
    {
        var binder = model.GetBinder(token.Parent);
        var completions = new List<CompletionItem>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        var tokenText = token.Text;
        var replacementSpan = new TextSpan(token.Position, tokenText.Length);

        if (token.IsKind(SyntaxKind.NewKeyword))
        {
            foreach (var symbol in binder.LookupAvailableSymbols())
            {
                if (symbol is INamedTypeSymbol type &&
                    !type.IsAbstract &&
                    type.Constructors.Any(c => c.DeclaredAccessibility == Accessibility.Public))
                {
                    if (seen.Add(type.Name))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: type.Name,
                            InsertionText: type.Name,
                            ReplacementSpan: replacementSpan,
                            CursorOffset: type.Name.Length,
                            Description: type.ToDisplayString(),
                            Symbol: type
                        ));
                    }
                }
            }
        }

        /*
        var objectInitializer = token.GetAncestor<InitializerExpressionSyntax>();
        if (objectInitializer is not null && objectInitializer.Parent is ObjectCreationExpressionSyntax objectCreation)
        {
            var symbolInfo = model.GetSymbolInfo(objectCreation.Type);
            if (symbolInfo.Symbol is INamedTypeSymbol type)
            {
                foreach (var member in type.GetMembers()
                    .OfType<IPropertySymbol>()
                    .Where(p => p.SetMethod is not null && p.SetMethod.DeclaredAccessibility == Accessibility.Public))
                {
                    if (seen.Add(member.Name))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: member.Name,
                            InsertionText: member.Name,
                            ReplacementSpan: replacementSpan,
                            CursorOffset: member.Name.Length,
                            Description: member.ToDisplayString(),
                            Symbol: member
                        ));
                    }
                }
            }
        }
        */

        /*
        if (token.Parent is AttributeSyntax attribute && attribute.Name is IdentifierNameSyntax attrName)
        {
            foreach (var symbol in binder.LookupAvailableSymbols())
            {
                if (symbol is INamedTypeSymbol { IsAttribute: true } attrType)
                {
                    var name = attrType.Name;
                    if (name.EndsWith("Attribute") && name.Length > "Attribute".Length)
                        name = name[..^"Attribute".Length];

                    if (string.IsNullOrEmpty(tokenText) || name.StartsWith(tokenText, StringComparison.OrdinalIgnoreCase))
                    {
                        if (seen.Add(name))
                        {
                            completions.Add(new CompletionItem(
                                DisplayText: name,
                                InsertionText: name,
                                ReplacementSpan: replacementSpan,
                                CursorOffset: name.Length,
                                Description: attrType.ToDisplayString(),
                                Symbol: attrType
                            ));
                        }
                    }
                }
            }
        }
        */

        // Member access: Console.Wri
        var memberAccess = token.GetAncestor<MemberAccessExpressionSyntax>();
        if (memberAccess is not null)
        {
            var dotToken = memberAccess.OperatorToken;

            if (position >= dotToken.End)
            {
                var symbolInfo = model.GetSymbolInfo(memberAccess.Expression);
                var typeInfo = model.GetTypeInfo(memberAccess.Expression).Type;
                IEnumerable<ISymbol>? members = null;

                if (symbolInfo.Symbol is INamedTypeSymbol typeSymbol && symbolInfo.Symbol == typeInfo)
                {
                    // Accessing a type name: show static members
                    members = typeSymbol.GetMembers().Where(m => m.IsStatic && m.DeclaredAccessibility == Accessibility.Public);
                }
                else if (typeInfo is INamedTypeSymbol instanceType)
                {
                    // Accessing an instance: show instance members
                    members = instanceType.GetMembers().Where(m => !m.IsStatic && m.DeclaredAccessibility == Accessibility.Public);
                }

                if (members is not null)
                {
                    var prefix = memberAccess.Name.Identifier.Text;
                    var nameSpan = memberAccess.Name.Identifier.Span;

                    foreach (var member in members.Where(m => string.IsNullOrEmpty(prefix) || m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
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
                                Description: member.ToDisplayString(),
                                Symbol: member
                            ));
                        }
                    }

                    return completions;
                }
            }
        }

        var qualifiedName = token.GetAncestor<QualifiedNameSyntax>();
        if (qualifiedName is not null && qualifiedName.Right is SimpleNameSyntax simple)
        {
            var symbolInfo = model.GetSymbolInfo(qualifiedName.Left);
            if (symbolInfo.Symbol is INamespaceOrTypeSymbol nsOrType)
            {
                var prefix = simple.Identifier.Text;
                var nameSpan = simple.Identifier.Span;

                foreach (var member in nsOrType.GetMembers()
                    .Where(m => string.IsNullOrEmpty(prefix)
                    || m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                    .Where(m => m.DeclaredAccessibility == Accessibility.NotApplicable || m.DeclaredAccessibility == Accessibility.Public))
                {
                    if (seen.Add(member.Name))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: member.Name,
                            InsertionText: member.Name,
                            ReplacementSpan: nameSpan,
                            CursorOffset: member is ITypeSymbol ? member.Name.Length : (int?)null,
                            Description: member.ToDisplayString(),
                            Symbol: member
                        ));
                    }
                }
            }
        }

        // Language keywords
        var keywords = new[] { "if", "else", "while", "for", "return", "let", "var", "new", "true", "false", "null" };
        foreach (var keyword in keywords.Where(k => string.IsNullOrEmpty(tokenText) || k.StartsWith(tokenText, StringComparison.OrdinalIgnoreCase)))
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
        if (token.Parent is IdentifierNameSyntax { Parent: BlockStatementSyntax or ExpressionStatementSyntax } || token.IsKind(SyntaxKind.IdentifierToken))
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

                if (!string.IsNullOrEmpty(tokenText) &&
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
                        Description: symbol.ToDisplayString(),
                        Symbol: symbol
                    ));
                }
            }
        }

        return completions;
    }
}
