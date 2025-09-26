using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class CompletionProvider
{
    public static IEnumerable<CompletionItem> GetCompletions(SyntaxToken token, SemanticModel model, int position)
    {
        var binder = model.GetBinder(token.Parent);
        var completions = new List<CompletionItem>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        bool IsAccessible(ISymbol symbol)
        {
            if (symbol is null)
                return false;

            if (symbol.DeclaredAccessibility == Accessibility.NotApplicable)
                return true;

            return binder.IsSymbolAccessible(symbol);
        }

        static string SafeToDisplayString(ISymbol symbol)
        {
            try
            {
                return symbol.ToDisplayString();
            }
            catch
            {
                return symbol.Name;
            }
        }

        INamedTypeSymbol? GetSelfType()
        {
            for (Binder? current = binder; current is not null; current = current.ParentBinder)
            {
                if (current.ContainingSymbol is IMethodSymbol method)
                {
                    if (!method.IsStatic || method.IsNamedConstructor)
                        return method.ContainingType;

                    return null;
                }
            }

            return null;
        }

        var tokenText = token.Text;
        var replacementSpan = new TextSpan(token.Position, tokenText.Length);
        var literalReplacementSpan = new TextSpan(position, 0);

        static ITypeSymbol UnwrapAliases(ITypeSymbol type)
        {
            while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
                type = alias;

            return type;
        }

        ITypeSymbol? TryGetLiteralCompletionTargetType()
        {
            if (token.GetAncestor<EqualsValueClauseSyntax>() is { } equalsClause)
            {
                if (position < equalsClause.EqualsToken.Span.End)
                    return null;

                return equalsClause.Parent switch
                {
                    VariableDeclaratorSyntax declarator => model.GetDeclaredSymbol(declarator) switch
                    {
                        ILocalSymbol local => local.Type,
                        IFieldSymbol field => field.Type,
                        _ => null
                    },
                    PropertyDeclarationSyntax property => (model.GetDeclaredSymbol(property) as IPropertySymbol)?.Type,
                    IndexerDeclarationSyntax indexer => (model.GetDeclaredSymbol(indexer) as IPropertySymbol)?.Type,
                    ParameterSyntax parameter => (model.GetDeclaredSymbol(parameter) as IParameterSymbol)?.Type,
                    EnumMemberDeclarationSyntax enumMember => (model.GetDeclaredSymbol(enumMember) as IFieldSymbol)?.Type,
                    _ => null
                };
            }

            if (token.GetAncestor<AssignmentExpressionSyntax>() is { } assignment)
            {
                if (position < assignment.OperatorToken.Span.End)
                    return null;

                return model.GetTypeInfo(assignment.Left).Type;
            }

            return null;
        }

        bool TryCollectLiteralMembers(ITypeSymbol? type, List<ISymbol> results)
        {
            if (type is null)
                return false;

            type = UnwrapAliases(type);

            switch (type)
            {
                case LiteralTypeSymbol literal:
                    results.Add(literal);
                    return true;

                case NullTypeSymbol nullType:
                    results.Add(nullType);
                    return true;

                case IUnionTypeSymbol union:
                {
                    var start = results.Count;
                    foreach (var member in union.Types)
                    {
                        if (!TryCollectLiteralMembers(member, results))
                        {
                            results.RemoveRange(start, results.Count - start);
                            return false;
                        }
                    }

                    return results.Count > start;
                }

                default:
                    return false;
            }
        }

        var literalTargetType = TryGetLiteralCompletionTargetType();
        if (literalTargetType is not null)
        {
            var literalMembers = new List<ISymbol>();
            if (TryCollectLiteralMembers(literalTargetType, literalMembers))
            {
                foreach (var literal in literalMembers)
                {
                    var display = literal.Name;
                    if (seen.Add(display))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: display,
                            InsertionText: display,
                            ReplacementSpan: literalReplacementSpan,
                            Description: SafeToDisplayString(literal),
                            Symbol: literal
                        ));
                    }
                }
            }
        }

        var importDirective = token.GetAncestor<ImportDirectiveSyntax>();
        if (importDirective is not null)
        {
            var qualified = token.GetAncestor<QualifiedNameSyntax>();
            if (qualified is not null && qualified.Right is SimpleNameSyntax importSimple)
            {
                var nameToken = importSimple.Identifier;
                if (position >= nameToken.Position)
                {
                    var symbolInfo = model.GetSymbolInfo(qualified.Left);
                    var symbol = symbolInfo.Symbol?.UnderlyingSymbol;
                    if (symbol is INamespaceOrTypeSymbol nsOrType)
                    {
                        var prefix = nameToken.Text;
                        var nameSpan = nameToken.Span;
                        foreach (var member in nsOrType.GetMembers()
                            .OfType<INamespaceOrTypeSymbol>()
                            .Where(m => string.IsNullOrEmpty(prefix) || m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
                        {
                            if (seen.Add(member.Name))
                            {
                                completions.Add(new CompletionItem(
                                    DisplayText: member.Name,
                                    InsertionText: member.Name,
                                    ReplacementSpan: nameSpan,
                                    CursorOffset: member is ITypeSymbol ? member.Name.Length : (int?)null,
                                    Description: SafeToDisplayString(member),
                                    Symbol: member
                                ));
                            }
                        }
                    }
                    return completions;
                }
            }

            if (token.Parent is IdentifierNameSyntax { Parent: ImportDirectiveSyntax })
            {
                foreach (var symbol in binder.LookupAvailableSymbols())
                {
                    if (symbol is INamespaceOrTypeSymbol nsOrType &&
                        (string.IsNullOrEmpty(tokenText) || nsOrType.Name.StartsWith(tokenText, StringComparison.OrdinalIgnoreCase)))
                    {
                        if (seen.Add(nsOrType.Name))
                        {
                            completions.Add(new CompletionItem(
                                DisplayText: nsOrType.Name,
                                InsertionText: nsOrType.Name,
                                ReplacementSpan: replacementSpan,
                                CursorOffset: nsOrType is ITypeSymbol ? nsOrType.Name.Length : (int?)null,
                                Description: SafeToDisplayString(nsOrType),
                                Symbol: nsOrType
                            ));
                        }
                    }
                }
                return completions;
            }
        }

        var aliasDirective = token.GetAncestor<AliasDirectiveSyntax>();
        if (aliasDirective is not null)
        {
            var qualified = token.GetAncestor<QualifiedNameSyntax>();
            if (qualified is not null && qualified.Right is SimpleNameSyntax aliasSimple)
            {
                var nameToken = aliasSimple.Identifier;
                if (position >= nameToken.Position)
                {
                    var symbolInfo = model.GetSymbolInfo(qualified.Left);
                    var symbol = symbolInfo.Symbol?.UnderlyingSymbol;
                    if (symbol is INamespaceOrTypeSymbol nsOrType)
                    {
                        var prefix = nameToken.Text;
                        var nameSpan = nameToken.Span;
                        foreach (var member in nsOrType.GetMembers()
                            .Where(m => string.IsNullOrEmpty(prefix) || m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
                        {
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
                                    Description: SafeToDisplayString(member),
                                    Symbol: member
                                ));
                            }
                        }
                    }
                    return completions;
                }
            }

            if (token.Parent is IdentifierNameSyntax { Parent: AliasDirectiveSyntax })
            {
                foreach (var symbol in binder.LookupAvailableSymbols())
                {
                    if (!string.IsNullOrEmpty(tokenText) && !symbol.Name.StartsWith(tokenText, StringComparison.OrdinalIgnoreCase))
                        continue;

                    var insertText = symbol is IMethodSymbol ? symbol.Name + "()" : symbol.Name;
                    var cursorOffset = symbol switch
                    {
                        IMethodSymbol => insertText.Length - 1,
                        IPropertySymbol => insertText.Length - 1,
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
                            Description: SafeToDisplayString(symbol),
                            Symbol: symbol
                        ));
                    }
                }
                return completions;
            }

            if (token.IsKind(SyntaxKind.EqualsToken))
            {
                var span = new TextSpan(position, 0);
                foreach (var symbol in binder.LookupAvailableSymbols())
                {
                    var insertText = symbol is IMethodSymbol ? symbol.Name + "()" : symbol.Name;
                    var cursorOffset = symbol switch
                    {
                        IMethodSymbol => insertText.Length - 1,
                        IPropertySymbol => insertText.Length - 1,
                        ITypeSymbol => insertText.Length - 1,
                        _ => (int?)null
                    };

                    if (seen.Add(symbol.Name))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: symbol.Name,
                            InsertionText: insertText,
                            ReplacementSpan: span,
                            CursorOffset: cursorOffset,
                            Description: SafeToDisplayString(symbol),
                            Symbol: symbol
                        ));
                    }
                }
                return completions;
            }
        }

        if (token.IsKind(SyntaxKind.NewKeyword))
        {
            foreach (var symbol in binder.LookupAvailableSymbols())
            {
                if (symbol is INamedTypeSymbol type &&
                    !type.IsAbstract &&
                    IsAccessible(type) &&
                    type.Constructors.Any(IsAccessible))
                {
                    if (seen.Add(type.Name))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: type.Name,
                            InsertionText: type.Name,
                            ReplacementSpan: replacementSpan,
                            CursorOffset: type.Name.Length,
                            Description: SafeToDisplayString(type),
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
                        Description: SafeToDisplayString(member),
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
                                Description: SafeToDisplayString(attrType),
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
                var symbol = symbolInfo.Symbol?.UnderlyingSymbol;
                var typeInfo = model.GetTypeInfo(memberAccess.Expression).Type;
                var type = typeInfo?.UnderlyingSymbol as ITypeSymbol;
                IEnumerable<ISymbol>? members = null;
                INamedTypeSymbol? instanceTypeForExtensions = null;

                if (symbol is INamespaceSymbol ns)
                {
                    // Namespace or namespace alias: list its public members
                    members = ns.GetMembers().Where(IsAccessible);
                }
                else if (symbol is INamedTypeSymbol typeSymbol && SymbolEqualityComparer.Default.Equals(symbol, type))
                {
                    // Accessing a type name: show static members
                    members = typeSymbol.GetMembers().Where(m => m.IsStatic && IsAccessible(m));
                }
                else if (type is ITypeSymbol instanceType)
                {
                    // Accessing an instance: show instance members
                    members = instanceType.GetMembers().Where(m => !m.IsStatic && IsAccessible(m));
                    instanceTypeForExtensions = instanceType switch
                    {
                        INamedTypeSymbol named => named,
                        LiteralTypeSymbol literal => literal.UnderlyingType as INamedTypeSymbol,
                        _ => null
                    };
                }

                if (members is not null)
                {
                    var prefix = memberAccess.Name.Identifier.Text;
                    var nameSpan = memberAccess.Name.Identifier.Span;

                    foreach (var member in members.Where(m => string.IsNullOrEmpty(prefix) || m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
                    {
                        if (member is IMethodSymbol method &&
                            (method.MethodKind == MethodKind.PropertyGet ||
                             method.MethodKind == MethodKind.PropertySet ||
                             method.MethodKind == MethodKind.EventAdd ||
                             method.MethodKind == MethodKind.EventRemove ||
                             method.MethodKind == MethodKind.EventRaise))
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
                                Description: SafeToDisplayString(member),
                                Symbol: member
                            ));
                        }
                    }

                    if (instanceTypeForExtensions is not null)
                    {
                        foreach (var method in binder.LookupExtensionMethods(null, instanceTypeForExtensions, includePartialMatches: true))
                        {
                            if (!IsAccessible(method))
                                continue;

                            if (!string.IsNullOrEmpty(prefix) && !method.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                                continue;

                            var insertText = method.Name + "()";
                            var cursorOffset = insertText.Length - 1;

                            if (seen.Add(method.Name))
                            {
                                completions.Add(new CompletionItem(
                                    DisplayText: method.Name,
                                    InsertionText: insertText,
                                    ReplacementSpan: nameSpan,
                                    CursorOffset: cursorOffset,
                                    Description: SafeToDisplayString(method),
                                    Symbol: method
                                ));
                            }
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
            var symbol = symbolInfo.Symbol?.UnderlyingSymbol;
            if (symbol is INamespaceOrTypeSymbol nsOrType)
            {
                var prefix = simple.Identifier.Text;
                var nameSpan = simple.Identifier.Span;

                foreach (var member in nsOrType.GetMembers()
                    .Where(m => string.IsNullOrEmpty(prefix)
                    || m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                    .Where(IsAccessible))
                {
                    if (seen.Add(member.Name))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: member.Name,
                            InsertionText: member.Name,
                            ReplacementSpan: nameSpan,
                            CursorOffset: member is ITypeSymbol ? member.Name.Length : (int?)null,
                            Description: SafeToDisplayString(member),
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
        var offerValueCompletions = token.Parent is IdentifierNameSyntax { Parent: BlockStatementSyntax or ExpressionStatementSyntax }
            || token.IsKind(SyntaxKind.IdentifierToken);

        if (offerValueCompletions)
        {
            var selfType = GetSelfType();
            if (selfType is not null &&
                (string.IsNullOrEmpty(tokenText) || "self".StartsWith(tokenText, StringComparison.OrdinalIgnoreCase)) &&
                seen.Add("self"))
            {
                completions.Add(new CompletionItem(
                    DisplayText: "self",
                    InsertionText: "self",
                    ReplacementSpan: replacementSpan,
                    Description: $"Current instance of {SafeToDisplayString(selfType)}",
                    Symbol: selfType
                ));
            }
        }

        if (offerValueCompletions)
        {
            foreach (var symbol in binder.LookupAvailableSymbols())
            {
                if (!IsAccessible(symbol))
                    continue;

                if (symbol is IMethodSymbol { IsConstructor: true })
                    continue;

                if (symbol is IMethodSymbol method &&
                    (method.MethodKind == MethodKind.PropertyGet ||
                     method.MethodKind == MethodKind.PropertySet ||
                     method.MethodKind == MethodKind.EventAdd ||
                     method.MethodKind == MethodKind.EventRemove ||
                     method.MethodKind == MethodKind.EventRaise))
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
                        Description: SafeToDisplayString(symbol),
                        Symbol: symbol
                    ));
                }
            }
        }

        return completions;
    }
}
