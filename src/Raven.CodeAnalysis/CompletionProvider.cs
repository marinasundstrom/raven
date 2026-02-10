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
        var tokenValueText = token.ValueText;
        var replacementSpan = new TextSpan(token.Position, tokenText.Length);
        var literalReplacementSpan = new TextSpan(position, 0);

        bool ShouldOfferSelfCompletion()
        {
            if (token.IsKind(SyntaxKind.IdentifierToken))
                return true;

            if (token.Parent is IdentifierNameSyntax { Identifier.IsMissing: false })
                return true;

            return token.GetAncestor<BlockStatementSyntax>() is not null
                || token.GetAncestor<BlockSyntax>() is not null;
        }

        string GetSelfCompletionPrefix()
        {
            if (token.IsKind(SyntaxKind.IdentifierToken))
                return token.ValueText;

            if (token.Parent is IdentifierNameSyntax { Identifier: { IsMissing: false } identifier })
                return identifier.ValueText;

            return string.Empty;
        }

        TextSpan GetSelfReplacementSpan()
        {
            if (token.IsKind(SyntaxKind.IdentifierToken))
                return replacementSpan;

            if (token.Parent is IdentifierNameSyntax { Identifier: { IsMissing: false } identifier })
                return identifier.Span;

            return new TextSpan(position, 0);
        }

        static string EscapeIdentifierForInsertion(string identifier)
        {
            if (string.IsNullOrEmpty(identifier))
                return identifier;

            if (identifier[0] == '@')
                return identifier;

            return SyntaxFacts.TryParseKeyword(identifier, out _)
                ? "@" + identifier
                : identifier;
        }

        static (string displayText, string insertionText) CreateCompletionParts(ISymbol symbol)
        {
            var escapedName = EscapeIdentifierForInsertion(symbol.Name);
            var insertionText = symbol is IMethodSymbol
                ? escapedName + "()"
                : escapedName;

            return (escapedName, insertionText);
        }

        static int? GetDefaultCursorOffset(ISymbol symbol, string insertionText)
        {
            return symbol switch
            {
                IMethodSymbol => insertionText.Length - 1,
                IPropertySymbol => insertionText.Length - 1,
                ITypeSymbol => insertionText.Length - 1,
                _ => (int?)null
            };
        }

        static ITypeSymbol UnwrapAliases(ITypeSymbol type)
        {
            while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
                type = alias;

            return type;
        }

        ITypeSymbol? TryGetLiteralCompletionTargetType(SyntaxToken currentToken)
        {
            if (currentToken.GetAncestor<EqualsValueClauseSyntax>() is { } equalsClause)
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
                    EventDeclarationSyntax @event => (model.GetDeclaredSymbol(@event) as IEventSymbol)?.Type,
                    IndexerDeclarationSyntax indexer => (model.GetDeclaredSymbol(indexer) as IPropertySymbol)?.Type,
                    ParameterSyntax parameter => (model.GetDeclaredSymbol(parameter) as IParameterSymbol)?.Type,
                    EnumMemberDeclarationSyntax enumMember => (model.GetDeclaredSymbol(enumMember) as IFieldSymbol)?.Type,
                    _ => null
                };
            }

            if (currentToken.GetAncestor<AssignmentExpressionSyntax>() is { } assignment)
            {
                if (position < assignment.OperatorToken.Span.End)
                    return null;

                if (assignment.Left is ExpressionSyntax left)
                    return model.GetTypeInfo(left).Type;
                return null;
            }

            if (currentToken.GetAncestor<AssignmentStatementSyntax>() is { } assignmentStatement)
            {
                if (position < assignmentStatement.OperatorToken.Span.End)
                    return null;

                if (assignmentStatement.Left is ExpressionSyntax left)
                    return model.GetTypeInfo(left).Type;
                return null;
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

                case ITypeUnionSymbol union:
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

        var literalTargetType = TryGetLiteralCompletionTargetType(token);
        if (literalTargetType is null)
        {
            var syntaxTree = token.SyntaxTree;
            if (syntaxTree is not null && token.Span.Start > 0)
            {
                var previousToken = syntaxTree.GetRoot().FindToken(token.Span.Start - 1);
                if (previousToken != token)
                    literalTargetType = TryGetLiteralCompletionTargetType(previousToken);
            }
        }
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
                        var prefix = nameToken.ValueText;
                        var nameSpan = nameToken.Span;
                        foreach (var member in nsOrType.GetMembers()
                            .OfType<INamespaceOrTypeSymbol>()
                            .Where(m => string.IsNullOrEmpty(prefix) || m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
                        {
                            var (displayText, insertText) = CreateCompletionParts(member);
                            var cursorOffset = member is ITypeSymbol ? insertText.Length : (int?)null;

                            if (seen.Add(member.Name))
                            {
                                completions.Add(new CompletionItem(
                                    DisplayText: displayText,
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

            if (token.Parent is IdentifierNameSyntax { Parent: ImportDirectiveSyntax })
            {
                foreach (var symbol in binder.LookupAvailableSymbols())
                {
                    if (symbol is INamespaceOrTypeSymbol nsOrType &&
                        (string.IsNullOrEmpty(tokenValueText) || nsOrType.Name.StartsWith(tokenValueText, StringComparison.OrdinalIgnoreCase)))
                    {
                        var (displayText, insertText) = CreateCompletionParts(nsOrType);
                        var cursorOffset = nsOrType is ITypeSymbol ? insertText.Length : (int?)null;

                        if (seen.Add(nsOrType.Name))
                        {
                            completions.Add(new CompletionItem(
                                DisplayText: displayText,
                                InsertionText: insertText,
                                ReplacementSpan: replacementSpan,
                                CursorOffset: cursorOffset,
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
                        var prefix = nameToken.ValueText;
                        var nameSpan = nameToken.Span;
                        foreach (var member in nsOrType.GetMembers()
                            .Where(m => string.IsNullOrEmpty(prefix) || m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
                        {
                            var (displayText, insertText) = CreateCompletionParts(member);
                            var cursorOffset = member is ITypeSymbol ? insertText.Length : GetDefaultCursorOffset(member, insertText);

                            if (seen.Add(member.Name))
                            {
                                completions.Add(new CompletionItem(
                                    DisplayText: displayText,
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
                    if (!string.IsNullOrEmpty(tokenValueText) && !symbol.Name.StartsWith(tokenValueText, StringComparison.OrdinalIgnoreCase))
                        continue;

                    var (displayText, insertText) = CreateCompletionParts(symbol);
                    var cursorOffset = GetDefaultCursorOffset(symbol, insertText);

                    if (seen.Add(symbol.Name))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: displayText,
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
                    var (displayText, insertText) = CreateCompletionParts(symbol);
                    var cursorOffset = GetDefaultCursorOffset(symbol, insertText);

                    if (seen.Add(symbol.Name))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: displayText,
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
                        var (displayText, insertText) = CreateCompletionParts(type);
                        var cursorOffset = insertText.Length;

                        completions.Add(new CompletionItem(
                            DisplayText: displayText,
                            InsertionText: insertText,
                            ReplacementSpan: replacementSpan,
                            CursorOffset: cursorOffset,
                            Description: SafeToDisplayString(type),
                            Symbol: type
                        ));
                    }
                }
            }
        }

        if (token.Parent is GotoStatementSyntax gotoStatement)
        {
            var identifier = gotoStatement.Identifier;
            var prefix = identifier.IsMissing ? string.Empty : identifier.ValueText;
            var replacement = identifier.IsMissing
                ? new TextSpan(position, 0)
                : identifier.Span;

            foreach (var symbol in binder.LookupAvailableSymbols())
            {
                if (symbol is not ILabelSymbol label)
                    continue;

                if (string.IsNullOrEmpty(label.Name))
                    continue;

                if (!string.IsNullOrEmpty(prefix) &&
                    !label.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                {
                    continue;
                }

                if (seen.Add(label.Name))
                {
                    var (displayText, insertText) = CreateCompletionParts(label);

                    completions.Add(new CompletionItem(
                        DisplayText: displayText,
                        InsertionText: insertText,
                        ReplacementSpan: replacement,
                        CursorOffset: insertText.Length,
                        Description: SafeToDisplayString(label),
                        Symbol: label));
                }
            }

            return completions;
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
                else if (memberAccess.Expression is SelfExpressionSyntax && GetSelfType() is { } currentSelfType)
                {
                    members = currentSelfType.GetMembers().Where(m => !m.IsStatic && IsAccessible(m));
                    instanceTypeForExtensions = currentSelfType;
                }

                if (members is not null)
                {
                    var prefix = memberAccess.Name.Identifier.ValueText;
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

                        var (displayText, insertText) = CreateCompletionParts(member);
                        var cursorOffset = GetDefaultCursorOffset(member, insertText);

                        if (seen.Add(member.Name))
                        {
                            completions.Add(new CompletionItem(
                                DisplayText: displayText,
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
                        var extensionMembers = ExtensionMemberLookup.Lookup(
                            binder,
                            instanceTypeForExtensions,
                            includePartialMatches: true,
                            kinds: ExtensionMemberKinds.InstanceMethods);

                        foreach (var method in extensionMembers.InstanceMethods)
                        {
                            if (!IsAccessible(method))
                                continue;

                            if (!string.IsNullOrEmpty(prefix) && !method.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                                continue;

                            var (displayText, insertText) = CreateCompletionParts(method);
                            var cursorOffset = GetDefaultCursorOffset(method, insertText);

                            if (seen.Add(method.Name))
                            {
                                completions.Add(new CompletionItem(
                                    DisplayText: displayText,
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
                var prefix = simple.Identifier.ValueText;
                var nameSpan = simple.Identifier.Span;

                foreach (var member in nsOrType.GetMembers()
                    .Where(m => string.IsNullOrEmpty(prefix)
                    || m.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                    .Where(IsAccessible))
                {
                    var (displayText, insertText) = CreateCompletionParts(member);
                    var cursorOffset = member is ITypeSymbol ? insertText.Length : GetDefaultCursorOffset(member, insertText);

                    if (seen.Add(member.Name))
                    {
                        completions.Add(new CompletionItem(
                            DisplayText: displayText,
                            InsertionText: insertText,
                            ReplacementSpan: nameSpan,
                            CursorOffset: cursorOffset,
                            Description: SafeToDisplayString(member),
                            Symbol: member
                        ));
                    }
                }
            }
        }

        var selfType = GetSelfType();
        if (selfType is not null && ShouldOfferSelfCompletion())
        {
            var prefix = GetSelfCompletionPrefix();
            if (string.IsNullOrEmpty(prefix) || "self".StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
            {
                if (seen.Add("self"))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: "self",
                        InsertionText: "self",
                        ReplacementSpan: GetSelfReplacementSpan(),
                        Description: $"Current instance of {SafeToDisplayString(selfType)}",
                        Symbol: selfType
                    ));
                }
            }
        }

        // Visible symbols (locals, globals, etc.)
        var offerValueCompletions = token.Parent is IdentifierNameSyntax { Parent: BlockStatementSyntax or ExpressionStatementSyntax }
            || token.IsKind(SyntaxKind.IdentifierToken);

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

                if (!string.IsNullOrEmpty(tokenValueText) &&
                    !symbol.Name.StartsWith(tokenValueText, StringComparison.OrdinalIgnoreCase))
                    continue;

                var (displayText, insertText) = CreateCompletionParts(symbol);
                var cursorOffset = GetDefaultCursorOffset(symbol, insertText);

                if (seen.Add(symbol.Name))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: displayText,
                        InsertionText: insertText,
                        ReplacementSpan: replacementSpan,
                        CursorOffset: cursorOffset,
                        Description: SafeToDisplayString(symbol),
                        Symbol: symbol
                    ));
                }
            }
        }

        // Language keywords (added after symbol completions so escaped identifiers win on duplicates)
        foreach (var keyword in CompletionService.BasicKeywords.Where(k => string.IsNullOrEmpty(tokenValueText) || k.StartsWith(tokenValueText, StringComparison.OrdinalIgnoreCase)))
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

        return completions;
    }
}
