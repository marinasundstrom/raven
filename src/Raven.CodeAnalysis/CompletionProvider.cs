using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class CompletionProvider
{
    private readonly record struct MacroCompletionContext(
        MacroKind Kind,
        string Prefix,
        TextSpan ReplacementSpan,
        bool WrapAttachedAttribute = false,
        bool PreserveInvocationSuffix = false);

    public static IEnumerable<CompletionItem> GetCompletions(
        SyntaxToken token,
        SemanticModel model,
        int position,
        bool forceInsertionAtCaret = false)
        => GetCompletionsCore(token, model, position, forceInsertionAtCaret, semanticContext: null);

    internal static IEnumerable<CompletionItem> GetCompletionsForMacroFragment(
        SyntaxToken token,
        SemanticModel model,
        SyntaxNode invocation,
        ImmutableArray<MacroFragmentLocal> fragmentLocals,
        int position,
        bool forceInsertionAtCaret = false)
        => GetCompletionsCore(token, model, position, forceInsertionAtCaret, invocation, fragmentLocals);

    private static IEnumerable<CompletionItem> GetCompletionsCore(
        SyntaxToken token,
        SemanticModel model,
        int position,
        bool forceInsertionAtCaret,
        SyntaxNode? semanticContext,
        ImmutableArray<MacroFragmentLocal> fragmentLocals = default)
    {
        if (IsNonCodePosition(model.SyntaxTree, position))
            return [];

        var sourceText = model.SyntaxTree.GetText();

        if (TryGetMacroCompletionContext(token, sourceText, position, out var macroContext))
        {
            var macroCompletions = GetMacroCompletions(
                model.Compilation,
                token.Parent ?? model.SyntaxTree.GetRoot(),
                macroContext).ToArray();
            if (macroCompletions.Length > 0)
                return macroCompletions;
        }

        var macroParameterCompletions = GetMacroParameterCompletions(
            model.Compilation,
            token,
            sourceText,
            position).ToArray();
        var semanticContextNode = semanticContext ?? token.Parent;
        var binder = model.GetBinder(semanticContextNode);
        model.EnsurePrecedingGlobalStatementsBoundForSemanticQuery(semanticContextNode);
        var fragmentLocalSymbols = fragmentLocals.IsDefaultOrEmpty
            ? ImmutableArray<Symbol>.Empty
            : fragmentLocals.Select(local => local.IsParameter
                ? (Symbol)new SourceParameterSymbol(
                    local.Name,
                    local.Type,
                    binder.ContainingSymbol ?? model.Compilation.GlobalNamespace,
                    binder.ContainingSymbol?.ContainingType,
                    binder.ContainingSymbol as NamespaceSymbol ?? binder.ContainingSymbol?.ContainingNamespace,
                    locations: [],
                    declaringSyntaxReferences: [])
                : new SourceLocalSymbol(
                    local.Name,
                    local.Type,
                    isMutable: false,
                    binder.ContainingSymbol ?? model.Compilation.GlobalNamespace,
                    binder.ContainingSymbol?.ContainingType,
                    binder.ContainingSymbol as NamespaceSymbol ?? binder.ContainingSymbol?.ContainingNamespace,
                    locations: [],
                    declaringSyntaxReferences: [])).ToImmutableArray();

        var completions = new List<CompletionItem>(macroParameterCompletions);
        var seen = new HashSet<string>(
            macroParameterCompletions.Select(static completion => completion.DisplayText),
            StringComparer.Ordinal);

        bool IsAccessible(Symbol symbol)
        {
            if (symbol is null)
                return false;

            if (symbol.DeclaredAccessibility == Accessibility.NotApplicable)
                return true;

            return binder.IsSymbolAccessible(symbol);
        }

        bool IsInSemanticModel(SyntaxNode node)
            => ReferenceEquals(node.SyntaxTree, model.SyntaxTree);

        Binder GetContextualBinder(SyntaxNode node)
            => IsInSemanticModel(node) ? model.GetBinder(node) : binder;

        TypeSymbol? GetContextualType(ExpressionSyntax node)
            => IsInSemanticModel(node) ? model.GetTypeInfo(node).Type : null;

        bool CanAccessPrivateSelfMembers()
        {
            for (Binder? current = binder; current is not null; current = current.ParentBinder)
            {
                if (current.ContainingSymbol is MethodSymbol method)
                {
                    if (method.IsExtensionMethod)
                        return false;

                    if (!method.IsStatic)
                        return true;

                    if (method is not LambdaSymbol)
                        return false;
                }
            }

            return token.GetAncestor<BaseMethodDeclarationSyntax>() is not null ||
                token.GetAncestor<ConstructorDeclarationSyntax>() is not null;
        }

        bool IsAccessibleOnSelf(Symbol symbol, TypeSymbol selfType)
        {
            if (IsAccessible(symbol))
                return true;

            if (!CanAccessPrivateSelfMembers())
                return false;

            return symbol.DeclaredAccessibility == Accessibility.Private &&
                symbol.ContainingType is { } containingType &&
                SymbolEqualityComparer.Default.Equals(containingType, selfType);
        }

        static string SafeToDisplayString(Symbol symbol)
        {
            try
            {
                return symbol.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat);
            }
            catch
            {
                return symbol.Name;
            }
        }

        TypeSymbol? GetSelfType()
        {
            static TypeSymbol? GetMethodSelfType(MethodSymbol method)
            {
                if (method.IsExtensionMethod && !method.Parameters.IsDefaultOrEmpty)
                    return method.Parameters[0].Type;

                if (!method.IsStatic)
                    return method.ContainingType;

                return null;
            }

            for (Binder? current = binder; current is not null; current = current.ParentBinder)
            {
                if (current.ContainingSymbol is MethodSymbol method)
                {
                    if (GetMethodSelfType(method) is { } methodSelfType)
                        return methodSelfType;

                    if (method is not LambdaSymbol)
                        return null;
                }
            }

            var containingMethodSyntax = (SyntaxNode?)token.GetAncestor<BaseMethodDeclarationSyntax>()
                ?? token.GetAncestor<ConstructorDeclarationSyntax>();
            if (containingMethodSyntax is not null &&
                model.GetDeclaredSymbol(containingMethodSyntax) is MethodSymbol declaredMethod &&
                GetMethodSelfType(declaredMethod) is { } declaredMethodSelfType)
            {
                return declaredMethodSelfType;
            }

            return null;
        }

        Symbol? TryLookupValueSymbolByName(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                return null;

            var fragmentLocal = fragmentLocalSymbols.FirstOrDefault(local =>
                string.Equals(local.Name, name, StringComparison.Ordinal));
            if (fragmentLocal is not null)
                return fragmentLocal;

            if (model.TryLookupVisibleValueSymbol(
                    semanticContextNode,
                    name,
                    allowBindingFallback: semanticContext is not null) is { } visibleSymbol)
            {
                return visibleSymbol;
            }

            var candidates = binder.LookupAvailableSymbols()
                .Where(symbol => string.Equals(symbol.Name, name, StringComparison.Ordinal))
                .ToArray();

            return candidates.FirstOrDefault(static symbol =>
                       symbol is LocalSymbol or ParameterSymbol or FieldSymbol or PropertySymbol or EventSymbol)
                   ?? candidates.FirstOrDefault();
        }

        bool TryGetPreferredSymbolInfo(SyntaxNode node, out SymbolInfo symbolInfo)
        {
            if (!IsInSemanticModel(node))
            {
                symbolInfo = default;
                return false;
            }

            if (model.TryGetNodeInterestSymbolInfo(node, out symbolInfo) &&
                HasSymbolInfo(symbolInfo))
            {
                return true;
            }

            if (model.TryGetCachedSymbolInfo(node, out symbolInfo) &&
                HasSymbolInfo(symbolInfo))
            {
                return true;
            }

            try
            {
                symbolInfo = model.GetSymbolInfo(node);
                return true;
            }
            catch
            {
                symbolInfo = default;
                return false;
            }
        }

        static bool HasSymbolInfo(SymbolInfo symbolInfo)
            => symbolInfo.Symbol is not null || !symbolInfo.CandidateSymbols.IsDefaultOrEmpty;

        static bool IsUsableNamespaceOrTypeSymbol(NamespaceOrTypeSymbol symbol)
            => symbol is not ErrorTypeSymbol and not TypeSymbol { TypeKind: TypeKind.Error };

        static NamespaceOrTypeSymbol? AsUsableNamespaceOrTypeSymbol(Symbol? symbol)
        {
            var namespaceOrType = symbol?.UnderlyingSymbol as NamespaceOrTypeSymbol;
            return namespaceOrType is not null && IsUsableNamespaceOrTypeSymbol(namespaceOrType)
                ? namespaceOrType
                : null;
        }

        NamespaceOrTypeSymbol? TryResolveNamespaceOrType(NameSyntax name)
        {
            NamespaceOrTypeSymbol? resolved = null;
            if (TryGetPreferredSymbolInfo(name, out var symbolInfo))
                resolved = AsUsableNamespaceOrTypeSymbol(symbolInfo.Symbol);

            if (resolved is not null)
                return resolved;

            return name switch
            {
                IdentifierNameSyntax identifier => TryResolveIdentifierNamespaceOrType(identifier),
                QualifiedNameSyntax qualified => TryResolveQualifiedNamespaceOrType(qualified),
                GenericNameSyntax generic => TryResolveGenericNamespaceOrType(generic),
                _ => null
            };
        }

        NamespaceOrTypeSymbol? TryResolveImportNamespaceOrType(NameSyntax name)
        {
            return name switch
            {
                IdentifierNameSyntax identifier => TryResolveIdentifierNamespaceOrType(identifier),
                QualifiedNameSyntax qualified => TryResolveImportQualifiedNamespaceOrType(qualified),
                GenericNameSyntax generic => TryResolveGenericNamespaceOrType(generic),
                _ => null
            };
        }

        NamespaceOrTypeSymbol? TryResolveImportQualifiedNamespaceOrType(QualifiedNameSyntax qualified)
        {
            var left = TryResolveImportNamespaceOrType(qualified.Left);
            if (left is null)
                return null;

            return qualified.Right switch
            {
                IdentifierNameSyntax identifier => LookupQualifiedMember(left, identifier.Identifier.ValueText, arity: 0),
                GenericNameSyntax generic => LookupQualifiedMember(left, generic.Identifier.ValueText, generic.TypeArgumentList.Arguments.Count),
                _ => null
            };
        }

        NamespaceOrTypeSymbol? TryResolveIdentifierNamespaceOrType(IdentifierNameSyntax identifier)
        {
            var name = identifier.Identifier.ValueText;
            if (string.IsNullOrWhiteSpace(name))
                return null;

            if (TryResolveEnclosingTypeParameter(identifier, name) is { } typeParameter)
                return typeParameter;

            return AsUsableNamespaceOrTypeSymbol(binder.LookupType(name))
                ?? binder.LookupNamespace(name)
                ?? AsUsableNamespaceOrTypeSymbol(binder.LookupSymbol(name))
                ?? LookupRootMember(name, arity: 0);
        }

        TypeParameterSymbol? TryResolveEnclosingTypeParameter(SyntaxNode node, string name)
        {
            foreach (var ancestor in node.Ancestors())
            {
                switch (ancestor)
                {
                    case MethodDeclarationSyntax methodDeclaration
                        when model.GetDeclaredSymbol(methodDeclaration) is MethodSymbol method:
                        return method.TypeParameters.FirstOrDefault(typeParameter => typeParameter.Name == name);

                    case FunctionStatementSyntax functionStatement
                        when model.GetDeclaredSymbol(functionStatement) is MethodSymbol function:
                        return function.TypeParameters.FirstOrDefault(typeParameter => typeParameter.Name == name);

                    case TypeDeclarationSyntax typeDeclaration
                        when model.GetDeclaredSymbol(typeDeclaration) is NamedTypeSymbol type:
                        return type.TypeParameters.FirstOrDefault(typeParameter => typeParameter.Name == name);

                    case InterfaceDeclarationSyntax interfaceDeclaration
                        when model.GetDeclaredSymbol(interfaceDeclaration) is NamedTypeSymbol @interface:
                        return @interface.TypeParameters.FirstOrDefault(typeParameter => typeParameter.Name == name);
                }
            }

            return null;
        }

        NamespaceOrTypeSymbol? TryResolveQualifiedNamespaceOrType(QualifiedNameSyntax qualified)
        {
            var left = TryResolveNamespaceOrType(qualified.Left);
            if (left is null)
                return TryResolveRootNamespaceOrType(qualified);

            var member = qualified.Right switch
            {
                IdentifierNameSyntax identifier => LookupQualifiedMember(left, identifier.Identifier.ValueText, arity: 0),
                GenericNameSyntax generic => LookupQualifiedMember(left, generic.Identifier.ValueText, generic.TypeArgumentList.Arguments.Count),
                _ => null
            };

            return member ?? TryResolveRootNamespaceOrType(qualified);
        }

        NamespaceOrTypeSymbol? TryResolveGenericNamespaceOrType(GenericNameSyntax generic)
        {
            var arity = generic.TypeArgumentList.Arguments.Count;
            var name = generic.Identifier.ValueText;

            var namedType = binder.LookupSymbols(name)
                .OfType<NamedTypeSymbol>()
                .FirstOrDefault(candidate => candidate.Arity == arity && IsUsableNamespaceOrTypeSymbol(candidate));

            return namedType
                ?? AsUsableNamespaceOrTypeSymbol(binder.LookupSymbol(name))
                ?? LookupRootMember(name, arity);
        }

        NamespaceOrTypeSymbol? TryResolveRootNamespaceOrType(NameSyntax name)
        {
            return name switch
            {
                IdentifierNameSyntax identifier => LookupRootMember(identifier.Identifier.ValueText, arity: 0),
                GenericNameSyntax generic => LookupRootMember(generic.Identifier.ValueText, generic.TypeArgumentList.Arguments.Count),
                QualifiedNameSyntax qualified => TryResolveRootQualifiedNamespaceOrType(qualified),
                _ => null
            };
        }

        NamespaceOrTypeSymbol? TryResolveRootQualifiedNamespaceOrType(QualifiedNameSyntax qualified)
        {
            var left = TryResolveRootNamespaceOrType(qualified.Left);
            if (left is null)
                return null;

            return qualified.Right switch
            {
                IdentifierNameSyntax identifier => LookupQualifiedMember(left, identifier.Identifier.ValueText, arity: 0),
                GenericNameSyntax generic => LookupQualifiedMember(left, generic.Identifier.ValueText, generic.TypeArgumentList.Arguments.Count),
                _ => null
            };
        }

        NamespaceOrTypeSymbol? LookupRootMember(string name, int arity)
        {
            if (string.IsNullOrWhiteSpace(name))
                return null;

            model.Compilation.EnsureSetup();

            return LookupQualifiedMember(model.Compilation.GlobalNamespace, name, arity);
        }

        NamespaceOrTypeSymbol? LookupQualifiedMember(NamespaceOrTypeSymbol? container, string name, int arity)
        {
            if (container is null || string.IsNullOrWhiteSpace(name))
                return null;

            if (container is NamespaceSymbol namespaceSymbol)
            {
                var directMember = (NamespaceOrTypeSymbol?)namespaceSymbol.LookupNamespace(name)
                    ?? SelectTypeMember(namespaceSymbol.GetMembers(name).OfType<NamedTypeSymbol>(), arity)
                    ?? (NamespaceOrTypeSymbol?)namespaceSymbol.LookupType(name);
                if (directMember is not null)
                    return directMember;

                foreach (var member in GetNamespaceCompletionMembers(namespaceSymbol).OfType<NamespaceOrTypeSymbol>())
                {
                    if (!string.Equals(member.Name, name, StringComparison.Ordinal))
                        continue;

                    if (member is NamedTypeSymbol namedType)
                        return SelectTypeMember(new[] { namedType }, arity);

                    return member;
                }

                return null;
            }

            if (container is TypeSymbol typeSymbol)
            {
                return SelectTypeMember(typeSymbol.GetMembers(name).OfType<NamedTypeSymbol>(), arity);
            }

            return null;
        }

        static NamedTypeSymbol? SelectTypeMember(IEnumerable<NamedTypeSymbol> candidates, int arity)
        {
            foreach (var candidate in candidates)
            {
                if (candidate.Arity == arity)
                    return candidate;
            }

            return null;
        }

        Symbol? TryResolveReceiverSymbol(ExpressionSyntax receiverExpression)
        {
            if (TryGetPreferredSymbolInfo(receiverExpression, out var preferredSymbolInfo) &&
                preferredSymbolInfo.Symbol is { } preferredSymbol)
            {
                return preferredSymbol.UnderlyingSymbol;
            }

            string? receiverName = null;
            if (receiverExpression is IdentifierNameSyntax receiverIdentifier)
            {
                receiverName = receiverIdentifier.Identifier.ValueText;
                if (string.IsNullOrWhiteSpace(receiverName))
                    return null;

                var symbol = TryLookupValueSymbolByName(receiverName);
                if (symbol is not null)
                    return symbol;
            }

            var receiverBinder = GetContextualBinder(receiverExpression);
            var binderResolvedSymbol = receiverName is not null
                ? receiverBinder.LookupSymbol(receiverName) ?? receiverBinder.LookupSymbols(receiverName).FirstOrDefault()
                : null;
            if (binderResolvedSymbol is not null)
                return binderResolvedSymbol;

            if (receiverExpression is IdentifierNameSyntax receiverIdentifier2 &&
                TryResolveEnclosingPatternOrLoopLocalSymbol(receiverExpression, receiverIdentifier2.Identifier.ValueText) is { } enclosingSymbol)
                return enclosingSymbol;

            var containingBlock = receiverExpression.GetAncestor<BlockStatementSyntax>();
            if (containingBlock is null)
                return null;

            if (receiverExpression is not IdentifierNameSyntax receiverIdentifier3)
                return null;

            receiverName = receiverIdentifier3.Identifier.ValueText;

            var localDeclarator = containingBlock
                .DescendantNodes()
                .OfType<VariableDeclaratorSyntax>()
                .Where(declarator =>
                    declarator.Identifier.ValueText == receiverName &&
                    declarator.Span.Start <= receiverExpression.Span.Start)
                .OrderByDescending(static declarator => declarator.Span.Start)
                .FirstOrDefault();

            return localDeclarator is not null
                ? model.GetDeclaredSymbol(localDeclarator)
                : null;
        }

        Symbol? TryResolveEnclosingPatternOrLoopLocalSymbol(
            ExpressionSyntax receiverExpression,
            string name)
        {
            foreach (var ancestor in receiverExpression.Ancestors())
            {
                switch (ancestor)
                {
                    case ForStatementSyntax forStatement
                        when forStatement.Body.Span.Contains(receiverExpression.Span):
                        {
                            if (TryResolveForTargetSymbol(forStatement, receiverExpression, name) is { } forSymbol)
                                return forSymbol;

                            break;
                        }

                    case IfStatementSyntax ifStatement
                        when ifStatement.ThenStatement.Span.Contains(receiverExpression.Span):
                        {
                            if (TryResolvePatternDesignationSymbol(ifStatement.Condition, receiverExpression, name) is { } ifPatternSymbol)
                                return ifPatternSymbol;

                            break;
                        }

                    case IfPatternStatementSyntax ifPatternStatement
                        when ifPatternStatement.ThenStatement.Span.Contains(receiverExpression.Span):
                        {
                            if (TryResolvePatternDesignationSymbol(ifPatternStatement.Pattern, receiverExpression, name) is { } ifPatternSymbol)
                                return ifPatternSymbol;

                            break;
                        }

                    case IfPatternExpressionSyntax ifPatternExpression
                        when ifPatternExpression.Expression.Span.Contains(receiverExpression.Span):
                        {
                            if (TryResolvePatternDesignationSymbol(ifPatternExpression.Pattern, receiverExpression, name) is { } ifPatternSymbol)
                                return ifPatternSymbol;

                            break;
                        }

                    case WhilePatternStatementSyntax whilePatternStatement
                        when whilePatternStatement.Statement.Span.Contains(receiverExpression.Span):
                        {
                            if (TryResolvePatternDesignationSymbol(whilePatternStatement.Pattern, receiverExpression, name) is { } whilePatternSymbol)
                                return whilePatternSymbol;

                            break;
                        }

                    case MatchArmSyntax matchArm:
                        {
                            if (TryResolvePatternDesignationSymbol(matchArm.Pattern, receiverExpression, name) is { } armPatternSymbol)
                                return armPatternSymbol;

                            break;
                        }
                }
            }

            return null;
        }

        Symbol? TryResolveForTargetSymbol(
            ForStatementSyntax forStatement,
            ExpressionSyntax receiverExpression,
            string name)
        {
            var iterationType = TryGetForIterationElementType(forStatement.Expression);

            if (forStatement.Target is IdentifierNameSyntax target &&
                target.Identifier.ValueText == name)
            {
                return iterationType is not null
                    ? CreateSyntheticReceiverLocalSymbol(name, iterationType, receiverExpression)
                    : null;
            }

            if (forStatement.Target is PatternSyntax pattern)
            {
                if (TryResolvePatternDesignationSymbol(pattern, receiverExpression, name) is { } symbol)
                    return symbol;
            }

            return null;
        }

        Symbol? TryResolvePatternDesignationSymbol(
            SyntaxNode patternRoot,
            ExpressionSyntax receiverExpression,
            string name)
        {
            var designation = patternRoot
                .DescendantNodesAndSelf()
                .OfType<SingleVariableDesignationSyntax>()
                .Where(single =>
                    single.Identifier.ValueText == name &&
                    single.Span.Start <= receiverExpression.Span.Start)
                .OrderByDescending(static single => single.Span.Start)
                .FirstOrDefault();

            return designation is not null
                ? model.GetDeclaredSymbol(designation)
                : null;
        }

        LocalSymbol CreateSyntheticReceiverLocalSymbol(string name, TypeSymbol type, ExpressionSyntax receiverExpression)
        {
            var containingSymbol = GetContextualBinder(receiverExpression).ContainingSymbol ?? model.Compilation.GlobalNamespace;
            return new SourceLocalSymbol(
                name,
                type,
                isMutable: false,
                containingSymbol,
                containingSymbol.ContainingType,
                containingSymbol as NamespaceSymbol ?? containingSymbol.ContainingNamespace,
                locations: [],
                declaringSyntaxReferences: []);
        }

        TypeSymbol? TryGetForIterationElementType(ExpressionSyntax expression)
        {
            var collectionType = TryGetExpressionType(expression);
            if (collectionType is null || collectionType.TypeKind == TypeKind.Error)
                return null;

            return SequenceTypeUtilities.TryGetElementType(model.Compilation, collectionType);
        }

        TypeSymbol? TryGetExpressionType(ExpressionSyntax expression)
        {
            if (TryGetPreferredSymbolInfo(expression, out var symbolInfo) &&
                GetTypeFromSymbol(symbolInfo.Symbol?.UnderlyingSymbol) is { } symbolType)
            {
                return symbolType;
            }

            return GetContextualType(expression);
        }

        var tokenValueText = forceInsertionAtCaret ? string.Empty : GetCompletionPrefix(token);
        var hasExistingArgumentList = token.Parent?.AncestorsAndSelf()
            .OfType<InvocationExpressionSyntax>()
            .Any(invocation => invocation.Expression.Span.End == token.Span.End &&
                !invocation.ArgumentList.OpenParenToken.IsMissing) == true;
        var replacementSpan = forceInsertionAtCaret
            ? new TextSpan(position, 0)
            : token.Span;
        var literalReplacementSpan = new TextSpan(position, 0);
        var isInvocationArgumentStartToken =
            token.Parent is ArgumentListSyntax argumentList &&
            argumentList.OpenParenToken == token &&
            argumentList.Parent is InvocationExpressionSyntax;
        var isElementArgumentStartToken =
            token.Parent is BracketedArgumentListSyntax bracketedArgumentList &&
            bracketedArgumentList.OpenBracketToken == token &&
            bracketedArgumentList.Parent is ElementAccessExpressionSyntax or ElementBindingExpressionSyntax;
        var isArgumentStartToken = isInvocationArgumentStartToken || isElementArgumentStartToken;
        var argumentStartReplacementSpan = isArgumentStartToken
            ? new TextSpan(position, 0)
            : replacementSpan;

        bool ShouldOfferSelfCompletion()
        {
            if (semanticContext is not null && token.Kind == SyntaxKind.None)
                return true;

            if (token.IsKind(SyntaxKind.IdentifierToken))
                return true;

            if (token.Parent is IdentifierNameSyntax { Identifier.IsMissing: false })
                return true;

            if (token.GetAncestor<BaseMethodDeclarationSyntax>() is not null ||
                token.GetAncestor<ConstructorDeclarationSyntax>() is not null)
            {
                return true;
            }

            return token.GetAncestor<BlockStatementSyntax>() is not null
                || token.GetAncestor<BlockSyntax>() is not null;
        }

        string GetSelfCompletionPrefix()
        {
            if (forceInsertionAtCaret)
                return string.Empty;

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

        string EscapeIdentifierForInsertion(string identifier)
        {
            if (string.IsNullOrEmpty(identifier))
                return identifier;

            if (SyntaxFacts.RequiresBacktickEscaping(identifier))
                return "`" + identifier + "`";

            return SyntaxFacts.TryParseKeyword(identifier, out var keyword) &&
                (keyword != SyntaxKind.CharKeyword || model.SyntaxTree.Options.EnableCharKeyword)
                ? "@" + identifier
                : identifier;
        }

        string GetCompletionPrefix(SyntaxToken identifier)
        {
            if (position < identifier.SpanStart || position >= identifier.Span.End)
                return identifier.ValueText;

            var prefix = identifier.Text[..(position - identifier.SpanStart)];
            return prefix.StartsWith('@') || prefix.StartsWith('`') ? prefix[1..] : prefix;
        }

        (string displayText, string insertionText, string dedupKey) CreateCompletionParts(Symbol symbol)
        {
            var escapedName = EscapeIdentifierForInsertion(symbol.Name);
            var insertionText = symbol is MethodSymbol && !hasExistingArgumentList
                ? escapedName + "()"
                : escapedName;
            var displayText = CompletionService.GetCompletionDisplayText(symbol, escapedName);
            var dedupKey = CompletionService.GetCompletionDedupKey(symbol, displayText);

            return (displayText, insertionText, dedupKey);
        }

        static bool NameMatchesPrefix(string symbolName, string prefix)
        {
            if (string.IsNullOrEmpty(prefix))
                return true;

            if (symbolName.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                return true;

            return symbolName.Length > 1 &&
                symbolName[0] == '@' &&
                symbolName.AsSpan(1).StartsWith(prefix.AsSpan(), StringComparison.OrdinalIgnoreCase);
        }

        static int? GetDefaultCursorOffset(Symbol symbol, string insertionText)
        {
            return symbol switch
            {
                MethodSymbol when insertionText.EndsWith("()", StringComparison.Ordinal) => insertionText.Length - 1,
                _ => (int?)null
            };
        }

        static bool IsSuppressedCompletionMethod(MethodSymbol method)
        {
            if (method.MethodKind == MethodKind.Constructor || method.IsConstructor)
                return true;

            if (method.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.EventAdd or MethodKind.EventRemove or MethodKind.EventRaise)
                return true;

            return method.Name.StartsWith("get_", StringComparison.Ordinal) ||
                   method.Name.StartsWith("set_", StringComparison.Ordinal) ||
                   method.Name.StartsWith("add_", StringComparison.Ordinal) ||
                   method.Name.StartsWith("remove_", StringComparison.Ordinal) ||
                   method.Name.StartsWith("op_", StringComparison.Ordinal) ||
                   string.Equals(method.Name, "Finalize", StringComparison.Ordinal) ||
                   string.Equals(method.Name, ".ctor", StringComparison.Ordinal) ||
                   string.Equals(method.Name, ".cctor", StringComparison.Ordinal);
        }

        static bool IsWeakExtensionReceiverType(TypeSymbol receiverType)
        {
            if (receiverType is null)
                return true;

            if (receiverType is TypeParameterSymbol)
                return true;

            if (receiverType.SpecialType == SpecialType.System_Object)
                return true;

            return false;
        }

        static bool IsImportDirectiveTypeMemberCompletion(Symbol member)
            => member is NamespaceOrTypeSymbol or FieldSymbol { IsConst: true };

        static bool ShouldIncludeExtensionMethod(MethodSymbol method, string prefix)
        {
            if (method.Parameters.IsDefaultOrEmpty || method.Parameters.Length == 0)
                return false;

            // C#-like behavior: keep broad "this T"/"this object" extensions out
            // of empty-prefix completion lists; still allow them when user typed a prefix.
            if (!string.IsNullOrEmpty(prefix))
                return true;

            var receiverType = method.Parameters[0].Type;
            return !IsWeakExtensionReceiverType(receiverType);
        }

        static bool IsTypeAccessSymbol(Symbol? symbol, TypeSymbol? type)
        {
            return symbol switch
            {
                NamedTypeSymbol or TypeParameterSymbol => type is not null && SymbolEqualityComparer.Default.Equals(symbol, type),
                _ => false
            };
        }

        static TypeSymbol? TryGetTypeAccessSymbol(Symbol? symbol, TypeSymbol? type)
        {
            if (IsTypeAccessSymbol(symbol, type) && symbol is TypeSymbol symbolType)
                return symbolType;

            return null;
        }

        static IEnumerable<Symbol> GetTypeMembersIncludingBase(TypeSymbol type, bool includeStatic)
        {
            var yielded = new HashSet<Symbol>(SymbolEqualityComparer.Default);
            foreach (var member in type.GetMembersRecursive(
                TypeSymbolLookupExtensions.MemberLookupFlags.Default,
                member => member.IsStatic == includeStatic))
            {
                if (yielded.Add(member))
                    yield return member;
            }

            if (type is not NamedTypeSymbol namedType)
                yield break;

            foreach (var interfaceType in namedType.AllInterfaces)
            {
                foreach (var member in interfaceType.GetMembersRecursive(
                    TypeSymbolLookupExtensions.MemberLookupFlags.Default,
                    member => member.IsStatic == includeStatic))
                {
                    if (yielded.Add(member))
                        yield return member;
                }
            }
        }

        static IEnumerable<Symbol> GetInstanceMembersIncludingBase(TypeSymbol type)
        {
            return GetTypeMembersIncludingBase(type, includeStatic: false)
                .Where(IsInstanceCompletionMember);
        }

        static bool IsInstanceCompletionMember(Symbol member)
        {
            if (!member.CanBeReferencedByName || member is NamespaceOrTypeSymbol)
                return false;

            return member switch
            {
                MethodSymbol method => method.AssociatedSymbol is null,
                FieldSymbol field => field.AssociatedSymbol is null,
                PropertySymbol or EventSymbol => true,
                _ => false
            };
        }

        static bool IsTypeCompletionMember(Symbol member)
        {
            if (!member.CanBeReferencedByName)
                return false;

            return member switch
            {
                NamedTypeSymbol => true,
                MethodSymbol method => method.IsStatic && method.AssociatedSymbol is null,
                FieldSymbol field => field.IsStatic && field.AssociatedSymbol is null,
                PropertySymbol property => property.IsStatic,
                EventSymbol @event => @event.IsStatic,
                _ => false
            };
        }

        static TypeSymbol UnwrapAliases(TypeSymbol type)
        {
            while (type.IsAlias && type.UnderlyingSymbol is TypeSymbol alias)
                type = alias;

            return type;
        }

        static bool TryGetOptionPayloadType(TypeSymbol? type, out TypeSymbol payload)
        {
            payload = null!;
            if (type is null)
                return false;

            type = type.UnwrapLiteralType() ?? type;
            if (type is NamedTypeSymbol named &&
                named.Arity == 1 &&
                string.Equals(named.Name, OptionalFacts.TypeName, StringComparison.Ordinal))
            {
                payload = named.TypeArguments[0];
                return true;
            }

            return false;
        }

        static bool TryGetResultPayloadType(TypeSymbol? type, out TypeSymbol payload)
        {
            payload = null!;
            if (type is null)
                return false;

            type = type.UnwrapLiteralType() ?? type;
            if (type is NamedTypeSymbol named &&
                named.Arity == 2 &&
                string.Equals(named.Name, "Result", StringComparison.Ordinal))
            {
                payload = named.TypeArguments[0];
                return true;
            }

            return false;
        }

        static TypeSymbol? GetCarrierConditionalAccessLookupType(TypeSymbol? type)
        {
            if (type is null)
                return null;

            type = type.UnwrapLiteralType() ?? type;
            if (TryGetOptionPayloadType(type, out var optionPayload))
                return optionPayload.GetNonNullableType();

            if (TryGetResultPayloadType(type, out var resultPayload))
                return resultPayload.GetNonNullableType();

            return type.GetNonNullableType();
        }

        static TypeSymbol? GetTypeFromSymbol(Symbol? symbol)
        {
            while (symbol is not null)
            {
                switch (symbol)
                {
                    case TypeSymbol type:
                        return type;
                    case LocalSymbol local:
                        return local.Type;
                    case FieldSymbol field:
                        return field.Type;
                    case PropertySymbol property:
                        return property.Type;
                    case EventSymbol @event:
                        return @event.Type;
                    case ParameterSymbol parameter:
                        return parameter.Type;
                    case MethodSymbol method:
                        return method.ReturnType;
                }

                var underlying = symbol.UnderlyingSymbol;
                if (ReferenceEquals(underlying, symbol))
                    break;

                symbol = underlying;
            }

            return null;
        }

        static SyntaxToken? GetImportCompletionNameToken(NameSyntax name)
            => name switch
            {
                SimpleNameSyntax simpleName => simpleName.Identifier,
                WildcardNameSyntax wildcardName => wildcardName.StartToken,
                _ => null
            };

        static TextSpan GetImportCompletionReplacementSpan(SyntaxToken token, int position)
            => token.IsMissing
                ? new TextSpan(position, 0)
                : token.Span;

        static bool IsAtImportCompletionName(QualifiedNameSyntax qualified, SyntaxToken token, int position)
            => position >= token.Position ||
               (token.IsMissing && position >= qualified.DotToken.Span.End);

        bool IsImportLineCompletionContext(SyntaxNode node)
        {
            var text = sourceText.ToString();
            var start = Math.Clamp(node.Span.Start, 0, text.Length);
            var lineStart = start;

            while (lineStart > 0)
            {
                var ch = text[lineStart - 1];
                if (ch is '\r' or '\n')
                    break;

                lineStart--;
            }

            var prefix = text.Substring(lineStart, start - lineStart).TrimStart();
            return prefix.StartsWith("import ", StringComparison.Ordinal) ||
                   string.Equals(prefix, "import", StringComparison.Ordinal);
        }

        (Symbol? Symbol, TypeSymbol? Type) ResolveReceiver(ExpressionSyntax expression)
        {
            var symbol = TryGetPreferredSymbolInfo(expression, out var symbolInfo)
                ? symbolInfo.Symbol?.UnderlyingSymbol
                : null;

            if (expression is InvocationExpressionSyntax &&
                symbol is MethodSymbol invokedMethod)
            {
                return (symbol, invokedMethod.ReturnType);
            }

            if (symbol is null &&
                expression is IdentifierNameSyntax receiverIdentifier &&
                TryLookupValueSymbolByName(receiverIdentifier.Identifier.ValueText) is { } visibleSymbol)
            {
                symbol = visibleSymbol;
            }

            var type = expression is NameOfExpressionSyntax
                ? GetContextualType(expression)
                : null;
            if (type is not null && type.TypeKind != TypeKind.Error)
                return (symbol, type);

            type = GetTypeFromSymbol(symbol);
            if (type is not null && type.TypeKind != TypeKind.Error)
                return (symbol, type);

            if (TryGetNullableSuppressionReceiverType(expression, out var suppressedReceiverType))
                return (symbol, suppressedReceiverType);

            var binderSymbol = GetContextualBinder(expression).BindSymbol(expression).Symbol?.UnderlyingSymbol;
            if (binderSymbol is not null)
            {
                symbol = binderSymbol;
                type = GetTypeFromSymbol(symbol);
                if (type is not null && type.TypeKind != TypeKind.Error)
                    return (symbol, type);
            }

            type = GetContextualType(expression);
            if ((type is null || type.TypeKind == TypeKind.Error) &&
                TryGetNullableSuppressionReceiverType(expression, out suppressedReceiverType))
            {
                type = suppressedReceiverType;
            }

            return (symbol, type);
        }

        bool TryGetNullableSuppressionReceiverType(
            ExpressionSyntax expression,
            [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out TypeSymbol? receiverType)
        {
            receiverType = null;
            if (expression is not PostfixOperatorExpressionSyntax
                {
                    Kind: SyntaxKind.SuppressNullableWarningExpression
                } suppression)
            {
                return false;
            }

            // The semantic result also covers carrier unwrap, whose payload can
            // have entirely different members from the carrier itself.
            if (IsInSemanticModel(suppression))
            {
                var unwrapped = model.GetTypeInfo(suppression).Type;
                if (unwrapped is { TypeKind: not TypeKind.Error })
                {
                    receiverType = unwrapped;
                    return true;
                }
            }

            TypeSymbol? operandType = null;
            if (suppression.Expression is DefaultExpressionSyntax &&
                TryGetTargetTypedDefaultSuppressionReceiverType(suppression, out var defaultTargetType))
            {
                operandType = defaultTargetType.GetDefaultValueType();
            }
            else
            {
                var operandInfo = IsInSemanticModel(suppression.Expression)
                    ? model.GetTypeInfo(suppression.Expression)
                    : default;
                operandType = operandInfo.Type ?? operandInfo.ConvertedType;
            }

            if (operandType is null || operandType.TypeKind == TypeKind.Error)
                return false;

            receiverType = operandType.GetNonNullableType();
            return receiverType.TypeKind != TypeKind.Error;
        }

        bool TryGetTargetTypedDefaultSuppressionReceiverType(
            PostfixOperatorExpressionSyntax suppression,
            [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out TypeSymbol? targetType)
        {
            targetType = null;
            if (suppression.Parent is not MemberAccessExpressionSyntax memberAccess ||
                !IsSameSyntaxNode(memberAccess.Expression, suppression))
            {
                return false;
            }

            var context = memberAccess.Parent;
            if (context is ReturnStatementSyntax returnStatement &&
                returnStatement.Expression is { } returnExpression &&
                IsSameSyntaxNode(returnExpression, memberAccess))
            {
                targetType = TryGetContainingReturnType(returnStatement);
                return targetType is not null && targetType.TypeKind != TypeKind.Error;
            }

            if (context is ReturnExpressionSyntax returnExpressionSyntax &&
                IsSameSyntaxNode(returnExpressionSyntax.Expression, memberAccess))
            {
                targetType = TryGetContainingReturnType(returnExpressionSyntax);
                return targetType is not null && targetType.TypeKind != TypeKind.Error;
            }

            return false;
        }

        TypeSymbol? TryGetContainingReturnType(SyntaxNode node)
        {
            foreach (var ancestor in node.Ancestors())
            {
                switch (ancestor)
                {
                    case BaseMethodDeclarationSyntax methodDeclaration
                        when model.GetDeclaredSymbol(methodDeclaration) is MethodSymbol method:
                        return method.ReturnType;

                    case FunctionStatementSyntax functionStatement
                        when model.GetDeclaredSymbol(functionStatement) is MethodSymbol function:
                        return function.ReturnType;

                    case AccessorDeclarationSyntax accessor
                        when model.GetDeclaredSymbol(accessor) is MethodSymbol accessorMethod:
                        return accessorMethod.ReturnType;
                }
            }

            return null;
        }

        static bool IsSameSyntaxNode(SyntaxNode left, SyntaxNode right)
            => left.Kind == right.Kind &&
               left.Span == right.Span &&
               ReferenceEquals(left.SyntaxTree, right.SyntaxTree);

        static bool NeedsReceiverFallback(Symbol? symbol, TypeSymbol? type)
        {
            if (symbol is null)
                return true;

            if (symbol.Kind is SymbolKind.Error or SymbolKind.ErrorType)
                return true;

            if (type is null)
                return true;

            return type.TypeKind == TypeKind.Error;
        }

        IEnumerable<Symbol> GetTypeAccessMembers(TypeSymbol typeAccessSymbol)
        {
            if (typeAccessSymbol is NamedTypeSymbol namedType)
            {
                var staticMembers = namedType.GetMembers().Where(m =>
                    IsTypeCompletionMember(m) &&
                    IsAccessible(m) &&
                    !(model.Compilation.Options.FrameworkProjectionMode == FrameworkProjectionMode.Standard &&
                      FrameworkProjectionCatalog.TryGetStandard(namedType, m.Name, out var descriptor) &&
                      model.Compilation.GetTypeByMetadataName(descriptor.ProjectedContainer) is { } projectedContainer &&
                      projectedContainer.GetMembers(m.Name).OfType<MethodSymbol>().Any()));
                if (model.Compilation.Options.FrameworkProjectionMode == FrameworkProjectionMode.Standard)
                    staticMembers = staticMembers.Concat(FrameworkProjectionCatalog.GetStandardMethods(model.Compilation, namedType));
                return namedType is UnionSymbol union
                    ? staticMembers.Concat(union.Variants.Where(IsAccessible))
                    : staticMembers;
            }

            if (typeAccessSymbol is not TypeParameterSymbol typeParameter)
                return Array.Empty<Symbol>();

            binder.EnsureTypeParameterConstraintTypesResolved(ImmutableArray.Create(typeParameter));

            var members = new List<Symbol>();
            var seen = new HashSet<Symbol>(SymbolEqualityComparer.Default);
            var visitedTypeParameters = new HashSet<TypeParameterSymbol>(SymbolEqualityComparer.Default);

            void AddConstraintMembers(TypeSymbol constraintType)
            {
                if (constraintType is NamedTypeSymbol namedConstraint)
                {
                    foreach (var member in namedConstraint.GetMembers())
                    {
                        if (!member.IsStatic || !IsAccessible(member) || !seen.Add(member))
                            continue;

                        members.Add(member);
                    }

                    return;
                }

                if (constraintType is TypeParameterSymbol nestedTypeParameter &&
                    visitedTypeParameters.Add(nestedTypeParameter))
                {
                    foreach (var nestedConstraint in nestedTypeParameter.ConstraintTypes)
                        AddConstraintMembers(nestedConstraint);
                }
            }

            visitedTypeParameters.Add(typeParameter);
            foreach (var constraintType in typeParameter.ConstraintTypes)
                AddConstraintMembers(constraintType);

            return members;
        }

        IEnumerable<Symbol> GetNamespaceCompletionMembers(NamespaceSymbol namespaceSymbol)
        {
            var includeNamespaceMembers = model.Compilation.Options.AllowNamespaceMembers &&
                                          model.Compilation.Options.AllowNamespaceMemberImports;

            return namespaceSymbol.GetMembers()
                .Concat(model.Compilation.GetNamespaceMembers(namespaceSymbol, includeNamespaceMembers))
                .Where(IsAccessible);
        }

        void AddCompletionItem(Symbol symbol, TextSpan replacementSpan)
        {
            if (symbol is MethodSymbol method && IsSuppressedCompletionMethod(method))
                return;

            var (displayText, insertText, dedupKey) = CreateCompletionParts(symbol);
            var cursorOffset = GetDefaultCursorOffset(symbol, insertText);

            if (!seen.Add(dedupKey))
                return;

            completions.Add(new CompletionItem(
                DisplayText: displayText,
                InsertionText: insertText,
                ReplacementSpan: replacementSpan,
                CursorOffset: cursorOffset,
                Description: SafeToDisplayString(symbol),
                Symbol: symbol
            ));
        }

        void AddAttributeCompletionItem(NamedTypeSymbol type, string prefix, TextSpan replacementSpan)
        {
            if (!IsAccessible(type) || !IsAttributeType(type) || !AttributeNameMatchesPrefix(type, prefix))
                return;

            var name = GetAttributeCompletionName(type);
            var escapedName = EscapeIdentifierForInsertion(name);
            var dedupKey = type.ToFullyQualifiedMetadataName() ?? type.Name;

            if (!seen.Add(dedupKey))
                return;

            completions.Add(new CompletionItem(
                DisplayText: escapedName,
                InsertionText: escapedName,
                ReplacementSpan: replacementSpan,
                CursorOffset: escapedName.Length,
                Description: SafeToDisplayString(type),
                Symbol: type));
        }

        void AddAttributeTypeCompletions(string prefix, TextSpan replacementSpan)
        {
            foreach (var symbol in binder.LookupAvailableSymbols())
            {
                if (symbol is NamedTypeSymbol type)
                    AddAttributeCompletionItem(type, prefix, replacementSpan);
            }
        }

        void AddNamespaceCompletionItem(NamespaceSymbol namespaceSymbol, string prefix, TextSpan replacementSpan)
        {
            if (!NameMatchesPrefix(namespaceSymbol.Name, prefix))
                return;

            var (displayText, insertText, dedupKey) = CreateCompletionParts(namespaceSymbol);

            if (!seen.Add(dedupKey))
                return;

            completions.Add(new CompletionItem(
                DisplayText: displayText,
                InsertionText: insertText,
                ReplacementSpan: replacementSpan,
                Description: SafeToDisplayString(namespaceSymbol),
                Symbol: namespaceSymbol));
        }

        void AddQualifiedAttributeNameCompletions(NamespaceOrTypeSymbol nsOrType, string prefix, TextSpan replacementSpan)
        {
            var members = nsOrType is NamespaceSymbol attributeNamespace
                ? GetNamespaceCompletionMembers(attributeNamespace)
                : nsOrType.GetMembers().Where(IsAccessible);

            foreach (var member in members)
            {
                if (member is NamespaceSymbol namespaceSymbol)
                    AddNamespaceCompletionItem(namespaceSymbol, prefix, replacementSpan);
                else if (member is NamedTypeSymbol type)
                    AddAttributeCompletionItem(type, prefix, replacementSpan);
            }
        }

        NamespaceOrTypeSymbol? TryResolveAttributeCompletionReceiver(NameSyntax name)
        {
            var metadataName = name.ToString();
            if (model.Compilation.SymbolLookup.GetNamespace(metadataName) is { } namespaceSymbol)
                return namespaceSymbol;

            if (TryResolveNamespaceOrType(name) is { } resolved)
                return resolved;

            return model.Compilation.SymbolLookup.GetTypeByMetadataNameSourceFirst(metadataName);
        }

        void AddWildcardImportCompletion(string prefix, TextSpan replacementSpan)
        {
            if (!NameMatchesPrefix("*", prefix))
                return;

            if (!seen.Add("__import_wildcard"))
                return;

            completions.Add(new CompletionItem(
                DisplayText: "*",
                InsertionText: "*",
                ReplacementSpan: replacementSpan,
                Description: "Import all accessible members"));
        }

        void AddImportDirectiveMemberCompletions(NamespaceOrTypeSymbol nsOrType, string prefix, TextSpan replacementSpan)
        {
            AddWildcardImportCompletion(prefix, replacementSpan);

            var members = nsOrType is NamespaceSymbol importNamespace
                ? GetNamespaceCompletionMembers(importNamespace)
                : nsOrType.GetMembers()
                    .Where(IsAccessible)
                    .Where(IsImportDirectiveTypeMemberCompletion);

            foreach (var member in members
                .Where(m => NameMatchesPrefix(m.Name, prefix)))
            {
                var (displayText, insertText, dedupKey) = CreateCompletionParts(member);
                var cursorOffset = member is TypeSymbol ? insertText.Length : (int?)null;

                if (seen.Add(dedupKey))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: displayText,
                        InsertionText: insertText,
                        ReplacementSpan: replacementSpan,
                        CursorOffset: cursorOffset,
                        Description: SafeToDisplayString(member),
                        Symbol: member
                    ));
                }
            }
        }

        void AddExtensionMemberCompletions(
            TypeSymbol receiverType,
            string prefix,
            TextSpan replacementSpan,
            ExtensionMemberKinds kinds)
        {
            ExtensionMemberLookupResult extensionMembers;
            try
            {
                model.Compilation.EnsureSetup();
                extensionMembers = model.LookupApplicableExtensionMembers(
                    receiverType,
                    token.Parent,
                    string.IsNullOrEmpty(prefix) ? null : prefix,
                    includePartialMatches: !string.IsNullOrEmpty(prefix),
                    kinds: kinds);
            }
            catch
            {
                // Extension members are an enrichment over ordinary member access
                // completions. Metadata lookup or an incomplete project snapshot
                // must not collapse the whole member list into keyword fallback.
                return;
            }

            if (kinds.HasFlag(ExtensionMemberKinds.InstanceMethods))
            {
                foreach (var method in extensionMembers.InstanceMethods)
                {
                    if (!IsAccessible(method) ||
                        !ShouldIncludeExtensionMethod(method, prefix) ||
                        !NameMatchesPrefix(method.Name, prefix))
                    {
                        continue;
                    }

                    AddCompletionItem(method, replacementSpan);
                }
            }

            if (kinds.HasFlag(ExtensionMemberKinds.InstanceProperties))
            {
                foreach (var property in extensionMembers.InstanceProperties)
                {
                    if (!IsAccessible(property) || !NameMatchesPrefix(property.Name, prefix))
                        continue;

                    AddCompletionItem(property, replacementSpan);
                }
            }

            if (kinds.HasFlag(ExtensionMemberKinds.StaticMethods))
            {
                foreach (var method in extensionMembers.StaticMethods)
                {
                    if (!IsAccessible(method) || !NameMatchesPrefix(method.Name, prefix))
                        continue;

                    AddCompletionItem(method, replacementSpan);
                }
            }

            if (kinds.HasFlag(ExtensionMemberKinds.StaticProperties))
            {
                foreach (var property in extensionMembers.StaticProperties)
                {
                    if (!IsAccessible(property) || !NameMatchesPrefix(property.Name, prefix))
                        continue;

                    AddCompletionItem(property, replacementSpan);
                }
            }
        }

        static bool ContainsTypeParameter(TypeSymbol type)
        {
            type = type.UnwrapLiteralType() ?? type;

            return type switch
            {
                TypeParameterSymbol => true,
                NamedTypeSymbol named => named.TypeArguments.Any(ContainsTypeParameter),
                ArrayTypeSymbol array => ContainsTypeParameter(array.ElementType),
                PointerTypeSymbol pointer => ContainsTypeParameter(pointer.PointedAtType),
                AddressTypeSymbol address => ContainsTypeParameter(address.ReferencedType),
                _ => false
            };
        }

        bool CanPipeReceiverConvertTo(TypeSymbol receiverType, TypeSymbol targetType)
        {
            receiverType = receiverType.UnwrapLiteralType() ?? receiverType;
            targetType = targetType.UnwrapLiteralType() ?? targetType;

            if (receiverType.TypeKind == TypeKind.Error || targetType.TypeKind == TypeKind.Error)
                return false;

            var conversion = model.Compilation.ClassifyConversion(receiverType, targetType);
            if (conversion.Exists && conversion.IsImplicit)
                return true;

            // Generic pipe targets often use a type parameter in the first
            // parameter position; full method inference happens during binding.
            return ContainsTypeParameter(targetType);
        }

        bool IsPipeCompatibleFunctionTarget(MethodSymbol method, TypeSymbol receiverType)
        {
            if (IsSuppressedCompletionMethod(method) ||
                method.IsExtensionMethod ||
                method.Parameters.IsDefaultOrEmpty ||
                method.Parameters[0].RefKind is RefKind.Out or RefKind.Ref)
            {
                return false;
            }

            return CanPipeReceiverConvertTo(receiverType, method.Parameters[0].Type);
        }

        bool IsPipeCompatiblePropertyTarget(PropertySymbol property, TypeSymbol receiverType)
            => property is { IsMutable: true, IsIndexer: false } &&
               CanPipeReceiverConvertTo(receiverType, property.Type);

        bool TryGetPipeTargetCompletionContext(
            SyntaxToken currentToken,
            out InfixOperatorExpressionSyntax pipeExpression,
            out string prefix,
            out TextSpan targetSpan)
        {
            pipeExpression = null!;
            prefix = string.Empty;
            targetSpan = default;

            if (currentToken.IsKind(SyntaxKind.PipeToken) &&
                currentToken.Parent is InfixOperatorExpressionSyntax pipeFromOperator &&
                pipeFromOperator.OperatorToken == currentToken &&
                position >= currentToken.Span.End)
            {
                pipeExpression = pipeFromOperator;
                targetSpan = new TextSpan(position, 0);
                return true;
            }

            if (currentToken.Parent is not IdentifierNameSyntax identifier)
                return false;

            var candidate = identifier
                .Ancestors()
                .OfType<InfixOperatorExpressionSyntax>()
                .FirstOrDefault(pipe =>
                    pipe.OperatorToken.Kind == SyntaxKind.PipeToken &&
                    position >= pipe.OperatorToken.Span.End &&
                    pipe.Right.Span.Start <= identifier.Span.Start &&
                    identifier.Span.End <= Math.Max(pipe.Right.Span.End, identifier.Span.End));

            if (candidate is null)
                return false;

            var isPipeTargetName =
                candidate.Right is IdentifierNameSyntax rightIdentifier &&
                rightIdentifier.Identifier.Span == identifier.Identifier.Span;

            if (!isPipeTargetName &&
                candidate.Right is InvocationExpressionSyntax { Expression: IdentifierNameSyntax invocationIdentifier } &&
                invocationIdentifier.Identifier.Span == identifier.Identifier.Span)
            {
                isPipeTargetName = true;
            }

            if (!isPipeTargetName)
                return false;

            pipeExpression = candidate;
            prefix = identifier.Identifier.ValueText;
            targetSpan = identifier.Identifier.Span;
            return true;
        }

        void AddPipeTargetCompletions(TypeSymbol receiverType, string prefix, TextSpan targetSpan)
        {
            receiverType = receiverType.UnwrapLiteralType() ?? receiverType;
            model.Compilation.EnsureSetup();

            void AddPipeTargetIfApplicable(Symbol symbol)
            {
                if (!IsAccessible(symbol) || !NameMatchesPrefix(symbol.Name, prefix))
                    return;

                switch (symbol)
                {
                    case MethodSymbol method when IsPipeCompatibleFunctionTarget(method, receiverType):
                        AddCompletionItem(method, targetSpan);
                        break;

                    case PropertySymbol property when IsPipeCompatiblePropertyTarget(property, receiverType):
                        AddCompletionItem(property, targetSpan);
                        break;
                }
            }

            NamespaceSymbol GetCurrentNamespace()
            {
                if (token.GetAncestor<NamespaceDeclarationSyntax>() is { } namespaceDeclaration &&
                    model.GetDeclaredSymbol(namespaceDeclaration) is NamespaceSymbol namespaceSymbol)
                {
                    return namespaceSymbol;
                }

                return model.Compilation.GlobalNamespace;
            }

            var contextNode = semanticContext ?? token.Parent ?? model.SyntaxTree.GetRoot();
            foreach (var symbol in model.GetVisibleValueSymbols(contextNode))
                AddPipeTargetIfApplicable(symbol);

            foreach (var symbol in binder.LookupAvailableSymbols())
                AddPipeTargetIfApplicable(symbol);

            foreach (var symbol in GetNamespaceCompletionMembers(GetCurrentNamespace()))
                AddPipeTargetIfApplicable(symbol);

            AddExtensionMemberCompletions(
                receiverType,
                prefix,
                targetSpan,
                ExtensionMemberKinds.InstanceMethods);
        }

        TypeSymbol? TryGetExplicitlyAnnotatedType(Symbol? symbol)
        {
            if (symbol is null)
                return null;

            foreach (var syntaxRef in symbol.DeclaringSyntaxReferences)
            {
                ExpressionSyntax? typeSyntax = syntaxRef.GetSyntax() switch
                {
                    VariableDeclaratorSyntax { TypeAnnotation: { } annotation } => annotation.Type as ExpressionSyntax,
                    ParameterSyntax { TypeAnnotation: { } annotation } => annotation.Type as ExpressionSyntax,
                    PropertyDeclarationSyntax { Type: { Type: { } propertyType } } => propertyType as ExpressionSyntax,
                    EventDeclarationSyntax { Type: { Type: { } eventType } } => eventType as ExpressionSyntax,
                    IndexerDeclarationSyntax { Type: { Type: { } indexerType } } => indexerType as ExpressionSyntax,
                    _ => null
                };

                if (typeSyntax is null)
                    continue;

                var declaredTypeSymbol = model.GetTypeInfo(typeSyntax).Type
                    ?? (TryGetPreferredSymbolInfo(typeSyntax, out var symbolInfo)
                        ? symbolInfo.Symbol?.UnderlyingSymbol as TypeSymbol
                        : null);
                if (declaredTypeSymbol is not null)
                    return declaredTypeSymbol;
            }

            return null;
        }

        bool IsValueCompletionSymbol(Symbol symbol, bool allowTypes)
            => symbol switch
            {
                LocalSymbol => true,
                ParameterSymbol => true,
                FieldSymbol => true,
                PropertySymbol => true,
                EventSymbol => true,
                MethodSymbol { IsConstructor: false } => true,
                TypeSymbol when allowTypes => true,
                NamespaceSymbol => true,
                _ => false
            };

        TypeSymbol? TryGetLiteralCompletionTargetType(SyntaxToken currentToken)
        {
            if (currentToken.GetAncestor<EqualsValueClauseSyntax>() is { } equalsClause)
            {
                if (position < equalsClause.EqualsToken.Span.End)
                    return null;

                return equalsClause.Parent switch
                {
                    VariableDeclaratorSyntax declarator => declarator.TypeAnnotation?.Type is ExpressionSyntax declaratorType
                        ? model.GetTypeInfo(declaratorType).Type ?? (TryGetPreferredSymbolInfo(declaratorType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as TypeSymbol
                            : null)
                        : model.GetDeclaredSymbol(declarator) switch
                        {
                            LocalSymbol local => local.Type,
                            FieldSymbol field => field.Type,
                            _ => null
                        },
                    PropertyDeclarationSyntax property => property.Type.Type is ExpressionSyntax propertyType
                        ? model.GetTypeInfo(propertyType).Type ?? (TryGetPreferredSymbolInfo(propertyType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as TypeSymbol
                            : null)
                        : (model.GetDeclaredSymbol(property) as PropertySymbol)?.Type,
                    EventDeclarationSyntax @event => @event.Type.Type is ExpressionSyntax eventType
                        ? model.GetTypeInfo(eventType).Type ?? (TryGetPreferredSymbolInfo(eventType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as TypeSymbol
                            : null)
                        : (model.GetDeclaredSymbol(@event) as EventSymbol)?.Type,
                    IndexerDeclarationSyntax indexer => indexer.Type.Type is ExpressionSyntax indexerType
                        ? model.GetTypeInfo(indexerType).Type ?? (TryGetPreferredSymbolInfo(indexerType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as TypeSymbol
                            : null)
                        : (model.GetDeclaredSymbol(indexer) as PropertySymbol)?.Type,
                    ParameterSyntax parameter => parameter.TypeAnnotation?.Type is ExpressionSyntax parameterType
                        ? model.GetTypeInfo(parameterType).Type ?? (TryGetPreferredSymbolInfo(parameterType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as TypeSymbol
                            : null)
                        : (model.GetDeclaredSymbol(parameter) as ParameterSymbol)?.Type,
                    EnumMemberDeclarationSyntax enumMember => (model.GetDeclaredSymbol(enumMember) as FieldSymbol)?.Type,
                    _ => null
                };
            }

            if (currentToken.GetAncestor<AssignmentExpressionSyntax>() is { } assignment)
            {
                if (position < assignment.OperatorToken.Span.End)
                    return null;

                if (assignment.Left is ExpressionSyntax left)
                {
                    var leftSymbol = TryGetPreferredSymbolInfo(left, out var symbolInfo)
                        ? symbolInfo.Symbol?.UnderlyingSymbol
                        : TryResolveReceiverSymbol(left);
                    return TryGetExplicitlyAnnotatedType(leftSymbol)
                        ?? GetTypeFromSymbol(leftSymbol)
                        ?? GetContextualType(left);
                }
                return null;
            }

            if (currentToken.GetAncestor<AssignmentStatementSyntax>() is { } assignmentStatement)
            {
                if (position < assignmentStatement.OperatorToken.Span.End)
                    return null;

                if (assignmentStatement.Left is ExpressionSyntax left)
                {
                    var leftSymbol = TryGetPreferredSymbolInfo(left, out var symbolInfo)
                        ? symbolInfo.Symbol?.UnderlyingSymbol
                        : TryResolveReceiverSymbol(left);
                    return TryGetExplicitlyAnnotatedType(leftSymbol)
                        ?? GetTypeFromSymbol(leftSymbol)
                        ?? GetContextualType(left);
                }
                return null;
            }

            return null;
        }

        TypeSymbol? TryGetTargetTypedMemberCompletionType(MemberBindingExpressionSyntax memberBinding)
        {
            var targetType = TryGetLiteralCompletionTargetType(memberBinding.Name.Identifier);
            if (targetType is not null)
                return targetType;

            return TryGetAttributeArgumentTargetType(memberBinding);
        }

        TypeSymbol? TryGetAttributeArgumentTargetType(MemberBindingExpressionSyntax memberBinding)
        {
            var argument = memberBinding.GetAncestor<ArgumentSyntax>();
            var attribute = argument?.GetAncestor<AttributeSyntax>();
            if (argument is null ||
                attribute?.ArgumentList is null ||
                argument.NameColon is not null)
            {
                return null;
            }

            var arguments = attribute.ArgumentList.Arguments;
            var argumentIndex = -1;
            for (var i = 0; i < arguments.Count; i++)
            {
                if (ReferenceEquals(arguments[i], argument))
                {
                    argumentIndex = i;
                    break;
                }
            }

            if (argumentIndex < 0)
                return null;

            var attributeType = TryResolveAttributeType(attribute);
            return attributeType is null
                ? null
                : TryGetAttributeConstructorParameterType(attributeType, arguments, argumentIndex);
        }

        NamedTypeSymbol? TryResolveAttributeType(AttributeSyntax attribute)
        {
            if (attribute.Name is NameSyntax name &&
                TryResolveNamespaceOrType(name) is NamedTypeSymbol namedType &&
                IsAttributeType(namedType))
            {
                return namedType;
            }

            var attributeName = attribute.Name.ToString();
            if (!attributeName.EndsWith("Attribute", StringComparison.Ordinal))
                attributeName += "Attribute";

            return binder.LookupType(attributeName) as NamedTypeSymbol
                ?? model.Compilation.GetTypeByMetadataName(attributeName) as NamedTypeSymbol;
        }

        static bool IsAttributeType(NamedTypeSymbol type)
            => type.Name.EndsWith("Attribute", StringComparison.Ordinal) ||
               type.BaseType?.ToFullyQualifiedMetadataName() == "System.Attribute";

        static string GetAttributeCompletionName(NamedTypeSymbol type)
        {
            const string suffix = "Attribute";
            return type.Name.EndsWith(suffix, StringComparison.Ordinal) &&
                   type.Name.Length > suffix.Length
                ? type.Name[..^suffix.Length]
                : type.Name;
        }

        static bool AttributeNameMatchesPrefix(NamedTypeSymbol type, string prefix)
            => NameMatchesPrefix(GetAttributeCompletionName(type), prefix) ||
               NameMatchesPrefix(type.Name, prefix);

        static bool IsAttributeNameCompletionPosition(AttributeSyntax attribute, int position)
        {
            if (position < attribute.Name.Span.Start)
                return false;

            if (attribute.ArgumentList is not null &&
                position >= attribute.ArgumentList.Span.Start)
                return false;

            return position <= Math.Max(attribute.Name.Span.End, attribute.Name.Span.Start);
        }

        static TypeSymbol? TryGetAttributeConstructorParameterType(
            NamedTypeSymbol attributeType,
            SeparatedSyntaxList<ArgumentSyntax> arguments,
            int argumentIndex)
        {
            if (arguments[argumentIndex].NameColon is not null)
                return null;

            var positionalIndex = 0;
            for (var i = 0; i < argumentIndex; i++)
            {
                if (arguments[i].NameColon is null)
                    positionalIndex++;
            }

            TypeSymbol? targetType = null;
            foreach (var constructor in attributeType.Constructors.Where(static constructor => !constructor.IsStatic))
            {
                if (constructor.Parameters.Length <= positionalIndex)
                    continue;

                var parameterType = constructor.Parameters[positionalIndex].Type;
                if (targetType is null)
                {
                    targetType = parameterType;
                    continue;
                }

                if (!HaveSameTypeIdentity(targetType, parameterType))
                    return null;
            }

            return targetType;
        }

        static bool HaveSameTypeIdentity(TypeSymbol left, TypeSymbol right)
        {
            if (SymbolEqualityComparer.Default.Equals(left, right))
                return true;

            if (left is NamedTypeSymbol leftNamed &&
                right is NamedTypeSymbol rightNamed)
            {
                return string.Equals(
                    leftNamed.ToFullyQualifiedMetadataName(),
                    rightNamed.ToFullyQualifiedMetadataName(),
                    StringComparison.Ordinal);
            }

            return false;
        }

        bool TryCollectLiteralMembers(TypeSymbol? type, List<Symbol> results)
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

                default:
                    return false;
            }
        }

        static bool IsLiteralCandidateToken(SyntaxToken token)
            => token.Kind is SyntaxKind.StringLiteralToken
                or SyntaxKind.MultiLineStringLiteralToken
                or SyntaxKind.CharacterLiteralToken
                or SyntaxKind.NumericLiteralToken
                or SyntaxKind.TrueKeyword
                or SyntaxKind.FalseKeyword
                or SyntaxKind.NullKeyword;

        static void CollectLiteralTextsFromNode(SyntaxNode? node, List<string> results)
        {
            if (node is null)
                return;

            foreach (var child in node.ChildNodesAndTokens())
            {
                if (child.TryGetToken(out var token))
                {
                    if (IsLiteralCandidateToken(token))
                    {
                        var text = token.Text;
                        if (!string.IsNullOrEmpty(text))
                            results.Add(text);
                    }

                    continue;
                }

                CollectLiteralTextsFromNode(child.AsNode(), results);
            }
        }

        static SyntaxNode? TryGetAnnotatedTypeNode(EqualsValueClauseSyntax equalsClause)
            => equalsClause.Parent switch
            {
                VariableDeclaratorSyntax { TypeAnnotation: { } annotation } => annotation.Type,
                ParameterSyntax { TypeAnnotation: { } annotation } => annotation.Type,
                PropertyDeclarationSyntax { Type: { Type: { } propertyType } } => propertyType,
                EventDeclarationSyntax { Type: { Type: { } eventType } } => eventType,
                IndexerDeclarationSyntax { Type: { Type: { } indexerType } } => indexerType,
                _ => null
            };

        bool TryGetExplicitLiteralCandidates(SyntaxToken currentToken, out ImmutableArray<string> candidates)
        {
            candidates = [];
            var equalsClause = currentToken.GetAncestor<EqualsValueClauseSyntax>();
            if (equalsClause is null || position < equalsClause.EqualsToken.Span.End)
                return false;

            var typeNode = TryGetAnnotatedTypeNode(equalsClause);
            if (typeNode is null)
                return false;

            var buffer = new List<string>();
            CollectLiteralTextsFromNode(typeNode, buffer);
            if (buffer.Count == 0)
                return false;

            candidates = buffer
                .Distinct(StringComparer.Ordinal)
                .ToImmutableArray();
            return candidates.Length > 0;
        }

        static SyntaxNode? TryGetDeclaredTypeNodeFromSymbol(Symbol? symbol)
            => symbol?.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() switch
            {
                VariableDeclaratorSyntax { TypeAnnotation: { } annotation } => annotation.Type,
                ParameterSyntax { TypeAnnotation: { } annotation } => annotation.Type,
                PropertyDeclarationSyntax { Type: { Type: { } propertyType } } => propertyType,
                EventDeclarationSyntax { Type: { Type: { } eventType } } => eventType,
                IndexerDeclarationSyntax { Type: { Type: { } indexerType } } => indexerType,
                _ => null
            };

        bool TryGetDeclaredLiteralCandidatesFromSymbol(Symbol? symbol, out ImmutableArray<string> candidates)
        {
            candidates = [];
            var typeNode = TryGetDeclaredTypeNodeFromSymbol(symbol);
            if (typeNode is null)
                return false;

            var buffer = new List<string>();
            CollectLiteralTextsFromNode(typeNode, buffer);
            if (buffer.Count == 0)
                return false;

            candidates = buffer
                .Distinct(StringComparer.Ordinal)
                .ToImmutableArray();
            return candidates.Length > 0;
        }

        Symbol? TryGetAssignmentTargetSymbol(SyntaxToken currentToken)
        {
            if (currentToken.GetAncestor<AssignmentExpressionSyntax>() is { } assignmentExpr)
            {
                if (position < assignmentExpr.OperatorToken.Span.End)
                    return null;

                return assignmentExpr.Left is ExpressionSyntax leftExpr
                    ? TryGetPreferredSymbolInfo(leftExpr, out var symbolInfo)
                        ? symbolInfo.Symbol?.UnderlyingSymbol
                        : TryResolveReceiverSymbol(leftExpr)
                    : null;
            }

            if (currentToken.GetAncestor<AssignmentStatementSyntax>() is { } assignmentStmt)
            {
                if (position < assignmentStmt.OperatorToken.Span.End)
                    return null;

                return assignmentStmt.Left is ExpressionSyntax leftExpr
                    ? TryGetPreferredSymbolInfo(leftExpr, out var symbolInfo)
                        ? symbolInfo.Symbol?.UnderlyingSymbol
                        : TryResolveReceiverSymbol(leftExpr)
                    : null;
            }

            return null;
        }

        bool TryAddPropertyPatternMemberCompletions()
        {
            var propertyPattern = token.GetAncestor<PropertyPatternSyntax>();
            if (propertyPattern is null ||
                position < propertyPattern.PropertyPatternClause.OpenBraceToken.Span.End ||
                position > propertyPattern.PropertyPatternClause.CloseBraceToken.Span.Start)
            {
                return false;
            }

            TypeSymbol? receiverType = propertyPattern.Type is not null
                ? model.GetTypeInfo(propertyPattern.Type).Type
                : TryGetPropertyPatternInputType(propertyPattern);
            if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
                return false;

            var activeSubpattern = propertyPattern.PropertyPatternClause.Properties
                .FirstOrDefault(property =>
                    position >= property.Span.Start &&
                    position <= Math.Max(property.Pattern.Span.Start, property.NameColon.ColonToken.Span.End));

            var prefix = string.Empty;
            var targetSpan = new TextSpan(position, 0);

            if (activeSubpattern is not null)
            {
                var segments = activeSubpattern.MemberPath
                    .Concat([activeSubpattern.NameColon.Name])
                    .ToImmutableArray();
                var activeSegmentIndex = segments.Length - 1;

                for (var i = 0; i < segments.Length; i++)
                {
                    var identifier = segments[i].Identifier;
                    if (identifier.IsMissing || position <= identifier.Span.End)
                    {
                        activeSegmentIndex = i;
                        break;
                    }
                }

                for (var i = 0; i < activeSegmentIndex; i++)
                {
                    var segmentName = segments[i].Identifier.ValueText;
                    var member = GetReadablePropertyPatternMembers(receiverType)
                        .FirstOrDefault(candidate => string.Equals(candidate.Name, segmentName, StringComparison.Ordinal));
                    receiverType = member switch
                    {
                        PropertySymbol property => property.Type,
                        FieldSymbol field => field.Type,
                        _ => null
                    };

                    if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
                        return true;
                }

                var activeIdentifier = segments[activeSegmentIndex].Identifier;
                if (!activeIdentifier.IsMissing)
                {
                    prefix = activeIdentifier.ValueText;
                    targetSpan = activeIdentifier.Span;
                }
            }

            foreach (var member in GetReadablePropertyPatternMembers(receiverType))
            {
                if (!NameMatchesPrefix(member.Name, prefix))
                    continue;

                AddCompletionItem(member, targetSpan);
            }

            return true;
        }

        TypeSymbol? TryGetPropertyPatternInputType(PropertyPatternSyntax propertyPattern)
        {
            ExpressionSyntax? expression = propertyPattern.GetAncestor<IsPatternExpressionSyntax>()?.Expression
                ?? propertyPattern.GetAncestor<IfPatternStatementSyntax>()?.Expression
                ?? propertyPattern.GetAncestor<IfPatternExpressionSyntax>()?.Value
                ?? propertyPattern.GetAncestor<WhilePatternStatementSyntax>()?.Expression
                ?? propertyPattern.GetAncestor<MatchExpressionSyntax>()?.Expression
                ?? propertyPattern.GetAncestor<PostfixMatchExpressionSyntax>()?.Expression
                ?? propertyPattern.GetAncestor<MatchStatementSyntax>()?.Expression;

            return expression is null ? null : model.GetTypeInfo(expression).Type;
        }

        IEnumerable<Symbol> GetReadablePropertyPatternMembers(TypeSymbol type)
        {
            type = UnwrapAliases(type).GetNonNullableType();
            return GetTypeMembersIncludingBase(type, includeStatic: false)
                .Where(member => member switch
                {
                    PropertySymbol property => property.GetMethod is not null && property.Parameters.Length == 0,
                    FieldSymbol => true,
                    _ => false
                })
                .Where(IsAccessible);
        }

        if (TryAddPropertyPatternMemberCompletions())
            return completions;

        ImmutableArray<string> explicitLiteralCandidates = [];
        if (!TryGetExplicitLiteralCandidates(token, out explicitLiteralCandidates))
        {
            var syntaxTree = token.SyntaxTree;
            if (syntaxTree is not null && token.Span.Start > 0)
            {
                var previousToken = syntaxTree.GetRoot().FindToken(token.Span.Start - 1);
                if (previousToken != token)
                    _ = TryGetExplicitLiteralCandidates(previousToken, out explicitLiteralCandidates);
            }
        }

        if (explicitLiteralCandidates.Length == 0)
        {
            var assignmentTarget = TryGetAssignmentTargetSymbol(token);
            if (assignmentTarget is null)
            {
                var syntaxTree = token.SyntaxTree;
                if (syntaxTree is not null && token.Span.Start > 0)
                {
                    var previousToken = syntaxTree.GetRoot().FindToken(token.Span.Start - 1);
                    if (previousToken != token)
                        assignmentTarget = TryGetAssignmentTargetSymbol(previousToken);
                }
            }

            if (assignmentTarget is not null)
                _ = TryGetDeclaredLiteralCandidatesFromSymbol(assignmentTarget, out explicitLiteralCandidates);
        }

        if (explicitLiteralCandidates.Length > 0)
        {
            foreach (var candidate in explicitLiteralCandidates)
            {
                if (seen.Add(candidate))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: candidate,
                        InsertionText: candidate,
                        ReplacementSpan: literalReplacementSpan,
                        Description: candidate));
                }
            }

            if (completions.Count > 0)
                return completions;
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
            var literalMembers = new List<Symbol>();
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

                if (completions.Count > 0)
                    return completions;
            }
        }

        var targetTypedMemberBinding = token.GetAncestor<MemberBindingExpressionSyntax>();
        if (targetTypedMemberBinding is not null &&
            (!forceInsertionAtCaret || targetTypedMemberBinding.Name.Identifier.IsMissing) &&
            targetTypedMemberBinding.Parent is not ConditionalAccessExpressionSyntax &&
            position >= targetTypedMemberBinding.OperatorToken.Span.End &&
            TryGetTargetTypedMemberCompletionType(targetTypedMemberBinding) is NamedTypeSymbol { TypeKind: TypeKind.Enum } targetEnum)
        {
            var hasNameAtCaret = !targetTypedMemberBinding.Name.Identifier.IsMissing &&
                position > targetTypedMemberBinding.Name.Identifier.Span.Start;
            var prefix = hasNameAtCaret
                ? GetCompletionPrefix(targetTypedMemberBinding.Name.Identifier)
                : string.Empty;
            var nameSpan = hasNameAtCaret
                ? targetTypedMemberBinding.Name.Identifier.Span
                : new TextSpan(position, 0);

            foreach (var field in targetEnum.GetMembers()
                .OfType<FieldSymbol>()
                .Where(field => field.IsConst && NameMatchesPrefix(field.Name, prefix)))
            {
                AddCompletionItem(field, nameSpan);
            }

            return completions;
        }

        if (TryGetPipeTargetCompletionContext(token, out var pipeExpression, out var pipePrefix, out var pipeTargetSpan))
        {
            if (TryGetExpressionType(pipeExpression.Left) is { TypeKind: not TypeKind.Error } pipeReceiverType)
                AddPipeTargetCompletions(pipeReceiverType, pipePrefix, pipeTargetSpan);

            return completions;
        }

        var importDirective = token.GetAncestor<ImportDirectiveSyntax>();
        if (importDirective is not null)
        {
            var qualified = token.GetAncestor<QualifiedNameSyntax>();
            if (qualified is not null && GetImportCompletionNameToken(qualified.Right) is { } importNameToken)
            {
                if (IsAtImportCompletionName(qualified, importNameToken, position))
                {
                    var symbol = TryResolveImportNamespaceOrType(qualified.Left);
                    if (symbol is NamespaceOrTypeSymbol nsOrType)
                    {
                        var prefix = importNameToken.ValueText;
                        var nameSpan = GetImportCompletionReplacementSpan(importNameToken, position);
                        AddImportDirectiveMemberCompletions(nsOrType, prefix, nameSpan);
                    }
                    return completions;
                }
            }

            if (token.Parent is IdentifierNameSyntax { Parent: ImportDirectiveSyntax })
            {
                foreach (var symbol in binder.LookupAvailableSymbols())
                {
                    if (symbol is NamespaceOrTypeSymbol nsOrType &&
                        NameMatchesPrefix(nsOrType.Name, tokenValueText))
                    {
                        var (displayText, insertText, dedupKey) = CreateCompletionParts(nsOrType);
                        var cursorOffset = nsOrType is TypeSymbol ? insertText.Length : (int?)null;

                        if (seen.Add(dedupKey))
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
                    var symbol = TryResolveImportNamespaceOrType(qualified.Left);
                    if (symbol is NamespaceOrTypeSymbol nsOrType)
                    {
                        var prefix = nameToken.ValueText;
                        var nameSpan = nameToken.Span;
                        var aliasMembers = nsOrType is NamespaceSymbol aliasNamespace
                            ? GetNamespaceCompletionMembers(aliasNamespace)
                            : nsOrType.GetMembers().Where(IsAccessible);
                        foreach (var member in aliasMembers
                            .Where(m => NameMatchesPrefix(m.Name, prefix)))
                        {
                            var (displayText, insertText, dedupKey) = CreateCompletionParts(member);
                            var cursorOffset = member is TypeSymbol ? insertText.Length : GetDefaultCursorOffset(member, insertText);

                            if (seen.Add(dedupKey))
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
                    if (!NameMatchesPrefix(symbol.Name, tokenValueText))
                        continue;

                    var (displayText, insertText, dedupKey) = CreateCompletionParts(symbol);
                    var cursorOffset = GetDefaultCursorOffset(symbol, insertText);

                    if (seen.Add(dedupKey))
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
                    var (displayText, insertText, dedupKey) = CreateCompletionParts(symbol);
                    var cursorOffset = GetDefaultCursorOffset(symbol, insertText);

                    if (seen.Add(dedupKey))
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
                if (symbol is NamedTypeSymbol type &&
                    !type.IsAbstract &&
                    IsAccessible(type) &&
                    type.Constructors.Any(IsAccessible))
                {
                    var (displayText, insertText, dedupKey) = CreateCompletionParts(type);
                    if (seen.Add(dedupKey))
                    {
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

            foreach (var label in EnumerateVisibleLabelNames(gotoStatement))
            {
                if (string.IsNullOrEmpty(label))
                    continue;

                if (!string.IsNullOrEmpty(prefix) &&
                    !NameMatchesPrefix(label, prefix))
                {
                    continue;
                }

                if (seen.Add(label))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: label,
                        InsertionText: label,
                        ReplacementSpan: replacement,
                        CursorOffset: label.Length,
                        Description: label));
                }
            }

            foreach (var symbol in binder.LookupAvailableSymbols())
            {
                if (symbol is not LabelSymbol label)
                    continue;

                if (string.IsNullOrEmpty(label.Name))
                    continue;

                if (!string.IsNullOrEmpty(prefix) &&
                    !NameMatchesPrefix(label.Name, prefix))
                {
                    continue;
                }

                var (displayText, insertText, dedupKey) = CreateCompletionParts(label);
                if (seen.Add(dedupKey))
                {

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

        static IEnumerable<string> EnumerateVisibleLabelNames(GotoStatementSyntax gotoStatement)
        {
            var scope = gotoStatement.Ancestors()
                .FirstOrDefault(static ancestor => ancestor is BlockStatementSyntax or CompilationUnitSyntax);
            if (scope is null)
                yield break;

            foreach (var label in scope.DescendantNodes()
                         .OfType<LabeledStatementSyntax>())
            {
                var name = label.Identifier.ValueText;
                if (!string.IsNullOrWhiteSpace(name))
                    yield return name;
            }
        }

        var attributeNameContext = token.GetAncestor<AttributeSyntax>();
        if (attributeNameContext is not null &&
            IsAttributeNameCompletionPosition(attributeNameContext, position))
        {
            if (attributeNameContext.Name is QualifiedNameSyntax attributeQualifiedName &&
                attributeQualifiedName.Right is SimpleNameSyntax attributeSimpleName &&
                position >= attributeQualifiedName.DotToken.Span.End)
            {
                var receiver = TryResolveAttributeCompletionReceiver(attributeQualifiedName.Left);
                if (receiver is NamespaceOrTypeSymbol nsOrType)
                {
                    var attributePrefix = attributeSimpleName.Identifier.IsMissing
                        ? string.Empty
                        : attributeSimpleName.Identifier.ValueText;
                    var attributeNameSpan = attributeSimpleName.Identifier.IsMissing
                        ? new TextSpan(position, 0)
                        : attributeSimpleName.Identifier.Span;
                    AddQualifiedAttributeNameCompletions(nsOrType, attributePrefix, attributeNameSpan);
                }

                return completions;
            }

            var unqualifiedAttributePrefix = tokenValueText;
            var unqualifiedAttributeSpan = replacementSpan;
            if (attributeNameContext.Name is SimpleNameSyntax unqualifiedAttributeSimple &&
                !unqualifiedAttributeSimple.Identifier.IsMissing)
            {
                unqualifiedAttributePrefix = unqualifiedAttributeSimple.Identifier.ValueText;
                unqualifiedAttributeSpan = unqualifiedAttributeSimple.Identifier.Span;
            }

            AddAttributeTypeCompletions(unqualifiedAttributePrefix, unqualifiedAttributeSpan);
            return completions;
        }

        // Conditional member access: value?.Member
        var memberBinding = token.GetAncestor<MemberBindingExpressionSyntax>();
        if (memberBinding is not null &&
            (!forceInsertionAtCaret || memberBinding.Name.Identifier.IsMissing) &&
            memberBinding.Parent is ConditionalAccessExpressionSyntax conditionalAccess)
        {
            var operatorToken = memberBinding.OperatorToken;

            if (position >= operatorToken.Span.End)
            {
                var (symbol, type) = ResolveReceiver(conditionalAccess.Expression);
                IEnumerable<Symbol>? members = null;
                TypeSymbol? instanceTypeForExtensions = null;

                if (NeedsReceiverFallback(symbol, type) && conditionalAccess.Expression is IdentifierNameSyntax receiverIdentifier)
                {
                    symbol = TryResolveReceiverSymbol(receiverIdentifier);
                    type = GetTypeFromSymbol(symbol) ?? type;
                }

                // For invocation expressions, prefer the invoked method return type.
                // This avoids leaking argument/literal types into completion and
                // correctly suppresses member access on unit/void returns.
                if (conditionalAccess.Expression is InvocationExpressionSyntax &&
                    symbol is MethodSymbol invokedMethod)
                {
                    type = invokedMethod.ReturnType;
                    if (type.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
                        return completions;
                }

                if (symbol is NamespaceSymbol ns)
                {
                    members = GetNamespaceCompletionMembers(ns);
                }
                else if (conditionalAccess.Expression is SelfExpressionSyntax && GetSelfType() is { } currentSelfType)
                {
                    var completionType = GetCarrierConditionalAccessLookupType(currentSelfType) ?? currentSelfType.GetNonNullableType();
                    members = GetInstanceMembersIncludingBase(completionType)
                        .Where(member => IsAccessibleOnSelf(member, completionType));
                    instanceTypeForExtensions = completionType switch
                    {
                        NamedTypeSymbol named => named,
                        ArrayTypeSymbol array => array,
                        LiteralTypeSymbol literal => literal.UnderlyingType as TypeSymbol,
                        _ => null
                    };
                }
                else if (TryGetTypeAccessSymbol(symbol, type) is { } typeAccessSymbol)
                {
                    members = GetTypeAccessMembers(typeAccessSymbol);
                }
                else if (type is TypeSymbol instanceType)
                {
                    var completionType = GetCarrierConditionalAccessLookupType(instanceType) ?? instanceType.GetNonNullableType();
                    members = GetInstanceMembersIncludingBase(completionType).Where(IsAccessible);
                    instanceTypeForExtensions = completionType switch
                    {
                        NamedTypeSymbol named => named,
                        ArrayTypeSymbol array => array,
                        LiteralTypeSymbol literal => literal.UnderlyingType as TypeSymbol,
                        _ => null
                    };
                }
                if (members is not null)
                {
                    var hasNameAtCaret = !memberBinding.Name.Identifier.IsMissing &&
                        position > memberBinding.Name.Identifier.Span.Start;
                    var prefix = hasNameAtCaret
                        ? GetCompletionPrefix(memberBinding.Name.Identifier)
                        : string.Empty;
                    var nameSpan = hasNameAtCaret
                        ? memberBinding.Name.Identifier.Span
                        : new TextSpan(position, 0);

                    foreach (var member in members.Where(m => NameMatchesPrefix(m.Name, prefix)))
                        AddCompletionItem(member, nameSpan);

                    if (TryGetTypeAccessSymbol(symbol, type) is { } staticExtensionReceiver)
                        AddExtensionMemberCompletions(staticExtensionReceiver, prefix, nameSpan, ExtensionMemberKinds.StaticMethods | ExtensionMemberKinds.StaticProperties);

                    if (instanceTypeForExtensions is not null)
                        AddExtensionMemberCompletions(instanceTypeForExtensions, prefix, nameSpan, ExtensionMemberKinds.InstanceMethods | ExtensionMemberKinds.InstanceProperties);

                    return completions;
                }

                // In conditional member-access context, avoid broad/global
                // completions when the receiver cannot be resolved.
                return completions;
            }
        }

        // Member access: Console.Wri
        var memberAccess = token.GetAncestor<MemberAccessExpressionSyntax>();
        if (memberAccess is not null && (!forceInsertionAtCaret || memberAccess.Name.Identifier.IsMissing))
        {
            var dotToken = memberAccess.OperatorToken;

            if (position >= dotToken.Span.End)
            {
                Symbol? symbol = null;
                TypeSymbol? type = null;
                IEnumerable<Symbol>? members = null;
                TypeSymbol? instanceTypeForExtensions = null;

                var isRecoveredImportCompletion = IsImportLineCompletionContext(memberAccess);
                if (isRecoveredImportCompletion)
                {
                    if (memberAccess.Expression is NameSyntax importReceiver &&
                        TryResolveImportNamespaceOrType(importReceiver) is { } importReceiverSymbol)
                    {
                        var hasNameAtCaret = !memberAccess.Name.Identifier.IsMissing &&
                            position > memberAccess.Name.Identifier.Span.Start;
                        var prefix = hasNameAtCaret
                            ? GetCompletionPrefix(memberAccess.Name.Identifier)
                            : string.Empty;
                        var nameSpan = hasNameAtCaret
                            ? memberAccess.Name.Identifier.Span
                            : new TextSpan(position, 0);

                        AddImportDirectiveMemberCompletions(importReceiverSymbol, prefix, nameSpan);
                    }

                    return completions;
                }

                if (memberAccess.Expression is NameSyntax nameReceiver &&
                    TryResolveNamespaceOrType(nameReceiver) is { } namespaceOrTypeReceiver)
                {
                    symbol = namespaceOrTypeReceiver;
                    type = namespaceOrTypeReceiver as TypeSymbol;
                }
                else
                {
                    (symbol, type) = ResolveReceiver(memberAccess.Expression);
                }

                if (NeedsReceiverFallback(symbol, type) && memberAccess.Expression is IdentifierNameSyntax receiverIdentifier)
                {
                    symbol = TryResolveReceiverSymbol(receiverIdentifier);
                    type = GetTypeFromSymbol(symbol) ?? type;
                }

                // For invocation expressions, prefer the invoked method return type.
                // This avoids leaking argument/literal types into completion and
                // correctly suppresses member access on unit/void returns.
                if (memberAccess.Expression is InvocationExpressionSyntax &&
                    symbol is MethodSymbol invokedMethod)
                {
                    type = invokedMethod.ReturnType;
                    if (type.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
                        return completions;
                }
                else if (memberAccess.Expression is InvocationExpressionSyntax &&
                    type?.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
                {
                    return completions;
                }

                if (symbol is NamespaceSymbol ns)
                {
                    // Namespace or namespace alias: list its public members
                    members = GetNamespaceCompletionMembers(ns);
                }
                else if (memberAccess.Expression is SelfExpressionSyntax && GetSelfType() is { } currentSelfType)
                {
                    var completionType = currentSelfType.GetNonNullableType();
                    members = GetInstanceMembersIncludingBase(completionType)
                        .Where(member => IsAccessibleOnSelf(member, completionType));
                    instanceTypeForExtensions = completionType switch
                    {
                        NamedTypeSymbol named => named,
                        ArrayTypeSymbol array => array,
                        LiteralTypeSymbol literal => literal.UnderlyingType as TypeSymbol,
                        _ => null
                    };
                }
                else if (TryGetTypeAccessSymbol(symbol, type) is { } typeAccessSymbol)
                {
                    // Accessing a type-like receiver (named type or constrained type parameter):
                    // show static members.
                    members = GetTypeAccessMembers(typeAccessSymbol);
                }
                else if (type is TypeSymbol instanceType)
                {
                    var completionType = instanceType.GetNonNullableType();
                    // Accessing an instance: show instance members
                    members = GetInstanceMembersIncludingBase(completionType).Where(IsAccessible);
                    instanceTypeForExtensions = completionType switch
                    {
                        NamedTypeSymbol named => named,
                        ArrayTypeSymbol array => array,
                        LiteralTypeSymbol literal => literal.UnderlyingType as TypeSymbol,
                        _ => null
                    };
                }
                if (members is not null)
                {
                    var hasNameAtCaret = !memberAccess.Name.Identifier.IsMissing &&
                        position > memberAccess.Name.Identifier.Span.Start;
                    var prefix = hasNameAtCaret
                        ? GetCompletionPrefix(memberAccess.Name.Identifier)
                        : string.Empty;
                    var nameSpan = hasNameAtCaret
                        ? memberAccess.Name.Identifier.Span
                        : new TextSpan(position, 0);

                    foreach (var member in members.Where(m => NameMatchesPrefix(m.Name, prefix)))
                        AddCompletionItem(member, nameSpan);

                    if (TryGetTypeAccessSymbol(symbol, type) is { } staticExtensionReceiver)
                        AddExtensionMemberCompletions(staticExtensionReceiver, prefix, nameSpan, ExtensionMemberKinds.StaticMethods | ExtensionMemberKinds.StaticProperties);

                    if (instanceTypeForExtensions is not null)
                        AddExtensionMemberCompletions(instanceTypeForExtensions, prefix, nameSpan, ExtensionMemberKinds.InstanceMethods | ExtensionMemberKinds.InstanceProperties);

                    return completions;
                }

                // In member-access context, avoid falling back to broad/global
                // completions when the receiver cannot be resolved.
                return completions;
            }
        }

        var qualifiedName = token.GetAncestor<QualifiedNameSyntax>();
        if (qualifiedName is not null && qualifiedName.Right is SimpleNameSyntax simple &&
            (!forceInsertionAtCaret || simple.Identifier.IsMissing))
        {
            var isImportLineCompletion = IsImportLineCompletionContext(qualifiedName);
            var symbol = (Symbol?)(isImportLineCompletion
                ? TryResolveImportNamespaceOrType(qualifiedName.Left)
                : TryResolveNamespaceOrType(qualifiedName.Left));
            var prefix = GetCompletionPrefix(simple.Identifier);
            var nameSpan = isImportLineCompletion
                ? GetImportCompletionReplacementSpan(simple.Identifier, position)
                : simple.Identifier.Span;

            if (symbol is NamespaceOrTypeSymbol nsOrType)
            {
                if (isImportLineCompletion)
                {
                    AddImportDirectiveMemberCompletions(nsOrType, prefix, nameSpan);
                    return completions;
                }

                var qualifiedMembers = nsOrType is NamespaceSymbol qualifiedNamespace
                    ? GetNamespaceCompletionMembers(qualifiedNamespace)
                    : nsOrType.GetMembers().Where(IsAccessible);
                foreach (var member in qualifiedMembers
                    .Where(m => string.IsNullOrEmpty(prefix)
                    || NameMatchesPrefix(m.Name, prefix)))
                {
                    var (displayText, insertText, dedupKey) = CreateCompletionParts(member);
                    var cursorOffset = member is TypeSymbol ? insertText.Length : GetDefaultCursorOffset(member, insertText);

                    if (seen.Add(dedupKey))
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

                return completions;
            }

            if (isImportLineCompletion)
                return completions;

            var receiverType = GetTypeFromSymbol(symbol)
                ?? model.GetTypeInfo(qualifiedName.Left).Type;
            if (receiverType is TypeSymbol typeSymbolForQualified)
            {
                var members = GetInstanceMembersIncludingBase(typeSymbolForQualified)
                    .Where(IsAccessible);

                foreach (var member in members.Where(m => NameMatchesPrefix(m.Name, prefix)))
                {
                    if (member is MethodSymbol method &&
                        IsSuppressedCompletionMethod(method))
                    {
                        continue;
                    }

                    var (displayText, insertText, dedupKey) = CreateCompletionParts(member);
                    var cursorOffset = GetDefaultCursorOffset(member, insertText);

                    if (seen.Add(dedupKey))
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

                if (receiverType is TypeSymbol { TypeKind: not TypeKind.Error } typedReceiver)
                {
                    var extensionMembers = ExtensionMemberLookup.Lookup(
                        binder,
                        typedReceiver,
                        includePartialMatches: false,
                        kinds: ExtensionMemberKinds.InstanceMethods);

                    foreach (var method in extensionMembers.InstanceMethods)
                    {
                        if (!IsAccessible(method) ||
                            IsSuppressedCompletionMethod(method) ||
                            !ShouldIncludeExtensionMethod(method, prefix) ||
                            !NameMatchesPrefix(method.Name, prefix))
                            continue;

                        var (displayText, insertText, dedupKey) = CreateCompletionParts(method);
                        var cursorOffset = GetDefaultCursorOffset(method, insertText);
                        if (seen.Add(dedupKey))
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

                if (completions.Count > 0)
                    return completions;
            }

            // In qualified/member-name context, avoid broad fallback suggestions.
            return completions;
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
        var offerValueCompletions = forceInsertionAtCaret
            || token.Parent is IdentifierNameSyntax { Parent: BlockStatementSyntax or ExpressionStatementSyntax }
            || token.IsKind(SyntaxKind.IdentifierToken)
            || token.IsKind(SyntaxKind.EndOfFileToken)
            || (semanticContext is not null && token.Kind == SyntaxKind.None)
            || isArgumentStartToken;

        if (offerValueCompletions)
        {
            var valuePrefix = isArgumentStartToken ? string.Empty : tokenValueText;
            var prefixWithoutEscape = valuePrefix.TrimStart('@');
            var allowTypesInValuePosition = prefixWithoutEscape.Length > 0 && char.IsUpper(prefixWithoutEscape[0]);
            var contextNode = semanticContext ?? token.Parent ?? model.SyntaxTree.GetRoot();

            foreach (var symbol in fragmentLocalSymbols)
            {
                if (!string.IsNullOrEmpty(valuePrefix) &&
                    !NameMatchesPrefix(symbol.Name, valuePrefix))
                {
                    continue;
                }

                var (displayText, insertText, dedupKey) = CreateCompletionParts(symbol);
                if (seen.Add(dedupKey))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: displayText,
                        InsertionText: insertText,
                        ReplacementSpan: argumentStartReplacementSpan,
                        CursorOffset: GetDefaultCursorOffset(symbol, insertText),
                        Description: SafeToDisplayString(symbol),
                        Symbol: symbol));
                }
            }

            foreach (var symbol in model.GetVisibleValueSymbols(
                contextNode,
                allowBindingFallback: semanticContext is not null))
            {
                if (!IsAccessible(symbol))
                    continue;

                if (!IsValueCompletionSymbol(symbol, allowTypes: false))
                    continue;

                if (!string.IsNullOrEmpty(valuePrefix) &&
                    !NameMatchesPrefix(symbol.Name, valuePrefix))
                    continue;

                var (displayText, insertText, dedupKey) = CreateCompletionParts(symbol);
                var cursorOffset = GetDefaultCursorOffset(symbol, insertText);

                if (seen.Add(dedupKey))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: displayText,
                        InsertionText: insertText,
                        ReplacementSpan: argumentStartReplacementSpan,
                        CursorOffset: cursorOffset,
                        Description: SafeToDisplayString(symbol),
                        Symbol: symbol
                    ));
                }
            }

            foreach (var symbol in binder.LookupAvailableSymbols())
            {
                if (!IsAccessible(symbol))
                    continue;

                if (!IsValueCompletionSymbol(symbol, allowTypesInValuePosition))
                    continue;

                if (symbol is MethodSymbol method && IsSuppressedCompletionMethod(method))
                    continue;

                if (!string.IsNullOrEmpty(valuePrefix) &&
                    !NameMatchesPrefix(symbol.Name, valuePrefix))
                    continue;

                var (displayText, insertText, dedupKey) = CreateCompletionParts(symbol);
                var cursorOffset = GetDefaultCursorOffset(symbol, insertText);

                if (seen.Add(dedupKey))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: displayText,
                        InsertionText: insertText,
                        ReplacementSpan: argumentStartReplacementSpan,
                        CursorOffset: cursorOffset,
                        Description: SafeToDisplayString(symbol),
                        Symbol: symbol
                    ));
                }
            }
        }

        // Language keywords (added after symbol completions so escaped identifiers win on duplicates)
        var shouldOfferKeywords = forceInsertionAtCaret ||
            token.GetAncestor<MemberAccessExpressionSyntax>() is null &&
            token.GetAncestor<QualifiedNameSyntax>() is null &&
            !token.IsKind(SyntaxKind.DotToken);

        if (shouldOfferKeywords)
        {
            var keywordPrefix = isArgumentStartToken ? string.Empty : tokenValueText;
            foreach (var keyword in CompletionService.BasicKeywords.Where(k => string.IsNullOrEmpty(keywordPrefix) || k.StartsWith(keywordPrefix, StringComparison.OrdinalIgnoreCase)))
            {
                if (seen.Add(keyword))
                {
                    completions.Add(new CompletionItem(
                        DisplayText: keyword,
                        InsertionText: keyword,
                        ReplacementSpan: argumentStartReplacementSpan
                    ));
                }
            }
        }

        return completions;
    }

    private static bool IsNonCodePosition(SyntaxTree tree, int position)
    {
        if (position <= 0)
            return false;

        var token = tree.GetRoot().FindToken(position - 1);
        foreach (var trivia in token.LeadingTrivia.Concat(token.TrailingTrivia))
        {
            if (position <= trivia.Span.Start || position > trivia.Span.End)
                continue;

            if (trivia.Kind is SyntaxKind.SingleLineCommentTrivia or SyntaxKind.DocumentationCommentTrivia)
                return true;

            if (trivia.Kind == SyntaxKind.MultiLineCommentTrivia &&
                (position < trivia.Span.End || !trivia.Text.EndsWith("*/", StringComparison.Ordinal)))
                return true;
        }

        if (position <= token.Span.Start || position > token.Span.End)
            return false;

        if (token.Parent is InterpolatedStringTextSyntax || token.Kind == SyntaxKind.StringStartToken)
            return true;

        if (token.Kind == SyntaxKind.MultiLineStringLiteralToken)
            return position < token.Span.End || token.Text.Length < 6 ||
                !token.Text.EndsWith("\"\"\"", StringComparison.Ordinal);

        if (token.Kind is SyntaxKind.StringLiteralToken or SyntaxKind.CharacterLiteralToken)
        {
            var delimiter = token.Kind == SyntaxKind.CharacterLiteralToken ? '\'' : '"';
            return position < token.Span.End || token.Text.Length < 2 || token.Text[^1] != delimiter;
        }

        return false;
    }

    private static bool TryGetMacroCompletionContext(
        SyntaxToken token,
        SourceText sourceText,
        int position,
        out MacroCompletionContext context)
    {
        context = default;

        var attribute = token.Parent?.AncestorsAndSelf()
            .OfType<AttributeListSyntax>()
            .SelectMany(static list => list.Attributes)
            .FirstOrDefault(static candidate => candidate.IsMacroAttribute());
        if (attribute is not null &&
            position >= ((AttributeListSyntax)attribute.Parent!).HashToken.Span.End &&
            (attribute.ArgumentList is null || position <= attribute.ArgumentList.Span.Start))
        {
            CreateMacroCompletionContext(attribute.Name, MacroKind.AttachedDeclaration, sourceText, position, out context);
            return true;
        }

        var freestandingMacro = token.Parent?.AncestorsAndSelf().OfType<FreestandingMacroExpressionSyntax>().FirstOrDefault();
        var freestandingNameStart = freestandingMacro?.Name.Span.Start ?? int.MaxValue;
        var freestandingNameEnd = freestandingMacro?.ExclamationToken.SpanStart ?? int.MinValue;
        if (freestandingMacro is not null &&
            position >= freestandingNameStart &&
            position <= freestandingNameEnd)
        {
            CreateMacroCompletionContext(
                freestandingMacro.Name,
                MacroKind.Freestanding,
                sourceText,
                position,
                out context,
                preserveInvocationSuffix: true);
            return true;
        }

        if (token.Parent is IncompleteMemberDeclarationSyntax incompleteMember &&
            incompleteMember.SkippedTokens.LeadingTrivia
                .Where(static trivia => trivia.Kind == SyntaxKind.SkippedTokensTrivia)
                .Select(static trivia => trivia.GetStructure())
                .OfType<SkippedTokensTrivia>()
                .SelectMany(static trivia => trivia.Tokens)
                .Any(static skipped => skipped.IsKind(SyntaxKind.HashToken)))
        {
            context = new MacroCompletionContext(
                MacroKind.AttachedDeclaration,
                Prefix: string.Empty,
                ReplacementSpan: new TextSpan(position, 0),
                WrapAttachedAttribute: true);
            return true;
        }

        return false;
    }

    private static IEnumerable<CompletionItem> GetMacroParameterCompletions(
        Compilation compilation,
        SyntaxToken token,
        SourceText sourceText,
        int position)
    {
        var argumentList = token.Parent?.AncestorsAndSelf()
            .OfType<ArgumentListSyntax>()
            .FirstOrDefault();
        if (argumentList is null ||
            position < argumentList.OpenParenToken.Span.End ||
            position > argumentList.CloseParenToken.SpanStart)
        {
            yield break;
        }

        MacroDefinitionDescriptor? descriptor = argumentList.Parent switch
        {
            AttributeSyntax attribute when
                attribute.TryGetMacroName(out var name) &&
                compilation.GetMacroRegistry().TryResolveAttachedMacro(
                    compilation,
                    attribute,
                    name,
                    out var loaded,
                    out _) =>
                loaded.Descriptor,
            ParenthesizedMacroCarrierSyntax { Parent: FreestandingMacroExpressionSyntax expression } when
                expression.TryGetMacroName(out var name) &&
                compilation.GetMacroRegistry().TryResolveFreestandingMacro(
                    compilation,
                    expression,
                    name,
                    out var loaded,
                    out _) =>
                loaded.Descriptor,
            _ => null
        };
        if (descriptor is null)
            yield break;

        var replacementSpan = new TextSpan(position, 0);
        var prefix = string.Empty;
        string? currentNamedArgument = null;
        var isParameterNamePosition =
            token.IsKind(SyntaxKind.OpenParenToken) ||
            token.IsKind(SyntaxKind.CommaToken);
        if (token.Parent?.GetAncestor<ArgumentSyntax>() is { } currentArgument &&
            currentArgument.Parent == argumentList)
        {
            currentNamedArgument = currentArgument.NameColon?.Name.Identifier.ValueText;
            var candidateToken = currentArgument.NameColon?.Name.Identifier ?? token;
            if (candidateToken.IsKind(SyntaxKind.IdentifierToken) &&
                position >= candidateToken.Span.Start &&
                position <= candidateToken.Span.End)
            {
                replacementSpan = candidateToken.Span;
                var prefixLength = Math.Clamp(position - candidateToken.Span.Start, 0, candidateToken.Span.Length);
                prefix = sourceText.ToString(new TextSpan(candidateToken.Span.Start, prefixLength));
                isParameterNamePosition =
                    currentArgument.NameColon is not null ||
                    currentArgument.Expression is IdentifierNameSyntax;
            }
        }

        if (!isParameterNamePosition)
            yield break;

        var usedNames = argumentList.Arguments
            .Select(static argument => argument.NameColon?.Name.Identifier.ValueText)
            .Where(static name => !string.IsNullOrEmpty(name))
            .ToHashSet(StringComparer.Ordinal);
        if (currentNamedArgument is not null)
            usedNames.Remove(currentNamedArgument);

        foreach (var parameter in descriptor.Parameters)
        {
            if (usedNames.Contains(parameter.Name) ||
                !parameter.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
            {
                continue;
            }

            yield return new CompletionItem(
                DisplayText: parameter.Name,
                InsertionText: parameter.Name + ": ",
                ReplacementSpan: replacementSpan,
                Description: $"macro argument: {parameter.TypeDisplayName}");
        }
    }

    private static void CreateMacroCompletionContext(
        SyntaxNode nameNode,
        MacroKind kind,
        SourceText sourceText,
        int position,
        out MacroCompletionContext context,
        bool preserveInvocationSuffix = false)
    {
        var replacementSpan = nameNode.Span.Length > 0
            ? nameNode.Span
            : new TextSpan(position, 0);
        var prefixLength = Math.Clamp(position - replacementSpan.Start, 0, replacementSpan.Length);
        var prefix = prefixLength > 0
            ? sourceText.ToString(new TextSpan(replacementSpan.Start, prefixLength))
            : string.Empty;

        context = new MacroCompletionContext(
            kind,
            prefix,
            replacementSpan,
            PreserveInvocationSuffix: preserveInvocationSuffix);
    }

    private static IEnumerable<CompletionItem> GetMacroCompletions(
        Compilation compilation,
        SyntaxNode contextNode,
        MacroCompletionContext context)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var (name, descriptor) in EnumerateMacros(compilation, contextNode, context.Kind))
        {
            if (!seen.Add(name) || !MacroNameMatchesPrefix(name, context.Prefix))
                continue;

            if (!compilation.GetMacroRegistry().TryResolveMacroSymbol(
                    compilation,
                    contextNode,
                    name,
                    out var macroSymbol,
                    out _))
            {
                continue;
            }

            var insertionText = context.PreserveInvocationSuffix
                ? name
                : descriptor switch
                {
                    { HasTokenBody: true, AcceptsArguments: true } => name + "() { }",
                    { HasTokenBody: true } => name + " { }",
                    { AcceptsArguments: true } when context.WrapAttachedAttribute => $"[{name}()]",
                    _ when context.WrapAttachedAttribute => $"[{name}]",
                    _ when context.Kind == MacroKind.Freestanding => name + "()",
                    _ => name
                };
            var cursorOffset = context.PreserveInvocationSuffix
                ? null
                : descriptor switch
                {
                    { HasTokenBody: true, AcceptsArguments: true } => name.Length + 1,
                    { HasTokenBody: true } => insertionText.Length - 1,
                    { AcceptsArguments: true } when context.WrapAttachedAttribute => name.Length + 2,
                    { AcceptsArguments: true } when context.Kind == MacroKind.Freestanding => insertionText.Length - 1,
                    _ => (int?)null
                };

            yield return new CompletionItem(
                DisplayText: name,
                InsertionText: insertionText,
                ReplacementSpan: context.ReplacementSpan,
                CursorOffset: cursorOffset,
                Description: CreateMacroDescription(descriptor),
                Symbol: macroSymbol);
        }
    }

    private static IEnumerable<(string Name, MacroDefinitionDescriptor Descriptor)> EnumerateMacros(
        Compilation compilation,
        SyntaxNode context,
        MacroKind kind)
        => compilation.GetMacroRegistry().GetVisibleMacros(compilation, context, kind);

    private static string CreateMacroDescription(MacroDefinitionDescriptor descriptor)
    {
        var kindDisplay = descriptor.ApplicationKind switch
        {
            MacroApplicationKind.Attached => "attached declaration macro",
            MacroApplicationKind.Freestanding => "freestanding procedural macro",
            _ => "macro"
        };
        var targets = descriptor.AttachmentTargets;
        var targetsDisplay = targets == MacroTarget.None
            ? null
            : $"targets: {FormatMacroTargets(targets)}";
        var argumentsDisplay = descriptor switch
        {
            { HasTokenBody: true, AcceptsArguments: true } => "accepts arguments and a token-tree body",
            { HasTokenBody: true } => "accepts a token-tree body",
            { AcceptsArguments: true } => "accepts arguments",
            _ => "no arguments"
        };

        return string.Join(
            " • ",
            new[] { kindDisplay, targetsDisplay, argumentsDisplay }.Where(static part => !string.IsNullOrWhiteSpace(part)));
    }

    private static string FormatMacroTargets(MacroTarget targets)
    {
        return string.Join(
            ", ",
            Enum.GetValues<MacroTarget>()
                .Where(target => target != MacroTarget.None && targets.HasFlag(target))
                .Select(static target => target.ToString()));
    }

    private static bool MacroNameMatchesPrefix(string candidate, string prefix)
    {
        if (string.IsNullOrEmpty(prefix))
            return true;

        return candidate.StartsWith(prefix, StringComparison.OrdinalIgnoreCase);
    }
}
