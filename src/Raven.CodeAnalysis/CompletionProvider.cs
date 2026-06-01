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
        TextSpan ReplacementSpan);

    public static IEnumerable<CompletionItem> GetCompletions(
        SyntaxToken token,
        SemanticModel model,
        int position,
        bool forceInsertionAtCaret = false)
    {
        var sourceText = model.SyntaxTree.GetText();

        if (TryGetMacroCompletionContext(token, sourceText, position, out var macroContext))
        {
            var macroCompletions = GetMacroCompletions(model.Compilation, macroContext).ToArray();
            if (macroCompletions.Length > 0)
                return macroCompletions;
        }

        var binder = model.GetBinder(token.Parent);
        if (token.Parent is { } tokenParent)
            model.EnsurePrecedingGlobalStatementsBoundForSemanticQuery(tokenParent);

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

        bool CanAccessPrivateSelfMembers()
        {
            for (Binder? current = binder; current is not null; current = current.ParentBinder)
            {
                if (current.ContainingSymbol is IMethodSymbol method)
                {
                    if (method.IsExtensionMethod)
                        return false;

                    if (!method.IsStatic)
                        return true;

                    if (method is not ILambdaSymbol)
                        return false;
                }
            }

            return token.GetAncestor<BaseMethodDeclarationSyntax>() is not null ||
                token.GetAncestor<ConstructorDeclarationSyntax>() is not null;
        }

        bool IsAccessibleOnSelf(ISymbol symbol, ITypeSymbol selfType)
        {
            if (IsAccessible(symbol))
                return true;

            if (!CanAccessPrivateSelfMembers())
                return false;

            return symbol.DeclaredAccessibility == Accessibility.Private &&
                symbol.ContainingType is { } containingType &&
                SymbolEqualityComparer.Default.Equals(containingType, selfType);
        }

        static string SafeToDisplayString(ISymbol symbol)
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

        ITypeSymbol? GetSelfType()
        {
            static ITypeSymbol? GetMethodSelfType(IMethodSymbol method)
            {
                if (method.IsExtensionMethod && !method.Parameters.IsDefaultOrEmpty)
                    return method.Parameters[0].Type;

                if (!method.IsStatic)
                    return method.ContainingType;

                return null;
            }

            for (Binder? current = binder; current is not null; current = current.ParentBinder)
            {
                if (current.ContainingSymbol is IMethodSymbol method)
                {
                    if (GetMethodSelfType(method) is { } methodSelfType)
                        return methodSelfType;

                    if (method is not ILambdaSymbol)
                        return null;
                }
            }

            var containingMethodSyntax = (SyntaxNode?)token.GetAncestor<BaseMethodDeclarationSyntax>()
                ?? token.GetAncestor<ConstructorDeclarationSyntax>();
            if (containingMethodSyntax is not null &&
                model.GetDeclaredSymbol(containingMethodSyntax) is IMethodSymbol declaredMethod &&
                GetMethodSelfType(declaredMethod) is { } declaredMethodSelfType)
            {
                return declaredMethodSelfType;
            }

            return null;
        }

        ISymbol? TryLookupValueSymbolByName(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                return null;

            if (token.Parent is { } contextNode &&
                model.TryLookupVisibleValueSymbol(contextNode, name) is { } visibleSymbol)
            {
                return visibleSymbol;
            }

            var candidates = binder.LookupAvailableSymbols()
                .Where(symbol => string.Equals(symbol.Name, name, StringComparison.Ordinal))
                .ToArray();

            return candidates.FirstOrDefault(static symbol =>
                       symbol is ILocalSymbol or IParameterSymbol or IFieldSymbol or IPropertySymbol or IEventSymbol)
                   ?? candidates.FirstOrDefault();
        }

        bool TryGetPreferredSymbolInfo(SyntaxNode node, out SymbolInfo symbolInfo)
        {
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

        static bool IsUsableNamespaceOrTypeSymbol(INamespaceOrTypeSymbol symbol)
            => symbol is not IErrorTypeSymbol and not ITypeSymbol { TypeKind: TypeKind.Error };

        static INamespaceOrTypeSymbol? AsUsableNamespaceOrTypeSymbol(ISymbol? symbol)
        {
            var namespaceOrType = symbol?.UnderlyingSymbol as INamespaceOrTypeSymbol;
            return namespaceOrType is not null && IsUsableNamespaceOrTypeSymbol(namespaceOrType)
                ? namespaceOrType
                : null;
        }

        INamespaceOrTypeSymbol? TryResolveNamespaceOrType(NameSyntax name)
        {
            INamespaceOrTypeSymbol? resolved = null;
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

        INamespaceOrTypeSymbol? TryResolveImportNamespaceOrType(NameSyntax name)
        {
            return name switch
            {
                IdentifierNameSyntax identifier => TryResolveIdentifierNamespaceOrType(identifier),
                QualifiedNameSyntax qualified => TryResolveImportQualifiedNamespaceOrType(qualified),
                GenericNameSyntax generic => TryResolveGenericNamespaceOrType(generic),
                _ => null
            };
        }

        INamespaceOrTypeSymbol? TryResolveImportQualifiedNamespaceOrType(QualifiedNameSyntax qualified)
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

        INamespaceOrTypeSymbol? TryResolveIdentifierNamespaceOrType(IdentifierNameSyntax identifier)
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

        ITypeParameterSymbol? TryResolveEnclosingTypeParameter(SyntaxNode node, string name)
        {
            foreach (var ancestor in node.Ancestors())
            {
                switch (ancestor)
                {
                    case MethodDeclarationSyntax methodDeclaration
                        when model.GetDeclaredSymbol(methodDeclaration) is IMethodSymbol method:
                        return method.TypeParameters.FirstOrDefault(typeParameter => typeParameter.Name == name);

                    case FunctionStatementSyntax functionStatement
                        when model.GetDeclaredSymbol(functionStatement) is IMethodSymbol function:
                        return function.TypeParameters.FirstOrDefault(typeParameter => typeParameter.Name == name);

                    case TypeDeclarationSyntax typeDeclaration
                        when model.GetDeclaredSymbol(typeDeclaration) is INamedTypeSymbol type:
                        return type.TypeParameters.FirstOrDefault(typeParameter => typeParameter.Name == name);

                    case InterfaceDeclarationSyntax interfaceDeclaration
                        when model.GetDeclaredSymbol(interfaceDeclaration) is INamedTypeSymbol @interface:
                        return @interface.TypeParameters.FirstOrDefault(typeParameter => typeParameter.Name == name);
                }
            }

            return null;
        }

        INamespaceOrTypeSymbol? TryResolveQualifiedNamespaceOrType(QualifiedNameSyntax qualified)
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

        INamespaceOrTypeSymbol? TryResolveGenericNamespaceOrType(GenericNameSyntax generic)
        {
            var arity = generic.TypeArgumentList.Arguments.Count;
            var name = generic.Identifier.ValueText;

            var namedType = binder.LookupSymbols(name)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault(candidate => candidate.Arity == arity && IsUsableNamespaceOrTypeSymbol(candidate));

            return namedType
                ?? AsUsableNamespaceOrTypeSymbol(binder.LookupSymbol(name))
                ?? LookupRootMember(name, arity);
        }

        INamespaceOrTypeSymbol? TryResolveRootNamespaceOrType(NameSyntax name)
        {
            return name switch
            {
                IdentifierNameSyntax identifier => LookupRootMember(identifier.Identifier.ValueText, arity: 0),
                GenericNameSyntax generic => LookupRootMember(generic.Identifier.ValueText, generic.TypeArgumentList.Arguments.Count),
                QualifiedNameSyntax qualified => TryResolveRootQualifiedNamespaceOrType(qualified),
                _ => null
            };
        }

        INamespaceOrTypeSymbol? TryResolveRootQualifiedNamespaceOrType(QualifiedNameSyntax qualified)
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

        INamespaceOrTypeSymbol? LookupRootMember(string name, int arity)
        {
            if (string.IsNullOrWhiteSpace(name))
                return null;

            model.Compilation.EnsureSetup();

            return LookupQualifiedMember(model.Compilation.GlobalNamespace, name, arity);
        }

        INamespaceOrTypeSymbol? LookupQualifiedMember(INamespaceOrTypeSymbol? container, string name, int arity)
        {
            if (container is null || string.IsNullOrWhiteSpace(name))
                return null;

            if (container is INamespaceSymbol namespaceSymbol)
            {
                var directMember = (INamespaceOrTypeSymbol?)namespaceSymbol.LookupNamespace(name)
                    ?? SelectTypeMember(namespaceSymbol.GetMembers(name).OfType<INamedTypeSymbol>(), arity)
                    ?? (INamespaceOrTypeSymbol?)namespaceSymbol.LookupType(name);
                if (directMember is not null)
                    return directMember;

                foreach (var member in GetNamespaceCompletionMembers(namespaceSymbol).OfType<INamespaceOrTypeSymbol>())
                {
                    if (!string.Equals(member.Name, name, StringComparison.Ordinal))
                        continue;

                    if (member is INamedTypeSymbol namedType)
                        return SelectTypeMember(new[] { namedType }, arity);

                    return member;
                }

                return null;
            }

            if (container is ITypeSymbol typeSymbol)
            {
                return SelectTypeMember(typeSymbol.GetMembers(name).OfType<INamedTypeSymbol>(), arity);
            }

            return null;
        }

        static INamedTypeSymbol? SelectTypeMember(IEnumerable<INamedTypeSymbol> candidates, int arity)
        {
            foreach (var candidate in candidates)
            {
                if (candidate.Arity == arity)
                    return candidate;
            }

            return null;
        }

        ISymbol? TryResolveReceiverSymbol(ExpressionSyntax receiverExpression)
        {
            string? receiverName = null;
            if (receiverExpression is IdentifierNameSyntax receiverIdentifier)
            {
                receiverName = receiverIdentifier.Identifier.ValueText;
                if (string.IsNullOrWhiteSpace(receiverName))
                    return null;

                var symbol = model.TryLookupVisibleValueSymbol(receiverExpression, receiverName)
                    ?? binder.LookupSymbol(receiverName)
                    ?? TryLookupValueSymbolByName(receiverName);
                if (symbol is not null)
                    return symbol;
            }

            var receiverBinder = model.GetBinder(receiverExpression);
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

        ISymbol? TryResolveEnclosingPatternOrLoopLocalSymbol(
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

        ISymbol? TryResolveForTargetSymbol(
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

        ISymbol? TryResolvePatternDesignationSymbol(
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

        ILocalSymbol CreateSyntheticReceiverLocalSymbol(string name, ITypeSymbol type, ExpressionSyntax receiverExpression)
        {
            var containingSymbol = model.GetBinder(receiverExpression).ContainingSymbol ?? model.Compilation.GlobalNamespace;
            return new SourceLocalSymbol(
                name,
                type,
                isMutable: false,
                containingSymbol,
                containingSymbol.ContainingType,
                containingSymbol as INamespaceSymbol ?? containingSymbol.ContainingNamespace,
                locations: [],
                declaringSyntaxReferences: []);
        }

        ITypeSymbol? TryGetForIterationElementType(ExpressionSyntax expression)
        {
            var collectionType = TryGetExpressionType(expression);
            if (collectionType is null || collectionType.TypeKind == TypeKind.Error)
                return null;

            return TryGetSequenceElementType(collectionType);
        }

        ITypeSymbol? TryGetExpressionType(ExpressionSyntax expression)
        {
            if (TryGetPreferredSymbolInfo(expression, out var symbolInfo) &&
                GetTypeFromSymbol(symbolInfo.Symbol?.UnderlyingSymbol) is { } symbolType)
            {
                return symbolType;
            }

            return model.GetTypeInfo(expression).Type;
        }

        ITypeSymbol? TryGetSequenceElementType(ITypeSymbol collectionType)
        {
            if (collectionType is IArrayTypeSymbol arrayType)
                return arrayType.ElementType;

            if (collectionType.SpecialType == SpecialType.System_String)
                return model.Compilation.GetSpecialType(SpecialType.System_Char);

            if (collectionType is INamedTypeSymbol namedType)
            {
                foreach (var candidate in EnumerateSelfAndInterfaces(namedType))
                {
                    if (candidate.TypeArguments.Length == 1 &&
                        candidate.Name is "IEnumerable" or "IAsyncEnumerable")
                    {
                        return candidate.TypeArguments[0];
                    }
                }
            }

            return null;
        }

        static IEnumerable<INamedTypeSymbol> EnumerateSelfAndInterfaces(INamedTypeSymbol type)
        {
            yield return type;
            foreach (var iface in type.AllInterfaces)
                yield return iface;
        }

        var tokenText = forceInsertionAtCaret ? string.Empty : token.Text;
        var tokenValueText = forceInsertionAtCaret ? string.Empty : token.ValueText;
        var replacementSpan = forceInsertionAtCaret
            ? new TextSpan(position, 0)
            : new TextSpan(token.Position, tokenText.Length);
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

        static (string displayText, string insertionText, string dedupKey) CreateCompletionParts(ISymbol symbol)
        {
            var escapedName = EscapeIdentifierForInsertion(symbol.Name);
            var insertionText = symbol is IMethodSymbol
                ? escapedName + "()"
                : escapedName;
            var displayText = symbol is IUnionCaseTypeSymbol unionCase
                ? ((INamedTypeSymbol)unionCase).FormatUnionCaseForDiagnostic()
                : escapedName;
            var dedupKey = symbol is IUnionCaseTypeSymbol
                ? displayText
                : symbol.Name;

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

        static int? GetDefaultCursorOffset(ISymbol symbol, string insertionText)
        {
            return symbol switch
            {
                IMethodSymbol => insertionText.Length - 1,
                _ => (int?)null
            };
        }

        static bool IsSuppressedCompletionMethod(IMethodSymbol method)
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

        static bool IsWeakExtensionReceiverType(ITypeSymbol receiverType)
        {
            if (receiverType is null)
                return true;

            if (receiverType is ITypeParameterSymbol)
                return true;

            if (receiverType.SpecialType == SpecialType.System_Object)
                return true;

            return false;
        }

        static bool IsImportDirectiveTypeMemberCompletion(ISymbol member)
            => member is INamespaceOrTypeSymbol or IFieldSymbol { IsConst: true };

        static bool ShouldIncludeExtensionMethod(IMethodSymbol method, string prefix)
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

        static bool IsTypeAccessSymbol(ISymbol? symbol, ITypeSymbol? type)
        {
            return symbol switch
            {
                INamedTypeSymbol or ITypeParameterSymbol => type is not null && SymbolEqualityComparer.Default.Equals(symbol, type),
                _ => false
            };
        }

        static ITypeSymbol? TryGetTypeAccessSymbol(ISymbol? symbol, ITypeSymbol? type)
        {
            if (IsTypeAccessSymbol(symbol, type) && symbol is ITypeSymbol symbolType)
                return symbolType;

            return null;
        }

        static IEnumerable<ISymbol> GetTypeMembersIncludingBase(ITypeSymbol type, bool includeStatic) =>
            type.GetMembersRecursive(
                TypeSymbolLookupExtensions.MemberLookupFlags.Default,
                member => member.IsStatic == includeStatic);

        static ITypeSymbol UnwrapAliases(ITypeSymbol type)
        {
            while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
                type = alias;

            return type;
        }

        static bool TryGetOptionPayloadType(ITypeSymbol? type, out ITypeSymbol payload)
        {
            payload = null!;
            if (type is null)
                return false;

            type = type.UnwrapLiteralType() ?? type;
            if (type is INamedTypeSymbol named &&
                named.Arity == 1 &&
                string.Equals(named.Name, "Option", StringComparison.Ordinal))
            {
                payload = named.TypeArguments[0];
                return true;
            }

            return false;
        }

        static bool TryGetResultPayloadType(ITypeSymbol? type, out ITypeSymbol payload)
        {
            payload = null!;
            if (type is null)
                return false;

            type = type.UnwrapLiteralType() ?? type;
            if (type is INamedTypeSymbol named &&
                named.Arity == 2 &&
                string.Equals(named.Name, "Result", StringComparison.Ordinal))
            {
                payload = named.TypeArguments[0];
                return true;
            }

            return false;
        }

        static ITypeSymbol? GetCarrierConditionalAccessLookupType(ITypeSymbol? type)
        {
            if (type is null)
                return null;

            type = type.UnwrapLiteralType() ?? type;
            if (TryGetOptionPayloadType(type, out var optionPayload))
                return optionPayload.GetPlainType();

            if (TryGetResultPayloadType(type, out var resultPayload))
                return resultPayload.GetPlainType();

            return type.GetPlainType();
        }

        static ITypeSymbol? GetTypeFromSymbol(ISymbol? symbol)
        {
            while (symbol is not null)
            {
                switch (symbol)
                {
                    case ITypeSymbol type:
                        return type;
                    case ILocalSymbol local:
                        return local.Type;
                    case IFieldSymbol field:
                        return field.Type;
                    case IPropertySymbol property:
                        return property.Type;
                    case IEventSymbol @event:
                        return @event.Type;
                    case IParameterSymbol parameter:
                        return parameter.Type;
                    case IMethodSymbol method:
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

        (ISymbol? Symbol, ITypeSymbol? Type) ResolveReceiver(ExpressionSyntax expression)
        {
            var symbol = TryGetPreferredSymbolInfo(expression, out var symbolInfo)
                ? symbolInfo.Symbol?.UnderlyingSymbol
                : null;

            if (expression is InvocationExpressionSyntax &&
                symbol is IMethodSymbol invokedMethod)
            {
                return (symbol, invokedMethod.ReturnType);
            }

            if (symbol is null &&
                expression is IdentifierNameSyntax receiverIdentifier &&
                model.TryLookupVisibleValueSymbol(expression, receiverIdentifier.Identifier.ValueText) is { } visibleSymbol)
            {
                symbol = visibleSymbol;
            }

            var type = expression is NameOfExpressionSyntax
                ? model.GetTypeInfo(expression).Type
                : null;
            if (type is not null && type.TypeKind != TypeKind.Error)
                return (symbol, type);

            type = GetTypeFromSymbol(symbol);
            if (type is not null && type.TypeKind != TypeKind.Error)
                return (symbol, type);

            var binderSymbol = model.GetBinder(expression).BindSymbol(expression).Symbol?.UnderlyingSymbol;
            if (binderSymbol is not null)
            {
                symbol = binderSymbol;
                type = GetTypeFromSymbol(symbol);
                if (type is not null && type.TypeKind != TypeKind.Error)
                    return (symbol, type);
            }

            type = model.GetTypeInfo(expression).Type;
            return (symbol, type);
        }

        static bool NeedsReceiverFallback(ISymbol? symbol, ITypeSymbol? type)
        {
            if (symbol is null)
                return true;

            if (symbol.Kind is SymbolKind.Error or SymbolKind.ErrorType)
                return true;

            if (type is null)
                return true;

            return type.TypeKind == TypeKind.Error;
        }

        IEnumerable<ISymbol> GetTypeAccessMembers(ITypeSymbol typeAccessSymbol)
        {
            if (typeAccessSymbol is INamedTypeSymbol namedType)
            {
                var staticMembers = namedType.GetMembers().Where(m => m.IsStatic && IsAccessible(m));
                return namedType is IUnionSymbol union
                    ? staticMembers.Concat(union.CaseTypes.Where(IsAccessible))
                    : staticMembers;
            }

            if (typeAccessSymbol is not ITypeParameterSymbol typeParameter)
                return Array.Empty<ISymbol>();

            binder.EnsureTypeParameterConstraintTypesResolved(ImmutableArray.Create(typeParameter));

            var members = new List<ISymbol>();
            var seen = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
            var visitedTypeParameters = new HashSet<ITypeParameterSymbol>(SymbolEqualityComparer.Default);

            void AddConstraintMembers(ITypeSymbol constraintType)
            {
                if (constraintType is INamedTypeSymbol namedConstraint)
                {
                    foreach (var member in namedConstraint.GetMembers())
                    {
                        if (!member.IsStatic || !IsAccessible(member) || !seen.Add(member))
                            continue;

                        members.Add(member);
                    }

                    return;
                }

                if (constraintType is ITypeParameterSymbol nestedTypeParameter &&
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

        IEnumerable<ISymbol> GetNamespaceCompletionMembers(INamespaceSymbol namespaceSymbol)
        {
            var includeNamespaceMembers = model.Compilation.Options.AllowNamespaceMembers &&
                                          model.Compilation.Options.AllowNamespaceMemberImports;

            return namespaceSymbol.GetMembers()
                .Concat(model.Compilation.GetNamespaceMembers(namespaceSymbol, includeNamespaceMembers))
                .Where(IsAccessible);
        }

        void AddCompletionItem(ISymbol symbol, TextSpan replacementSpan)
        {
            if (symbol is IMethodSymbol method && IsSuppressedCompletionMethod(method))
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

        void AddImportDirectiveMemberCompletions(INamespaceOrTypeSymbol nsOrType, string prefix, TextSpan replacementSpan)
        {
            AddWildcardImportCompletion(prefix, replacementSpan);

            var members = nsOrType is INamespaceSymbol importNamespace
                ? GetNamespaceCompletionMembers(importNamespace)
                : nsOrType.GetMembers()
                    .Where(IsAccessible)
                    .Where(IsImportDirectiveTypeMemberCompletion);

            foreach (var member in members
                .Where(m => NameMatchesPrefix(m.Name, prefix)))
            {
                var (displayText, insertText, dedupKey) = CreateCompletionParts(member);
                var cursorOffset = member is ITypeSymbol ? insertText.Length : (int?)null;

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
            ITypeSymbol receiverType,
            string prefix,
            TextSpan replacementSpan,
            ExtensionMemberKinds kinds)
        {
            ExtensionMemberLookupResult extensionMembers;
            try
            {
                extensionMembers = ExtensionMemberLookup.Lookup(
                    binder,
                    receiverType,
                    includePartialMatches: false,
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

        static bool ContainsTypeParameter(ITypeSymbol type)
        {
            type = type.UnwrapLiteralType() ?? type;

            return type switch
            {
                ITypeParameterSymbol => true,
                INamedTypeSymbol named => named.TypeArguments.Any(ContainsTypeParameter),
                IArrayTypeSymbol array => ContainsTypeParameter(array.ElementType),
                IPointerTypeSymbol pointer => ContainsTypeParameter(pointer.PointedAtType),
                IAddressTypeSymbol address => ContainsTypeParameter(address.ReferencedType),
                ITypeUnionSymbol union => union.Types.Any(ContainsTypeParameter),
                _ => false
            };
        }

        bool CanPipeReceiverConvertTo(ITypeSymbol receiverType, ITypeSymbol targetType)
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

        bool IsPipeCompatibleFunctionTarget(IMethodSymbol method, ITypeSymbol receiverType)
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

        bool IsPipeCompatiblePropertyTarget(IPropertySymbol property, ITypeSymbol receiverType)
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

        void AddPipeTargetCompletions(ITypeSymbol receiverType, string prefix, TextSpan targetSpan)
        {
            receiverType = receiverType.UnwrapLiteralType() ?? receiverType;
            model.Compilation.EnsureSetup();

            void AddPipeTargetIfApplicable(ISymbol symbol)
            {
                if (!IsAccessible(symbol) || !NameMatchesPrefix(symbol.Name, prefix))
                    return;

                switch (symbol)
                {
                    case IMethodSymbol method when IsPipeCompatibleFunctionTarget(method, receiverType):
                        AddCompletionItem(method, targetSpan);
                        break;

                    case IPropertySymbol property when IsPipeCompatiblePropertyTarget(property, receiverType):
                        AddCompletionItem(property, targetSpan);
                        break;
                }
            }

            INamespaceSymbol GetCurrentNamespace()
            {
                if (token.GetAncestor<NamespaceDeclarationSyntax>() is { } namespaceDeclaration &&
                    model.GetDeclaredSymbol(namespaceDeclaration) is INamespaceSymbol namespaceSymbol)
                {
                    return namespaceSymbol;
                }

                return model.Compilation.GlobalNamespace;
            }

            var contextNode = token.Parent ?? model.SyntaxTree.GetRoot();
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

        ITypeSymbol? TryGetExplicitlyAnnotatedType(ISymbol? symbol)
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
                        ? symbolInfo.Symbol?.UnderlyingSymbol as ITypeSymbol
                        : null);
                if (declaredTypeSymbol is not null)
                    return declaredTypeSymbol;
            }

            return null;
        }

        bool IsValueCompletionSymbol(ISymbol symbol, bool allowTypes)
            => symbol switch
            {
                ILocalSymbol => true,
                IParameterSymbol => true,
                IFieldSymbol => true,
                IPropertySymbol => true,
                IEventSymbol => true,
                IMethodSymbol { IsConstructor: false } => true,
                ITypeSymbol when allowTypes => true,
                _ => false
            };

        ITypeSymbol? TryGetLiteralCompletionTargetType(SyntaxToken currentToken)
        {
            if (currentToken.GetAncestor<EqualsValueClauseSyntax>() is { } equalsClause)
            {
                if (position < equalsClause.EqualsToken.Span.End)
                    return null;

                return equalsClause.Parent switch
                {
                    VariableDeclaratorSyntax declarator => declarator.TypeAnnotation?.Type is ExpressionSyntax declaratorType
                        ? model.GetTypeInfo(declaratorType).Type ?? (TryGetPreferredSymbolInfo(declaratorType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as ITypeSymbol
                            : null)
                        : model.GetDeclaredSymbol(declarator) switch
                        {
                            ILocalSymbol local => local.Type,
                            IFieldSymbol field => field.Type,
                            _ => null
                        },
                    PropertyDeclarationSyntax property => property.Type.Type is ExpressionSyntax propertyType
                        ? model.GetTypeInfo(propertyType).Type ?? (TryGetPreferredSymbolInfo(propertyType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as ITypeSymbol
                            : null)
                        : (model.GetDeclaredSymbol(property) as IPropertySymbol)?.Type,
                    EventDeclarationSyntax @event => @event.Type.Type is ExpressionSyntax eventType
                        ? model.GetTypeInfo(eventType).Type ?? (TryGetPreferredSymbolInfo(eventType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as ITypeSymbol
                            : null)
                        : (model.GetDeclaredSymbol(@event) as IEventSymbol)?.Type,
                    IndexerDeclarationSyntax indexer => indexer.Type.Type is ExpressionSyntax indexerType
                        ? model.GetTypeInfo(indexerType).Type ?? (TryGetPreferredSymbolInfo(indexerType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as ITypeSymbol
                            : null)
                        : (model.GetDeclaredSymbol(indexer) as IPropertySymbol)?.Type,
                    ParameterSyntax parameter => parameter.TypeAnnotation?.Type is ExpressionSyntax parameterType
                        ? model.GetTypeInfo(parameterType).Type ?? (TryGetPreferredSymbolInfo(parameterType, out var symbolInfo)
                            ? symbolInfo.Symbol?.UnderlyingSymbol as ITypeSymbol
                            : null)
                        : (model.GetDeclaredSymbol(parameter) as IParameterSymbol)?.Type,
                    EnumMemberDeclarationSyntax enumMember => (model.GetDeclaredSymbol(enumMember) as IFieldSymbol)?.Type,
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
                        : null;
                    return TryGetExplicitlyAnnotatedType(leftSymbol)
                        ?? GetTypeFromSymbol(leftSymbol)
                        ?? model.GetTypeInfo(left).Type;
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
                        : null;
                    return TryGetExplicitlyAnnotatedType(leftSymbol)
                        ?? GetTypeFromSymbol(leftSymbol)
                        ?? model.GetTypeInfo(left).Type;
                }
                return null;
            }

            return null;
        }

        ITypeSymbol? TryGetTargetTypedMemberCompletionType(MemberBindingExpressionSyntax memberBinding)
        {
            var targetType = TryGetLiteralCompletionTargetType(memberBinding.Name.Identifier);
            if (targetType is not null)
                return targetType;

            return TryGetAttributeArgumentTargetType(memberBinding);
        }

        ITypeSymbol? TryGetAttributeArgumentTargetType(MemberBindingExpressionSyntax memberBinding)
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

        INamedTypeSymbol? TryResolveAttributeType(AttributeSyntax attribute)
        {
            if (attribute.Name is NameSyntax name &&
                TryResolveNamespaceOrType(name) is INamedTypeSymbol namedType &&
                IsAttributeType(namedType))
            {
                return namedType;
            }

            var attributeName = attribute.Name.ToString();
            if (!attributeName.EndsWith("Attribute", StringComparison.Ordinal))
                attributeName += "Attribute";

            return binder.LookupType(attributeName) as INamedTypeSymbol
                ?? model.Compilation.GetTypeByMetadataName(attributeName) as INamedTypeSymbol;
        }

        static bool IsAttributeType(INamedTypeSymbol type)
            => type.Name.EndsWith("Attribute", StringComparison.Ordinal) ||
               type.BaseType?.ToFullyQualifiedMetadataName() == "System.Attribute";

        static ITypeSymbol? TryGetAttributeConstructorParameterType(
            INamedTypeSymbol attributeType,
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

            ITypeSymbol? targetType = null;
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

        static bool HaveSameTypeIdentity(ITypeSymbol left, ITypeSymbol right)
        {
            if (SymbolEqualityComparer.Default.Equals(left, right))
                return true;

            if (left is INamedTypeSymbol leftNamed &&
                right is INamedTypeSymbol rightNamed)
            {
                return string.Equals(
                    leftNamed.ToFullyQualifiedMetadataName(),
                    rightNamed.ToFullyQualifiedMetadataName(),
                    StringComparison.Ordinal);
            }

            return false;
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

        static SyntaxNode? TryGetDeclaredTypeNodeFromSymbol(ISymbol? symbol)
            => symbol?.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() switch
            {
                VariableDeclaratorSyntax { TypeAnnotation: { } annotation } => annotation.Type,
                ParameterSyntax { TypeAnnotation: { } annotation } => annotation.Type,
                PropertyDeclarationSyntax { Type: { Type: { } propertyType } } => propertyType,
                EventDeclarationSyntax { Type: { Type: { } eventType } } => eventType,
                IndexerDeclarationSyntax { Type: { Type: { } indexerType } } => indexerType,
                _ => null
            };

        bool TryGetDeclaredLiteralCandidatesFromSymbol(ISymbol? symbol, out ImmutableArray<string> candidates)
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

        ISymbol? TryGetAssignmentTargetSymbol(SyntaxToken currentToken)
        {
            if (currentToken.GetAncestor<AssignmentExpressionSyntax>() is { } assignmentExpr)
            {
                if (position < assignmentExpr.OperatorToken.Span.End)
                    return null;

                return assignmentExpr.Left is ExpressionSyntax leftExpr
                    ? TryGetPreferredSymbolInfo(leftExpr, out var symbolInfo)
                        ? symbolInfo.Symbol?.UnderlyingSymbol
                        : null
                    : null;
            }

            if (currentToken.GetAncestor<AssignmentStatementSyntax>() is { } assignmentStmt)
            {
                if (position < assignmentStmt.OperatorToken.Span.End)
                    return null;

                return assignmentStmt.Left is ExpressionSyntax leftExpr
                    ? TryGetPreferredSymbolInfo(leftExpr, out var symbolInfo)
                        ? symbolInfo.Symbol?.UnderlyingSymbol
                        : null
                    : null;
            }

            return null;
        }

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

                if (completions.Count > 0)
                    return completions;
            }
        }

        var targetTypedMemberBinding = token.GetAncestor<MemberBindingExpressionSyntax>();
        if (targetTypedMemberBinding is not null &&
            targetTypedMemberBinding.Parent is not ConditionalAccessExpressionSyntax &&
            position >= targetTypedMemberBinding.OperatorToken.Span.End &&
            TryGetTargetTypedMemberCompletionType(targetTypedMemberBinding) is INamedTypeSymbol { TypeKind: TypeKind.Enum } targetEnum)
        {
            var hasNameAtCaret = !targetTypedMemberBinding.Name.Identifier.IsMissing &&
                position > targetTypedMemberBinding.Name.Identifier.Span.Start;
            var prefix = hasNameAtCaret
                ? targetTypedMemberBinding.Name.Identifier.ValueText
                : string.Empty;
            var nameSpan = hasNameAtCaret
                ? targetTypedMemberBinding.Name.Identifier.Span
                : new TextSpan(position, 0);

            foreach (var field in targetEnum.GetMembers()
                .OfType<IFieldSymbol>()
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
                    if (symbol is INamespaceOrTypeSymbol nsOrType)
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
                    if (symbol is INamespaceOrTypeSymbol nsOrType &&
                        NameMatchesPrefix(nsOrType.Name, tokenValueText))
                    {
                        var (displayText, insertText, dedupKey) = CreateCompletionParts(nsOrType);
                        var cursorOffset = nsOrType is ITypeSymbol ? insertText.Length : (int?)null;

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
                    if (symbol is INamespaceOrTypeSymbol nsOrType)
                    {
                        var prefix = nameToken.ValueText;
                        var nameSpan = nameToken.Span;
                        var aliasMembers = nsOrType is INamespaceSymbol aliasNamespace
                            ? GetNamespaceCompletionMembers(aliasNamespace)
                            : nsOrType.GetMembers().Where(IsAccessible);
                        foreach (var member in aliasMembers
                            .Where(m => NameMatchesPrefix(m.Name, prefix)))
                        {
                            var (displayText, insertText, dedupKey) = CreateCompletionParts(member);
                            var cursorOffset = member is ITypeSymbol ? insertText.Length : GetDefaultCursorOffset(member, insertText);

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
                if (symbol is INamedTypeSymbol type &&
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
                if (symbol is not ILabelSymbol label)
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

        // Conditional member access: value?.Member
        var memberBinding = token.GetAncestor<MemberBindingExpressionSyntax>();
        if (memberBinding is not null &&
            memberBinding.Parent is ConditionalAccessExpressionSyntax conditionalAccess)
        {
            var operatorToken = memberBinding.OperatorToken;

            if (position >= operatorToken.Span.End)
            {
                var (symbol, type) = ResolveReceiver(conditionalAccess.Expression);
                IEnumerable<ISymbol>? members = null;
                ITypeSymbol? instanceTypeForExtensions = null;

                if (NeedsReceiverFallback(symbol, type) && conditionalAccess.Expression is IdentifierNameSyntax receiverIdentifier)
                {
                    symbol = TryResolveReceiverSymbol(receiverIdentifier);
                    type = GetTypeFromSymbol(symbol) ?? type;
                }

                // For invocation expressions, prefer the invoked method return type.
                // This avoids leaking argument/literal types into completion and
                // correctly suppresses member access on unit/void returns.
                if (conditionalAccess.Expression is InvocationExpressionSyntax &&
                    symbol is IMethodSymbol invokedMethod)
                {
                    type = invokedMethod.ReturnType;
                    if (type.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
                        return completions;
                }

                if (symbol is INamespaceSymbol ns)
                {
                    members = GetNamespaceCompletionMembers(ns);
                }
                else if (conditionalAccess.Expression is SelfExpressionSyntax && GetSelfType() is { } currentSelfType)
                {
                    var completionType = GetCarrierConditionalAccessLookupType(currentSelfType) ?? currentSelfType.GetPlainType();
                    members = GetTypeMembersIncludingBase(completionType, includeStatic: false)
                        .Where(member => IsAccessibleOnSelf(member, completionType));
                    instanceTypeForExtensions = completionType switch
                    {
                        INamedTypeSymbol named => named,
                        IArrayTypeSymbol array => array,
                        LiteralTypeSymbol literal => literal.UnderlyingType as ITypeSymbol,
                        _ => null
                    };
                }
                else if (TryGetTypeAccessSymbol(symbol, type) is { } typeAccessSymbol)
                {
                    members = GetTypeAccessMembers(typeAccessSymbol);
                }
                else if (type is ITypeSymbol instanceType)
                {
                    var completionType = GetCarrierConditionalAccessLookupType(instanceType) ?? instanceType.GetPlainType();
                    members = GetTypeMembersIncludingBase(completionType, includeStatic: false).Where(IsAccessible);
                    instanceTypeForExtensions = completionType switch
                    {
                        INamedTypeSymbol named => named,
                        IArrayTypeSymbol array => array,
                        LiteralTypeSymbol literal => literal.UnderlyingType as ITypeSymbol,
                        _ => null
                    };
                }
                if (members is not null)
                {
                    var hasNameAtCaret = !memberBinding.Name.Identifier.IsMissing &&
                        position > memberBinding.Name.Identifier.Span.Start;
                    var prefix = hasNameAtCaret
                        ? memberBinding.Name.Identifier.ValueText
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
        if (memberAccess is not null)
        {
            var dotToken = memberAccess.OperatorToken;

            if (position >= dotToken.Span.End)
            {
                ISymbol? symbol = null;
                ITypeSymbol? type = null;
                IEnumerable<ISymbol>? members = null;
                ITypeSymbol? instanceTypeForExtensions = null;

                var isRecoveredImportCompletion = IsImportLineCompletionContext(memberAccess);
                if (isRecoveredImportCompletion)
                {
                    if (memberAccess.Expression is NameSyntax importReceiver &&
                        TryResolveImportNamespaceOrType(importReceiver) is { } importReceiverSymbol)
                    {
                        var hasNameAtCaret = !memberAccess.Name.Identifier.IsMissing &&
                            position > memberAccess.Name.Identifier.Span.Start;
                        var prefix = hasNameAtCaret
                            ? memberAccess.Name.Identifier.ValueText
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
                    type = namespaceOrTypeReceiver as ITypeSymbol;
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
                    symbol is IMethodSymbol invokedMethod)
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

                if (symbol is INamespaceSymbol ns)
                {
                    // Namespace or namespace alias: list its public members
                    members = GetNamespaceCompletionMembers(ns);
                }
                else if (memberAccess.Expression is SelfExpressionSyntax && GetSelfType() is { } currentSelfType)
                {
                    var completionType = currentSelfType.GetPlainType();
                    members = GetTypeMembersIncludingBase(completionType, includeStatic: false)
                        .Where(member => IsAccessibleOnSelf(member, completionType));
                    instanceTypeForExtensions = completionType switch
                    {
                        INamedTypeSymbol named => named,
                        IArrayTypeSymbol array => array,
                        LiteralTypeSymbol literal => literal.UnderlyingType as ITypeSymbol,
                        _ => null
                    };
                }
                else if (TryGetTypeAccessSymbol(symbol, type) is { } typeAccessSymbol)
                {
                    // Accessing a type-like receiver (named type or constrained type parameter):
                    // show static members.
                    members = GetTypeAccessMembers(typeAccessSymbol);
                }
                else if (type is ITypeSymbol instanceType)
                {
                    // Accessing an instance: show instance members
                    members = GetTypeMembersIncludingBase(instanceType, includeStatic: false).Where(IsAccessible);
                    instanceTypeForExtensions = instanceType switch
                    {
                        INamedTypeSymbol named => named,
                        IArrayTypeSymbol array => array,
                        LiteralTypeSymbol literal => literal.UnderlyingType as ITypeSymbol,
                        _ => null
                    };
                }
                if (members is not null)
                {
                    var hasNameAtCaret = !memberAccess.Name.Identifier.IsMissing &&
                        position > memberAccess.Name.Identifier.Span.Start;
                    var prefix = hasNameAtCaret
                        ? memberAccess.Name.Identifier.ValueText
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
        if (qualifiedName is not null && qualifiedName.Right is SimpleNameSyntax simple)
        {
            var isImportLineCompletion = IsImportLineCompletionContext(qualifiedName);
            var symbol = (ISymbol?)(isImportLineCompletion
                ? TryResolveImportNamespaceOrType(qualifiedName.Left)
                : TryResolveNamespaceOrType(qualifiedName.Left));
            var prefix = simple.Identifier.ValueText;
            var nameSpan = isImportLineCompletion
                ? GetImportCompletionReplacementSpan(simple.Identifier, position)
                : simple.Identifier.Span;

            if (symbol is INamespaceOrTypeSymbol nsOrType)
            {
                if (isImportLineCompletion)
                {
                    AddImportDirectiveMemberCompletions(nsOrType, prefix, nameSpan);
                    return completions;
                }

                var qualifiedMembers = nsOrType is INamespaceSymbol qualifiedNamespace
                    ? GetNamespaceCompletionMembers(qualifiedNamespace)
                    : nsOrType.GetMembers().Where(IsAccessible);
                foreach (var member in qualifiedMembers
                    .Where(m => string.IsNullOrEmpty(prefix)
                    || NameMatchesPrefix(m.Name, prefix)))
                {
                    var (displayText, insertText, dedupKey) = CreateCompletionParts(member);
                    var cursorOffset = member is ITypeSymbol ? insertText.Length : GetDefaultCursorOffset(member, insertText);

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
            if (receiverType is ITypeSymbol typeSymbolForQualified)
            {
                var members = GetTypeMembersIncludingBase(typeSymbolForQualified, includeStatic: false)
                    .Where(IsAccessible);

                foreach (var member in members.Where(m => NameMatchesPrefix(m.Name, prefix)))
                {
                    if (member is IMethodSymbol method &&
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

                if (receiverType is ITypeSymbol { TypeKind: not TypeKind.Error } typedReceiver)
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
        var offerValueCompletions = token.Parent is IdentifierNameSyntax { Parent: BlockStatementSyntax or ExpressionStatementSyntax }
            || token.IsKind(SyntaxKind.IdentifierToken)
            || token.IsKind(SyntaxKind.EndOfFileToken)
            || isArgumentStartToken;

        if (offerValueCompletions)
        {
            var valuePrefix = isArgumentStartToken ? string.Empty : tokenValueText;
            var prefixWithoutEscape = valuePrefix.TrimStart('@');
            var allowTypesInValuePosition = prefixWithoutEscape.Length > 0 && char.IsUpper(prefixWithoutEscape[0]);
            var contextNode = token.Parent ?? model.SyntaxTree.GetRoot();

            foreach (var symbol in model.GetVisibleValueSymbols(contextNode))
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

                if (symbol is IMethodSymbol method && IsSuppressedCompletionMethod(method))
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
        var shouldOfferKeywords =
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

    private static bool TryGetMacroCompletionContext(
        SyntaxToken token,
        SourceText sourceText,
        int position,
        out MacroCompletionContext context)
    {
        context = default;

        var attribute = token.Parent?.AncestorsAndSelf()
            .OfType<AttributeSyntax>()
            .FirstOrDefault(static candidate => candidate.IsMacroAttribute());
        if (attribute is not null &&
            position >= attribute.HashToken.Span.End &&
            (attribute.ArgumentList is null || position <= attribute.ArgumentList.Span.Start))
        {
            CreateMacroCompletionContext(attribute.Name, MacroKind.AttachedDeclaration, sourceText, position, out context);
            return true;
        }

        var freestandingMacro = token.Parent?.AncestorsAndSelf().OfType<FreestandingMacroExpressionSyntax>().FirstOrDefault();
        if (freestandingMacro is not null &&
            position >= freestandingMacro.HashToken.Span.End &&
            position <= freestandingMacro.ArgumentList.OpenParenToken.SpanStart)
        {
            CreateMacroCompletionContext(freestandingMacro.Name, MacroKind.FreestandingExpression, sourceText, position, out context);
            return true;
        }

        return false;
    }

    private static void CreateMacroCompletionContext(
        SyntaxNode nameNode,
        MacroKind kind,
        SourceText sourceText,
        int position,
        out MacroCompletionContext context)
    {
        var replacementSpan = nameNode.Span.Length > 0
            ? nameNode.Span
            : new TextSpan(position, 0);
        var prefixLength = Math.Clamp(position - replacementSpan.Start, 0, replacementSpan.Length);
        var prefix = prefixLength > 0
            ? sourceText.ToString(new TextSpan(replacementSpan.Start, prefixLength))
            : string.Empty;

        context = new MacroCompletionContext(kind, prefix, replacementSpan);
    }

    private static IEnumerable<CompletionItem> GetMacroCompletions(
        Compilation compilation,
        MacroCompletionContext context)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var macro in EnumerateMacros(compilation, context.Kind))
        {
            if (!seen.Add(macro.Name) || !MacroNameMatchesPrefix(macro.Name, context.Prefix))
                continue;

            var insertionText = context.Kind == MacroKind.FreestandingExpression
                ? macro.Name + "()"
                : macro.Name;
            var cursorOffset = context.Kind == MacroKind.FreestandingExpression && macro.AcceptsArguments
                ? insertionText.Length - 1
                : (int?)null;

            yield return new CompletionItem(
                DisplayText: macro.Name,
                InsertionText: insertionText,
                ReplacementSpan: context.ReplacementSpan,
                CursorOffset: cursorOffset,
                Description: CreateMacroDescription(macro));
        }
    }

    private static IEnumerable<IMacroDefinition> EnumerateMacros(Compilation compilation, MacroKind kind)
    {
        foreach (var macroReference in compilation.MacroReferences)
        {
            IEnumerable<IRavenMacroPlugin> plugins;
            try
            {
                plugins = macroReference.GetPlugins().ToArray();
            }
            catch
            {
                continue;
            }

            foreach (var plugin in plugins)
            {
                ImmutableArray<IMacroDefinition> macros;
                try
                {
                    macros = plugin.GetMacros();
                }
                catch
                {
                    continue;
                }

                foreach (var macro in macros)
                {
                    if (macro.Kind == kind)
                        yield return macro;
                }
            }
        }
    }

    private static string CreateMacroDescription(IMacroDefinition macro)
    {
        var kindDisplay = macro.Kind switch
        {
            MacroKind.AttachedDeclaration => "attached declaration macro",
            MacroKind.FreestandingExpression => "freestanding expression macro",
            _ => "macro"
        };
        var targetsDisplay = macro.Targets == MacroTarget.None
            ? null
            : $"targets: {FormatMacroTargets(macro.Targets)}";
        var argumentsDisplay = macro.AcceptsArguments
            ? "accepts arguments"
            : "no arguments";

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
