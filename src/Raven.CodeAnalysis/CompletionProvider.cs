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
                return symbol.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat);
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
                    if (!method.IsStatic)
                        return method.ContainingType;

                    return null;
                }
            }

            return null;
        }

        ISymbol? TryLookupValueSymbolByName(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                return null;

            var candidates = binder.LookupAvailableSymbols()
                .Where(symbol => string.Equals(symbol.Name, name, StringComparison.Ordinal))
                .ToArray();

            return candidates.FirstOrDefault(static symbol =>
                       symbol is ILocalSymbol or IParameterSymbol or IFieldSymbol or IPropertySymbol or IEventSymbol)
                   ?? candidates.FirstOrDefault();
        }

        INamespaceOrTypeSymbol? TryResolveNamespaceOrType(NameSyntax name)
        {
            var resolved = model.GetSymbolInfo(name).Symbol?.UnderlyingSymbol as INamespaceOrTypeSymbol;
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

        INamespaceOrTypeSymbol? TryResolveIdentifierNamespaceOrType(IdentifierNameSyntax identifier)
        {
            var name = identifier.Identifier.ValueText;
            if (string.IsNullOrWhiteSpace(name))
                return null;

            return (INamespaceOrTypeSymbol?)binder.LookupType(name)
                ?? binder.LookupNamespace(name)
                ?? (binder.LookupSymbol(name)?.UnderlyingSymbol as INamespaceOrTypeSymbol);
        }

        INamespaceOrTypeSymbol? TryResolveQualifiedNamespaceOrType(QualifiedNameSyntax qualified)
        {
            var left = TryResolveNamespaceOrType(qualified.Left);
            if (left is null)
                return null;

            return qualified.Right switch
            {
                IdentifierNameSyntax identifier => LookupQualifiedMember(left, identifier.Identifier.ValueText, arity: 0),
                GenericNameSyntax generic => LookupQualifiedMember(left, generic.Identifier.ValueText, generic.TypeArgumentList.Arguments.Count),
                _ => null
            };
        }

        INamespaceOrTypeSymbol? TryResolveGenericNamespaceOrType(GenericNameSyntax generic)
        {
            var arity = generic.TypeArgumentList.Arguments.Count;
            var name = generic.Identifier.ValueText;

            var namedType = binder.LookupSymbols(name)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault(candidate => candidate.Arity == arity);

            return namedType
                ?? (binder.LookupSymbol(name)?.UnderlyingSymbol as INamespaceOrTypeSymbol);
        }

        static INamespaceOrTypeSymbol? LookupQualifiedMember(INamespaceOrTypeSymbol? container, string name, int arity)
        {
            if (container is null || string.IsNullOrWhiteSpace(name))
                return null;

            if (container is INamespaceSymbol namespaceSymbol)
            {
                return (INamespaceOrTypeSymbol?)namespaceSymbol.LookupNamespace(name)
                    ?? SelectTypeMember(namespaceSymbol.GetMembers(name).OfType<INamedTypeSymbol>(), arity)
                    ?? (INamespaceOrTypeSymbol?)namespaceSymbol.LookupType(name);
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
            if (receiverExpression is not IdentifierNameSyntax receiverIdentifier)
                return null;

            var name = receiverIdentifier.Identifier.ValueText;
            if (string.IsNullOrWhiteSpace(name))
                return null;

            var symbol = binder.LookupSymbol(name)
                ?? TryLookupValueSymbolByName(name);
            if (symbol is not null)
                return symbol;

            var receiverBinder = model.GetBinder(receiverExpression);
            symbol = receiverBinder.LookupSymbol(name)
                ?? receiverBinder.LookupSymbols(name).FirstOrDefault();
            if (symbol is not null)
                return symbol;

            if (TryResolveEnclosingPatternOrLoopLocalSymbol(receiverExpression, name) is { } enclosingSymbol)
                return enclosingSymbol;

            var containingBlock = receiverExpression.GetAncestor<BlockStatementSyntax>();
            if (containingBlock is null)
                return null;

            var localDeclarator = containingBlock
                .DescendantNodes()
                .OfType<VariableDeclaratorSyntax>()
                .Where(declarator =>
                    declarator.Identifier.ValueText == name &&
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
            if (forStatement.Target is IdentifierNameSyntax target &&
                target.Identifier.ValueText == name)
            {
                var iterationType = TryGetForIterationElementType(forStatement.Expression);
                return iterationType is not null
                    ? CreateSyntheticReceiverLocalSymbol(name, iterationType)
                    : null;
            }

            if (forStatement.Target is PatternSyntax pattern)
                return TryResolvePatternDesignationSymbol(pattern, receiverExpression, name);

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

        ILocalSymbol CreateSyntheticReceiverLocalSymbol(string name, ITypeSymbol type)
        {
            var containingSymbol = binder.ContainingSymbol;
            return new SourceLocalSymbol(
                name,
                type,
                isMutable: false,
                containingSymbol,
                containingSymbol.ContainingType,
                containingSymbol.ContainingNamespace,
                locations: [],
                declaringSyntaxReferences: []);
        }

        ITypeSymbol? TryGetForIterationElementType(ExpressionSyntax expression)
        {
            var collectionType = model.GetTypeInfo(expression).Type;
            if (collectionType is null || collectionType.TypeKind == TypeKind.Error)
                return null;

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

            static IEnumerable<INamedTypeSymbol> EnumerateSelfAndInterfaces(INamedTypeSymbol type)
            {
                yield return type;
                foreach (var iface in type.AllInterfaces)
                    yield return iface;
            }
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
                IPropertySymbol => insertionText.Length - 1,
                ITypeSymbol => insertionText.Length - 1,
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
            => symbol switch
            {
                ILocalSymbol local => local.Type,
                IFieldSymbol field => field.Type,
                IPropertySymbol property => property.Type,
                IEventSymbol @event => @event.Type,
                IParameterSymbol parameter => parameter.Type,
                _ => null
            };

        static bool NeedsReceiverFallback(ISymbol? symbol, ITypeSymbol? type)
        {
            if (symbol is null)
                return true;

            if (symbol.Kind is SymbolKind.Error or SymbolKind.ErrorType)
                return true;

            return type is { TypeKind: TypeKind.Error };
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
                    ?? model.GetSymbolInfo(typeSyntax).Symbol?.UnderlyingSymbol as ITypeSymbol;
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
                        ? model.GetTypeInfo(declaratorType).Type ?? (model.GetSymbolInfo(declaratorType).Symbol?.UnderlyingSymbol as ITypeSymbol)
                        : model.GetDeclaredSymbol(declarator) switch
                        {
                            ILocalSymbol local => local.Type,
                            IFieldSymbol field => field.Type,
                            _ => null
                        },
                    PropertyDeclarationSyntax property => property.Type.Type is ExpressionSyntax propertyType
                        ? model.GetTypeInfo(propertyType).Type ?? (model.GetSymbolInfo(propertyType).Symbol?.UnderlyingSymbol as ITypeSymbol)
                        : (model.GetDeclaredSymbol(property) as IPropertySymbol)?.Type,
                    EventDeclarationSyntax @event => @event.Type.Type is ExpressionSyntax eventType
                        ? model.GetTypeInfo(eventType).Type ?? (model.GetSymbolInfo(eventType).Symbol?.UnderlyingSymbol as ITypeSymbol)
                        : (model.GetDeclaredSymbol(@event) as IEventSymbol)?.Type,
                    IndexerDeclarationSyntax indexer => indexer.Type.Type is ExpressionSyntax indexerType
                        ? model.GetTypeInfo(indexerType).Type ?? (model.GetSymbolInfo(indexerType).Symbol?.UnderlyingSymbol as ITypeSymbol)
                        : (model.GetDeclaredSymbol(indexer) as IPropertySymbol)?.Type,
                    ParameterSyntax parameter => parameter.TypeAnnotation?.Type is ExpressionSyntax parameterType
                        ? model.GetTypeInfo(parameterType).Type ?? (model.GetSymbolInfo(parameterType).Symbol?.UnderlyingSymbol as ITypeSymbol)
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
                    var leftSymbol = model.GetSymbolInfo(left).Symbol?.UnderlyingSymbol;
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
                    var leftSymbol = model.GetSymbolInfo(left).Symbol?.UnderlyingSymbol;
                    return TryGetExplicitlyAnnotatedType(leftSymbol)
                        ?? GetTypeFromSymbol(leftSymbol)
                        ?? model.GetTypeInfo(left).Type;
                }
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
                    ? model.GetSymbolInfo(leftExpr).Symbol?.UnderlyingSymbol
                    : null;
            }

            if (currentToken.GetAncestor<AssignmentStatementSyntax>() is { } assignmentStmt)
            {
                if (position < assignmentStmt.OperatorToken.Span.End)
                    return null;

                return assignmentStmt.Left is ExpressionSyntax leftExpr
                    ? model.GetSymbolInfo(leftExpr).Symbol?.UnderlyingSymbol
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

        var importDirective = token.GetAncestor<ImportDirectiveSyntax>();
        if (importDirective is not null)
        {
            var qualified = token.GetAncestor<QualifiedNameSyntax>();
            if (qualified is not null && qualified.Right is SimpleNameSyntax importSimple)
            {
                var nameToken = importSimple.Identifier;
                if (position >= nameToken.Position)
                {
                    var symbol = TryResolveNamespaceOrType(qualified.Left);
                    if (symbol is INamespaceOrTypeSymbol nsOrType)
                    {
                        var prefix = nameToken.ValueText;
                        var nameSpan = nameToken.Span;
                        foreach (var member in nsOrType.GetMembers()
                            .OfType<INamespaceOrTypeSymbol>()
                            .Where(m => NameMatchesPrefix(m.Name, prefix)))
                        {
                            var (displayText, insertText, dedupKey) = CreateCompletionParts(member);
                            var cursorOffset = member is ITypeSymbol ? insertText.Length : (int?)null;

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
                    var symbol = TryResolveNamespaceOrType(qualified.Left);
                    if (symbol is INamespaceOrTypeSymbol nsOrType)
                    {
                        var prefix = nameToken.ValueText;
                        var nameSpan = nameToken.Span;
                        foreach (var member in nsOrType.GetMembers()
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
                var symbolInfo = model.GetSymbolInfo(conditionalAccess.Expression);
                var symbol = symbolInfo.Symbol?.UnderlyingSymbol;
                var typeInfo = model.GetTypeInfo(conditionalAccess.Expression).Type;
                var type = typeInfo?.UnderlyingSymbol as ITypeSymbol;
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
                    members = ns.GetMembers().Where(IsAccessible);
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
                else if (conditionalAccess.Expression is SelfExpressionSyntax && GetSelfType() is { } currentSelfType)
                {
                    members = currentSelfType.GetMembers().Where(m => !m.IsStatic && IsAccessible(m));
                    instanceTypeForExtensions = currentSelfType;
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
                    {
                        if (member is IMethodSymbol method && IsSuppressedCompletionMethod(method))
                            continue;

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

                    if (instanceTypeForExtensions is not null)
                    {
                        var extensionMembers = ExtensionMemberLookup.Lookup(
                            binder,
                            instanceTypeForExtensions,
                            includePartialMatches: false,
                            kinds: ExtensionMemberKinds.InstanceMethods);

                        foreach (var method in extensionMembers.InstanceMethods)
                        {
                            if (!IsAccessible(method) ||
                                IsSuppressedCompletionMethod(method) ||
                                !ShouldIncludeExtensionMethod(method, prefix))
                                continue;

                            if (!NameMatchesPrefix(method.Name, prefix))
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
                var symbolInfo = model.GetSymbolInfo(memberAccess.Expression);
                var symbol = symbolInfo.Symbol?.UnderlyingSymbol;
                var typeInfo = model.GetTypeInfo(memberAccess.Expression).Type;
                var type = typeInfo?.UnderlyingSymbol as ITypeSymbol;
                IEnumerable<ISymbol>? members = null;
                ITypeSymbol? instanceTypeForExtensions = null;

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

                if (symbol is INamespaceSymbol ns)
                {
                    // Namespace or namespace alias: list its public members
                    members = ns.GetMembers().Where(IsAccessible);
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
                else if (memberAccess.Expression is SelfExpressionSyntax && GetSelfType() is { } currentSelfType)
                {
                    members = currentSelfType.GetMembers().Where(m => !m.IsStatic && IsAccessible(m));
                    instanceTypeForExtensions = currentSelfType;
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
                    {
                        if (member is IMethodSymbol method && IsSuppressedCompletionMethod(method))
                            continue;

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

                    if (instanceTypeForExtensions is not null)
                    {
                        var extensionMembers = ExtensionMemberLookup.Lookup(
                            binder,
                            instanceTypeForExtensions,
                            includePartialMatches: false,
                            kinds: ExtensionMemberKinds.InstanceMethods);

                        foreach (var method in extensionMembers.InstanceMethods)
                        {
                            if (!IsAccessible(method) ||
                                IsSuppressedCompletionMethod(method) ||
                                !ShouldIncludeExtensionMethod(method, prefix))
                                continue;

                            if (!NameMatchesPrefix(method.Name, prefix))
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
            var symbol = (ISymbol?)TryResolveNamespaceOrType(qualifiedName.Left);
            var prefix = simple.Identifier.ValueText;
            var nameSpan = simple.Identifier.Span;

            if (symbol is INamespaceOrTypeSymbol nsOrType)
            {
                foreach (var member in nsOrType.GetMembers()
                    .Where(m => string.IsNullOrEmpty(prefix)
                    || NameMatchesPrefix(m.Name, prefix))
                    .Where(IsAccessible))
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
