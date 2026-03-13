using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static class SymbolResolver
{
    public static SymbolResolutionResult? ResolveSymbolAtPosition(SemanticModel semanticModel, SyntaxNode root, int offset)
    {
        foreach (var candidate in GetCandidateNodes(root, offset))
        {
            if (TryResolveMemberTokenFastPath(semanticModel, candidate.Token, out var memberTokenSymbol))
                return new SymbolResolutionResult(
                    memberTokenSymbol.UnderlyingSymbol,
                    candidate.Token.Parent ?? candidate.Node);

            if (TryResolveParameterDeclarationTokenFastPath(semanticModel, candidate.Token, out var parameterSymbol, out var parameterNode))
            {
                return new SymbolResolutionResult(
                    parameterSymbol.UnderlyingSymbol,
                    parameterNode);
            }

            if (ShouldSkipCandidateNode(candidate.Node, candidate.Token))
                continue;

            var symbol = ResolveSymbolFromNode(semanticModel, candidate.Node, candidate.Token);
            if (symbol is not null)
                return new SymbolResolutionResult(symbol.UnderlyingSymbol, candidate.Token.Parent ?? candidate.Node);
        }

        return null;
    }

    private static bool TryResolveMemberTokenFastPath(
        SemanticModel semanticModel,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        if (token.Parent is not IdentifierNameSyntax identifier ||
            identifier.Parent is not MemberAccessExpressionSyntax memberAccess ||
            !ReferenceEquals(memberAccess.Name, identifier))
        {
            return false;
        }

        if (!TryResolveMemberFromReceiverType(semanticModel, memberAccess, out var resolved))
            return false;

        symbol = ProjectSymbolForDisplay(resolved);
        return symbol is not null;
    }

    private static IEnumerable<CandidateNode> GetCandidateNodes(SyntaxNode root, int offset)
    {
        TextSpan? primaryTokenSpan = null;
        var primaryTokenKind = SyntaxKind.None;

        foreach (var normalizedOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            SyntaxToken token;
            try
            {
                token = root.FindToken(normalizedOffset);
            }
            catch
            {
                continue;
            }

            if (primaryTokenSpan is null)
            {
                primaryTokenSpan = token.Span;
                primaryTokenKind = token.Kind;
            }
            else if (primaryTokenSpan.Value.Contains(offset) &&
                     IsStickyPrimaryTokenKind(primaryTokenKind))
            {
                // Original offset already maps to a concrete token; don't let fallback
                // offsets (for boundary handling) hijack resolution to adjacent symbols.
                continue;
            }

            var current = token.Parent;
            while (current is not null)
            {
                yield return new CandidateNode(current, token);
                current = current.Parent;
            }
        }
    }

    private static IEnumerable<int> NormalizeOffsets(int offset, int maxOffset)
    {
        if (maxOffset < 0)
            yield break;

        var clamped = Math.Clamp(offset, 0, maxOffset);
        yield return clamped;

        if (clamped > 0)
            yield return clamped - 1;
    }

    private static bool IsStickyPrimaryTokenKind(SyntaxKind kind)
        => kind is SyntaxKind.IdentifierToken
            or SyntaxKind.NumericLiteralToken
            or SyntaxKind.StringLiteralToken
            or SyntaxKind.CharacterLiteralToken
            or SyntaxKind.TrueKeyword
            or SyntaxKind.FalseKeyword
            or SyntaxKind.NullKeyword;

    private static ISymbol? ResolveSymbolFromNode(SemanticModel semanticModel, SyntaxNode node, SyntaxToken token)
    {
        if (TryResolveCompoundEventAssignmentSymbol(semanticModel, node, token, out var compoundEventSymbol))
            return compoundEventSymbol;

        if (TryResolveInvocationTargetSymbol(semanticModel, node, token, out var invocationSymbol))
            return invocationSymbol;

        if (TryResolvePipeRightHandSymbol(semanticModel, node, token, out var pipeRhsSymbol))
            return pipeRhsSymbol;

        if (TryResolveRecordPatternCaseSymbol(semanticModel, node, token, out var recordPatternCaseSymbol))
            return recordPatternCaseSymbol;

        if (TryResolveParameterDeclarationSymbol(semanticModel, node, token, out var parameterDeclarationSymbol))
            return parameterDeclarationSymbol;

        if (node is FunctionStatementSyntax functionStatement &&
            IsFunctionDeclarationToken(functionStatement, token))
        {
            return semanticModel.GetDeclaredSymbol(functionStatement);
        }

        if (node is FunctionExpressionSyntax lambdaExpression &&
            IsFunctionExpressionDeclarationToken(lambdaExpression, token) &&
            !IsInsideFunctionExpressionBody(lambdaExpression, token))
        {
            var lambdaSymbolInfo = semanticModel.GetSymbolInfo(lambdaExpression);
            var lambdaSymbol = ChoosePreferredSymbol(lambdaSymbolInfo.Symbol, lambdaSymbolInfo.CandidateSymbols, lambdaExpression);
            if (IsFunctionExpressionIdentifierToken(lambdaExpression, token) && lambdaSymbol is not null)
                return ProjectSymbolForDisplay(lambdaSymbol);

            var lambdaTypeInfo = semanticModel.GetTypeInfo(lambdaExpression);
            var lambdaTargetType = lambdaTypeInfo.ConvertedType ?? lambdaTypeInfo.Type;
            if (lambdaTargetType is INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType)
                return delegateType;

            if (lambdaSymbol is not null)
                return ProjectSymbolForDisplay(lambdaSymbol);
        }

        if (TryResolvePatternDeclaredSymbol(semanticModel, node, token, out var patternSymbol))
            return patternSymbol;

        if (TryResolvePatternOperationDeclaredLocal(semanticModel, node, token, out var patternOperationSymbol))
            return patternOperationSymbol;

        if (TryResolveContainingStatementDesignatorSymbol(semanticModel, node, token, out var statementDesignatorSymbol))
            return statementDesignatorSymbol;

        if (TryResolveTypePositionSymbol(semanticModel, node, token, out var typePositionSymbol))
            return typePositionSymbol;

        if (TryResolveFromEnclosingBlockLocals(semanticModel, node, token, out var blockLocalSymbol))
            return blockLocalSymbol;

        if (TryResolveMemberSegmentSymbol(semanticModel, node, token, out var memberSegmentSymbol))
            return memberSegmentSymbol;

        if (TryResolveMemberReceiverSymbol(semanticModel, node, token, out var receiverSymbol))
            return receiverSymbol;

        var symbolInfo = semanticModel.GetSymbolInfo(node);
        if (symbolInfo.Symbol is not null || !symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            var chosen = ChoosePreferredSymbol(symbolInfo.Symbol, symbolInfo.CandidateSymbols, node);
            if (chosen is null)
                return null;

            if (IsTypeContext(node))
                chosen = ProjectTypeContextSymbol(chosen);

            return chosen;
        }

        var operation = semanticModel.GetOperation(node);
        var tokenReferencedSymbol = FindReferencedSymbolAtToken(operation, token.Span);
        if (tokenReferencedSymbol is not null)
            return ProjectSymbolForDisplay(tokenReferencedSymbol);

        var operationSymbol = operation is null ? null : GetOperationSymbol(operation);

        if (operationSymbol is not null)
            return ProjectSymbolForDisplay(operationSymbol);

        var containingStatement = node.AncestorsAndSelf().OfType<StatementSyntax>().FirstOrDefault();
        if (containingStatement is not null)
        {
            var statementOperation = semanticModel.GetOperation(containingStatement);
            var statementSymbol = FindReferencedSymbolAtToken(statementOperation, token.Span);
            if (statementSymbol is not null)
                return ProjectSymbolForDisplay(statementSymbol);
        }

        if (TryResolveByDeclaringSyntaxReference(semanticModel, token, out var declaringReferenceSymbol))
            return ProjectSymbolForDisplay(declaringReferenceSymbol);

        return null;
    }

    private static bool TryResolveParameterDeclarationSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var parameter = node as ParameterSyntax ?? node.AncestorsAndSelf().OfType<ParameterSyntax>().FirstOrDefault();
        if (parameter is null)
            return false;

        if (!parameter.Span.Contains(token.Span))
            return false;

        // Let nested syntax keep their own hover identity:
        // parameter type annotation should hover the type, and default values
        // should hover symbols inside the expression.
        if (parameter.TypeAnnotation?.Type.Span.Contains(token.Span) == true)
            return false;

        if (parameter.DefaultValue?.Value.Span.Contains(token.Span) == true)
            return false;

        symbol = semanticModel.GetFunctionExpressionParameterSymbol(parameter)
            ?? semanticModel.GetDeclaredSymbol(parameter);
        return symbol is not null;
    }

    private static bool TryResolveMemberReceiverSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        if (node is not IdentifierNameSyntax identifier ||
            identifier.Parent is not MemberAccessExpressionSyntax memberAccess ||
            !ReferenceEquals(memberAccess.Expression, identifier) ||
            !identifier.Span.Contains(token.Span))
        {
            return false;
        }

        // Prime member-access binding first so receiver symbols are resolved in the
        // final overload-selected lambda scope.
        _ = semanticModel.GetSymbolInfo(memberAccess);

        var receiverInfo = semanticModel.GetSymbolInfo(identifier);
        var receiverCandidate = ChoosePreferredSymbol(receiverInfo.Symbol, receiverInfo.CandidateSymbols, identifier);
        if (receiverCandidate is IParameterSymbol parameter &&
            parameter.Type?.TypeKind != TypeKind.Error)
        {
            symbol = ProjectSymbolForDisplay(parameter);
            return symbol is not null;
        }

        var operation = semanticModel.GetOperation(memberAccess);
        var referenced = FindReferencedSymbolAtToken(operation, identifier.Identifier.Span);
        if (referenced is null)
            return false;

        symbol = ProjectSymbolForDisplay(referenced);
        return symbol is not null;
    }

    private static bool IsTypeContext(SyntaxNode node)
        => node.AncestorsAndSelf().OfType<TypeSyntax>().Any();

    private static ISymbol ProjectTypeContextSymbol(ISymbol symbol)
    {
        if (symbol is IMethodSymbol { MethodKind: MethodKind.Constructor } constructor)
            return constructor.ContainingType ?? symbol;

        return symbol;
    }

    private static bool TryResolveMemberSegmentSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        SyntaxNode? targetNode = node switch
        {
            MemberBindingExpressionSyntax memberBinding
                when memberBinding.Name.Span.Contains(token.Span) || memberBinding.OperatorToken == token => memberBinding,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberBindingExpressionSyntax memberBinding &&
                     memberBinding.Name == identifier => memberBinding,
            MemberAccessExpressionSyntax memberAccess
                when memberAccess.Name.Span.Contains(token.Span) || memberAccess.OperatorToken == token => memberAccess,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                     memberAccess.Name == identifier => memberAccess,
            _ => null
        };

        if (targetNode is null)
            return false;

        if (targetNode is MemberAccessExpressionSyntax memberAccessTarget)
        {
            if (TryResolveMemberFromReceiverType(semanticModel, memberAccessTarget, out var typeMember))
            {
                symbol = ProjectSymbolForDisplay(typeMember);
                if (symbol is not null)
                    return true;
            }

            var nameInfo = semanticModel.GetSymbolInfo(memberAccessTarget.Name);
            if (nameInfo.Symbol is not null || !nameInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                symbol = ChoosePreferredSymbol(nameInfo.Symbol, nameInfo.CandidateSymbols, memberAccessTarget.Name);
                symbol = ProjectSymbolForDisplay(symbol);
                if (symbol is not null)
                    return true;
            }
        }

        if (targetNode is MemberBindingExpressionSyntax memberBindingTarget)
        {
            var nameInfo = semanticModel.GetSymbolInfo(memberBindingTarget.Name);
            if (nameInfo.Symbol is not null || !nameInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                symbol = ChoosePreferredSymbol(nameInfo.Symbol, nameInfo.CandidateSymbols, memberBindingTarget.Name);
                symbol = ProjectSymbolForDisplay(symbol);
                if (symbol is not null)
                    return true;
            }
        }

        var directInfo = semanticModel.GetSymbolInfo(targetNode);
        if (directInfo.Symbol is not null || !directInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            symbol = ChoosePreferredSymbol(directInfo.Symbol, directInfo.CandidateSymbols, targetNode);
            symbol = ProjectSymbolForDisplay(symbol);
            return symbol is not null;
        }

        foreach (var conditionalAccess in targetNode.AncestorsAndSelf().OfType<ConditionalAccessExpressionSyntax>())
        {
            if (!conditionalAccess.WhenNotNull.Span.Contains(token.Span))
                continue;

            var operation = semanticModel.GetOperation(conditionalAccess);
            var referenced = FindReferencedSymbolAtToken(operation, token.Span);
            if (referenced is not null)
            {
                symbol = ProjectSymbolForDisplay(referenced);
                return true;
            }
        }

        var fallbackOperation = semanticModel.GetOperation(targetNode);
        var fallbackSymbol = FindReferencedSymbolAtToken(fallbackOperation, token.Span);
        if (fallbackSymbol is not null)
        {
            symbol = ProjectSymbolForDisplay(fallbackSymbol);
            return true;
        }

        // Last-chance fallback: bind at statement scope and find the symbol by token span.
        // This helps when node-local symbol caches are stale for function-expression bodies.
        var containingStatement = targetNode.AncestorsAndSelf().OfType<StatementSyntax>().FirstOrDefault();
        if (containingStatement is not null)
        {
            var statementOperation = semanticModel.GetOperation(containingStatement);
            var statementSymbol = FindReferencedSymbolAtToken(statementOperation, token.Span);
            if (statementSymbol is not null)
            {
                symbol = ProjectSymbolForDisplay(statementSymbol);
                return true;
            }
        }

        return false;
    }

    private static bool TryResolveParameterDeclarationTokenFastPath(
        SemanticModel semanticModel,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol,
        [NotNullWhen(true)] out SyntaxNode? node)
    {
        symbol = null;
        node = null;

        var parameter = token.Parent?.AncestorsAndSelf().OfType<ParameterSyntax>().FirstOrDefault();
        if (parameter is null)
            return false;

        if (!parameter.Span.Contains(token.Span))
            return false;

        if (parameter.TypeAnnotation?.Type.Span.Contains(token.Span) == true)
            return false;

        if (parameter.DefaultValue?.Value.Span.Contains(token.Span) == true)
            return false;

        var resolved = semanticModel.GetFunctionExpressionParameterSymbol(parameter)
            ?? semanticModel.GetDeclaredSymbol(parameter);
        if (resolved is null)
            return false;

        symbol = resolved;
        node = parameter;
        return true;
    }

    private static bool TryResolveTypePositionSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var typeSyntaxes = node.AncestorsAndSelf()
            .OfType<TypeSyntax>()
            .Where(typeSyntax => typeSyntax.Span.Contains(token.Span));

        foreach (var typeSyntax in typeSyntaxes)
        {
            if (!TryResolveTypeSymbolFromSyntax(semanticModel, typeSyntax, out var resolvedType))
                continue;

            // In type positions we always prefer the union carrier type over a case type,
            // otherwise hover displays the case as "Name(...)".
            symbol = resolvedType.UnderlyingDiscriminatedUnion ?? resolvedType;
            return true;
        }

        return false;
    }

    private static bool TryResolveTypeSymbolFromSyntax(
        SemanticModel semanticModel,
        TypeSyntax typeSyntax,
        out ITypeSymbol? resolvedType)
    {
        var typeInfo = semanticModel.GetTypeInfo(typeSyntax);
        resolvedType = typeInfo.Type ?? typeInfo.ConvertedType;
        if (resolvedType is not null && resolvedType.TypeKind != TypeKind.Error)
            return true;

        var typeSymbol = semanticModel.GetSymbolInfo(typeSyntax).Symbol;
        resolvedType = typeSymbol switch
        {
            ITypeSymbol resolved => resolved,
            IAliasSymbol { UnderlyingSymbol: ITypeSymbol aliasedType } => aliasedType,
            _ => resolvedType
        };

        if (resolvedType is null || resolvedType.TypeKind == TypeKind.Error)
            return false;

        return true;
    }

    private static ISymbol? ChoosePreferredSymbol(
        ISymbol? primarySymbol,
        ImmutableArray<ISymbol> candidates,
        SyntaxNode node)
    {
        var all = new List<ISymbol>(capacity: 1 + (candidates.IsDefaultOrEmpty ? 0 : candidates.Length));
        if (primarySymbol is not null)
            all.Add(primarySymbol);

        if (!candidates.IsDefaultOrEmpty)
            all.AddRange(candidates.Where(static c => c is not null));

        if (all.Count == 0)
            return null;

        return all
            .Distinct(SymbolEqualityComparer.Default)
            .OrderByDescending(symbol => ScoreSymbol(symbol, node))
            .FirstOrDefault();
    }

    private static int ScoreSymbol(ISymbol symbol, SyntaxNode node)
    {
        var score = 0;

        // In type-position, namespace fallback should lose against an actual type.
        var inTypePosition = node.AncestorsAndSelf().OfType<TypeSyntax>().Any();
        if (inTypePosition)
            score += symbol.Kind == SymbolKind.Type ? 300 : symbol.Kind == SymbolKind.Namespace ? -300 : 0;

        if (symbol.DeclaringSyntaxReferences.Length > 0)
            score += 200;

        var docs = symbol.GetDocumentationComment();
        if (docs is not null)
        {
            if (docs.Format == DocumentationFormat.Markdown)
                score += 120;
            else if (docs.Format == DocumentationFormat.Xml)
                score += 40;
        }

        if (symbol.Kind == SymbolKind.Method)
            score += 20;

        if (symbol.Kind == SymbolKind.Namespace)
            score -= 20;

        return score;
    }

    private static ISymbol? ProjectSymbolForDisplay(ISymbol? symbol)
    {
        if (symbol is IMethodSymbol methodSymbol &&
            methodSymbol.AssociatedSymbol is { } associatedMember &&
            associatedMember is IPropertySymbol or IEventSymbol)
        {
            return associatedMember;
        }

        if (symbol is IFieldSymbol fieldSymbol &&
            fieldSymbol.AssociatedSymbol is { } associatedFieldMember &&
            associatedFieldMember is IPropertySymbol or IEventSymbol)
        {
            return associatedFieldMember;
        }

        return symbol;
    }

    private static ISymbol? FindReferencedSymbolAtToken(IOperation? operation, TextSpan tokenSpan)
    {
        if (operation is null || !operation.Syntax.Span.Contains(tokenSpan))
            return null;

        var current = GetOperationSymbol(operation);

        foreach (var child in operation.ChildOperations)
        {
            if (!child.Syntax.Span.Contains(tokenSpan))
                continue;

            var childSymbol = FindReferencedSymbolAtToken(child, tokenSpan);
            if (childSymbol is not null)
                return childSymbol;
        }

        return current;
    }

    private static ISymbol? GetOperationSymbol(IOperation operation)
    {
        return operation switch
        {
            ILiteralOperation literal => literal.Type?.UnwrapLiteralType(),
            ISymbolReferenceOperation<ISymbol> symbolReference => symbolReference.Symbol,
            ISingleVariableDesignatorOperation designator => designator.Local,
            IVariableDeclaratorOperation declarator => declarator.Symbol,
            IInvocationOperation invocation => invocation.TargetMethod,
            _ => null
        };
    }

    private static bool TryResolveInvocationTargetSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var invocation = node switch
        {
            InvocationExpressionSyntax direct => direct,
            IdentifierNameSyntax { Parent: InvocationExpressionSyntax direct } => direct,
            MemberAccessExpressionSyntax { Parent: InvocationExpressionSyntax parent } => parent,
            IdentifierNameSyntax identifier
                when identifier.Parent is MemberAccessExpressionSyntax memberAccess &&
                     memberAccess.Name == identifier &&
                     memberAccess.Parent is InvocationExpressionSyntax parent => parent,
            _ => null
        };

        if (invocation is null)
            return false;

        // Only resolve invocation targets when hovering the call target itself,
        // not arguments/parameter lists/lambda bodies.
        if (!invocation.Expression.Span.Contains(token.Span))
            return false;

        var symbolInfo = semanticModel.GetSymbolInfo(invocation);
        if (symbolInfo.Symbol is not null)
        {
            symbol = ProjectInvocationSymbolForDisplay(symbolInfo.Symbol, semanticModel, invocation);
            return true;
        }

        if (!symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            symbol = ProjectInvocationSymbolForDisplay(symbolInfo.CandidateSymbols[0], semanticModel, invocation);
            return true;
        }

        if (semanticModel.GetOperation(invocation) is IInvocationOperation operation &&
            operation.TargetMethod is not null)
        {
            symbol = ProjectInvocationSymbolForDisplay(operation.TargetMethod, semanticModel, invocation);
            return true;
        }

        if (TryResolveUnionCaseFromInvocationContext(semanticModel, invocation, out var unionCaseSymbol))
        {
            symbol = unionCaseSymbol;
            return true;
        }

        // When this invocation is the right-hand side of a pipe expression the binder
        // resolves overloads using the piped value as an implicit first argument and
        // stores the constructed symbol on the BinaryExpression node, not on the
        // InvocationExpression itself.
        if (invocation.Parent is InfixOperatorExpressionSyntax { Kind: SyntaxKind.PipeExpression } pipeExpr &&
            pipeExpr.Right == invocation)
        {
            var pipeInfo = semanticModel.GetSymbolInfo(pipeExpr);
            if (pipeInfo.Symbol is not null)
            {
                symbol = ProjectInvocationSymbolForDisplay(pipeInfo.Symbol, semanticModel, invocation);
                return true;
            }

            if (!pipeInfo.CandidateSymbols.IsDefaultOrEmpty)
            {
                symbol = ProjectInvocationSymbolForDisplay(pipeInfo.CandidateSymbols[0], semanticModel, invocation);
                return true;
            }

            if (semanticModel.GetOperation(pipeExpr) is IInvocationOperation pipeOp &&
                pipeOp.TargetMethod is not null)
            {
                symbol = ProjectInvocationSymbolForDisplay(pipeOp.TargetMethod, semanticModel, invocation);
                return true;
            }
        }

        return false;
    }

    private static bool TryResolveMemberFromReceiverType(
        SemanticModel semanticModel,
        MemberAccessExpressionSyntax memberAccess,
        out ISymbol? symbol)
    {
        symbol = null;

        if (memberAccess.Name is not IdentifierNameSyntax identifierName)
            return false;

        var memberName = identifierName.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(memberName))
            return false;

        var receiverType = semanticModel.GetTypeInfo(memberAccess.Expression).Type;
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            var receiverSymbol = semanticModel.GetSymbolInfo(memberAccess.Expression).Symbol;
            receiverType = receiverSymbol switch
            {
                ILocalSymbol local => local.Type,
                IParameterSymbol parameter => parameter.Type,
                IPropertySymbol property => property.Type,
                IFieldSymbol field => field.Type,
                _ => null
            };
        }

        if ((receiverType is null || receiverType.TypeKind == TypeKind.Error) &&
            memberAccess.Expression is IdentifierNameSyntax receiverIdentifier &&
            TryGetEnclosingFunctionExpression(memberAccess, out var enclosingFunctionExpression))
        {
            if (TryInferFunctionParameterTypeFromInvocationContext(
                    semanticModel,
                    enclosingFunctionExpression,
                    receiverIdentifier.Identifier.ValueText,
                    memberName,
                    out var inferredFromInvocation))
            {
                receiverType = inferredFromInvocation;
            }

            var functionType = semanticModel.GetTypeInfo(enclosingFunctionExpression).ConvertedType ??
                               semanticModel.GetTypeInfo(enclosingFunctionExpression).Type;
            if (functionType is INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType &&
                delegateType.GetDelegateInvokeMethod() is { } invokeMethod)
            {
                IParameterSymbol? matchedParameter = null;

                if (invokeMethod.Parameters.Length == 1)
                {
                    matchedParameter = invokeMethod.Parameters[0];
                }
                else
                {
                    matchedParameter = invokeMethod.Parameters.FirstOrDefault(
                        p => string.Equals(p.Name, receiverIdentifier.Identifier.ValueText, StringComparison.Ordinal));
                }

                if (matchedParameter is not null)
                    receiverType = matchedParameter.Type;
            }

            var functionInfo = semanticModel.GetSymbolInfo(enclosingFunctionExpression);
            if (functionInfo.Symbol is IMethodSymbol lambdaMethod)
            {
                var parameter = lambdaMethod.Parameters.FirstOrDefault(
                    p => string.Equals(p.Name, receiverIdentifier.Identifier.ValueText, StringComparison.Ordinal));

                if (parameter is not null)
                    receiverType = parameter.Type;
            }
        }

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return false;

        var namedReceiverType = receiverType as INamedTypeSymbol;
        if (namedReceiverType is null)
            return false;

        foreach (var candidate in namedReceiverType.GetMembers(memberName))
        {
            if (candidate is IPropertySymbol or IFieldSymbol or IEventSymbol or IMethodSymbol)
            {
                symbol = candidate;
                return true;
            }
        }

        return false;
    }

    private static bool TryInferFunctionParameterTypeFromInvocationContext(
        SemanticModel semanticModel,
        FunctionExpressionSyntax functionExpression,
        string receiverIdentifierName,
        string requiredMemberName,
        out ITypeSymbol? inferredType)
    {
        inferredType = null;

        if (functionExpression.Parent is not ArgumentSyntax argument ||
            argument.Parent is not ArgumentListSyntax argumentList ||
            argumentList.Parent is not InvocationExpressionSyntax invocation)
        {
            return false;
        }

        var argumentIndex = 0;
        foreach (var candidate in argumentList.Arguments)
        {
            if (ReferenceEquals(candidate, argument))
                break;

            argumentIndex++;
        }

        IEnumerable<IMethodSymbol> candidateMethods;
        if (invocation.Expression is MemberAccessExpressionSyntax invocationMemberAccess &&
            invocationMemberAccess.Name is IdentifierNameSyntax memberIdentifier)
        {
            var invocationReceiverType = semanticModel.GetTypeInfo(invocationMemberAccess.Expression).Type;
            if (invocationReceiverType is not INamedTypeSymbol invocationReceiverNamed)
                return false;

            candidateMethods = invocationReceiverNamed
                .GetMembers(memberIdentifier.Identifier.ValueText)
                .OfType<IMethodSymbol>();
        }
        else
        {
            return false;
        }

        ITypeSymbol? fallback = null;
        foreach (var method in candidateMethods)
        {
            if (method.Parameters.Length <= argumentIndex)
                continue;

            var delegateType = method.Parameters[argumentIndex].Type as INamedTypeSymbol;
            if (delegateType?.TypeKind != TypeKind.Delegate)
                continue;

            var invokeMethod = delegateType.GetDelegateInvokeMethod();
            if (invokeMethod is null || invokeMethod.Parameters.IsDefaultOrEmpty)
                continue;

            IParameterSymbol? lambdaParameter = invokeMethod.Parameters.Length == 1
                ? invokeMethod.Parameters[0]
                : invokeMethod.Parameters.FirstOrDefault(
                    p => string.Equals(p.Name, receiverIdentifierName, StringComparison.Ordinal));

            if (lambdaParameter is null)
                continue;

            var parameterType = lambdaParameter.Type is NullableTypeSymbol nullable
                ? nullable.UnderlyingType
                : lambdaParameter.Type;

            fallback ??= parameterType;

            if (parameterType is INamedTypeSymbol named &&
                named.GetMembers(requiredMemberName).Length > 0)
            {
                inferredType = parameterType;
                return true;
            }
        }

        inferredType = fallback;
        return inferredType is not null;
    }

    private static bool TryGetEnclosingFunctionExpression(SyntaxNode node, out FunctionExpressionSyntax functionExpression)
    {
        for (var current = node.Parent; current is not null; current = current.Parent)
        {
            if (current is FunctionExpressionSyntax candidate)
            {
                functionExpression = candidate;
                return true;
            }
        }

        functionExpression = null!;
        return false;
    }

    private static ISymbol ProjectInvocationSymbolForDisplay(
        ISymbol symbol,
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation)
    {
        if (symbol is IMethodSymbol { MethodKind: MethodKind.Constructor } constructor)
        {
            // Prefer the instantiated type at this call site (for example Option<int>)
            // instead of the constructor's containing generic definition.
            if (TryGetInstantiatedInvocationType(semanticModel, invocation) is { } instantiated)
                return instantiated;

            return constructor.ContainingType ?? symbol;
        }

        return symbol;
    }

    private static INamedTypeSymbol? TryGetInstantiatedInvocationType(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation)
    {
        if (semanticModel.GetOperation(invocation) is IInvocationOperation { TargetMethod: { MethodKind: MethodKind.Constructor } method } &&
            method.ContainingType is INamedTypeSymbol methodContainingType)
        {
            return methodContainingType;
        }

        var invocationTypeInfo = semanticModel.GetTypeInfo(invocation);
        if ((invocationTypeInfo.ConvertedType ?? invocationTypeInfo.Type) is INamedTypeSymbol invocationType)
            return invocationType;

        var expressionTypeInfo = semanticModel.GetTypeInfo(invocation.Expression);
        if ((expressionTypeInfo.ConvertedType ?? expressionTypeInfo.Type) is INamedTypeSymbol expressionType)
            return expressionType;

        return null;
    }

    private static bool TryResolveCompoundEventAssignmentSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
    out ISymbol? symbol)
    {
        symbol = null;

        // foo.Click += ... / foo.Click -= ...
        var assignment = node.AncestorsAndSelf().OfType<AssignmentExpressionSyntax>().FirstOrDefault();
        if (assignment is null)
            return false;

        var opKind = assignment.OperatorToken.Kind;
        if (opKind != SyntaxKind.PlusEqualsToken && opKind != SyntaxKind.MinusEqualsToken)
            return false;

        if (assignment.Left is not MemberAccessExpressionSyntax memberAccess)
            return false;

        // Only when hovering the member name itself (e.g. Click)
        if (!memberAccess.Name.Span.Contains(token.Span))
            return false;

        // Prefer symbol info on the Name node so we get the IEventSymbol, not add/remove accessor methods.
        var nameInfo = semanticModel.GetSymbolInfo(memberAccess.Name);
        if (nameInfo.Symbol is not null || !nameInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            var chosen = ChoosePreferredSymbol(nameInfo.Symbol, nameInfo.CandidateSymbols, memberAccess.Name);
            chosen = ProjectSymbolForDisplay(chosen);
            if (chosen is not null)
            {
                symbol = chosen;
                return true;
            }
        }

        // Fallback: resolve on the full member access
        var memberInfo = semanticModel.GetSymbolInfo(memberAccess);
        if (memberInfo.Symbol is not null || !memberInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            var chosen = ChoosePreferredSymbol(memberInfo.Symbol, memberInfo.CandidateSymbols, memberAccess);
            chosen = ProjectSymbolForDisplay(chosen);
            if (chosen is not null)
            {
                symbol = chosen;
                return true;
            }
        }

        return false;
    }

    private static bool TryResolveUnionCaseFromInvocationContext(
        SemanticModel semanticModel,
        InvocationExpressionSyntax invocation,
        out ISymbol? symbol)
    {
        symbol = null;

        var invokedName = invocation.Expression switch
        {
            IdentifierNameSyntax identifier => identifier.Identifier.ValueText,
            MemberAccessExpressionSyntax { Name: IdentifierNameSyntax identifier } => identifier.Identifier.ValueText,
            _ => null
        };

        if (string.IsNullOrWhiteSpace(invokedName))
            return false;

        var typeInfo = semanticModel.GetTypeInfo(invocation);
        var targetType = typeInfo.ConvertedType ?? typeInfo.Type;
        if (targetType is null)
            return false;

        if (targetType is IDiscriminatedUnionSymbol union && targetType.IsDiscriminatedUnion)
        {
            var unionCase = union.Cases.FirstOrDefault(c => string.Equals(c.Name, invokedName, StringComparison.Ordinal));
            if (unionCase is not null)
            {
                symbol = unionCase;
                return true;
            }
        }

        if (targetType is IDiscriminatedUnionCaseSymbol caseType &&
            targetType.IsDiscriminatedUnionCase &&
            string.Equals(caseType.Name, invokedName, StringComparison.Ordinal))
        {
            symbol = caseType;
            return true;
        }

        return false;
    }

    private static bool TryResolveRecordPatternCaseSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        if (node is not IdentifierNameSyntax identifier ||
            token != identifier.Identifier ||
            identifier.Parent is not RecordPatternSyntax recordPattern ||
            recordPattern.Type != identifier)
        {
            return false;
        }

        var caseName = identifier.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(caseName))
            return false;

        ITypeSymbol? scrutineeType = null;

        if (recordPattern.GetAncestor<MatchExpressionSyntax>() is { } matchExpression)
            scrutineeType = semanticModel.GetTypeInfo(matchExpression.Expression).Type;
        else if (recordPattern.GetAncestor<MatchStatementSyntax>() is { } matchStatement)
            scrutineeType = semanticModel.GetTypeInfo(matchStatement.Expression).Type;

        if (scrutineeType is null)
            return false;

        IDiscriminatedUnionSymbol? union = null;
        if (scrutineeType is IDiscriminatedUnionSymbol unionType && scrutineeType.IsDiscriminatedUnion)
            union = unionType;
        else if (scrutineeType is IDiscriminatedUnionCaseSymbol caseType && scrutineeType.IsDiscriminatedUnionCase)
            union = caseType.Union;

        if (union is null)
            return false;

        var caseSymbol = union.Cases.FirstOrDefault(c => string.Equals(c.Name, caseName, StringComparison.Ordinal));
        if (caseSymbol is null)
            return false;

        symbol = caseSymbol;
        return true;
    }

    // Handles bare method references on the RHS of a pipe: `expr |> WriteLine`
    // These have no InvocationExpressionSyntax (no parentheses), so
    // TryResolveInvocationTargetSymbol never fires. The binder stores the
    // resolved / overload-selected symbol on the BinaryExpression itself.
    private static bool TryResolvePipeRightHandSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        // Only act on tokens that sit inside the right-hand side of a pipe and
        // whose RHS is NOT itself an InvocationExpression (that case is handled
        // inside TryResolveInvocationTargetSymbol).
        var pipeExpr = node
            .AncestorsAndSelf()
            .OfType<InfixOperatorExpressionSyntax>()
            .FirstOrDefault(b =>
                b.Kind == SyntaxKind.PipeExpression &&
                b.Right is not InvocationExpressionSyntax &&
                b.Right.Span.Contains(token.Span));

        if (pipeExpr is null)
            return false;

        var pipeInfo = semanticModel.GetSymbolInfo(pipeExpr);
        if (pipeInfo.Symbol is not null)
        {
            symbol = ProjectSymbolForDisplay(pipeInfo.Symbol);
            return symbol is not null;
        }

        if (!pipeInfo.CandidateSymbols.IsDefaultOrEmpty)
        {
            symbol = ProjectSymbolForDisplay(pipeInfo.CandidateSymbols[0]);
            return symbol is not null;
        }

        if (semanticModel.GetOperation(pipeExpr) is IInvocationOperation op &&
            op.TargetMethod is not null)
        {
            symbol = op.TargetMethod;
            return true;
        }

        return false;
    }

    private static bool ShouldSkipCandidateNode(SyntaxNode node, SyntaxToken token)
    {
        return node switch
        {
            CompilationUnitSyntax => true,
            GlobalStatementSyntax => true,
            LocalDeclarationStatementSyntax => true,
            VariableDeclarationSyntax => true,
            VariableDeclaratorSyntax declarator => token != declarator.Identifier,
            FunctionStatementSyntax functionStatement => !IsFunctionDeclarationToken(functionStatement, token),
            FunctionExpressionSyntax functionExpression =>
                !IsFunctionExpressionDeclarationToken(functionExpression, token) ||
                IsInsideFunctionExpressionBody(functionExpression, token),
            MethodDeclarationSyntax methodDeclaration => token != methodDeclaration.Identifier,
            BaseTypeDeclarationSyntax typeDeclaration => token != typeDeclaration.Identifier,
            UnionCaseClauseSyntax unionCaseClause => token != unionCaseClause.Identifier,
            _ => false
        };
    }

    private static bool IsFunctionDeclarationToken(FunctionStatementSyntax functionStatement, SyntaxToken token)
    {
        if (!token.Span.IntersectsWith(functionStatement.Span))
            return false;

        if (token == functionStatement.Identifier)
            return true;

        if (functionStatement.Body is { } body)
        {
            if (token == body.OpenBraceToken || token == body.CloseBraceToken)
                return true;

            return token.SpanStart < body.Span.Start;
        }

        if (functionStatement.ExpressionBody is { } expressionBody)
            return token.SpanStart < expressionBody.Span.Start;

        return true;
    }

    private static bool IsFunctionExpressionDeclarationToken(FunctionExpressionSyntax functionExpression, SyntaxToken token)
    {
        if (!token.Span.IntersectsWith(functionExpression.Span))
            return false;

        if (functionExpression.Body is { } body)
        {
            if (token == body.OpenBraceToken || token == body.CloseBraceToken)
                return true;

            return token.SpanStart < body.Span.Start;
        }

        if (functionExpression.ExpressionBody is { } expressionBody)
            return token.SpanStart < expressionBody.Expression.Span.Start;

        return true;
    }

    private static bool IsFunctionExpressionIdentifierToken(FunctionExpressionSyntax functionExpression, SyntaxToken token)
        => functionExpression is ParenthesizedFunctionExpressionSyntax { Identifier.IsMissing: false } parenthesized &&
           token == parenthesized.Identifier;

    private static bool IsInsideFunctionExpressionBody(FunctionExpressionSyntax functionExpression, SyntaxToken token)
    {
        if (functionExpression.Body?.Span.Contains(token.Span) == true)
            return true;

        if (functionExpression.ExpressionBody?.Expression.Span.Contains(token.Span) == true)
            return true;

        return false;
    }

    private static bool TryResolvePatternDeclaredSymbol(SemanticModel semanticModel, SyntaxNode node, SyntaxToken token, out ISymbol? symbol)
    {
        symbol = node switch
        {
            SingleVariableDesignationSyntax single when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            VariablePatternSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            TypedVariableDesignationSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            _ => null
        };

        if (symbol is not null)
            return true;

        symbol = node.Parent switch
        {
            SingleVariableDesignationSyntax single when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            VariablePatternSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            TypedVariableDesignationSyntax { Designation: SingleVariableDesignationSyntax single } when token == single.Identifier => semanticModel.GetDeclaredSymbol(single),
            _ => null
        };

        if (symbol is null &&
            node is IdentifierNameSyntax identifierName &&
            token == identifierName.Identifier &&
            identifierName.Parent is ConstantPatternSyntax &&
            TryResolveFunctionExpressionPatternLocalSymbol(semanticModel, identifierName, out var implicitPatternLocal))
        {
            symbol = implicitPatternLocal;
        }

        return symbol is not null;
    }

    private static bool TryResolveContainingStatementDesignatorSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var containingStatement = node.AncestorsAndSelf().OfType<StatementSyntax>().FirstOrDefault();
        if (containingStatement is null)
            return false;

        var statementOperation = semanticModel.GetOperation(containingStatement);
        if (statementOperation is null)
            return false;

        var stack = new Stack<IOperation>();
        stack.Push(statementOperation);

        while (stack.Count > 0)
        {
            var current = stack.Pop();
            switch (current)
            {
                case ISingleVariableDesignatorOperation designator
                    when TryMatchDeclaringSpan(designator.Local, token):
                    symbol = designator.Local;
                    return true;
                case IVariableDeclaratorOperation declarator
                    when TryMatchDeclaringSpan(declarator.Symbol, token):
                    symbol = declarator.Symbol;
                    return true;
            }

            foreach (var child in current.ChildOperations)
                stack.Push(child);
        }

        return false;
    }

    private static bool TryResolvePatternOperationDeclaredLocal(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var patternSyntax = node.AncestorsAndSelf().FirstOrDefault(static n =>
            n is PositionalPatternSyntax or SequencePatternSyntax);
        if (patternSyntax is null)
            return false;

        var operation = semanticModel.GetOperation(patternSyntax);
        if (operation is null)
            return false;

        var targetName = token.ValueText;
        var stack = new Stack<IOperation>();
        stack.Push(operation);

        while (stack.Count > 0)
        {
            var current = stack.Pop();
            switch (current)
            {
                case ISingleVariableDesignatorOperation designator
                    when designator.Syntax.Span.Contains(token.Span) ||
                         TryMatchDeclaringSpan(designator.Local, token) ||
                         (!string.IsNullOrWhiteSpace(targetName) &&
                          string.Equals(designator.Local.Name, targetName, StringComparison.Ordinal)):
                    symbol = designator.Local;
                    return true;
                case IVariableDeclaratorOperation declarator
                    when declarator.Syntax.Span.Contains(token.Span) ||
                         TryMatchDeclaringSpan(declarator.Symbol, token) ||
                         (!string.IsNullOrWhiteSpace(targetName) &&
                          string.Equals(declarator.Symbol.Name, targetName, StringComparison.Ordinal)):
                    symbol = declarator.Symbol;
                    return true;
            }

            foreach (var child in current.ChildOperations)
                stack.Push(child);
        }

        return false;
    }

    private static bool TryResolveFromEnclosingBlockLocals(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        foreach (var blockSyntax in node.AncestorsAndSelf().OfType<BlockStatementSyntax>())
        {
            if (semanticModel.GetOperation(blockSyntax) is not IBlockOperation blockOperation)
                continue;

            foreach (var local in blockOperation.Locals)
            {
                if (!TryMatchDeclaringSpan(local, token))
                    continue;

                symbol = local;
                return true;
            }
        }

        return false;
    }

    private static bool TryResolveFunctionExpressionPatternLocalSymbol(
        SemanticModel semanticModel,
        IdentifierNameSyntax identifierName,
        out ISymbol? symbol)
    {
        symbol = null;

        var parameter = identifierName.Ancestors().OfType<ParameterSyntax>().FirstOrDefault();
        if (parameter is null || parameter.Pattern is null)
            return false;

        var functionExpression = parameter.Ancestors().OfType<FunctionExpressionSyntax>().FirstOrDefault();
        if (functionExpression is null)
            return false;

        if (semanticModel.GetOperation(functionExpression) is not ILambdaOperation lambdaOperation ||
            lambdaOperation.Body is null)
        {
            return false;
        }

        var targetName = identifierName.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(targetName))
            return false;

        var stack = new Stack<IOperation>();
        stack.Push(lambdaOperation.Body);
        while (stack.Count > 0)
        {
            var current = stack.Pop();
            if (current is IVariableDeclaratorOperation declarator &&
                string.Equals(declarator.Symbol.Name, targetName, StringComparison.Ordinal))
            {
                symbol = declarator.Symbol;
                return true;
            }

            if (current is ISingleVariableDesignatorOperation designator &&
                string.Equals(designator.Local.Name, targetName, StringComparison.Ordinal))
            {
                symbol = designator.Local;
                return true;
            }

            foreach (var child in current.ChildOperations)
                stack.Push(child);
        }

        return false;
    }

    private static bool TryResolveByDeclaringSyntaxReference(
        SemanticModel semanticModel,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        if (token.Kind != SyntaxKind.IdentifierToken || string.IsNullOrWhiteSpace(token.ValueText))
            return false;

        var root = token.SyntaxTree!.GetRoot();
        foreach (var candidate in root.DescendantNodes().OfType<IdentifierNameSyntax>())
        {
            if (!string.Equals(candidate.Identifier.ValueText, token.ValueText, StringComparison.Ordinal))
                continue;

            var info = semanticModel.GetSymbolInfo(candidate);
            if (TryMatchDeclaringSpan(info.Symbol, token))
            {
                symbol = info.Symbol;
                return true;
            }

            if (info.CandidateSymbols.IsDefaultOrEmpty)
                continue;

            foreach (var candidateSymbol in info.CandidateSymbols)
            {
                if (!TryMatchDeclaringSpan(candidateSymbol, token))
                    continue;

                symbol = candidateSymbol;
                return true;
            }
        }

        return false;
    }

    private static bool TryMatchDeclaringSpan(ISymbol? symbol, SyntaxToken token)
    {
        if (symbol is null || symbol.DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return false;

        foreach (var reference in symbol.DeclaringSyntaxReferences)
        {
            if (reference.SyntaxTree == token.SyntaxTree &&
                reference.Span.Contains(token.SpanStart))
            {
                return true;
            }
        }

        return false;
    }
}

internal readonly record struct SymbolResolutionResult(ISymbol Symbol, SyntaxNode Node);
internal readonly record struct CandidateNode(SyntaxNode Node, SyntaxToken Token);
