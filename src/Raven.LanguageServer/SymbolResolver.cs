using System.Collections.Immutable;

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
            if (ShouldSkipCandidateNode(candidate.Node, candidate.Token))
                continue;

            var symbol = ResolveSymbolFromNode(semanticModel, candidate.Node, candidate.Token);
            if (symbol is not null)
                return new SymbolResolutionResult(symbol.UnderlyingSymbol, candidate.Node);
        }

        return null;
    }

    private static IEnumerable<CandidateNode> GetCandidateNodes(SyntaxNode root, int offset)
    {
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

        if (TryResolvePatternDeclaredSymbol(semanticModel, node, token, out var patternSymbol))
            return patternSymbol;

        if (TryResolveTypePositionSymbol(semanticModel, node, token, out var typePositionSymbol))
            return typePositionSymbol;

        if (TryResolveMemberSegmentSymbol(semanticModel, node, token, out var memberSegmentSymbol))
            return memberSegmentSymbol;

        if (node is ParameterSyntax parameterDeclaration && token == parameterDeclaration.Identifier)
            return semanticModel.GetDeclaredSymbol(parameterDeclaration);

        if (node.Parent is ParameterSyntax parentParameterDeclaration &&
            token == parentParameterDeclaration.Identifier)
            return semanticModel.GetDeclaredSymbol(parentParameterDeclaration);

        if (node is FunctionStatementSyntax functionStatement &&
            IsFunctionDeclarationToken(functionStatement, token))
        {
            return semanticModel.GetDeclaredSymbol(functionStatement);
        }

        if (node is FunctionExpressionSyntax lambdaExpression &&
            token.Span.IntersectsWith(lambdaExpression.Span))
        {
            var lambdaSymbolInfo = semanticModel.GetSymbolInfo(lambdaExpression);
            if (lambdaSymbolInfo.Symbol is not null || !lambdaSymbolInfo.CandidateSymbols.IsDefaultOrEmpty)
                return ChoosePreferredSymbol(lambdaSymbolInfo.Symbol, lambdaSymbolInfo.CandidateSymbols, lambdaExpression);
        }

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
        var operationSymbol = operation switch
        {
            ILiteralOperation literal => literal.Type?.UnwrapLiteralType(),
            IParameterReferenceOperation parameterReference => (ISymbol?)parameterReference.Parameter,
            ILocalReferenceOperation localReference => localReference.Local,
            IVariableReferenceOperation variableReference => variableReference.Variable,
            IFieldReferenceOperation fieldReference => fieldReference.Field,
            IPropertyReferenceOperation propertyReference => propertyReference.Property,
            IMethodReferenceOperation methodReference => methodReference.Method,
            IMemberReferenceOperation memberReference => memberReference.Symbol,
            IInvocationOperation invocation => invocation.TargetMethod,
            _ => null
        };

        return ProjectSymbolForDisplay(operationSymbol);
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

        return false;
    }

    private static bool TryResolveTypePositionSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        var typeSyntax = node.AncestorsAndSelf().OfType<TypeSyntax>().FirstOrDefault();
        if (typeSyntax is null || !typeSyntax.Span.Contains(token.Span))
            return false;

        var typeInfo = semanticModel.GetTypeInfo(typeSyntax);
        var resolvedType = typeInfo.Type ?? typeInfo.ConvertedType;
        if (resolvedType is null || resolvedType.TypeKind == TypeKind.Error)
            return false;

        // In type positions we always prefer the union carrier type over a case type,
        // otherwise hover displays the case as "Name(...)".
        symbol = resolvedType.UnderlyingDiscriminatedUnion ?? resolvedType;
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

        ISymbol? current = operation switch
        {
            IFieldReferenceOperation fieldReference => fieldReference.Field,
            IPropertyReferenceOperation propertyReference => propertyReference.Property,
            IMethodReferenceOperation methodReference => methodReference.Method,
            IMemberReferenceOperation memberReference => memberReference.Symbol,
            IInvocationOperation invocation => invocation.TargetMethod,
            _ => null
        };

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

        return symbol is not null;
    }
}

internal readonly record struct SymbolResolutionResult(ISymbol Symbol, SyntaxNode Node);
internal readonly record struct CandidateNode(SyntaxNode Node, SyntaxToken Token);
