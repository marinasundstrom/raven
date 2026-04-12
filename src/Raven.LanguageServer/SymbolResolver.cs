using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.LanguageServer;

internal static partial class SymbolResolver
{
    // Keep resolution ordered by syntax intent:
    // 1. direct member-access names
    // 2. direct identifier tokens
    // 3. broader ancestor-based fallback
    //
    // This keeps queries aligned with the user's exact syntax node instead of
    // forcing every hover/definition through one generic fallback chain.
    public static SymbolResolutionResult? ResolveSymbolAtPosition(SemanticModel semanticModel, SyntaxNode root, int offset)
    {
        if (TryResolveMemberAccessAtOffset(semanticModel, root, offset, out var memberAccessResolution))
            return memberAccessResolution;

        if (TryResolvePipeInvocationTargetAtOffset(semanticModel, root, offset, out var pipeInvocationResolution))
            return pipeInvocationResolution;

        if (TryResolveExactIdentifierSymbol(semanticModel, root, offset, out var exactIdentifierResolution))
            return exactIdentifierResolution;

        foreach (var candidate in GetCandidateNodes(root, offset))
        {
            if (TryResolveMemberTokenFastPath(semanticModel, candidate.Token, out var memberTokenSymbol))
                return new SymbolResolutionResult(
                    SymbolResolutionKind.MemberAccess,
                    memberTokenSymbol.UnderlyingSymbol,
                    candidate.Token.Parent ?? candidate.Node);

            if (TryResolveParameterDeclarationTokenFastPath(semanticModel, candidate.Token, out var parameterSymbol, out var parameterNode))
            {
                return new SymbolResolutionResult(
                    SymbolResolutionKind.ParameterDeclaration,
                    parameterSymbol.UnderlyingSymbol,
                    parameterNode);
            }

            if (ShouldSkipCandidateNode(candidate.Node, candidate.Token))
                continue;

            var symbol = ResolveSymbolFromNode(semanticModel, candidate.Node, candidate.Token);
            if (symbol is not null)
                return new SymbolResolutionResult(
                    GetResolutionKindForNode(candidate.Node, candidate.Token),
                    symbol.UnderlyingSymbol,
                    candidate.Token.Parent ?? candidate.Node);
        }

        return null;
    }

    private static bool TryResolveMemberAccessAtOffset(
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset,
        [NotNullWhen(true)] out SymbolResolutionResult? resolution)
    {
        resolution = null;

        foreach (var normalizedOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            var memberAccess = root.DescendantNodes()
                .OfType<MemberAccessExpressionSyntax>()
                .FirstOrDefault(access => access.Name.Span.Contains(normalizedOffset));

            if (memberAccess is null)
                continue;

            var symbol = ResolveExplicitMemberAccessSymbol(semanticModel, memberAccess, new TextSpan(normalizedOffset, 0));
            if (symbol is null)
                continue;

            resolution = new SymbolResolutionResult(SymbolResolutionKind.MemberAccess, symbol.UnderlyingSymbol, memberAccess.Name);
            return true;
        }

        return false;
    }

    private static bool TryResolvePipeInvocationTargetAtOffset(
        SemanticModel semanticModel,
        SyntaxNode root,
        int offset,
        [NotNullWhen(true)] out SymbolResolutionResult? resolution)
    {
        resolution = null;

        foreach (var normalizedOffset in NormalizeOffsets(offset, root.FullSpan.End))
        {
            var invocation = root.DescendantNodes()
                .OfType<InvocationExpressionSyntax>()
                .Where(invocation => invocation.Expression.Span.Contains(normalizedOffset) || invocation.Expression.Span.End == normalizedOffset)
                .OrderBy(invocation => invocation.Span.Length)
                .FirstOrDefault(invocation => invocation.Ancestors()
                    .OfType<InfixOperatorExpressionSyntax>()
                    .Any(pipe => pipe.Kind == SyntaxKind.PipeExpression && pipe.Right == invocation));
            if (invocation is null)
                continue;

            if (!TryResolvePipeBoundInvocationTargetSymbol(semanticModel, invocation, out var symbol) ||
                symbol is null)
            {
                continue;
            }

            resolution = new SymbolResolutionResult(SymbolResolutionKind.InvocationTarget, symbol.UnderlyingSymbol, invocation.Expression);
            return true;
        }

        return false;
    }

    private static SymbolResolutionKind GetResolutionKindForNode(SyntaxNode node, SyntaxToken token)
    {
        if (IsInvocationTargetPosition(node, token))
            return SymbolResolutionKind.InvocationTarget;

        if (node is InvocationExpressionSyntax)
            return SymbolResolutionKind.InvocationTarget;

        if (node is ParameterSyntax)
            return SymbolResolutionKind.ParameterDeclaration;

        if (node is MemberAccessExpressionSyntax or MemberBindingExpressionSyntax)
            return SymbolResolutionKind.MemberAccess;

        if (node is TypeSyntax || IsTypeContext(node))
            return SymbolResolutionKind.TypePosition;

        if (node is IdentifierNameSyntax identifier &&
            identifier.Parent is MemberAccessExpressionSyntax memberAccess)
        {
            if (HaveEquivalentSpan(memberAccess.Name, identifier))
                return SymbolResolutionKind.MemberSegment;

            if (HaveEquivalentSpan(memberAccess.Expression, identifier))
                return SymbolResolutionKind.MemberReceiver;
        }

        if (node.AncestorsAndSelf().Any(static n => n is PositionalPatternSyntax or SequencePatternSyntax))
            return SymbolResolutionKind.PatternLocal;

        if (node.AncestorsAndSelf().OfType<BlockStatementSyntax>().Any())
            return SymbolResolutionKind.BlockLocal;

        return token.Kind == SyntaxKind.IdentifierToken
            ? SymbolResolutionKind.Identifier
            : SymbolResolutionKind.SymbolInfo;
    }

    private static bool TryResolveMemberTokenFastPath(
        SemanticModel semanticModel,
        SyntaxToken token,
        [NotNullWhen(true)] out ISymbol? symbol)
    {
        symbol = null;

        if (token.Parent is not IdentifierNameSyntax identifier ||
            identifier.Parent is not MemberAccessExpressionSyntax memberAccess ||
            !HaveEquivalentSpan(memberAccess.Name, identifier))
        {
            return false;
        }

        symbol = ResolveExplicitMemberAccessSymbol(semanticModel, memberAccess, identifier.Identifier.Span);
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

        if (TryResolveNominalDeconstructionPatternCaseSymbol(semanticModel, node, token, out var nominalPatternCaseSymbol))
            return nominalPatternCaseSymbol;

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
            if (IsFunctionExpressionIdentifierToken(lambdaExpression, token) &&
                semanticModel.TryGetFunctionExpressionSymbol(lambdaExpression, out var declaredLambdaSymbol) &&
                declaredLambdaSymbol is not null)
            {
                return ProjectSymbolForDisplay(declaredLambdaSymbol);
            }

            ISymbol? lambdaSymbol = null;
            if (TryGetSymbolInfo(semanticModel, lambdaExpression, out var lambdaSymbolInfo))
                lambdaSymbol = ChoosePreferredSymbol(lambdaSymbolInfo.Symbol, lambdaSymbolInfo.CandidateSymbols, lambdaExpression);
            if (IsFunctionExpressionIdentifierToken(lambdaExpression, token) && lambdaSymbol is not null)
                return ProjectSymbolForDisplay(lambdaSymbol);

            var lambdaTypeInfo = semanticModel.GetTypeInfo(lambdaExpression);
            var lambdaTargetType = lambdaTypeInfo.ConvertedType ?? lambdaTypeInfo.Type;
            if (lambdaTargetType is INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType)
                return delegateType;

            if (lambdaSymbol is not null)
                return ProjectSymbolForDisplay(lambdaSymbol);
        }

        if (TryResolveEnclosingLambdaParameterReference(semanticModel, node, token, out var lambdaParameterSymbol))
            return lambdaParameterSymbol;

        if (TryResolvePatternDeclaredSymbol(semanticModel, node, token, out var patternSymbol))
            return patternSymbol;

        if (TryResolveTypePositionSymbol(semanticModel, node, token, out var typePositionSymbol))
            return typePositionSymbol;

        if (TryResolveMemberSegmentSymbol(semanticModel, node, token, out var memberSegmentSymbol))
            return memberSegmentSymbol;

        if (TryResolveMemberReceiverSymbol(semanticModel, node, token, out var receiverSymbol))
            return receiverSymbol;

        if (IsInvocationTargetPosition(node, token))
            return null;

        if (TryGetSymbolInfo(semanticModel, node, out var symbolInfo) &&
            (symbolInfo.Symbol is not null || !symbolInfo.CandidateSymbols.IsDefaultOrEmpty))
        {
            var chosen = ChoosePreferredSymbol(symbolInfo.Symbol, symbolInfo.CandidateSymbols, node);
            if (chosen is null)
                return null;

            if (IsTypeContext(node))
                chosen = ProjectTypeContextSymbol(chosen);

            return chosen;
        }

        if (TryResolvePatternOperationDeclaredLocal(semanticModel, node, token, out var patternOperationSymbol))
            return patternOperationSymbol;

        if (TryResolveContainingStatementDesignatorSymbol(semanticModel, node, token, out var statementDesignatorSymbol))
            return statementDesignatorSymbol;

        if (TryResolveFromEnclosingBlockLocals(semanticModel, node, token, out var blockLocalSymbol))
            return blockLocalSymbol;

        var operation = TryGetOperation(semanticModel, node);
        var tokenReferencedSymbol = FindReferencedSymbolAtToken(operation, token.Span);
        if (tokenReferencedSymbol is not null)
            return ProjectSymbolForDisplay(tokenReferencedSymbol);

        var operationSymbol = operation is null ? null : GetOperationSymbol(operation);

        if (operationSymbol is not null)
            return ProjectSymbolForDisplay(operationSymbol);

        var containingStatement = node.AncestorsAndSelf().OfType<StatementSyntax>().FirstOrDefault();
        if (containingStatement is not null)
        {
            var statementOperation = TryGetOperation(semanticModel, containingStatement);
            var statementSymbol = FindReferencedSymbolAtToken(statementOperation, token.Span);
            if (statementSymbol is not null)
                return ProjectSymbolForDisplay(statementSymbol);
        }

        if (TryResolveByDeclaringSyntaxReference(semanticModel, token, out var declaringReferenceSymbol))
            return ProjectSymbolForDisplay(declaringReferenceSymbol);

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
        if (TryGetSymbolInfo(semanticModel, memberAccess.Name, out var nameInfo) &&
            (nameInfo.Symbol is not null || !nameInfo.CandidateSymbols.IsDefaultOrEmpty))
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
        if (TryGetSymbolInfo(semanticModel, memberAccess, out var memberInfo) &&
            (memberInfo.Symbol is not null || !memberInfo.CandidateSymbols.IsDefaultOrEmpty))
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

    private static bool TryResolveNominalDeconstructionPatternCaseSymbol(
        SemanticModel semanticModel,
        SyntaxNode node,
        SyntaxToken token,
        out ISymbol? symbol)
    {
        symbol = null;

        if (node is not IdentifierNameSyntax identifier ||
            token != identifier.Identifier ||
            identifier.Parent is not NominalDeconstructionPatternSyntax nominalPattern ||
            nominalPattern.Type != identifier)
        {
            return false;
        }

        var caseName = identifier.Identifier.ValueText;
        if (string.IsNullOrWhiteSpace(caseName))
            return false;

        ITypeSymbol? scrutineeType = null;

        if (nominalPattern.GetAncestor<MatchExpressionSyntax>() is { } matchExpression)
            scrutineeType = semanticModel.GetTypeInfo(matchExpression.Expression).Type;
        else if (nominalPattern.GetAncestor<MatchStatementSyntax>() is { } matchStatement)
            scrutineeType = semanticModel.GetTypeInfo(matchStatement.Expression).Type;

        if (scrutineeType is null)
            return false;

        IUnionSymbol? union = null;
        if (scrutineeType is IUnionSymbol unionType && scrutineeType.IsUnion)
            union = unionType;
        else if (scrutineeType is IUnionCaseTypeSymbol caseType && scrutineeType.IsUnionCase)
            union = caseType.Union;

        if (union is null)
            return false;

        var caseSymbol = union.CaseTypes.FirstOrDefault(c => string.Equals(c.Name, caseName, StringComparison.Ordinal));
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

        if (TryGetSymbolInfo(semanticModel, pipeExpr, out var pipeInfo) &&
            pipeInfo.Symbol is not null)
        {
            symbol = ProjectSymbolForDisplay(pipeInfo.Symbol);
            return symbol is not null;
        }

        if (TryGetSymbolInfo(semanticModel, pipeExpr, out pipeInfo) &&
            !pipeInfo.CandidateSymbols.IsDefaultOrEmpty)
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

}

internal enum SymbolResolutionKind
{
    MemberAccess,
    Identifier,
    InvocationTarget,
    ParameterDeclaration,
    MemberSegment,
    MemberReceiver,
    TypePosition,
    PatternLocal,
    BlockLocal,
    SymbolInfo,
    Declaration
}

internal readonly record struct SymbolResolutionResult(SymbolResolutionKind Kind, ISymbol Symbol, SyntaxNode Node);
internal readonly record struct CandidateNode(SyntaxNode Node, SyntaxToken Token);
