using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class MethodBodyBinder : BlockBinder
{
    private readonly IMethodSymbol _methodSymbol;

    public MethodBodyBinder(IMethodSymbol methodSymbol, Binder parent)
        : base(methodSymbol, parent)
    {
        _methodSymbol = methodSymbol;
    }

    public override BoundBlockStatement BindBlockStatement(BlockStatementSyntax block)
    {
        var bound = base.BindBlockStatement(block);
        if (!IsMethodLikeBodyBlock(block))
            return bound;

        return FinalizeMethodBody(block, bound, () => base.BindBlockStatement(block));
    }

    private static bool IsMethodLikeBodyBlock(BlockStatementSyntax block)
    {
        return block.Parent switch
        {
            BaseMethodDeclarationSyntax => true,
            FunctionStatementSyntax => true,
            AccessorDeclarationSyntax => true,
            _ => false,
        };
    }

    private BoundBlockStatement BindArrowExpressionClause(ArrowExpressionClauseSyntax clause)
    {
        if (TryGetCachedBoundNode(clause) is BoundBlockStatement cached)
            return cached;

        var bound = BindArrowExpressionClauseCore(clause);
        return FinalizeMethodBody(clause, bound, () => BindArrowExpressionClauseCore(clause));
    }

    private BoundBlockStatement BindArrowExpressionClauseCore(ArrowExpressionClauseSyntax clause)
    {
        var expression = BindExpression(clause.Expression);

        if (expression is BoundBlockExpression blockExpression)
            return new BoundBlockStatement(blockExpression.Statements, blockExpression.LocalsToDispose);

        BoundStatement statement;
        if (_methodSymbol.ReturnType.SpecialType == SpecialType.System_Unit)
        {
            statement = new BoundExpressionStatement(expression);
        }
        else
        {
            var targetType = GetTrailingExpressionTargetType(_methodSymbol);
            var convertedExpression = expression;

            if (convertedExpression.Type is { } sourceType &&
                !targetType.ContainsErrorType() &&
                !sourceType.ContainsErrorType())
            {
                if (!IsAssignable(targetType, sourceType, out var conversion))
                {
                    ReportCannotConvertFromTypeToType(
                        sourceType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        targetType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        clause.Expression.GetLocation());
                    convertedExpression = new BoundErrorExpression(targetType, null, BoundExpressionReason.TypeMismatch);
                }
                else if (!SymbolEqualityComparer.Default.Equals(sourceType, targetType))
                {
                    convertedExpression = new BoundConversionExpression(convertedExpression, targetType, conversion);
                }
            }

            statement = new BoundReturnStatement(ValidateByRefReturnExpression(_methodSymbol, convertedExpression, clause.Expression));
        }

        return new BoundBlockStatement([statement]);
    }

    private BoundBlockStatement FinalizeMethodBody(SyntaxNode bodySyntax, BoundBlockStatement bound, Func<BoundBlockStatement> rebind)
    {
        if (_methodSymbol is SourceMethodSymbol
            {
                RequiresAsyncReturnTypeInference: true,
                AsyncReturnTypeInferenceComplete: false
            } sourceMethod)
        {
            var inferredReturnType = AsyncReturnTypeUtilities.InferAsyncReturnType(Compilation, bound);
            sourceMethod.SetReturnType(inferredReturnType);
            sourceMethod.CompleteAsyncReturnTypeInference();

            RemoveCachedBoundNode(bodySyntax);
            bound = rebind();
        }

        var unit = Compilation.UnitTypeSymbol;
        var skipTrailingExpressionCheck = ShouldSkipTrailingExpressionCheck(unit);

        if (!skipTrailingExpressionCheck &&
            !SymbolEqualityComparer.Default.Equals(GetTrailingExpressionTargetType(_methodSymbol), unit))
        {
            if (bound.Statements.LastOrDefault() is BoundExpressionStatement exprStmt &&
                exprStmt.Expression.Type is ITypeSymbol t &&
                !_methodSymbol.ReturnType.ContainsErrorType() &&
                !t.ContainsErrorType() &&
                !IsAssignable(GetTrailingExpressionTargetType(_methodSymbol), t, out _))
            {
                var exprLocation = bodySyntax switch
                {
                    BlockStatementSyntax block when block.Statements.LastOrDefault() is ExpressionStatementSyntax lastExpr
                        => lastExpr.Expression.GetLocation(),
                    ArrowExpressionClauseSyntax arrow => arrow.Expression.GetLocation(),
                    _ => bodySyntax.GetLocation()
                };
                ReportCannotConvertFromTypeToType(
                    t.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    GetTrailingExpressionTargetType(_methodSymbol).ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    exprLocation);
            }
        }

        if (_methodSymbol is SourceMethodSymbol { IsAsync: true } asyncMethod)
            AnalyzeAsyncBody(bodySyntax, asyncMethod, bound);

        CacheBoundNode(bodySyntax, bound);
        return bound;
    }

    private void AnalyzeAsyncBody(SyntaxNode bodySyntax, SourceMethodSymbol asyncMethod, BoundBlockStatement bound)
    {
        var containsAwait = AsyncLowerer.ContainsAwait(bound) ||
            Compilation.ContainsAwaitExpressionOutsideNestedFunctions(bodySyntax);
        asyncMethod.SetContainsAwait(containsAwait);

        if (containsAwait)
            return;

        var memberDescription = AsyncDiagnosticUtilities.GetAsyncMemberDescription(asyncMethod);
        var location = AsyncDiagnosticUtilities.GetAsyncKeywordLocation(asyncMethod, bodySyntax);

        _diagnostics.ReportAsyncLacksAwait(memberDescription, location);
    }

    private bool ShouldSkipTrailingExpressionCheck(ITypeSymbol unitType)
    {
        if (_methodSymbol is SourceMethodSymbol { ShouldDeferAsyncReturnDiagnostics: true })
            return true;

        if (!_methodSymbol.IsAsync)
            return false;

        var returnType = _methodSymbol.ReturnType;

        if (returnType is ErrorTypeSymbol)
            return false;

        if (AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, returnType) is { } resultType)
        {
            if (resultType is not null && SymbolEqualityComparer.Default.Equals(resultType, unitType))
                return true;
        }

        return false;
    }

    private ITypeSymbol GetTrailingExpressionTargetType(IMethodSymbol method)
    {
        if (method is SourceMethodSymbol { HasAsyncReturnTypeError: true } or SourceLambdaSymbol { HasAsyncReturnTypeError: true })
            return method.ReturnType;

        var returnType = method.ReturnType;
        if (returnType is ErrorTypeSymbol)
            return returnType;

        if (method.IsAsync &&
            AsyncReturnTypeUtilities.ExtractAsyncResultType(compilation: Compilation, asyncReturnType: returnType) is { } resultType &&
            resultType.SpecialType is not SpecialType.System_Unit and not SpecialType.System_Void)
        {
            return resultType;
        }

        return returnType;
    }

    public override BoundNode GetOrBind(SyntaxNode node)
    {
        if (TryGetCachedBoundNode(node) is BoundNode cached)
            return cached;

        if (node is BlockStatementSyntax blockStmt)
            return BindBlockStatement(blockStmt);
        if (node is BlockSyntax block)
            return BindBlock(block, allowReturn: true);
        if (node is ArrowExpressionClauseSyntax arrow)
            return BindArrowExpressionClause(arrow);

        return base.GetOrBind(node);
    }
}
