using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    /// <summary>
    /// Gets ordinary Raven semantic information at an authored position inside a
    /// fragment reported by a token-tree macro.
    /// </summary>
    public MacroFragmentSemanticInfo? GetMacroFragmentSemanticInfo(
        FreestandingMacroExpressionSyntax expression,
        int position,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(expression);
        if (expression.SyntaxTree != SyntaxTree)
            throw new ArgumentException("Macro invocation is not part of this semantic model's syntax tree.", nameof(expression));
        if ((uint)position > (uint)SyntaxTree.GetRoot(cancellationToken).FullSpan.End)
            throw new ArgumentOutOfRangeException(nameof(position));

        using var semanticAccess = EnterSemanticAccess(cancellationToken);
        using var semanticQueryBinding = EnterSemanticQueryBinding();

        var region = GetMacroInputSnapshot(expression, cancellationToken).FindFragmentRegion(position);
        if (region is null)
            return null;

        var context = new TokenTreeMacroContext(Compilation, this, expression, cancellationToken);
        SyntaxNode fragment;
        switch (region.Kind)
        {
            case MacroFragmentKind.Expression:
                fragment = context.ParseExpression(region.BodyRelativeSpan);
                break;
            case MacroFragmentKind.Statement:
                fragment = context.ParseStatement(region.BodyRelativeSpan);
                break;
            default:
                return null;
        }

        var parentBinder = GetBinder(expression);
        var binder = new MacroFragmentBinder(
            parentBinder,
            region.Locals,
            GetVisibleValueSymbols(expression, allowBindingFallback: true),
            SyntaxTree);
        switch (fragment)
        {
            case ExpressionSyntax fragmentExpression:
                binder.BindExpression(fragmentExpression);
                break;
            case StatementSyntax fragmentStatement:
                binder.BindStatement(fragmentStatement);
                break;
        }

        var searchPosition = Math.Clamp(position, fragment.FullSpan.Start, fragment.FullSpan.End);
        if (searchPosition == fragment.FullSpan.End && searchPosition > fragment.FullSpan.Start)
            searchPosition--;
        var token = fragment.FindToken(searchPosition);
        if (token.Kind == SyntaxKind.None || token.Parent is null)
            return null;

        foreach (var candidate in token.Parent.AncestorsAndSelf())
        {
            if (!fragment.FullSpan.Contains(candidate.Span))
                break;

            var bound = TryGetCachedBoundNode(candidate);
            var symbolInfo = bound switch
            {
                BoundExpression boundExpression => boundExpression.GetSymbolInfo(),
                BoundStatement boundStatement => boundStatement.GetSymbolInfo(),
                _ => SymbolInfo.None
            };
            if (symbolInfo.Symbol is null && symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
                continue;

            var type = bound is BoundExpression expressionNode ? expressionNode.Type : null;
            return new MacroFragmentSemanticInfo(
                region,
                token.Span,
                symbolInfo,
                new TypeInfo(type, type),
                token.Parent);
        }

        return null;
    }
}
