using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private const int MaxMacroFragmentNestingDepth = 16;

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

        var parentBinder = GetBinder(expression);
        return GetMacroFragmentSemanticInfo(
            expression,
            region,
            position,
            parentBinder,
            MacroFragmentBinder.CreateVisibleSymbols(
                GetVisibleValueSymbols(expression, allowBindingFallback: true)),
            expression,
            nestingDepth: 0,
            cancellationToken);
    }

    /// <summary>
    /// Gets token metadata at an authored position inside a token-tree macro,
    /// including token-tree macros nested in reported Raven fragments.
    /// </summary>
    public MacroTokenInfo? GetMacroTokenInfo(
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
        return GetMacroTokenInfo(
            expression,
            position,
            expression,
            nestingDepth: 0,
            cancellationToken);
    }

    private MacroTokenInfo? GetMacroTokenInfo(
        FreestandingMacroExpressionSyntax expression,
        int position,
        SyntaxNode resolutionContext,
        int nestingDepth,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        var context = new TokenTreeMacroContext(Compilation, this, expression, cancellationToken);
        var snapshot = nestingDepth == 0
            ? GetMacroInputSnapshot(expression, cancellationToken)
            : new MacroInputSnapshot(
                context.BodySpan,
                MacroTokenInfoService.GetTokens(this, expression, resolutionContext, cancellationToken),
                MacroFragmentRegionService.GetFragmentRegions(this, expression, resolutionContext, cancellationToken));

        var region = snapshot.FindFragmentRegion(position);
        if (region is not null && nestingDepth < MaxMacroFragmentNestingDepth)
        {
            SyntaxNode? fragment = region.Kind switch
            {
                MacroFragmentKind.Expression => context.ParseExpression(region.BodyRelativeSpan),
                MacroFragmentKind.Statement => context.ParseStatement(region.BodyRelativeSpan),
                _ => null
            };
            if (fragment is not null)
            {
                var searchPosition = Math.Clamp(position, fragment.FullSpan.Start, fragment.FullSpan.End);
                if (searchPosition == fragment.FullSpan.End && searchPosition > fragment.FullSpan.Start)
                    searchPosition--;
                var token = fragment.FindToken(searchPosition);
                foreach (var nestedInvocation in token.Parent?.AncestorsAndSelf()
                    .OfType<FreestandingMacroExpressionSyntax>() ?? [])
                {
                    if (nestedInvocation.TokenTree?.Span.Contains(searchPosition) != true)
                        continue;

                    var nestedInfo = GetMacroTokenInfo(
                        nestedInvocation,
                        position,
                        resolutionContext,
                        nestingDepth + 1,
                        cancellationToken);
                    if (nestedInfo is not null)
                        return nestedInfo;
                }
            }
        }

        return snapshot.FindToken(position);
    }

    private MacroFragmentSemanticInfo? GetMacroFragmentSemanticInfo(
        FreestandingMacroExpressionSyntax expression,
        MacroFragmentRegion region,
        int position,
        Binder parentBinder,
        ImmutableArray<MacroFragmentVisibleSymbol> visibleSymbols,
        FreestandingMacroExpressionSyntax resolutionContext,
        int nestingDepth,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

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

        var binder = new MacroFragmentBinder(
            parentBinder,
            region.Locals,
            visibleSymbols,
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

        if (nestingDepth < MaxMacroFragmentNestingDepth)
        {
            foreach (var nestedInvocation in token.Parent.AncestorsAndSelf().OfType<FreestandingMacroExpressionSyntax>())
            {
                if (nestedInvocation.TokenTree?.Span.Contains(searchPosition) != true ||
                    !binder.TryGetNestedMacroVisibleSymbols(nestedInvocation, out var nestedVisibleSymbols))
                {
                    continue;
                }

                var nestedRegion = FindFragmentRegion(
                    MacroFragmentRegionService.GetFragmentRegions(
                        this,
                        nestedInvocation,
                        resolutionContext,
                        cancellationToken),
                    position);
                if (nestedRegion is null)
                    continue;

                var nestedInfo = GetMacroFragmentSemanticInfo(
                    nestedInvocation,
                    nestedRegion,
                    position,
                    binder,
                    nestedVisibleSymbols,
                    resolutionContext,
                    nestingDepth + 1,
                    cancellationToken);
                if (nestedInfo is not null)
                    return nestedInfo;
            }
        }

        foreach (var candidate in token.Parent.AncestorsAndSelf())
        {
            if (!fragment.FullSpan.Contains(candidate.Span))
                break;

            if (candidate is FreestandingMacroExpressionSyntax nestedInvocation &&
                nestedInvocation.TokenTree?.Span.Contains(searchPosition) == true)
            {
                continue;
            }

            var bound = TryGetCachedBoundNode(candidate);
            var symbolInfo = bound switch
            {
                BoundExpression boundExpression => boundExpression.GetSymbolInfo(),
                BoundStatement boundStatement => boundStatement.GetSymbolInfo(),
                _ => SymbolInfo.None
            };
            if (symbolInfo.Symbol is null && symbolInfo.CandidateSymbols.IsDefaultOrEmpty)
                continue;

            symbolInfo = UseAuthoredLocalName(symbolInfo, token.ValueText);

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

    private static SymbolInfo UseAuthoredLocalName(SymbolInfo symbolInfo, string authoredName)
    {
        if (symbolInfo.Symbol is not ILocalSymbol local ||
            !local.IsImplicitlyDeclared ||
            string.IsNullOrWhiteSpace(authoredName) ||
            string.Equals(local.Name, authoredName, StringComparison.Ordinal))
        {
            return symbolInfo;
        }

        var authoredLocal = new SourceLocalSymbol(
            authoredName,
            local.Type,
            local.IsMutable,
            local.ContainingSymbol!,
            local.ContainingType,
            local.ContainingNamespace,
            local.Locations.ToArray(),
            local.DeclaringSyntaxReferences.ToArray(),
            local.IsConst,
            local.ConstantValue,
            local.ScopedKind,
            isImplicitlyDeclared: true);
        return new SymbolInfo(authoredLocal);
    }

    private static MacroFragmentRegion? FindFragmentRegion(
        ImmutableArray<MacroFragmentRegion> regions,
        int position)
    {
        MacroFragmentRegion? best = null;
        foreach (var region in regions)
        {
            var containsPosition = region.Span.Length == 0
                ? position == region.Span.Start
                : position >= region.Span.Start && position <= region.Span.End;
            if (!containsPosition)
                continue;

            if (best is null || region.Span.Length < best.Span.Length)
                best = region;
        }

        return best;
    }
}
