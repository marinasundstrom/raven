using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal static class RefSafetyDiagnosticReporter
{
    public static void ReportCaptures(
        ImmutableArray<ISymbol> capturedVariables,
        Location fallbackLocation,
        DiagnosticBag diagnostics)
    {
        foreach (var captured in capturedVariables)
        {
            var scopedKind = captured switch
            {
                IParameterSymbol parameter => parameter.ScopedKind,
                ILocalSymbol local => local.ScopedKind,
                _ => ScopedKind.None,
            };
            if (scopedKind != ScopedKind.None)
            {
                diagnostics.ReportScopedVariableCannotBeCaptured(
                    captured.Name,
                    captured.Locations.FirstOrDefault() ?? fallbackLocation);
                continue;
            }

            if (captured.UnwrapType() is not { } refLikeType ||
                !SemanticFacts.MayBeRefLike(refLikeType))
            {
                continue;
            }

            diagnostics.ReportRefLikeVariableCannotBeCaptured(
                captured.Name,
                refLikeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                captured.Locations.FirstOrDefault() ?? fallbackLocation);
        }
    }

    public static void ReportLocalsAcrossAwait(BoundNode body, DiagnosticBag diagnostics)
    {
        foreach (var local in AsyncLowerer.GetLocalsCapturedAcrossAwait(body))
        {
            if (local.ScopedKind != ScopedKind.None)
            {
                diagnostics.ReportScopedVariableCannotCrossSuspension(
                    local.Name,
                    local.Locations.FirstOrDefault() ?? Location.None);
                continue;
            }

            if (local.Type is not { } refLikeType || !SemanticFacts.MayBeRefLike(refLikeType))
                continue;

            diagnostics.ReportRefLikeVariableCannotCrossAwait(
                local.Name,
                refLikeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                local.Locations.FirstOrDefault() ?? Location.None);
        }
    }

    public static void ReportParametersAcrossAwait(ISymbol symbol, DiagnosticBag diagnostics)
    {
        if (symbol is not IMethodSymbol method)
            return;

        foreach (var parameter in method.Parameters)
        {
            if (parameter.ScopedKind != ScopedKind.None)
            {
                diagnostics.ReportScopedVariableCannotCrossSuspension(
                    parameter.Name,
                    parameter.Locations.FirstOrDefault() ?? Location.None);
                continue;
            }

            if (parameter.Type is not { } refLikeType || !SemanticFacts.MayBeRefLike(refLikeType))
                continue;

            diagnostics.ReportRefLikeVariableCannotCrossAwait(
                parameter.Name,
                refLikeType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                parameter.Locations.FirstOrDefault() ?? Location.None);
        }
    }

    public static void ReportIteratorStorage(
        BoundNode body,
        IMethodSymbol method,
        DiagnosticBag diagnostics)
    {
        foreach (var local in RefLikeIteratorLocalCollector.Collect(body))
            ReportSymbol(local);

        foreach (var parameter in method.Parameters)
            ReportSymbol(parameter);

        void ReportSymbol(ISymbol symbol)
        {
            var scopedKind = symbol switch
            {
                ILocalSymbol local => local.ScopedKind,
                IParameterSymbol parameter => parameter.ScopedKind,
                _ => ScopedKind.None,
            };
            if (scopedKind != ScopedKind.None)
            {
                diagnostics.ReportScopedVariableCannotCrossSuspension(
                    symbol.Name,
                    symbol.Locations.FirstOrDefault() ?? Location.None);
                return;
            }

            if (symbol.UnwrapType() is not { } type || !SemanticFacts.MayBeRefLike(type))
                return;

            diagnostics.ReportRefLikeVariableCannotBeStoredInIterator(
                symbol.Name,
                type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                symbol.Locations.FirstOrDefault() ?? Location.None);
        }
    }

    public static void Report(
        BoundNode body,
        Location fallbackLocation,
        bool expressionResultEscapes,
        DiagnosticBag diagnostics)
    {
        var result = RefSafetyAnalysis.Analyze(body, expressionResultEscapes);
        foreach (var violation in result.Violations)
        {
            var location = violation.Expression switch
            {
                BoundLocalAccess localAccess => localAccess.Local.Locations.FirstOrDefault(),
                BoundVariableExpression variable => variable.Variable.Locations.FirstOrDefault(),
                _ => violation.Origin?.Locations.FirstOrDefault(),
            };

            switch (violation.Kind)
            {
                case RefSafetyViolationKind.StackAllocationEscape:
                    diagnostics.ReportStackAllocValueCannotEscape(location ?? fallbackLocation);
                    break;
                case RefSafetyViolationKind.LocalReferenceEscape:
                    diagnostics.ReportStackBoundRefLikeValueCannotEscape(location ?? fallbackLocation);
                    break;
                case RefSafetyViolationKind.ScopedValueEscape
                    when violation.Origin is IParameterSymbol parameter:
                    diagnostics.ReportScopedValueCannotEscape(
                        parameter.Name,
                        location ?? fallbackLocation);
                    break;
                case RefSafetyViolationKind.ScopedValueEscape
                    when violation.Origin is { } origin:
                    diagnostics.ReportScopedLocalCannotEscape(
                        origin.Name,
                        location ?? fallbackLocation);
                    break;
            }
        }
    }

    private sealed class RefLikeIteratorLocalCollector : BoundTreeWalker
    {
        private readonly HashSet<ILocalSymbol> _locals = new(SymbolEqualityComparer.Default);

        public static ImmutableArray<ILocalSymbol> Collect(BoundNode body)
        {
            var collector = new RefLikeIteratorLocalCollector();
            collector.Visit(body);
            return collector._locals.ToImmutableArray();
        }

        public override void VisitVariableDeclarator(BoundVariableDeclarator node)
        {
            _locals.Add(node.Local);
            base.VisitVariableDeclarator(node);
        }

        public override void VisitForStatement(BoundForStatement node)
        {
            if (node.Local is not null)
                _locals.Add(node.Local);

            base.VisitForStatement(node);
        }

        public override void VisitFunctionExpression(BoundFunctionExpression node)
        {
        }

        public override void VisitFunctionStatement(BoundFunctionStatement node)
        {
        }
    }
}
