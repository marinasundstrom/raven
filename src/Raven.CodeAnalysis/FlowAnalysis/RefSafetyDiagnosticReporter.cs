namespace Raven.CodeAnalysis;

internal static class RefSafetyDiagnosticReporter
{
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
}
