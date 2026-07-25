using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class RefSafetyContractAnalyzer
{
    public static void ReportWeakenedParameterContracts(
        IMethodSymbol contractMethod,
        IMethodSymbol implementationMethod,
        SeparatedSyntaxList<ParameterSyntax> parameterSyntaxes,
        DiagnosticBag diagnostics)
    {
        var count = Math.Min(contractMethod.Parameters.Length, implementationMethod.Parameters.Length);
        for (var i = 0; i < count; i++)
        {
            var contractParameter = contractMethod.Parameters[i];
            var implementationParameter = implementationMethod.Parameters[i];
            if (contractParameter.ScopedKind == ScopedKind.None ||
                implementationParameter.ScopedKind != ScopedKind.None)
            {
                continue;
            }

            var location = i < parameterSyntaxes.Count
                ? parameterSyntaxes[i].Identifier.GetLocation()
                : implementationParameter.Locations.FirstOrDefault() ?? Location.None;
            diagnostics.ReportScopedParameterContractCannotBeWeakened(
                implementationParameter.Name,
                contractMethod.ToDisplayString(),
                location);
        }
    }

    public static void ReportMismatchedPartialParameters(
        ImmutableArray<ScopedKind> existingScopedKinds,
        IMethodSymbol incomingPart,
        SeparatedSyntaxList<ParameterSyntax> parameterSyntaxes,
        DiagnosticBag diagnostics)
    {
        var count = Math.Min(existingScopedKinds.Length, incomingPart.Parameters.Length);
        for (var i = 0; i < count; i++)
        {
            var incomingParameter = incomingPart.Parameters[i];
            if (existingScopedKinds[i] == incomingParameter.ScopedKind)
                continue;

            var location = i < parameterSyntaxes.Count
                ? parameterSyntaxes[i].Identifier.GetLocation()
                : incomingParameter.Locations.FirstOrDefault() ?? Location.None;
            diagnostics.ReportScopedParameterDoesNotMatchPartialDefinition(
                incomingParameter.Name,
                location);
        }
    }
}
