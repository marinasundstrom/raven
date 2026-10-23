using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

// Names understood by compiler conveniences for Result. Payload members and
// arbitrary user unions are separate contracts, not aliases of these names.
internal static class ResultFacts
{
    public const string OkCaseName = "Ok";
    public const string FailureCaseName = "Failure";
    public const string LegacyErrorCaseName = "Error";

    public static bool IsCaseName(string name)
        => name is OkCaseName or FailureCaseName or LegacyErrorCaseName;

    public static IUnionCaseTypeSymbol? FindFailureCase(IUnionSymbol union)
        => union.DeclaredCaseTypes.FirstOrDefault(@case => @case.Name == FailureCaseName)
            ?? union.DeclaredCaseTypes.FirstOrDefault(@case => @case.Name == LegacyErrorCaseName);

    public static bool TryFindFailureCaseType(INamedTypeSymbol type, out INamedTypeSymbol caseType)
        => type.TryFindUnionCaseType(FailureCaseName, out caseType)
            || type.TryFindUnionCaseType(LegacyErrorCaseName, out caseType);
}
