namespace Raven.CodeAnalysis;

public static class DiagnosticBagExtensions
{
    public static void ReportUndefinedName(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.UndefinedName, location, name));

    public static void ReportUndefinedBinaryOperator(this DiagnosticBag diagnostics, string operatorName, ITypeSymbol leftType, ITypeSymbol rightType, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.UndefinedBinaryOperator, location, operatorName, leftType, rightType));

    public static void ReportInvalidInvocation(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.InvalidInvocation, location));

    public static void NotAMethod(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.NotAMethod, location, name));

    public static void ReportArgumentCountMismatch(this DiagnosticBag diagnostics, string methodName, int expectedCount, int actualCount, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.ArgumentCountMismatch, location, methodName, expectedCount, actualCount));

    public static void ReportSymbolNotFound(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.SymbolNotFound, location, name));

    public static void ReportNoMatchingOverload(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.NoMatchingOverload, location, name));

    public static void ReportAmbiguousMethod(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.AmbiguousMethod, location, name));

    //public static void ReportInvalidMemberAccess(this DiagnosticBag diagnostics, string memberName, Location location)
    //    => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.AmbiguousMethod, location, name));

    public static void ReportMemberAccessOnVoid(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.MemberAccessOnVoid, location, name));
}
