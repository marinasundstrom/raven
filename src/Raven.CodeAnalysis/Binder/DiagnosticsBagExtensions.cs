namespace Raven.CodeAnalysis;

public static class DiagnosticBagExtensions
{
    public static void ReportUndefinedName(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.UndefinedName, location, name));

    public static void ReportUndefinedBinaryOperator(this DiagnosticBag diagnostics, string operatorName, ITypeSymbol leftType, ITypeSymbol rightType, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.UndefinedBinaryOperator, location, operatorName, leftType, rightType));

    public static void ReportInvalidInvocation(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.InvalidInvocation, location));

    public static void NotAMethod(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.NotAMethod, location, name));

    public static void ReportArgumentCountMismatch(this DiagnosticBag diagnostics, string methodName, int expectedCount, int actualCount, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.ArgumentCountMismatch, location, methodName, expectedCount, actualCount));

    public static void ReportSymbolNotFound(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.SymbolNotFound, location, name));

    public static void ReportNoMatchingOverload(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.NoMatchingOverload, location, name));

    public static void ReportAmbiguousMethod(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.AmbiguousMethod, location, name));
}

public static class DiagnosticDescriptors
{
    public static readonly DiagnosticDescriptor UndefinedName =
        DiagnosticDescriptor.Create(
            id: "RAV001",
            title: "Undefined name",
            description: null,
            helpLinkUri: "",
            messageFormat: "The name '{0}' does not exist in the current context.",
            category: "Semantic",
            defaultSeverity: DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor UndefinedBinaryOperator =
        DiagnosticDescriptor.Create(
            id: "RAV002",
            title: "Binary operator undefined",
            description: null,
            helpLinkUri: "",
            messageFormat: "The binary operator '{0}' is not defined for types '{1}' and '{2}'.",
            category: "Semantic",
            defaultSeverity: DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor InvalidInvocation =
        DiagnosticDescriptor.Create(
            id: "RAV003",
            title: "Invalid invocation",
            description: null,
            helpLinkUri: "",
            messageFormat: "Invalid invocation expression.",
            category: "Semantic",
            defaultSeverity: DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor NotAMethod =
        DiagnosticDescriptor.Create(
            id: "RAV004",
            title: "Not a method",
            description: null,
            helpLinkUri: "",
            messageFormat: "'{0}' is not a method.",
            category: "Semantic",
            defaultSeverity: DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor ArgumentCountMismatch =
        DiagnosticDescriptor.Create(
            id: "RAV005",
            title: "Argument count mismatch",
            description: null,
            helpLinkUri: "",
            messageFormat: "Method '{0}' expects {1} arguments but got {2}.",
            category: "Semantic",
            defaultSeverity: DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor SymbolNotFound =
            DiagnosticDescriptor.Create(
            id: "RAV006",
            title: "Symbol not found",
            description: null,
            helpLinkUri: "",
            messageFormat: "No symbol named '{0}' could be found.",
            category: "Semantic",
            defaultSeverity: DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor NoMatchingOverload =
        DiagnosticDescriptor.Create(
        id: "RAV007",
        title: "No matching overload",
        description: null,
        helpLinkUri: "",
        messageFormat: "No matching overload for method '{0}' was found with the given argument types.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor AmbiguousMethod =
    DiagnosticDescriptor.Create(
        id: "RAV008",
        title: "Ambiguous method call",
        description: null,
        helpLinkUri: "",
        messageFormat: "The call to method '{0}' is ambiguous.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error);
}