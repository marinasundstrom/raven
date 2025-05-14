namespace Raven.CodeAnalysis;

public static class DiagnosticBagExtensions
{
    public static void ReportUndefinedName(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.UndefinedName, location, name));

    public static void ReportUndefinedBinaryOperator(this DiagnosticBag diagnostics, string operatorName, ITypeSymbol leftType, ITypeSymbol rightType, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.UndefinedBinaryOperator, location, operatorName, leftType, rightType));

    public static void ReportInvalidInvocation(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.InvalidInvocation, location));

    public static void ReportNotInvocable(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.NotInvocable, location, name));

    public static void ReportArgumentCountMismatch(this DiagnosticBag diagnostics, string methodName, int expectedCount, int actualCount, Location location)
        => diagnostics.Report(Diagnostic.Create(DiagnosticDescriptors.ArgumentCountMismatch, location, methodName, expectedCount, actualCount));
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

    public static readonly DiagnosticDescriptor NotInvocable =
        DiagnosticDescriptor.Create(
            id: "RAV004",
            title: "Not invocable",
            description: null,
            helpLinkUri: "",
            messageFormat: "'{0}' is not invocable.",
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
}