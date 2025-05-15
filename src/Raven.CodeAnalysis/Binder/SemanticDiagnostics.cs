namespace Raven.CodeAnalysis;

public static class SemanticDiagnostics
{
    private static DiagnosticDescriptor[]? _allDescriptors;

    private static DiagnosticDescriptor? _undefinedName;
    private static DiagnosticDescriptor? _undefinedBinaryOperator;
    private static DiagnosticDescriptor? _invalidInvocation;
    private static DiagnosticDescriptor? _notAMethod;
    private static DiagnosticDescriptor? _argumentCountMismatch;
    private static DiagnosticDescriptor? _symbolNotFound;
    private static DiagnosticDescriptor? _noMatchingOverload;
    private static DiagnosticDescriptor? _ambiguousMethod;

    public static DiagnosticDescriptor UndefinedName => _undefinedName ??= DiagnosticDescriptor.Create(
        id: "RAV001",
        title: "Undefined name",
        description: null,
        helpLinkUri: "",
        messageFormat: "The name '{0}' does not exist in the current context.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor UndefinedBinaryOperator => _undefinedBinaryOperator ??= DiagnosticDescriptor.Create(
        id: "RAV002",
        title: "Binary operator undefined",
        description: null,
        helpLinkUri: "",
        messageFormat: "The binary operator '{0}' is not defined for types '{1}' and '{2}'.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor InvalidInvocation => _invalidInvocation ??= DiagnosticDescriptor.Create(
        id: "RAV003",
        title: "Invalid invocation",
        description: null,
        helpLinkUri: "",
        messageFormat: "Invalid invocation expression.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor NotAMethod => _notAMethod ??= DiagnosticDescriptor.Create(
        id: "RAV004",
        title: "Not a method",
        description: null,
        helpLinkUri: "",
        messageFormat: "'{0}' is not a method.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor ArgumentCountMismatch => _argumentCountMismatch ??= DiagnosticDescriptor.Create(
        id: "RAV005",
        title: "Argument count mismatch",
        description: null,
        helpLinkUri: "",
        messageFormat: "Method '{0}' expects {1} arguments but got {2}.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor SymbolNotFound => _symbolNotFound ??= DiagnosticDescriptor.Create(
        id: "RAV006",
        title: "Symbol not found",
        description: null,
        helpLinkUri: "",
        messageFormat: "No symbol named '{0}' could be found.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor NoMatchingOverload => _noMatchingOverload ??= DiagnosticDescriptor.Create(
        id: "RAV007",
        title: "No matching overload",
        description: null,
        helpLinkUri: "",
        messageFormat: "No matching overload for method '{0}' was found with the given argument types.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor AmbiguousMethod => _ambiguousMethod ??= DiagnosticDescriptor.Create(
        id: "RAV008",
        title: "Ambiguous method call",
        description: null,
        helpLinkUri: "",
        messageFormat: "The call to method '{0}' is ambiguous.",
        category: "Semantic",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor[] All => _allDescriptors ??= [
        UndefinedName,
        UndefinedBinaryOperator,
        InvalidInvocation,
        NotAMethod,
        ArgumentCountMismatch,
        SymbolNotFound,
        NoMatchingOverload,
        AmbiguousMethod
    ];

    public static DiagnosticDescriptor? GetDescriptor(string diagnosticId) => diagnosticId switch
    {
        "RAV001" => UndefinedName,
        "RAV002" => UndefinedBinaryOperator,
        "RAV003" => InvalidInvocation,
        "RAV004" => NotAMethod,
        "RAV005" => ArgumentCountMismatch,
        "RAV006" => SymbolNotFound,
        "RAV007" => NoMatchingOverload,
        "RAV008" => AmbiguousMethod,
        _ => null
    };
}