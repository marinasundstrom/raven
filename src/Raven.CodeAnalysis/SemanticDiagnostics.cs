namespace Raven.CodeAnalysis;

public static class SemanticDiagnostics
{
    private static DiagnosticDescriptor[]? _allDescriptors;

    private static DiagnosticDescriptor? _invalidInvocation;
    private static DiagnosticDescriptor? _memberAccessOnVoid;

    /// <summary>
    /// RAV0030: Invalid invocation expression
    /// </summary>
    public static DiagnosticDescriptor InvalidInvocation => _invalidInvocation ??= DiagnosticDescriptor.Create(
        id: "RAV0030",
        title: "Invalid invocation",
        description: "",
        helpLinkUri: "",
        messageFormat: "Invalid invocation expression.",
        category: "compiler",
        defaultSeverity: DiagnosticSeverity.Error);

    /// <summary>
    /// RAV1526: Cannot access member '{0}' on a value of type 'void'
    /// </summary>
    public static DiagnosticDescriptor MemberAccessOnVoid => _memberAccessOnVoid ??= DiagnosticDescriptor.Create(
        id: "RAV1526",
        title: "Member access on void value",
        description: "Cannot access a member on a value of type 'void'.",
        helpLinkUri: "",
        messageFormat: "Cannot access member '{0}' on a value of type 'void'.",
        category: "compiler",
        defaultSeverity: DiagnosticSeverity.Error);

    public static DiagnosticDescriptor[] All => _allDescriptors ??= [
        InvalidInvocation,
        MemberAccessOnVoid
    ];

    public static DiagnosticDescriptor? GetDescriptor(string diagnosticId) => diagnosticId switch
    {
        "RAV0030" => InvalidInvocation,
        "RAV1526" => MemberAccessOnVoid,
        _ => null
    };
}