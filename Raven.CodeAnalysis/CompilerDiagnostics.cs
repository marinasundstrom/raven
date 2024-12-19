namespace Raven.CodeAnalysis;

internal class CompilerDiagnostics
{
    public static readonly DiagnosticDescriptor IdentifierExpected = DiagnosticDescriptor.Create(
        id: "RAV1001",
        title: "Identifier expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "Identifier; expected",
        category: "compiler",
        DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor SemicolonExpected = DiagnosticDescriptor.Create(
        id: "RAV1002",
        title: "Semicolon expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "; expected",
        category: "compiler",
        DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor CharacterExpected = DiagnosticDescriptor.Create(
        id: "RAV1003",
        title: "Character expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "'{0}' expected",
        category: "compiler",
        DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor DuplicateModifier = DiagnosticDescriptor.Create(
        id: "RAV1004",
        title: "Duplicate modifier",
        description: "",
        helpLinkUri: "",
        messageFormat: "Duplicate '{0}' modifier",
        category: "compiler",
        DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor UnrecognizedEscapeSequence = DiagnosticDescriptor.Create(
        id: "RAV1009",
        title: "Unrecognized escape sequence",
        description: "",
        helpLinkUri: "",
        messageFormat: "Unrecognized escape sequence",
        category: "compiler",
        DiagnosticSeverity.Error);

    public static readonly DiagnosticDescriptor NewlineInConstant = DiagnosticDescriptor.Create(
        id: "RAV1010",
        title: "Newline in constant",
        description: "",
        helpLinkUri: "",
        messageFormat: "Newline in constant",
        category: "compiler",
        DiagnosticSeverity.Error);
}