namespace Raven.CodeAnalysis;

internal class CompilerDiagnostics
{
    /// <summary>
    /// Identifier; expected
    /// </summary>
    public static readonly DiagnosticDescriptor IdentifierExpected = DiagnosticDescriptor.Create(
        id: "RAV1001",
        title: "Identifier expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "Identifier; expected",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// ';' expected
    /// </summary>
    public static readonly DiagnosticDescriptor SemicolonExpected = DiagnosticDescriptor.Create(
        id: "RAV1002",
        title: "Semicolon expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "';' expected",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// '{0}' expected
    /// </summary>
    public static readonly DiagnosticDescriptor CharacterExpected = DiagnosticDescriptor.Create(
        id: "RAV1003",
        title: "Character expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "'{0}' expected",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// Duplicate '{0}' modifier
    /// </summary>
    public static readonly DiagnosticDescriptor DuplicateModifier = DiagnosticDescriptor.Create(
        id: "RAV1004",
        title: "Duplicate modifier",
        description: "",
        helpLinkUri: "",
        messageFormat: "Duplicate '{0}' modifier",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// Unrecognized escape sequence
    /// </summary>
    public static readonly DiagnosticDescriptor UnrecognizedEscapeSequence = DiagnosticDescriptor.Create(
        id: "RAV1009",
        title: "Unrecognized escape sequence",
        description: "",
        helpLinkUri: "",
        messageFormat: "Unrecognized escape sequence",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// Newline in constant
    /// </summary>
    public static readonly DiagnosticDescriptor NewlineInConstant = DiagnosticDescriptor.Create(
        id: "RAV1010",
        title: "Newline in constant",
        description: "",
        helpLinkUri: "",
        messageFormat: "Newline in constant",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// Method name expected
    /// </summary>
    public static readonly DiagnosticDescriptor MethodNameExpected = DiagnosticDescriptor.Create(
        id: "RAV0149",
        title: "Method name expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "Method name expected",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// Non-invocable member '{0}' cannot be used like a method
    /// </summary>
    public static readonly DiagnosticDescriptor NonInvocableMember = DiagnosticDescriptor.Create(
        id: "RAV1955",
        title: "Non-invocable member",
        description: "",
        helpLinkUri: "",
        messageFormat: "Non-invocable member '{0}' cannot be used like a method",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// Use of unassigned out parameter '{0}'
    /// </summary>
    public static readonly DiagnosticDescriptor UnassignedOutParameter = DiagnosticDescriptor.Create(
        id: "RAV0269",
        title: "Unassigned out parameter",
        description: "",
        helpLinkUri: "",
        messageFormat: "Use of unassigned out parameter '{0}'",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// '{0}' is a variable but is used like a type
    /// </summary>
    public static readonly DiagnosticDescriptor VariableUsedLikeAType = DiagnosticDescriptor.Create(
        id: "RAV0118",
        title: "Variable used like a type",
        description: "",
        helpLinkUri: "",
        messageFormat: "'{0}' is a variable but is used like a type",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// Use of unassigned local variable '{0}'
    /// </summary>
    public static readonly DiagnosticDescriptor UseOfUnassignedVariable = DiagnosticDescriptor.Create(
        id: "RAV0165",
        title: "Use of unassigned local variable",
        description: "",
        helpLinkUri: "",
        messageFormat: "Use of unassigned local variable '{0}'",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// '{0}' does not contain a definition for '{1}'
    /// </summary>
    public static readonly DiagnosticDescriptor MemberDoesNotContainDefinition = DiagnosticDescriptor.Create(
        id: "RAV0117",
        title: "Member does not contain definition",
        description: "",
        helpLinkUri: "",
        messageFormat: "'{0}' does not contain a definition for '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// Operator '{0}' cannot be applied to operand of type '{1}'
    /// </summary>
    public static readonly DiagnosticDescriptor OperatorCannotBeAppliedToOperandOfType = DiagnosticDescriptor.Create(
        id: "RAV0023",
        title: "Operator cannot be applied to operand of specific type",
        description: "",
        helpLinkUri: "",
        messageFormat: "Operator '{0}' cannot be applied to operand of type '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// The type name '{0}' does not exist in the type '{1}'
    /// </summary>
    public static readonly DiagnosticDescriptor TypeNameDoesNotExistInType = DiagnosticDescriptor.Create(
        id: "RAV0426",
        title: "Type name does not exist in type",
        description: "",
        helpLinkUri: "",
        messageFormat: "The type name '{0}' does not exist in the type '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// The call is ambiguous between the following methods or properties: '{0}' and '{1}'
    /// </summary>
    public static readonly DiagnosticDescriptor CallIsAmbiguous = DiagnosticDescriptor.Create(
        id: "RAV0121",
        title: "Call is ambiguous",
        description: "",
        helpLinkUri: "",
        messageFormat: "The call is ambiguous between the following methods or properties: '{0}' and '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// Cannot convert from '{0}' to {1}
    /// </summary>
    public static readonly DiagnosticDescriptor CannotConvertFromTypeToType = DiagnosticDescriptor.Create(
        id: "RAV1503",
        title: "Cannot convert from type to type",
        description: "",
        helpLinkUri: "",
        messageFormat: "Cannot convert from '{0}' to {1}",
        category: "compiler",
        DiagnosticSeverity.Error);

    /// <summary>
    /// No overload for method {0} takes ‘{1}’ arguments
    /// </summary>
    public static readonly DiagnosticDescriptor NoOverloadForMethod = DiagnosticDescriptor.Create(
        id: "RAV1501",
        title: "No overload for method taking argument",
        description: "",
        helpLinkUri: "",
        messageFormat: "No overload for method {0} takes ‘{1}’ arguments",
        category: "compiler",
        DiagnosticSeverity.Error);
}