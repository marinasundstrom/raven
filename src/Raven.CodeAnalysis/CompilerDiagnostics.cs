
namespace Raven.CodeAnalysis;

internal class CompilerDiagnostics
{
    private static DiagnosticDescriptor[]? _allDescriptors;

    private static DiagnosticDescriptor? _identifierExpected;
    private static DiagnosticDescriptor? _semicolonExpected;
    private static DiagnosticDescriptor? _characterExpected;
    private static DiagnosticDescriptor? _duplicateModifier;
    private static DiagnosticDescriptor? _unrecognizedEscapeSequence;
    private static DiagnosticDescriptor? _newlineInConstant;
    private static DiagnosticDescriptor? _methodNameExpected;
    private static DiagnosticDescriptor? _nonInvocableMember;
    private static DiagnosticDescriptor? _unassignedOutParameter;
    private static DiagnosticDescriptor? _variableUsedLikeAType;
    private static DiagnosticDescriptor? _useOfUnassignedVariable;
    private static DiagnosticDescriptor? _memberDoesNotContainDefinition;
    private static DiagnosticDescriptor? _operatorCannotBeAppliedToOperandOfType;
    private static DiagnosticDescriptor? _typeNameDoesNotExistInType;
    private static DiagnosticDescriptor? _callIsAmbiguous;
    private static DiagnosticDescriptor? _cannotConvertFromTypeToType;
    private static DiagnosticDescriptor? _noOverloadForMethod;
    private static DiagnosticDescriptor? _typeOrNamespaceNameDoesNotExistInTheNamespace;
    private static DiagnosticDescriptor? _typeExpectedWithoutWildcard;
    private static DiagnosticDescriptor? _invalidExpressionTerm;
    private static DiagnosticDescriptor? _theNameDoesNotExistInTheCurrentContext;
    private static DiagnosticDescriptor? _cannotAssignVoidToAnImplicitlyTypedVariable;
    private static DiagnosticDescriptor? _leftHandSideOfAssignmentMustBeAVariablePropertyOrIndexer;
    private static DiagnosticDescriptor? _readOnlyFieldCannotBeAssignedTo;
    private static DiagnosticDescriptor? _thisValueIsNotMutable;
    private static DiagnosticDescriptor? _propertyOrIndexerCannotBeAssignedIsReadOnly;
    private static DiagnosticDescriptor? _cannotApplyIndexingWithToAnExpressionOfType;
    private static DiagnosticDescriptor _numericLiteralOutOfRange;
    private static DiagnosticDescriptor? _unterminatedCharacterLiteral;
    private static DiagnosticDescriptor? _invalidEscapeSequence;
    private static DiagnosticDescriptor? _memberAccessRequiresTargetType;
    private static DiagnosticDescriptor? _invalidAliasType;
    private static DiagnosticDescriptor? _typeRequiresTypeArguments;
    private static DiagnosticDescriptor? _nullableTypeInUnion;
    private static DiagnosticDescriptor? _typeAlreadyDefinesMember;

    /// <summary>
    /// RAV1001: Identifier; expected
    /// </summary>
    public static DiagnosticDescriptor IdentifierExpected => _identifierExpected ??= DiagnosticDescriptor.Create(
        id: "RAV1001",
        title: "Identifier expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "Identifier expected",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV1002: ';' expected
    /// </summary>
    public static DiagnosticDescriptor SemicolonExpected => _semicolonExpected ??= DiagnosticDescriptor.Create(
        id: "RAV1002",
        title: "Semicolon expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "';' expected",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV1003: '{0}' expected
    /// </summary>
    public static DiagnosticDescriptor CharacterExpected => _characterExpected ??= DiagnosticDescriptor.Create(
        id: "RAV1003",
        title: "Character expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "'{0}' expected",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV1004: Duplicate '{0}' modifier
    /// </summary>
    public static DiagnosticDescriptor DuplicateModifier => _duplicateModifier ??= DiagnosticDescriptor.Create(
        id: "RAV1004",
        title: "Duplicate modifier",
        description: "",
        helpLinkUri: "",
        messageFormat: "Duplicate '{0}' modifier",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV1009: Unrecognized escape sequence
    /// </summary>
    public static DiagnosticDescriptor UnrecognizedEscapeSequence => _unrecognizedEscapeSequence ??= DiagnosticDescriptor.Create(
        id: "RAV1009",
        title: "Unrecognized escape sequence",
        description: "",
        helpLinkUri: "",
        messageFormat: "Unrecognized escape sequence",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV1010: Newline in constant
    /// </summary>
    public static DiagnosticDescriptor NewlineInConstant => _newlineInConstant ??= DiagnosticDescriptor.Create(
        id: "RAV1010",
        title: "Newline in constant",
        description: "",
        helpLinkUri: "",
        messageFormat: "Newline in constant",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0149: Method name expected
    /// </summary>
    public static DiagnosticDescriptor MethodNameExpected => _methodNameExpected ??= DiagnosticDescriptor.Create(
        id: "RAV0149",
        title: "Method name expected",
        description: "",
        helpLinkUri: "",
        messageFormat: "Method name expected",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV1955: Non-invocable member '{0}' cannot be used like a method
    /// </summary>
    public static DiagnosticDescriptor NonInvocableMember => _nonInvocableMember ??= DiagnosticDescriptor.Create(
        id: "RAV1955",
        title: "Non-invocable member",
        description: "",
        helpLinkUri: "",
        messageFormat: "Non-invocable member '{0}' cannot be used like a method",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0269: Use of unassigned out parameter '{0}'
    /// </summary>
    public static DiagnosticDescriptor UnassignedOutParameter => _unassignedOutParameter ??= DiagnosticDescriptor.Create(
        id: "RAV0269",
        title: "Unassigned out parameter",
        description: "",
        helpLinkUri: "",
        messageFormat: "Use of unassigned out parameter '{0}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0118: '{0}' is a variable but is used like a type
    /// </summary>
    public static DiagnosticDescriptor VariableUsedLikeAType => _variableUsedLikeAType ??= DiagnosticDescriptor.Create(
        id: "RAV0118",
        title: "Variable used like a type",
        description: "",
        helpLinkUri: "",
        messageFormat: "'{0}' is a variable but is used like a type",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0165: Use of unassigned local variable '{0}'
    /// </summary>
    public static DiagnosticDescriptor UseOfUnassignedVariable => _useOfUnassignedVariable ??= DiagnosticDescriptor.Create(
        id: "RAV0165",
        title: "Use of unassigned local variable",
        description: "",
        helpLinkUri: "",
        messageFormat: "Use of unassigned local variable '{0}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0117: '{0}' does not contain a definition for '{1}'
    /// </summary>
    public static DiagnosticDescriptor MemberDoesNotContainDefinition => _memberDoesNotContainDefinition ??= DiagnosticDescriptor.Create(
        id: "RAV0117",
        title: "Member does not contain definition",
        description: "",
        helpLinkUri: "",
        messageFormat: "'{0}' does not contain a definition for '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0023: Operator '{0}' cannot be applied to operand of type '{1}'
    /// </summary>
    public static DiagnosticDescriptor OperatorCannotBeAppliedToOperandOfType => _operatorCannotBeAppliedToOperandOfType ??= DiagnosticDescriptor.Create(
        id: "RAV0023",
        title: "Operator cannot be applied to operand of specific type",
        description: "",
        helpLinkUri: "",
        messageFormat: "Operator '{0}' cannot be applied to operand of type '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    // RAV0024: Operator '{0}' cannot be applied to operands of type '{1}' and '{2}'
    /// </summary>
    public static DiagnosticDescriptor OperatorCannotBeAppliedToOperandsOfTypes => _operatorCannotBeAppliedToOperandOfType ??= DiagnosticDescriptor.Create(
        id: "RAV0024",
        title: "Operator cannot be applied to operanda of specific typea",
        description: "",
        helpLinkUri: "",
        messageFormat: "Operator '{0}' cannot be applied to operands of type '{1}' and '{2}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0426: The type name '{0}' does not exist in the type '{1}'
    /// </summary>
    public static DiagnosticDescriptor TypeNameDoesNotExistInType => _typeNameDoesNotExistInType ??= DiagnosticDescriptor.Create(
        id: "RAV0426",
        title: "Type name does not exist in type",
        description: "",
        helpLinkUri: "",
        messageFormat: "The type name '{0}' does not exist in the type '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0121: The call is ambiguous between the following methods or properties: '{0}' and '{1}'
    /// </summary>
    public static DiagnosticDescriptor CallIsAmbiguous => _callIsAmbiguous ??= DiagnosticDescriptor.Create(
        id: "RAV0121",
        title: "Call is ambiguous",
        description: "",
        helpLinkUri: "",
        messageFormat: "The call is ambiguous between the following methods or properties: '{0}' and '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV1503: Cannot convert from '{0}' to {1}
    /// </summary>
    public static DiagnosticDescriptor CannotConvertFromTypeToType => _cannotConvertFromTypeToType ??= DiagnosticDescriptor.Create(
        id: "RAV1503",
        title: "Cannot convert from type to type",
        description: "",
        helpLinkUri: "",
        messageFormat: "Cannot convert from '{0}' to '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV1501: No overload for method {0} takes ‘{1}’ arguments
    /// </summary>
    public static DiagnosticDescriptor NoOverloadForMethod => _noOverloadForMethod ??= DiagnosticDescriptor.Create(
        id: "RAV1501",
        title: "No overload for method taking argument",
        description: "",
        helpLinkUri: "",
        messageFormat: "No overload for method '{0}' takes {1} arguments",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0234: The type or namespace name '{0}' does not exist in the namespace '{1}'
    /// </summary>
    public static DiagnosticDescriptor TypeOrNamespaceNameDoesNotExistInTheNamespace => _typeOrNamespaceNameDoesNotExistInTheNamespace ??= DiagnosticDescriptor.Create(
        id: "RAV0234",
        title: "Type or namespace does not exist in namespace",
        description: "",
        helpLinkUri: "",
        messageFormat: "The type or namespace name '{0}' does not exist in the namespace '{1}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0235: Type is expected without wildcard
    /// </summary>
    public static DiagnosticDescriptor TypeExpectedWithoutWildcard => _typeExpectedWithoutWildcard ??= DiagnosticDescriptor.Create(
        id: "RAV0235",
        title: "Type expected without wildcard",
        description: "",
        helpLinkUri: "",
        messageFormat: "Type is expected without wildcard",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV1525: Invalid expression term '{0}'
    /// </summary>
    public static DiagnosticDescriptor InvalidExpressionTerm => _invalidExpressionTerm ??= DiagnosticDescriptor.Create(
        id: "RAV1525",
        title: "Invalid expression term",
        description: "",
        helpLinkUri: "",
        messageFormat: "Invalid expression term '{0}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0103: The name '{0}' does not exist in the current context
    /// </summary>
    public static DiagnosticDescriptor TheNameDoesNotExistInTheCurrentContext => _theNameDoesNotExistInTheCurrentContext ??= DiagnosticDescriptor.Create(
        id: "RAV0103",
        title: "The name does not exist in the current context",
        description: "",
        helpLinkUri: "",
        messageFormat: "The name '{0}' does not exist in the current context",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0815: Cannot assign unit to an implicitly-typed variable
    /// </summary>
    public static DiagnosticDescriptor CannotAssignVoidToAnImplicitlyTypedVariable => _cannotAssignVoidToAnImplicitlyTypedVariable ??= DiagnosticDescriptor.Create(
        id: "RAV0815",
        title: "Cannot assign unit to an implicitly-typed variable",
        description: "",
        helpLinkUri: "",
        messageFormat: "Cannot assign unit to an implicitly-typed variable",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0131: The left-hand side of an assignment must be a variable, property or indexer
    /// </summary>
    public static DiagnosticDescriptor LeftHandSideOfAssignmentMustBeAVariablePropertyOrIndexer => _leftHandSideOfAssignmentMustBeAVariablePropertyOrIndexer ??= DiagnosticDescriptor.Create(
        id: "RAV0131",
        title: "The left-hand side of an assignment must be a variable, property or indexer",
        description: "",
        helpLinkUri: "",
        messageFormat: "The left-hand side of an assignment must be a variable, property or indexer",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0191: A readonly field cannot be assigned to (except in a constructor or a variable initializer)
    /// </summary>
    public static DiagnosticDescriptor ReadOnlyFieldCannotBeAssignedTo => _readOnlyFieldCannotBeAssignedTo ??= DiagnosticDescriptor.Create(
        id: "RAV0191",
        title: "A readonly field cannot be assigned to (except in a constructor or a variable initializer)",
        description: "",
        helpLinkUri: "",
        messageFormat: "A readonly field cannot be assigned to (except in a constructor or a variable initializer)",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0027: This value is not mutable
    /// </summary>
    public static DiagnosticDescriptor ThisValueIsNotMutable => _thisValueIsNotMutable ??= DiagnosticDescriptor.Create(
        id: "RAV0027",
        title: "This value is not mutable",
        description: "",
        helpLinkUri: "",
        messageFormat: "This value is not mutable",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0200: Property or indexer '{0}' cannot be assigned to -- it is read only
    /// </summary>
    public static DiagnosticDescriptor PropertyOrIndexerCannotBeAssignedIsReadOnly => _propertyOrIndexerCannotBeAssignedIsReadOnly ??= DiagnosticDescriptor.Create(
        id: "RAV0200",
        title: "Property or indexer '{0}' cannot be assigned to -- it is read only",
        description: "",
        helpLinkUri: "",
        messageFormat: "Property or indexer '{0}' cannot be assigned to -- it is read only",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0021: Cannot apply indexing with [] to an expression of type 'method group'
    /// </summary>
    public static DiagnosticDescriptor CannotApplyIndexingWithToAnExpressionOfType => _cannotApplyIndexingWithToAnExpressionOfType ??= DiagnosticDescriptor.Create(
        id: "RAV0021",
        title: "Cannot apply indexing with [] to an expression of type '{0}'",
        description: "",
        helpLinkUri: "",
        messageFormat: "Cannot apply indexing with [] to an expression of type '{0}'",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV2001: The numeric literal is out of range for its target type
    /// </summary>
    public static DiagnosticDescriptor NumericLiteralOutOfRange => _numericLiteralOutOfRange ??= DiagnosticDescriptor.Create(
        id: "RAV2001",
        title: "Numeric literal out of range",
        description: "",
        helpLinkUri: "",
        messageFormat: "The numeric literal is out of range for its target type",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);


    public static DiagnosticDescriptor UnterminatedCharacterLiteral => _unterminatedCharacterLiteral ??= DiagnosticDescriptor.Create(
        id: "RAV2002",
        title: "Unterminated character literal",
        description: "",
        helpLinkUri: "",
        messageFormat: "Unterminated character literal",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor InvalidEscapeSequence => _invalidEscapeSequence ??= DiagnosticDescriptor.Create(
        id: "RAV2003",
        title: "Invalid escape sequence",
        description: "",
        helpLinkUri: "",
        messageFormat: "Invalid escape sequence in character literal",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);


    /// <summary>
    /// RAV2010: Cannot resolve member '.{0}' without a known target type
    /// </summary>
    public static DiagnosticDescriptor MemberAccessRequiresTargetType => _memberAccessRequiresTargetType ??= DiagnosticDescriptor.Create(
        id: "RAV2010",
        title: "Target-typed member access requires a known type",
        description: "Target-typed member access (like '.Test') requires a type context to resolve the member.",
        helpLinkUri: "",
        messageFormat: "Cannot resolve member '.{0}' without a known target type",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV2020: Invalid alias target
    /// </summary>
    public static DiagnosticDescriptor InvalidAliasType => _invalidAliasType ??= DiagnosticDescriptor.Create(
        id: "RAV2020",
        title: "Invalid alias target",
        description: "",
        helpLinkUri: "",
        messageFormat: "Invalid alias target. Supported targets are types, namespaces, unions, tuples, and predefined types like bool, char, int, string, and unit.",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0305: The type '{0}' requires {1} type argument(s)
    /// </summary>
    public static DiagnosticDescriptor TypeRequiresTypeArguments => _typeRequiresTypeArguments ??= DiagnosticDescriptor.Create(
        id: "RAV0305",
        title: "Type requires type arguments",
        description: "",
        helpLinkUri: "",
        messageFormat: "The type '{0}' requires {1} type argument(s)",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0400: Nullable types are not allowed in union types
    /// </summary>
    public static DiagnosticDescriptor NullableTypeInUnion => _nullableTypeInUnion ??= DiagnosticDescriptor.Create(
        id: "RAV0400",
        title: "Nullable type not allowed in union",
        description: "",
        helpLinkUri: "",
        messageFormat: "Nullable types are not allowed in union types",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    /// <summary>
    /// RAV0111: Type '{0}' already defines a member called '{1}' with the same parameter types
    /// </summary>
    public static DiagnosticDescriptor TypeAlreadyDefinesMember => _typeAlreadyDefinesMember ??= DiagnosticDescriptor.Create(
        id: "RAV0111",
        title: "Member already defined",
        description: "",
        helpLinkUri: "",
        messageFormat: "Type '{0}' already defines a member called '{1}' with the same parameter types",
        category: "compiler",
        DiagnosticSeverity.Error,
        isEnabledByDefault: true);

    public static DiagnosticDescriptor[] AllDescriptors => _allDescriptors ??=
    [
        IdentifierExpected,
        SemicolonExpected,
        CharacterExpected,
        DuplicateModifier,
        UnrecognizedEscapeSequence,
        NewlineInConstant,
        MethodNameExpected,
        NonInvocableMember,
        UnassignedOutParameter,
        VariableUsedLikeAType,
        UseOfUnassignedVariable,
        MemberDoesNotContainDefinition,
        OperatorCannotBeAppliedToOperandOfType,
        TypeNameDoesNotExistInType,
        CallIsAmbiguous,
        CannotConvertFromTypeToType,
        NoOverloadForMethod,
        TypeOrNamespaceNameDoesNotExistInTheNamespace,
        TypeExpectedWithoutWildcard,
        InvalidExpressionTerm,
        TheNameDoesNotExistInTheCurrentContext,
        CannotAssignVoidToAnImplicitlyTypedVariable,
        LeftHandSideOfAssignmentMustBeAVariablePropertyOrIndexer,
        ReadOnlyFieldCannotBeAssignedTo,
        ThisValueIsNotMutable,
        PropertyOrIndexerCannotBeAssignedIsReadOnly,
        CannotApplyIndexingWithToAnExpressionOfType,
        TypeRequiresTypeArguments,
        NumericLiteralOutOfRange,
        UnterminatedCharacterLiteral,
        InvalidEscapeSequence,
        InvalidAliasType,
        MemberAccessRequiresTargetType,
        NullableTypeInUnion,
        TypeAlreadyDefinesMember
    ];

    public static DiagnosticDescriptor? GetDescriptor(string diagnosticId)
    {
        return diagnosticId switch
        {
            "RAV1001" => IdentifierExpected,
            "RAV1002" => SemicolonExpected,
            "RAV1003" => CharacterExpected,
            "RAV1004" => DuplicateModifier,
            "RAV1009" => UnrecognizedEscapeSequence,
            "RAV1010" => NewlineInConstant,
            "RAV0149" => MethodNameExpected,
            "RAV1955" => NonInvocableMember,
            "RAV0269" => UnassignedOutParameter,
            "RAV0118" => VariableUsedLikeAType,
            "RAV0165" => UseOfUnassignedVariable,
            "RAV0117" => MemberDoesNotContainDefinition,
            "RAV0023" => OperatorCannotBeAppliedToOperandOfType,
            "RAV0426" => TypeNameDoesNotExistInType,
            "RAV0121" => CallIsAmbiguous,
            "RAV1503" => CannotConvertFromTypeToType,
            "RAV1501" => NoOverloadForMethod,
            "RAV0234" => TypeOrNamespaceNameDoesNotExistInTheNamespace,
            "RAV0235" => TypeExpectedWithoutWildcard,
            "RAV1525" => InvalidExpressionTerm,
            "RAV0103" => TheNameDoesNotExistInTheCurrentContext,
            "RAV0815" => CannotAssignVoidToAnImplicitlyTypedVariable,
            "RAV0131" => LeftHandSideOfAssignmentMustBeAVariablePropertyOrIndexer,
            "RAV0191" => ReadOnlyFieldCannotBeAssignedTo,
            "RAV0027" => ThisValueIsNotMutable,
            "RAV0200" => PropertyOrIndexerCannotBeAssignedIsReadOnly,
            "RAV0021" => CannotApplyIndexingWithToAnExpressionOfType,
            "RAV2001" => NumericLiteralOutOfRange,
            "RAV2002" => UnterminatedCharacterLiteral,
            "RAV2003" => InvalidEscapeSequence,
            "RAV2010" => MemberAccessRequiresTargetType,
            "RAV2020" => InvalidAliasType,
            "RAV0305" => TypeRequiresTypeArguments,
            "RAV0400" => NullableTypeInUnion,
            "RAV0111" => TypeAlreadyDefinesMember,
            _ => null // Return null if the diagnostic ID is not recognized
        };
    }
}
