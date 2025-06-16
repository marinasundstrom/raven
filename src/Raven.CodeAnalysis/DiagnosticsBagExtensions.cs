namespace Raven.CodeAnalysis;

public static class DiagnosticBagExtensions
{
    public static void ReportIdentifierExpected(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.IdentifierExpected, location));

    public static void ReportSemicolonExpected(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.SemicolonExpected, location));

    public static void ReportCharacterExpected(this DiagnosticBag diagnostics, string character, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.CharacterExpected, location, character));

    public static void ReportDuplicateModifier(this DiagnosticBag diagnostics, string modifier, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.DuplicateModifier, location, modifier));

    public static void ReportUnrecognizedEscapeSequence(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.UnrecognizedEscapeSequence, location));

    public static void ReportNewlineInConstant(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.NewlineInConstant, location));

    public static void ReportMethodNameExpected(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.MethodNameExpected, location));

    public static void ReportNonInvocableMember(this DiagnosticBag diagnostics, string memberName, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.NonInvocableMember, location, memberName));

    public static void ReportUnassignedOutParameter(this DiagnosticBag diagnostics, string parameterName, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.UnassignedOutParameter, location, parameterName));

    public static void ReportVariableUsedLikeAType(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.VariableUsedLikeAType, location, name));

    public static void ReportUseOfUnassignedVariable(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.UseOfUnassignedVariable, location, name));

    public static void ReportMemberDoesNotContainDefinition(this DiagnosticBag diagnostics, string container, string member, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.MemberDoesNotContainDefinition, location, container, member));

    public static void ReportOperatorCannotBeApplied(this DiagnosticBag diagnostics, string op, ITypeSymbol operandType, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.OperatorCannotBeAppliedToOperandOfType, location, op, operandType));

    public static void ReportTypeNameDoesNotExistInType(this DiagnosticBag diagnostics, string name, string container, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.TypeNameDoesNotExistInType, location, name, container));

    public static void ReportCallIsAmbiguous(this DiagnosticBag diagnostics, string method1, string method2, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.CallIsAmbiguous, location, method1, method2));

    public static void ReportCannotConvertFromTypeToType(this DiagnosticBag diagnostics, ITypeSymbol fromType, ITypeSymbol toType, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.CannotConvertFromTypeToType, location, fromType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat), toType));

    public static void ReportNoOverloadForMethod(this DiagnosticBag diagnostics, string method, int count, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.NoOverloadForMethod, location, method, count));

    public static void ReportTypeOrNamespaceNameDoesNotExistInTheNamespace(this DiagnosticBag diagnostics, string typeOrNs, string container, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.TypeOrNamespaceNameDoesNotExistInTheNamespace, location, typeOrNs, container));

    public static void ReportInvalidExpressionTerm(this DiagnosticBag diagnostics, string tokenText, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.InvalidExpressionTerm, location, tokenText));

    public static void ReportUndefinedName(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext, location, name));

    public static void ReportCannotAssignVoidToImplicitlyTypedVariable(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.CannotAssignVoidToAnImplicitlyTypedVariable, location));

    public static void ReportLeftHandSideMustBeVariablePropertyOrIndexer(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.LeftHandSideOfAssignmentMustBeAVariablePropertyOrIndexer, location));

    public static void ReportReadOnlyFieldCannotBeAssigned(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.ReadOnlyFieldCannotBeAssignedTo, location));

    public static void ReportThisValueIsNotMutable(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.ThisValueIsNotMutable, location));

    public static void ReportPropertyOrIndexerIsReadOnly(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.PropertyOrIndexerCannotBeAssignedIsReadOnly, location, name));

    public static void ReportCannotApplyIndexingWithToAnExpressionOfType(this DiagnosticBag diagnostics, string typeName, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.CannotApplyIndexingWithToAnExpressionOfType, location, typeName));

    public static void ReportInvalidInvocation(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.InvalidInvocation, location));

    public static void ReportMemberAccessOnVoid(this DiagnosticBag diagnostics, string memberName, Location location)
        => diagnostics.Report(Diagnostic.Create(SemanticDiagnostics.MemberAccessOnVoid, location, memberName));

    public static void ReportUndefinedIndexer(this DiagnosticBag diagnostics, ITypeSymbol receiverType, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.CannotApplyIndexingWithToAnExpressionOfType, location, receiverType.Name));

    public static void ReportInvalidIndexerAssignment(this DiagnosticBag diagnostics, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.LeftHandSideOfAssignmentMustBeAVariablePropertyOrIndexer, location));

    public static void ReportThisValueIsNotMutable(this DiagnosticBag diagnostics, string name, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.ThisValueIsNotMutable, location, name));

    public static void ReportUndefinedBinaryOperator(this DiagnosticBag diagnostics, string operatorToken, ITypeSymbol operandType, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.OperatorCannotBeAppliedToOperandOfType, location, operatorToken, operandType));

    public static void ReportUndefinedBinaryOperator(this DiagnosticBag diagnostics, string operatorToken, ITypeSymbol operandType1, ITypeSymbol operandType2, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes, location, operatorToken, operandType1, operandType2));

    internal static void ReportMemberAccessRequiresTargetType(this DiagnosticBag diagnostics, string memberName, Location location)
        => diagnostics.Report(Diagnostic.Create(CompilerDiagnostics.MemberAccessRequiresTargetType, location, memberName));

}