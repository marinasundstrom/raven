using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal partial class BlockBinder
{
    private BoundExpression BindCarrierUnwrap(
        PostfixOperatorExpressionSyntax syntax,
        BoundExpression operand,
        PropagationInfo propagation)
    {
        var extraction = propagation.TryGetOutputMethod;
        var extractedType = propagation.OkCaseType ?? propagation.OkPayloadType;
        extraction ??= propagation.UnionType.GetMembers("TryGetValue").OfType<IMethodSymbol>()
            .FirstOrDefault(method => !method.IsStatic && method.Parameters.Length == 1 &&
                method.Parameters[0].RefKind == RefKind.Out &&
                method.ReturnType.SpecialType == SpecialType.System_Boolean &&
                SymbolEqualityComparer.Default.Equals(method.Parameters[0].GetByRefElementType(), extractedType));
        if (extraction is null)
        {
            _diagnostics.ReportOperatorCannotBeAppliedToOperandOfType(
                "!", operand.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat),
                syntax.OperatorToken.GetLocation());
            return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
        }

        // Capture before extracting: chained accesses and side effects must observe
        // exactly one evaluation, including mutable value-type carriers.
        var receiver = CreateTempLocal("unwrapOperand", operand.Type, syntax);
        var output = CreateTempLocal("unwrapOutput", extractedType, syntax);
        var receiverAccess = new BoundLocalAccess(receiver);
        var outputAccess = new BoundLocalAccess(output);
        var condition = new BoundInvocationExpression(extraction,
            [new BoundAddressOfExpression(outputAccess)], receiverAccess,
            requiresReceiverAddress: operand.Type.IsValueType);
        BoundExpression value = outputAccess;
        if (propagation.OkCaseType is INamedTypeSymbol caseType)
        {
            var property = caseType.GetMembers("Value").OfType<IPropertySymbol>().FirstOrDefault();
            if (property is null)
                return ErrorExpression(reason: BoundExpressionReason.UnsupportedOperation);
            value = new BoundMemberAccessExpression(outputAccess, property);
        }

        var exceptionType = Compilation.GetTypeByMetadataName("System.InvalidOperationException");
        var constructor = exceptionType?.Constructors.FirstOrDefault(ctor => !ctor.IsStatic && ctor.Parameters.Length == 0);
        BoundExpression failure = constructor is null
            ? new BoundDefaultValueExpression(Compilation.GetSpecialType(SpecialType.System_Exception))
            : new BoundObjectCreationExpression(constructor, []);
        return new BoundBlockExpression([
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(receiver, operand)]),
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(output, null)]),
            new BoundIfStatement(condition, new BoundBlockStatement([]),
                new BoundBlockStatement([new BoundThrowStatement(failure)])),
            new BoundExpressionStatement(value)
        ], Compilation.UnitTypeSymbol);
    }
}
