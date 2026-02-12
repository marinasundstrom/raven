using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    private bool TryRewritePropagateLocalDeclaration(BoundLocalDeclarationStatement node, out List<BoundStatement> statements)
    {
        statements = new List<BoundStatement>();
        var changed = false;
        var declarators = ImmutableArray.CreateBuilder<BoundVariableDeclarator>();

        foreach (var declarator in node.Declarators)
        {
            if (declarator.Initializer is BoundPropagateExpression propagate)
            {
                var lowering = RewritePropagateExpression(propagate);
                if (lowering is null)
                {
                    var visitedInitializer = VisitExpression(propagate) ?? propagate;
                    declarators.Add(ReferenceEquals(visitedInitializer, propagate)
                        ? declarator
                        : new BoundVariableDeclarator(declarator.Local, visitedInitializer));
                    changed |= !ReferenceEquals(visitedInitializer, propagate);
                    continue;
                }

                if (declarators.Count > 0)
                {
                    statements.Add(new BoundLocalDeclarationStatement(declarators.ToImmutable(), node.IsUsing));
                    declarators.Clear();
                }

                statements.AddRange(lowering.Statements);
                statements.Add(new BoundLocalDeclarationStatement(
                    ImmutableArray.Create(new BoundVariableDeclarator(declarator.Local, lowering.SuccessExpression)),
                    node.IsUsing));
                changed = true;
                continue;
            }

            var initializer = VisitExpression(declarator.Initializer) ?? declarator.Initializer;
            if (!ReferenceEquals(initializer, declarator.Initializer))
                changed = true;

            declarators.Add(ReferenceEquals(initializer, declarator.Initializer)
                ? declarator
                : new BoundVariableDeclarator(declarator.Local, initializer));
        }

        if (!changed)
            return false;

        if (declarators.Count > 0)
            statements.Add(new BoundLocalDeclarationStatement(declarators.ToImmutable(), node.IsUsing));

        return true;
    }

    private bool TryRewritePropagateExpressionStatement(BoundExpressionStatement node, out List<BoundStatement> statements)
    {
        statements = new List<BoundStatement>();

        if (node.Expression is not BoundPropagateExpression propagate)
        {
            var expression = VisitExpression(node.Expression) ?? node.Expression;
            if (ReferenceEquals(expression, node.Expression))
                return false;

            statements.Add(new BoundExpressionStatement(expression));
            return true;
        }

        var lowering = RewritePropagateExpression(propagate);
        if (lowering is null)
            return false;

        statements.AddRange(lowering.Statements);
        statements.Add(new BoundExpressionStatement(lowering.SuccessExpression));
        return true;
    }

    private PropagateLowering? RewritePropagateExpression(BoundPropagateExpression propagate)
    {
        var compilation = GetCompilation();
        var operandType = propagate.Operand.Type;
        var operandNamedType = operandType?.GetPlainType() as INamedTypeSymbol;
        if (operandType is null || operandType.TypeKind == TypeKind.Error || operandNamedType is null)
            return null;

        var tryGetMethod = FindTryGetMethod(operandNamedType, propagate);
        if (tryGetMethod is null)
            return null;

        var okLocalType = tryGetMethod.Parameters[0].Type is RefTypeSymbol refTypeType
            ? refTypeType.ElementType
            : tryGetMethod.Parameters[0].Type;
        var okLocal = CreateTempLocal("propagateOk", okLocalType, isMutable: true);
        var operandLocal = CreateTempLocal("propagateOperand", operandType, isMutable: true);

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement(new[]
            {
                new BoundVariableDeclarator(operandLocal, VisitExpression(propagate.Operand) ?? propagate.Operand)
            }),
            new BoundLocalDeclarationStatement(new[]
            {
                new BoundVariableDeclarator(okLocal, null)
            })
        };

        var operandAccess = new BoundLocalAccess(operandLocal);
        var okAccess = new BoundLocalAccess(okLocal);
        var receiver = tryGetMethod.IsExtensionMethod ? null : operandAccess;
        var extensionReceiver = tryGetMethod.IsExtensionMethod ? operandAccess : null;

        var tryGetInvocation = new BoundInvocationExpression(
            tryGetMethod,
            new BoundExpression[] { new BoundAddressOfExpression(okAccess) },
            receiver,
            extensionReceiver,
            requiresReceiverAddress: operandType.IsValueType);

        var errorExpression = CreatePropagateErrorExpression(propagate, operandAccess, compilation);
        if (errorExpression is null)
            return null;

        var failureBlock = new BoundBlockStatement(new BoundStatement[]
        {
            new BoundReturnStatement(errorExpression)
        });

        statements.Add(new BoundIfStatement(
            tryGetInvocation,
            new BoundBlockStatement(Array.Empty<BoundStatement>()),
            failureBlock));

        var successExpression = CreatePropagateSuccessExpression(propagate, okAccess, compilation);
        if (successExpression.Type is null || successExpression.Type.TypeKind == TypeKind.Error)
            return null;

        return new PropagateLowering(statements, successExpression);
    }

    private BoundExpression? CreatePropagateErrorExpression(
        BoundPropagateExpression propagate,
        BoundExpression operandAccess,
        Compilation compilation)
    {
        var ctor = propagate.EnclosingErrorConstructor;
        var arguments = new List<BoundExpression>();

        if (ctor.Parameters.Length == 1)
        {
            var payload = GetPropagateErrorPayload(propagate, operandAccess, compilation);
            if (payload is null)
                return null;

            var targetType = ctor.Parameters[0].Type;
            payload = ApplyErrorConversion(payload, targetType, propagate.ErrorConversion, compilation);
            arguments.Add(payload);
        }
        else if (ctor.Parameters.Length != 0)
        {
            return null;
        }

        BoundExpression errorCaseExpression = ctor.MethodKind == MethodKind.Constructor
            ? new BoundObjectCreationExpression(ctor, arguments)
            : new BoundInvocationExpression(ctor, arguments);

        return ApplyConversionIfNeeded(errorCaseExpression, propagate.EnclosingResultType, compilation);
    }

    private BoundExpression? GetPropagateErrorPayload(
        BoundPropagateExpression propagate,
        BoundExpression operandAccess,
        Compilation compilation)
    {
        if (propagate.UnwrapErrorMethod is { } unwrapMethod)
        {
            var receiver = unwrapMethod.IsExtensionMethod ? null : operandAccess;
            var extensionReceiver = unwrapMethod.IsExtensionMethod ? operandAccess : null;

            return new BoundInvocationExpression(
                unwrapMethod,
                Array.Empty<BoundExpression>(),
                receiver,
                extensionReceiver,
                requiresReceiverAddress: receiver?.Type?.IsValueType == true);
        }

        var operandNamed = operandAccess.Type as INamedTypeSymbol;
        if (operandNamed is null)
            return null;

        var errorProperty = operandNamed.GetMembers("ErrorValue").OfType<IPropertySymbol>().FirstOrDefault()
            ?? operandNamed.GetMembers("Payload").OfType<IPropertySymbol>().FirstOrDefault();

        if (errorProperty is null)
            return null;

        var access = new BoundMemberAccessExpression(operandAccess, errorProperty);
        return ApplyConversionIfNeeded(access, errorProperty.Type, compilation);
    }

    private static BoundExpression ApplyErrorConversion(
        BoundExpression payload,
        ITypeSymbol targetType,
        Conversion conversion,
        Compilation compilation)
    {
        if (conversion.Exists && !conversion.IsIdentity)
            return new BoundConversionExpression(payload, targetType, conversion);

        return ApplyConversionIfNeeded(payload, targetType, compilation);
    }

    private static BoundExpression CreatePropagateSuccessExpression(
        BoundPropagateExpression propagate,
        BoundLocalAccess okAccess,
        Compilation compilation)
    {
        if (propagate.OkCaseType is null)
            return ApplyConversionIfNeeded(okAccess, propagate.OkType, compilation);

        var okCaseType = (INamedTypeSymbol)propagate.OkCaseType;
        var valueProperty = propagate.OkValueProperty
            ?? okCaseType.GetMembers("Value").OfType<IPropertySymbol>().FirstOrDefault();

        if (valueProperty is null)
            return new BoundErrorExpression(compilation.ErrorTypeSymbol, null, BoundExpressionReason.UnsupportedOperation);

        var caseAccess = ApplyConversionIfNeeded(okAccess, okCaseType, compilation);
        var valueAccess = new BoundMemberAccessExpression(caseAccess, valueProperty);
        return ApplyConversionIfNeeded(valueAccess, propagate.OkType, compilation);
    }

    private static IMethodSymbol? FindTryGetMethod(INamedTypeSymbol operandType, BoundPropagateExpression propagate)
    {
        var tryGetName = $"TryGet{propagate.OkCaseName}";
        var candidates = operandType.GetMembers(tryGetName).OfType<IMethodSymbol>()
            .Where(m => m.Parameters.Length == 1 && m.Parameters[0].RefKind == RefKind.Out)
            .ToArray();

        if (candidates.Length == 0)
            return null;

        var okCaseType = propagate.OkCaseType?.GetPlainType();
        if (okCaseType is not null)
        {
            var caseMatch = candidates.FirstOrDefault(m =>
                SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type.GetPlainType(), okCaseType));
            if (caseMatch is not null)
                return caseMatch;
        }

        var okPayloadType = propagate.OkType.GetPlainType();
        var payloadMatch = candidates.FirstOrDefault(m =>
            SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type.GetPlainType(), okPayloadType));

        return payloadMatch ?? candidates[0];
    }

    private sealed record PropagateLowering(List<BoundStatement> Statements, BoundExpression SuccessExpression);
}
