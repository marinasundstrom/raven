using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundExpression? VisitExpression(BoundExpression? node)
    {
        if (node is null)
            return null;

        if (node is BoundPropagateExpression propagate)
        {
            var lowering = RewritePropagateExpression(propagate);
            if (lowering is null)
                return propagate;

            lowering.Statements.Add(new BoundExpressionStatement(lowering.SuccessExpression));
            return new BoundBlockExpression(lowering.Statements, GetCompilation().UnitTypeSymbol);
        }

        return base.VisitExpression(node);
    }

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

        if (!UnionFacts.UsesCarrierRepresentation(operandNamedType))
            return null;

        var tryGetMethod = FindTryGetMethod(operandNamedType, propagate);
        if (tryGetMethod is null)
            return null;

        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        var okLocalType = tryGetMethod.Parameters[0].GetByRefElementType();
        var okLocal = CreateTempLocal("propagateOk", okLocalType, isMutable: true);
        var operandLocal = CreateTempLocal("propagateOperand", operandType, isMutable: true);

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement(new[]
            {
                new BoundVariableDeclarator(operandLocal, null)
            })
        };

        var operandInitializer = VisitExpression(propagate.Operand) ?? propagate.Operand;
        var operandAssignment = new BoundAssignmentStatement(
            new BoundLocalAssignmentExpression(
                operandLocal,
                new BoundLocalAccess(operandLocal),
                operandInitializer,
                unitType));

        var exceptionBaseType = compilation.GetSpecialType(SpecialType.System_Exception);
        var exceptionLocal = CreateTempLocal("propagateException", exceptionBaseType, isMutable: false);
        var caughtErrorExpression = CreatePropagateCaughtExceptionExpression(
            propagate,
            new BoundLocalAccess(exceptionLocal),
            compilation);

        if (caughtErrorExpression is not null)
        {
            statements.Add(
                new BoundTryStatement(
                    new BoundBlockStatement(new BoundStatement[] { operandAssignment }),
                    ImmutableArray.Create(
                        new BoundCatchClause(
                            exceptionBaseType,
                            exceptionLocal,
                            pattern: null,
                            guard: null,
                            new BoundBlockStatement(new BoundStatement[]
                            {
                                new BoundReturnStatement(caughtErrorExpression)
                            }))),
                    finallyBlock: null,
                    BoundTryStatementKind.PropagateRewrite));
        }
        else
        {
            statements.Add(operandAssignment);
        }

        statements.Add(new BoundLocalDeclarationStatement(new[]
        {
            new BoundVariableDeclarator(okLocal, null)
        }));

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

        var failureBlock = CreatePropagateFailureBlock(propagate, operandAccess, operandType, compilation);
        if (failureBlock is null)
            return null;

        statements.Add(new BoundIfStatement(
            tryGetInvocation,
            new BoundBlockStatement(Array.Empty<BoundStatement>()),
            failureBlock));

        var successExpression = CreatePropagateSuccessExpression(propagate, okAccess, compilation);
        if (successExpression.Type is null || successExpression.Type.TypeKind == TypeKind.Error)
            return null;

        return new PropagateLowering(statements, successExpression);
    }

    private static BoundExpression? CreatePropagateCaughtExceptionExpression(
        BoundPropagateExpression propagate,
        BoundExpression caughtException,
        Compilation compilation)
    {
        var ctor = propagate.EnclosingErrorConstructor;
        var arguments = new List<BoundExpression>();

        if (ctor.Parameters.Length == 1)
        {
            var targetType = ctor.Parameters[0].Type;
            var sourceType = caughtException.Type ?? compilation.ErrorTypeSymbol;
            var conversion = compilation.ClassifyConversion(sourceType, targetType);
            if (!conversion.Exists)
                return null;

            var payload = ApplyErrorConversion(caughtException, targetType, conversion, compilation);
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

    private BoundBlockStatement? CreatePropagateFailureBlock(
        BoundPropagateExpression propagate,
        BoundExpression operandAccess,
        ITypeSymbol operandType,
        Compilation compilation)
    {
        var ctor = propagate.EnclosingErrorConstructor;

        if (ctor.Parameters.Length == 0)
        {
            var expression = CreatePropagateErrorExpression(propagate, Array.Empty<BoundExpression>(), compilation);
            return new BoundBlockStatement(new BoundStatement[] { new BoundReturnStatement(expression) });
        }

        if (ctor.Parameters.Length != 1 || propagate.ErrorCaseType is not INamedTypeSymbol errorCaseType)
            return null;

        var operandNamedType = operandType.GetPlainType() as INamedTypeSymbol;
        var tryGetErrorMethod = operandNamedType is null
            ? null
            : FindTryGetMethodForCase(operandNamedType, errorCaseType);
        if (tryGetErrorMethod is null)
            return null;

        var errorLocalType = tryGetErrorMethod.Parameters[0].GetByRefElementType();
        var errorLocal = CreateTempLocal("propagateError", errorLocalType, isMutable: true);
        var errorAccess = new BoundLocalAccess(errorLocal);
        var receiver = tryGetErrorMethod.IsExtensionMethod ? null : operandAccess;
        var extensionReceiver = tryGetErrorMethod.IsExtensionMethod ? operandAccess : null;
        var tryGetErrorInvocation = new BoundInvocationExpression(
            tryGetErrorMethod,
            new BoundExpression[] { new BoundAddressOfExpression(errorAccess) },
            receiver,
            extensionReceiver,
            requiresReceiverAddress: operandType.IsValueType);

        var payloadProperty = errorCaseType.GetMembers("Data").OfType<IPropertySymbol>().FirstOrDefault()
            ?? errorCaseType.GetMembers("Error").OfType<IPropertySymbol>().FirstOrDefault()
            ?? errorCaseType.GetMembers("Value").OfType<IPropertySymbol>().FirstOrDefault();
        if (payloadProperty is null)
            return null;

        BoundExpression payload = new BoundMemberAccessExpression(errorAccess, payloadProperty);
        payload = ApplyErrorConversion(payload, ctor.Parameters[0].Type, propagate.ErrorConversion, compilation);
        var errorExpression = CreatePropagateErrorExpression(propagate, new[] { payload }, compilation);
        var invalidCarrier = new BoundThrowStatement(
            new BoundDefaultValueExpression(compilation.GetSpecialType(SpecialType.System_Exception)));

        return new BoundBlockStatement(new BoundStatement[]
        {
            new BoundLocalDeclarationStatement(new[]
            {
                new BoundVariableDeclarator(errorLocal, null)
            }),
            new BoundIfStatement(
                tryGetErrorInvocation,
                new BoundBlockStatement(new BoundStatement[] { new BoundReturnStatement(errorExpression) }),
                new BoundBlockStatement(new BoundStatement[] { invalidCarrier }))
        });
    }

    private static BoundExpression CreatePropagateErrorExpression(
        BoundPropagateExpression propagate,
        IReadOnlyList<BoundExpression> arguments,
        Compilation compilation)
    {
        var ctor = propagate.EnclosingErrorConstructor;
        BoundExpression errorCaseExpression = ctor.MethodKind == MethodKind.Constructor
            ? new BoundObjectCreationExpression(ctor, arguments)
            : new BoundInvocationExpression(ctor, arguments);

        return ApplyConversionIfNeeded(errorCaseExpression, propagate.EnclosingResultType, compilation);
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
        var candidates = operandType.GetMembers("TryGetValue").OfType<IMethodSymbol>()
            .Where(m => m.Parameters.Length == 1 && m.Parameters[0].RefKind == RefKind.Out)
            .ToArray();

        if (candidates.Length == 0)
            return null;

        var okCaseType = propagate.OkCaseType?.GetPlainType();
        if (okCaseType is not null)
        {
            var caseMatch = candidates.FirstOrDefault(m =>
            {
                var parameterType = m.Parameters[0].GetByRefElementType().GetPlainType();
                return SymbolEqualityComparer.Default.Equals(parameterType, okCaseType) ||
                    parameterType.MetadataIdentityEquals(okCaseType);
            });
            if (caseMatch is not null)
                return caseMatch;
        }

        var okPayloadType = propagate.OkType.GetPlainType();
        var payloadMatch = candidates.FirstOrDefault(m =>
            SymbolEqualityComparer.Default.Equals(m.Parameters[0].GetByRefElementType().GetPlainType(), okPayloadType));

        return payloadMatch ?? candidates[0];
    }

    private static IMethodSymbol? FindTryGetMethodForCase(INamedTypeSymbol operandType, ITypeSymbol caseType)
    {
        var expected = caseType.GetPlainType();
        return operandType.GetMembers("TryGetValue").OfType<IMethodSymbol>()
            .Where(method => method.Parameters.Length == 1 && method.Parameters[0].RefKind == RefKind.Out)
            .FirstOrDefault(method =>
            {
                var parameterType = method.Parameters[0].GetByRefElementType().GetPlainType();
                return SymbolEqualityComparer.Default.Equals(parameterType, expected) ||
                    parameterType.MetadataIdentityEquals(expected);
            });
    }

    private sealed record PropagateLowering(List<BoundStatement> Statements, BoundExpression SuccessExpression);
}
