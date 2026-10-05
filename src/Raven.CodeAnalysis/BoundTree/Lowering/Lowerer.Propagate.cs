using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitPropagateExpression(BoundPropagateExpression node)
    {
        var lowering = RewritePropagateExpression(node);
        if (lowering is null)
            return node;

        lowering.Statements.Add(new BoundExpressionStatement(lowering.SuccessExpression));
        return new BoundBlockExpression(lowering.Statements, GetCompilation().UnitTypeSymbol);
    }

    public override BoundNode? VisitAssignmentStatement(BoundAssignmentStatement node)
    {
        // A local target has no receiver or index evaluation to preserve before
        // the right-hand side. Keep residual returns outside its assignment.
        if (node.Expression is BoundLocalAssignmentExpression assignment
            && RewritePropagatingInitializer(assignment.Right) is { } lowering)
        {
            lowering.Statements.Add(new BoundAssignmentStatement(
                new BoundLocalAssignmentExpression(assignment.Local, assignment.Left,
                    lowering.SuccessExpression, assignment.UnitType)));
            return new BoundBlockStatement(lowering.Statements);
        }

        return base.VisitAssignmentStatement(node);
    }

    private bool TryRewritePropagateLocalDeclaration(BoundLocalDeclarationStatement node, out List<BoundStatement> statements)
    {
        statements = new List<BoundStatement>();
        var changed = false;
        var declarators = ImmutableArray.CreateBuilder<BoundVariableDeclarator>();

        foreach (var declarator in node.Declarators)
        {
            if (RewritePropagatingInitializer(declarator.Initializer) is { } lowering)
            {
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

    // Keep early returns at statement boundaries, rather than inside an expression
    // evaluated with preceding operands on the evaluation stack.
    private PropagateLowering? RewritePropagatingInitializer(BoundExpression? expression)
    {
        if (expression is BoundPropagateExpression propagate)
            return RewritePropagateExpression(propagate);

        if (expression is BoundConversionExpression conversion && RewritePropagatingInitializer(conversion.Expression) is { } converted)
            return new PropagateLowering(converted.Statements, new BoundConversionExpression(
                converted.SuccessExpression, conversion.Type, conversion.Conversion, conversion.IsNullableSuppression));

        if (expression is BoundIsPatternExpression pattern && RewritePropagatingInitializer(pattern.Expression) is { } matched)
            return new PropagateLowering(matched.Statements, pattern.Update(
                matched.SuccessExpression, pattern.Pattern, pattern.BooleanType, pattern.Reason));

        // Spill value arguments in source order before a residual can return.
        // Address-taking receivers and ref arguments need location-preserving lowering.
        if (expression is BoundInvocationExpression call && !call.RequiresReceiverAddress &&
            call.ExtensionReceiver is null && (call.Receiver is null || call.Receiver.Type.IsReferenceType) &&
            call.Method.Parameters.All(parameter => parameter.RefKind == RefKind.None))
        {
            var arguments = call.Arguments.ToArray();
            var receiver = RewritePropagatingInitializer(call.Receiver);
            var lowered = arguments.Select(RewritePropagatingInitializer).ToArray();
            if (receiver is not null || lowered.Any(item => item is not null))
            {
                var prefix = new List<BoundStatement>();
                BoundExpression Spill(BoundExpression original, PropagateLowering? item)
                {
                    if (item is not null) prefix.AddRange(item.Statements);
                    var value = item?.SuccessExpression ?? VisitExpression(original)!;
                    var temporary = CreateTempLocal("propagateArgument", value.Type, isMutable: false);
                    prefix.Add(new BoundLocalDeclarationStatement([new BoundVariableDeclarator(temporary, value)]));
                    return new BoundLocalAccess(temporary);
                }
                var spilledReceiver = call.Receiver is null ? null : Spill(call.Receiver, receiver);
                var spilledArguments = arguments.Select((argument, index) => Spill(argument, lowered[index])).ToArray();
                return new PropagateLowering(prefix, call.Update(call.Method, spilledArguments, spilledReceiver, null, false));
            }
        }

        if (expression is BoundBlockExpression block && block.LocalsToDispose.IsEmpty)
        {
            var items = block.Statements.ToArray();
            if (items.LastOrDefault() is BoundExpressionStatement last
                && !items.OfType<BoundLocalDeclarationStatement>().Any(declaration => declaration.IsUsing)
                && RewritePropagatingInitializer(last.Expression) is { } tail)
            {
                var prefix = (BoundBlockStatement)VisitBlockStatement(
                    new BoundBlockStatement(items.Take(items.Length - 1)))!;
                var expanded = prefix.Statements.ToList();
                expanded.AddRange(tail.Statements);
                return new PropagateLowering(expanded, tail.SuccessExpression);
            }
            return null;
        }

        if (expression is BoundIfExpression { ElseBranch: not null } conditional)
        {
            var then = RewritePropagatingInitializer(conditional.ThenBranch);
            var otherwise = RewritePropagatingInitializer(conditional.ElseBranch);
            if (then is null && otherwise is null)
                return null;

            var result = CreateTempLocal("propagateConditional", conditional.Type, isMutable: true);
            BoundBlockStatement Branch(BoundExpression original, PropagateLowering? lowering)
            {
                var branch = lowering?.Statements ?? new List<BoundStatement>();
                var value = lowering?.SuccessExpression ?? VisitExpression(original)!;
                branch.Add(new BoundAssignmentStatement(new BoundLocalAssignmentExpression(
                    result, new BoundLocalAccess(result),
                    ApplyConversionIfNeeded(value, conditional.Type, GetCompilation()),
                    GetCompilation().UnitTypeSymbol)));
                return new BoundBlockStatement(branch);
            }

            // Keep each residual return inside its selected branch, with no live
            // surrounding expression operands on the evaluation stack.
            return new PropagateLowering(new List<BoundStatement>
            {
                new BoundLocalDeclarationStatement([new BoundVariableDeclarator(result, null)]),
                new BoundIfStatement(VisitExpression(conditional.Condition)!,
                    Branch(conditional.ThenBranch, then), Branch(conditional.ElseBranch, otherwise))
            }, new BoundLocalAccess(result));
        }

        if (expression is not BoundBinaryExpression binary)
            return null;

        var left = RewritePropagatingInitializer(binary.Left);
        var right = RewritePropagatingInitializer(binary.Right);
        if (left is null && right is null)
            return null;

        var statements = left?.Statements ?? new List<BoundStatement>();
        var leftValue = left?.SuccessExpression ?? VisitExpression(binary.Left)!;
        if (binary.Operator.OperatorKind is BinaryOperatorKind.LogicalAnd or BinaryOperatorKind.LogicalOr)
        {
            var result = CreateTempLocal("propagateCondition", binary.Type, isMutable: true);
            statements.Add(new BoundLocalDeclarationStatement([new BoundVariableDeclarator(result, leftValue)]));
            var rightStatements = right?.Statements ?? new List<BoundStatement>();
            rightStatements.Add(new BoundAssignmentStatement(new BoundLocalAssignmentExpression(
                result, new BoundLocalAccess(result), right?.SuccessExpression ?? VisitExpression(binary.Right)!,
                GetCompilation().UnitTypeSymbol)));
            var selected = new BoundBlockStatement(rightStatements);
            var empty = new BoundBlockStatement([]);
            var isAnd = binary.Operator.OperatorKind == BinaryOperatorKind.LogicalAnd;
            statements.Add(new BoundIfStatement(new BoundLocalAccess(result),
                isAnd ? selected : empty, isAnd ? empty : selected));
            return new PropagateLowering(statements, new BoundLocalAccess(result));
        }

        var leftLocal = CreateTempLocal("propagateLeft", leftValue.Type, isMutable: false);
        // Even a local read must precede the right operand: that operand can mutate it.
        statements.Add(new BoundLocalDeclarationStatement(new[]
        {
            new BoundVariableDeclarator(leftLocal, leftValue)
        }));
        if (right is not null)
            statements.AddRange(right.Statements);

        return new PropagateLowering(statements, binary.Update(
            new BoundLocalAccess(leftLocal), binary.Operator,
            right?.SuccessExpression ?? VisitExpression(binary.Right)!));
    }

    private bool TryRewritePropagateExpressionStatement(BoundExpressionStatement node, out List<BoundStatement> statements)
    {
        statements = new List<BoundStatement>();

        if (node.Expression is not BoundPropagateExpression propagate)
        {
            if (RewritePropagatingInitializer(node.Expression) is { } nested)
            {
                statements.AddRange(nested.Statements);
                statements.Add(new BoundExpressionStatement(nested.SuccessExpression));
                return true;
            }
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
        // A target Void payload has a logical value, but an unused propagation result
        // must not load storage when codegen treats its expression type as no-result.
        if (GetCompilation().Options.RuntimePropagationContract is null
            || lowering.SuccessExpression.Type?.SpecialType != SpecialType.System_Void)
            statements.Add(new BoundExpressionStatement(lowering.SuccessExpression));
        return true;
    }

    private PropagateLowering? RewritePropagateExpression(BoundPropagateExpression propagate)
    {
        var compilation = GetCompilation();
        var operandType = propagate.Operand.Type;
        var operandNamedType = operandType?.GetNonNullableType() as INamedTypeSymbol;
        if (operandType is null || operandType.TypeKind == TypeKind.Error || operandNamedType is null)
            return null;

        var tryGetMethod = propagate.TryGetOutputMethod;
        if (tryGetMethod is null)
        {
            if (!UnionFacts.UsesCarrierRepresentation(operandNamedType))
                return null;

            tryGetMethod = FindTryGetMethod(operandNamedType, propagate);
        }
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
        var caughtErrorExpression = compilation.Options.RuntimePropagationContract is null
            ? CreatePropagateCaughtExceptionExpression(
            propagate,
            new BoundLocalAccess(exceptionLocal),
            compilation)
            : null;

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
            // Without a catch boundary, initialize at the declaration. Declaring
            // first and assigning later makes an await in the operand hoist an
            // uninitialized carrier into the async state machine.
            statements[0] = new BoundLocalDeclarationStatement(new[]
            {
                new BoundVariableDeclarator(operandLocal, operandInitializer)
            });
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

        return MaterializePropagateCaseCarrier(errorCaseExpression, propagate.EnclosingResultType, compilation);
    }

    private BoundBlockStatement? CreatePropagateFailureBlock(
        BoundPropagateExpression propagate,
        BoundExpression operandAccess,
        ITypeSymbol operandType,
        Compilation compilation)
    {
        var ctor = propagate.EnclosingErrorConstructor;

        if (propagate.TryGetResidualMethod is { } tryGetResidualMethod)
        {
            if (ctor.Parameters.Length != 1)
                return null;

            var residualLocalType = tryGetResidualMethod.Parameters[0].GetByRefElementType();
            var residualLocal = CreateTempLocal("propagateResidual", residualLocalType, isMutable: true);
            var residualAccess = new BoundLocalAccess(residualLocal);
            var residualReceiver = tryGetResidualMethod.IsExtensionMethod ? null : operandAccess;
            var residualExtensionReceiver = tryGetResidualMethod.IsExtensionMethod ? operandAccess : null;
            var tryGetResidualInvocation = new BoundInvocationExpression(
                tryGetResidualMethod,
                new BoundExpression[] { new BoundAddressOfExpression(residualAccess) },
                residualReceiver,
                residualExtensionReceiver,
                requiresReceiverAddress: operandType.IsValueType);

            BoundExpression residual = ApplyErrorConversion(
                residualAccess,
                ctor.Parameters[0].Type,
                propagate.ErrorConversion,
                compilation);
            var returnExpression = CreatePropagateErrorExpression(propagate, new[] { residual }, compilation);
            var invalidContractCarrier = new BoundThrowStatement(
                new BoundDefaultValueExpression(compilation.GetSpecialType(SpecialType.System_Exception)), compilerFailure: "Invalid propagation carrier");

            return new BoundBlockStatement(new BoundStatement[]
            {
                new BoundLocalDeclarationStatement(new[]
                {
                    new BoundVariableDeclarator(residualLocal, null)
                }),
                new BoundIfStatement(
                    tryGetResidualInvocation,
                    new BoundBlockStatement(new BoundStatement[] { new BoundReturnStatement(returnExpression) }),
                    new BoundBlockStatement(new BoundStatement[] { invalidContractCarrier }))
            });
        }

        if (ctor.Parameters.Length == 0)
        {
            var expression = CreatePropagateErrorExpression(propagate, Array.Empty<BoundExpression>(), compilation);
            return new BoundBlockStatement(new BoundStatement[] { new BoundReturnStatement(expression) });
        }

        if (ctor.Parameters.Length != 1 || propagate.ErrorCaseType is not INamedTypeSymbol errorCaseType)
            return null;

        var operandNamedType = operandType.GetNonNullableType() as INamedTypeSymbol;
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
            new BoundDefaultValueExpression(compilation.GetSpecialType(SpecialType.System_Exception)), compilerFailure: "Invalid propagation carrier");

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

        return MaterializePropagateCaseCarrier(errorCaseExpression, propagate.EnclosingResultType, compilation);
    }

    private static BoundExpression MaterializePropagateCaseCarrier(
        BoundExpression caseExpression,
        INamedTypeSymbol enclosingResultType,
        Compilation compilation)
    {
        var caseType = caseExpression.Type;
        if (caseType is null || SymbolEqualityComparer.Default.Equals(caseType, enclosingResultType))
            return caseExpression;

        if (!enclosingResultType.TryGetUnionCarrierConstructor(caseType, out var carrierConstructor))
            return ApplyConversionIfNeeded(caseExpression, enclosingResultType, compilation);

        var parameterType = carrierConstructor.Parameters[0].Type;
        var argument = caseExpression;
        if (!SymbolEqualityComparer.Default.Equals(caseType, parameterType))
        {
            var conversion = compilation.ClassifyConversion(caseType, parameterType, includeUserDefined: false);
            if (conversion is { Exists: true, IsIdentity: false })
                argument = new BoundConversionExpression(caseExpression, parameterType, conversion);
        }

        return new BoundObjectCreationExpression(carrierConstructor, new[] { argument });
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

        var okCaseType = propagate.OkCaseType?.GetNonNullableType();
        if (okCaseType is not null)
        {
            var caseMatch = candidates.FirstOrDefault(m =>
            {
                var parameterType = m.Parameters[0].GetByRefElementType().GetNonNullableType();
                return SymbolEqualityComparer.Default.Equals(parameterType, okCaseType) ||
                    parameterType.MetadataIdentityEquals(okCaseType);
            });
            if (caseMatch is not null)
                return caseMatch;
        }

        var okPayloadType = propagate.OkType.GetNonNullableType();
        var payloadMatch = candidates.FirstOrDefault(m =>
            SymbolEqualityComparer.Default.Equals(m.Parameters[0].GetByRefElementType().GetNonNullableType(), okPayloadType));

        return payloadMatch ?? candidates[0];
    }

    private static IMethodSymbol? FindTryGetMethodForCase(INamedTypeSymbol operandType, ITypeSymbol caseType)
    {
        var expected = caseType.GetNonNullableType();
        return operandType.GetMembers("TryGetValue").OfType<IMethodSymbol>()
            .Where(method => method.Parameters.Length == 1 && method.Parameters[0].RefKind == RefKind.Out)
            .FirstOrDefault(method =>
            {
                var parameterType = method.Parameters[0].GetByRefElementType().GetNonNullableType();
                return SymbolEqualityComparer.Default.Equals(parameterType, expected) ||
                    parameterType.MetadataIdentityEquals(expected);
            });
    }

    private sealed record PropagateLowering(List<BoundStatement> Statements, BoundExpression SuccessExpression);
}
