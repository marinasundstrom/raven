using System;
using System.Collections;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.CodeGen.DebugUtils;

namespace Raven.CodeAnalysis.CodeGen;

internal class StatementGenerator : Generator
{
    private readonly BoundStatement _statement;

    public StatementGenerator(Generator parent, BoundStatement statement) : base(parent)
    {
        _statement = statement;
    }

    public override void Emit()
    {
        PrintDebug($"Emitting bound statement: {_statement}", () => CodeGenFlags.PrintEmittedBoundNodes);

        MethodBodyGenerator.EmitSequencePoint(_statement);

        switch (_statement)
        {
            case BoundReturnStatement returnStatement:
                EmitReturnStatement(returnStatement);
                break;

            case BoundThrowStatement throwStatement:
                EmitThrowStatement(throwStatement);
                break;

            case BoundExpressionStatement expressionStatement:
                EmitExpressionStatement(expressionStatement);
                break;

            case BoundAssignmentStatement assignmentStatement:
                EmitAssignmentStatement(assignmentStatement);
                break;

            case BoundLocalDeclarationStatement localDeclarationStatement:
                EmitDeclarationStatement(localDeclarationStatement);
                break;

            case BoundIfStatement ifStatement:
                EmitIfStatement(ifStatement);
                break;

            case BoundForStatement forStatement:
                EmitForStatement(forStatement);
                break;

            case BoundBreakStatement:
                EmitBreakStatement();
                break;

            case BoundContinueStatement:
                EmitContinueStatement();
                break;

            case BoundTryStatement tryStatement:
                EmitTryStatement(tryStatement);
                break;

            case BoundBlockStatement blockStatement:
                EmitBlockStatement(blockStatement);
                break;

            case BoundLabeledStatement labeledStatement:
                EmitLabeledStatement(labeledStatement);
                break;

            case BoundGotoStatement gotoStatement:
                EmitGotoStatement(gotoStatement);
                break;

            case BoundConditionalGotoStatement conditionalGotoStatement:
                EmitConditionalGotoStatement(conditionalGotoStatement);
                break;

            case BoundMatchStatement:
                throw new InvalidOperationException("BoundMatchStatement should be lowered before code generation.");
        }
    }

    private void EmitIfStatement(BoundIfStatement ifStatement)
    {
        var scope = new Scope(this);
        var hasElse = ifStatement.ElseNode is not null;
        var thenTerminates = IsTerminatingStatement(ifStatement.ThenNode);
        var elseTerminates = hasElse && IsTerminatingStatement(ifStatement.ElseNode!);
        var bothBranchesTerminate = hasElse && thenTerminates && elseTerminates;
        var endLabel = ILGenerator.DefineLabel();
        var elseLabel = hasElse
            ? ILGenerator.DefineLabel()
            : endLabel;

        new ExpressionGenerator(scope, ifStatement.Condition)
            .EmitBranchOpForCondition(ifStatement.Condition, elseLabel);

        new StatementGenerator(scope, ifStatement.ThenNode).Emit();

        if (hasElse)
        {
            if (!IsTerminatingStatement(ifStatement.ThenNode))
                ILGenerator.Emit(OpCodes.Br, endLabel);

            ILGenerator.MarkLabel(elseLabel);
            new StatementGenerator(new Scope(this), ifStatement.ElseNode!).Emit();

            if (bothBranchesTerminate)
                return;
        }

        ILGenerator.MarkLabel(endLabel);
        ILGenerator.Emit(OpCodes.Nop);
    }

    private void EmitBreakStatement()
    {
        if (TryGetLoopBreakLabel(this, out var breakLabel))
            ILGenerator.Emit(OpCodes.Br, breakLabel);
    }

    private void EmitContinueStatement()
    {
        if (TryGetLoopContinueLabel(this, out var continueLabel))
            ILGenerator.Emit(OpCodes.Br, continueLabel);
    }

    private static bool TryGetLoopBreakLabel(Generator generator, out ILLabel label)
    {
        for (Generator? current = generator; current is not null; current = current.Parent)
        {
            if (current is Scope scope && scope.TryGetBreakLabel(out label))
                return true;
        }

        label = default;
        return false;
    }

    private static bool TryGetLoopContinueLabel(Generator generator, out ILLabel label)
    {
        for (Generator? current = generator; current is not null; current = current.Parent)
        {
            if (current is Scope scope && scope.TryGetContinueLabel(out label))
                return true;
        }

        label = default;
        return false;
    }

    private static bool IsTerminatingStatement(BoundStatement statement)
    {
        return statement switch
        {
            BoundReturnStatement => true,
            BoundThrowStatement => true,
            BoundExpressionStatement { Expression: BoundReturnExpression or BoundThrowExpression } => true,
            BoundBlockStatement block when block.Statements.Any() => IsTerminatingStatement(block.Statements.Last()),
            BoundIfStatement { ElseNode: not null } nestedIf =>
                IsTerminatingStatement(nestedIf.ThenNode) &&
                IsTerminatingStatement(nestedIf.ElseNode!),
            _ => false,
        };
    }

    private IILocal SpillValueToLocalIfNeeded(Type clrType, EmitInfo info)
    {
        // If the expression already came directly from an existing local (and we didn't spill),
        // reuse that local instead of introducing an intermediate temp.
        if (info.Local is not null && !info.WasCaptured && !info.WasSpilledToLocal)
        {
            // Clear the emitted value from the stack; we will reload from the local as needed.
            ILGenerator.Emit(OpCodes.Pop);
            return info.Local;
        }

        var tmp = ILGenerator.DeclareLocal(clrType);
        ILGenerator.Emit(OpCodes.Stloc, tmp);
        return tmp;
    }

    private void EmitReturnStatement(BoundReturnStatement returnStatement)
    {
        if (TryGetAsyncMoveNextMembers(out var asyncMembers) && returnStatement.Expression is not null)
        {
            EmitAsyncMoveNextReturn(returnStatement, asyncMembers);
            return;
        }

        var localsToDispose = EnumerateLocalsToDispose().ToImmutableArray();
        var returnType = GetEffectiveReturnTypeForEmission();
        var expression = returnStatement.Expression;
        ITypeSymbol? expressionType = expression?.Type;
        var expressionEmitInfo = EmitInfo.None;
        IILocal? resultTemp = null;

        var isVoidLikeReturn = returnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit;
        var hasExceptionExit = TryGetExceptionExitLabel(out _);
        var forceDiscriminatedUnionReturnConversion = false;
        ITypeSymbol? forcedConversionSourceType = null;

        if (expression is not null)
        {
            if (expression is BoundConversionExpression { Conversion.IsDiscriminatedUnion: true } duConversion &&
                !isVoidLikeReturn)
            {
                expression = duConversion.Expression;
                expressionType = expression.Type;
                forceDiscriminatedUnionReturnConversion = true;
                forcedConversionSourceType = expressionType;
            }

            var preserveResult = !isVoidLikeReturn;
            expressionEmitInfo = new ExpressionGenerator(this, expression, preserveResult).Emit2();

            if (preserveResult && localsToDispose.Length > 0 && expressionType is not null)
            {
                var clrType = ResolveClrType(expressionType);
                resultTemp = SpillValueToLocalIfNeeded(clrType, expressionEmitInfo);
            }
        }

        EmitDispose(localsToDispose);

        if (expression is not null && !isVoidLikeReturn)
        {
            if (resultTemp is not null)
                ILGenerator.Emit(OpCodes.Ldloc, resultTemp);

            if (forceDiscriminatedUnionReturnConversion &&
                forcedConversionSourceType is not null)
            {
                var conversion = Compilation.ClassifyConversion(forcedConversionSourceType, returnType);
                if (conversion.Exists && !conversion.IsIdentity)
                {
                    EmitConversion(forcedConversionSourceType, returnType, conversion);
                    expressionType = returnType;
                }
            }
            else if (expressionType is not null &&
                     !SymbolEqualityComparer.Default.Equals(expressionType, returnType))
            {
                var conversion = Compilation.ClassifyConversion(expressionType, returnType);
                if (conversion.Exists && !conversion.IsIdentity)
                {
                    EmitConversion(expressionType, returnType, conversion);
                    expressionType = returnType;
                }
            }

            if (expressionType?.IsValueType == true &&
                (returnType.SpecialType is SpecialType.System_Object || returnType is ITypeUnionSymbol))
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(expressionType));
            }

            if (hasExceptionExit)
            {
                var returnValueLocal = MethodBodyGenerator.EnsureReturnValueLocal();
                ILGenerator.Emit(OpCodes.Stloc, returnValueLocal);
            }
        }
        else if (expression is not null && isVoidLikeReturn)
        {
            // Defensive: void-like returns should not carry a value on the stack.
            // Some expressions emitted with preserveResult=false already leave no value.
            if (expressionEmitInfo.HasValueOnStack)
                ILGenerator.Emit(OpCodes.Pop);
            expressionType = null;
        }

        if (hasExceptionExit)
        {
            ILGenerator.Emit(OpCodes.Leave, MethodBodyGenerator.GetOrCreateReturnLabel());
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ret);
        }
    }

    private ITypeSymbol GetEffectiveReturnTypeForEmission()
    {
        if (!Compilation.Options.UseRuntimeAsync || !MethodSymbol.IsAsync)
            return MethodSymbol.ReturnType;

        return AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, MethodSymbol.ReturnType)
            ?? MethodSymbol.ReturnType;
    }

    private void EmitAsyncMoveNextReturn(BoundReturnStatement returnStatement, SynthesizedAsyncStateMachineTypeSymbol.ConstructedMembers members)
    {
        var localsToDispose = EnumerateLocalsToDispose().ToImmutableArray();
        var expression = returnStatement.Expression!;
        var expressionType = expression.Type;
        IILocal? resultTemp = null;
        var info = new ExpressionGenerator(this, expression, preserveResult: true).Emit2();

        if (expressionType is not null)
        {
            var clrType = ResolveClrType(expressionType);
            resultTemp = SpillValueToLocalIfNeeded(clrType, info);
        }

        EmitDispose(localsToDispose);

        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldc_I4, -2);
        var stateFieldInfo = (FieldInfo)MethodGenerator.TypeGenerator.CodeGen.GetMemberBuilder((SourceFieldSymbol)members.StateField);
        ILGenerator.Emit(OpCodes.Stfld, stateFieldInfo);

        var setResultMethod = members.StateMachineBuilderMembers.SetResult;
        if (setResultMethod is not null && resultTemp is not null)
        {
            ILGenerator.Emit(OpCodes.Ldarg_0);
            var builderFieldInfo = (FieldInfo)MethodGenerator.TypeGenerator.CodeGen.GetMemberBuilder((SourceFieldSymbol)members.BuilderField);
            ILGenerator.Emit(OpCodes.Ldflda, builderFieldInfo);
            ILGenerator.Emit(OpCodes.Ldloc, resultTemp);

            if (expressionType is not null &&
                !SymbolEqualityComparer.Default.Equals(expressionType, MethodSymbol.ReturnType))
            {
                var conversion = Compilation.ClassifyConversion(expressionType, MethodSymbol.ReturnType);
                if (conversion.Exists && !conversion.IsIdentity)
                {
                    EmitConversion(expressionType, MethodSymbol.ReturnType, conversion);
                }
            }

            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(setResultMethod));
        }

        if (TryGetExceptionExitLabel(out _))
            ILGenerator.Emit(OpCodes.Leave, MethodBodyGenerator.GetOrCreateReturnLabel());
        else
            ILGenerator.Emit(OpCodes.Ret);
    }

    private bool TryGetAsyncMoveNextMembers(out SynthesizedAsyncStateMachineTypeSymbol.ConstructedMembers members)
    {
        members = default;

        if (MethodSymbol is not IMethodSymbol method)
            return false;

        if (method.ContainingType is SynthesizedAsyncStateMachineTypeSymbol stateMachine)
        {
            if (!SymbolEqualityComparer.Default.Equals(method, stateMachine.MoveNextMethod))
                return false;

            members = stateMachine.GetConstructedMembers(stateMachine.AsyncMethod);
            return true;
        }

        if (method.ContainingType is ConstructedNamedTypeSymbol constructed
            && constructed.ConstructedFrom is SynthesizedAsyncStateMachineTypeSymbol constructedStateMachine)
        {
            members = constructedStateMachine.GetConstructedMembers(constructedStateMachine.AsyncMethod);
            return SymbolEqualityComparer.Default.Equals(method.OriginalDefinition, members.MoveNext.OriginalDefinition);
        }

        return false;
    }

    private void EmitThrowStatement(BoundThrowStatement throwStatement)
    {
        var localsToDispose = EnumerateLocalsToDispose().ToImmutableArray();
        var expression = throwStatement.Expression;
        var expressionType = expression.Type;

        IILocal? resultTemp = null;
        var hasValueOnStack = true;

        var info = new ExpressionGenerator(this, expression).Emit2();

        if (localsToDispose.Length > 0 && expressionType is { TypeKind: not TypeKind.Error })
        {
            var clrType = ResolveClrType(expressionType);
            resultTemp = SpillValueToLocalIfNeeded(clrType, info);
            hasValueOnStack = false;
        }
        else if (localsToDispose.Length > 0)
        {
            ILGenerator.Emit(OpCodes.Pop);
            hasValueOnStack = false;
        }

        EmitDispose(localsToDispose);

        if (resultTemp is not null)
        {
            ILGenerator.Emit(OpCodes.Ldloc, resultTemp);
            hasValueOnStack = true;
        }
        else if (!hasValueOnStack)
        {
            ILGenerator.Emit(OpCodes.Ldnull);
            hasValueOnStack = true;
        }

        if (expressionType is { TypeKind: not TypeKind.Error })
        {
            if (expressionType.IsValueType)
            {
                var valueClrType = ResolveClrType(expressionType);
                ILGenerator.Emit(OpCodes.Box, valueClrType);
            }

            var exceptionType = Compilation.GetTypeByMetadataName("System.Exception");
            if (exceptionType is { TypeKind: not TypeKind.Error })
            {
                var exceptionClrType = ResolveClrType(exceptionType);
                ILGenerator.Emit(OpCodes.Castclass, exceptionClrType);
            }
        }
        else if (hasValueOnStack)
        {
            ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.Emit(OpCodes.Ldnull);
        }

        ILGenerator.Emit(OpCodes.Throw);
    }

    private void EmitExpressionStatement(BoundExpressionStatement expressionStatement)
    {
        var expression = expressionStatement.Expression;

        // In statement position, `return <expr>` / `throw <expr>` can be represented
        // as expression forms. Emit them as real statements so we do not attempt to
        // pop a non-existent value from the stack after control-flow exits.
        if (expression is BoundReturnExpression returnExpression)
        {
            EmitReturnStatement(new BoundReturnStatement(returnExpression.Expression));
            return;
        }

        if (expression is BoundThrowExpression throwExpression)
        {
            EmitThrowStatement(new BoundThrowStatement(throwExpression.Expression));
            return;
        }

        // We are discarding the value of the expression. Never materialize Unit.Value in this case.
        // (ExpressionGenerator will still emit side-effects.)
        if (expression.Type.SpecialType is SpecialType.System_Void or SpecialType.System_Unit)
        {
            new ExpressionGenerator(this, expression, EmitContext.None).Emit2();
            return;
        }

        var expressionType = expression.Type?.UnwrapLiteralType() ?? expression.Type;

        if (expressionType is NullableTypeSymbol nullable &&
            nullable.UnderlyingType.SpecialType == SpecialType.System_Unit)
        {
            new ExpressionGenerator(this, expression, EmitContext.None).Emit2();
            return;
        }

        new ExpressionGenerator(this, expression, EmitContext.Value).Emit2();

        // Pop the result
        ILGenerator.Emit(OpCodes.Pop);
    }

    private void EmitAssignmentStatement(BoundAssignmentStatement assignmentStatement)
    {
        new ExpressionGenerator(this, assignmentStatement.Expression, preserveResult: false).Emit();
    }

    private void EmitForStatement(BoundForStatement forStatement)
    {
        var beginLabel = ILGenerator.DefineLabel();
        var continueLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        var scope = new Scope(this);
        scope.SetLoopTargets(endLabel, continueLabel);

        var iteration = forStatement.Iteration;

        if (iteration.Kind == ForIterationKind.Range)
        {
            var start = iteration.RangeStart
                ?? iteration.Range?.Left?.Value
                ?? throw new InvalidOperationException("Range iteration requires a start value.");
            var end = iteration.RangeEnd
                ?? iteration.Range?.Right?.Value
                ?? throw new InvalidOperationException("Range iteration requires an end value.");
            var step = iteration.RangeStep
                ?? throw new InvalidOperationException("Range iteration requires a step value.");
            var upperExclusive = iteration.RangeUpperExclusive;

            EmitRangeForLoop(forStatement, scope, beginLabel, continueLabel, endLabel, start, end, step, upperExclusive);
            return;
        }

        switch (iteration.Kind)
        {
            case ForIterationKind.Array:
                Debug.Assert(iteration.ArrayType is not null, "Missing array type for array iteration.");
                new ExpressionGenerator(scope, forStatement.Collection).Emit();
                EmitArrayForLoop(forStatement, scope, beginLabel, continueLabel, endLabel, iteration.ArrayType!);
                break;

            case ForIterationKind.Generic:
            case ForIterationKind.NonGeneric:
                Debug.Assert(iteration.GetEnumeratorMethod is not null, "Missing GetEnumerator method for enumerator iteration.");
                Debug.Assert(iteration.MoveNextMethod is not null, "Missing MoveNext method for enumerator iteration.");
                Debug.Assert(iteration.CurrentGetter is not null, "Missing Current getter for enumerator iteration.");
                EmitEnumeratorForLoop(
                    forStatement,
                    scope,
                    beginLabel,
                    continueLabel,
                    endLabel,
                    iteration.GetEnumeratorMethod!,
                    iteration.MoveNextMethod!,
                    iteration.CurrentGetter!);
                break;
        }
    }

    private void EmitRangeForLoop(
        BoundForStatement forStatement,
        Scope scope,
        ILLabel beginLabel,
        ILLabel continueLabel,
        ILLabel endLabel,
        BoundExpression rangeStart,
        BoundExpression rangeEnd,
        BoundExpression rangeStep,
        bool upperExclusive)
    {
        var elementType = forStatement.Iteration.ElementType.UnwrapLiteralType() ?? forStatement.Iteration.ElementType;
        var elementClrType = ResolveClrType(elementType);

        var startLocal = ILGenerator.DeclareLocal(elementClrType);
        new ExpressionGenerator(scope, rangeStart).Emit();
        ILGenerator.Emit(OpCodes.Stloc, startLocal);

        var endLocal = ILGenerator.DeclareLocal(elementClrType);
        new ExpressionGenerator(scope, rangeEnd).Emit();
        ILGenerator.Emit(OpCodes.Stloc, endLocal);

        var stepLocal = ILGenerator.DeclareLocal(elementClrType);
        new ExpressionGenerator(scope, rangeStep).Emit();
        ILGenerator.Emit(OpCodes.Stloc, stepLocal);

        var loopLocal = forStatement.Local;
        IILocal? elementLocal = null;
        if (loopLocal is not null)
        {
            elementLocal = ILGenerator.DeclareLocal(ResolveClrType(loopLocal.Type));
            scope.AddLocal(loopLocal, elementLocal);
        }

        // Runtime guard for dynamic steps.
        ILGenerator.Emit(OpCodes.Ldloc, stepLocal);
        EmitNumericZero(elementType);
        ILGenerator.Emit(OpCodes.Ceq);
        ILGenerator.Emit(OpCodes.Brtrue, endLabel);

        var positiveStepLabel = ILGenerator.DefineLabel();
        var negativeStepLabel = ILGenerator.DefineLabel();
        var bodyLabel = ILGenerator.DefineLabel();

        ILGenerator.MarkLabel(beginLabel);

        EmitBranchIfGreaterThanZero(elementType, stepLocal, positiveStepLabel);
        EmitBranchIfLessThanZero(elementType, stepLocal, negativeStepLabel);
        ILGenerator.Emit(OpCodes.Br, endLabel);

        ILGenerator.MarkLabel(positiveStepLabel);
        EmitRangeLoopBreakCondition(elementType, startLocal, endLocal, endLabel, positiveStep: true, upperExclusive);
        ILGenerator.Emit(OpCodes.Br, bodyLabel);

        ILGenerator.MarkLabel(negativeStepLabel);
        EmitRangeLoopBreakCondition(elementType, startLocal, endLocal, endLabel, positiveStep: false, upperExclusive);

        ILGenerator.MarkLabel(bodyLabel);
        ILGenerator.Emit(OpCodes.Ldloc, startLocal);
        if (elementLocal is not null)
            ILGenerator.Emit(OpCodes.Stloc, elementLocal);
        else
            ILGenerator.Emit(OpCodes.Pop);

        new StatementGenerator(scope, forStatement.Body).Emit();

        ILGenerator.MarkLabel(continueLabel);
        EmitRangeLoopIncrement(elementType, startLocal, stepLocal);
        ILGenerator.Emit(OpCodes.Br, beginLabel);
        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitRangeLoopBreakCondition(
        ITypeSymbol elementType,
        IILocal currentLocal,
        IILocal endLocal,
        ILLabel endLabel,
        bool positiveStep,
        bool upperExclusive)
    {
        var specialType = (elementType.UnwrapLiteralType() ?? elementType).SpecialType;

        if (specialType == SpecialType.System_Decimal)
        {
            ILGenerator.Emit(OpCodes.Ldloc, currentLocal);
            ILGenerator.Emit(OpCodes.Ldloc, endLocal);

            var compareMethod = typeof(decimal).GetMethod(
                nameof(decimal.Compare),
                BindingFlags.Public | BindingFlags.Static,
                binder: null,
                [typeof(decimal), typeof(decimal)],
                modifiers: null)
                ?? throw new InvalidOperationException("Failed to resolve decimal.Compare(decimal, decimal).");

            ILGenerator.Emit(OpCodes.Call, compareMethod);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            if (positiveStep)
                ILGenerator.Emit(upperExclusive ? OpCodes.Bge : OpCodes.Bgt, endLabel);
            else
                ILGenerator.Emit(upperExclusive ? OpCodes.Ble : OpCodes.Blt, endLabel);
            return;
        }

        ILGenerator.Emit(OpCodes.Ldloc, currentLocal);
        ILGenerator.Emit(OpCodes.Ldloc, endLocal);
        if (positiveStep)
        {
            ILGenerator.Emit(
                IsUnsignedIntegralType(specialType)
                    ? (upperExclusive ? OpCodes.Bge_Un : OpCodes.Bgt_Un)
                    : (upperExclusive ? OpCodes.Bge : OpCodes.Bgt),
                endLabel);
        }
        else
        {
            ILGenerator.Emit(
                IsUnsignedIntegralType(specialType)
                    ? (upperExclusive ? OpCodes.Ble_Un : OpCodes.Blt_Un)
                    : (upperExclusive ? OpCodes.Ble : OpCodes.Blt),
                endLabel);
        }
    }

    private void EmitBranchIfGreaterThanZero(ITypeSymbol elementType, IILocal valueLocal, ILLabel target)
    {
        var specialType = (elementType.UnwrapLiteralType() ?? elementType).SpecialType;

        if (specialType == SpecialType.System_Decimal)
        {
            EmitDecimalCompareWithZero(valueLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            ILGenerator.Emit(OpCodes.Bgt, target);
            return;
        }

        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);
        EmitNumericZero(elementType);
        ILGenerator.Emit(IsUnsignedIntegralType(specialType) ? OpCodes.Bgt_Un : OpCodes.Bgt, target);
    }

    private void EmitBranchIfLessThanZero(ITypeSymbol elementType, IILocal valueLocal, ILLabel target)
    {
        var specialType = (elementType.UnwrapLiteralType() ?? elementType).SpecialType;

        if (specialType == SpecialType.System_Decimal)
        {
            EmitDecimalCompareWithZero(valueLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            ILGenerator.Emit(OpCodes.Blt, target);
            return;
        }

        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);
        EmitNumericZero(elementType);
        ILGenerator.Emit(IsUnsignedIntegralType(specialType) ? OpCodes.Blt_Un : OpCodes.Blt, target);
    }

    private void EmitDecimalCompareWithZero(IILocal valueLocal)
    {
        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);

        var zeroField = typeof(decimal).GetField(nameof(decimal.Zero), BindingFlags.Public | BindingFlags.Static)
            ?? throw new InvalidOperationException("Failed to resolve decimal.Zero.");
        ILGenerator.Emit(OpCodes.Ldsfld, zeroField);

        var compareMethod = typeof(decimal).GetMethod(
            nameof(decimal.Compare),
            BindingFlags.Public | BindingFlags.Static,
            binder: null,
            [typeof(decimal), typeof(decimal)],
            modifiers: null)
            ?? throw new InvalidOperationException("Failed to resolve decimal.Compare(decimal, decimal).");

        ILGenerator.Emit(OpCodes.Call, compareMethod);
    }

    private void EmitRangeLoopIncrement(ITypeSymbol elementType, IILocal currentLocal, IILocal stepLocal)
    {
        var specialType = (elementType.UnwrapLiteralType() ?? elementType).SpecialType;

        if (specialType == SpecialType.System_Decimal)
        {
            ILGenerator.Emit(OpCodes.Ldloc, currentLocal);
            ILGenerator.Emit(OpCodes.Ldloc, stepLocal);

            var addMethod = typeof(decimal).GetMethod(
                nameof(decimal.Add),
                BindingFlags.Public | BindingFlags.Static,
                binder: null,
                [typeof(decimal), typeof(decimal)],
                modifiers: null)
                ?? throw new InvalidOperationException("Failed to resolve decimal.Add(decimal, decimal).");

            ILGenerator.Emit(OpCodes.Call, addMethod);
            ILGenerator.Emit(OpCodes.Stloc, currentLocal);
            return;
        }

        ILGenerator.Emit(OpCodes.Ldloc, currentLocal);
        ILGenerator.Emit(OpCodes.Ldloc, stepLocal);
        ILGenerator.Emit(OpCodes.Add);

        switch (specialType)
        {
            case SpecialType.System_Byte:
                ILGenerator.Emit(OpCodes.Conv_U1);
                break;
            case SpecialType.System_SByte:
                ILGenerator.Emit(OpCodes.Conv_I1);
                break;
            case SpecialType.System_Int16:
                ILGenerator.Emit(OpCodes.Conv_I2);
                break;
            case SpecialType.System_UInt16:
            case SpecialType.System_Char:
                ILGenerator.Emit(OpCodes.Conv_U2);
                break;
            case SpecialType.System_UInt32:
                ILGenerator.Emit(OpCodes.Conv_U4);
                break;
            case SpecialType.System_Int64:
                ILGenerator.Emit(OpCodes.Conv_I8);
                break;
            case SpecialType.System_UInt64:
                ILGenerator.Emit(OpCodes.Conv_U8);
                break;
        }

        ILGenerator.Emit(OpCodes.Stloc, currentLocal);
    }

    private void EmitNumericZero(ITypeSymbol elementType)
    {
        var specialType = (elementType.UnwrapLiteralType() ?? elementType).SpecialType;
        switch (specialType)
        {
            case SpecialType.System_Int64:
            case SpecialType.System_UInt64:
                ILGenerator.Emit(OpCodes.Ldc_I8, 0L);
                break;
            case SpecialType.System_Single:
                ILGenerator.Emit(OpCodes.Ldc_R4, 0f);
                break;
            case SpecialType.System_Double:
                ILGenerator.Emit(OpCodes.Ldc_R8, 0d);
                break;
            default:
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                break;
        }
    }

    private static bool IsUnsignedIntegralType(SpecialType specialType)
    {
        return specialType is
            SpecialType.System_Byte or
            SpecialType.System_UInt16 or
            SpecialType.System_Char or
            SpecialType.System_UInt32 or
            SpecialType.System_UInt64;
    }

    private void EmitArrayForLoop(
        BoundForStatement forStatement,
        Scope scope,
        ILLabel beginLabel,
        ILLabel continueLabel,
        ILLabel endLabel,
        IArrayTypeSymbol arrayType)
    {
        var collectionLocal = ILGenerator.DeclareLocal(ResolveClrType(arrayType));
        ILGenerator.Emit(OpCodes.Stloc, collectionLocal);

        var indexLocal = ILGenerator.DeclareLocal(ResolveClrType(Compilation.GetSpecialType(SpecialType.System_Int32)));
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Stloc, indexLocal);

        var loopLocal = forStatement.Local;
        var elementType = loopLocal?.Type ?? forStatement.Iteration.ElementType;

        IILocal? elementLocal = null;
        if (loopLocal is not null)
        {
            elementLocal = ILGenerator.DeclareLocal(ResolveClrType(elementType));
            scope.AddLocal(loopLocal, elementLocal);
        }

        ILGenerator.MarkLabel(beginLabel);

        ILGenerator.Emit(OpCodes.Ldloc, indexLocal);
        ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
        ILGenerator.Emit(OpCodes.Ldlen);
        ILGenerator.Emit(OpCodes.Conv_I4);
        ILGenerator.Emit(OpCodes.Bge, endLabel);

        ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
        ILGenerator.Emit(OpCodes.Ldloc, indexLocal);
        EmitLoadElement(elementType);
        if (elementLocal is not null)
            ILGenerator.Emit(OpCodes.Stloc, elementLocal);
        else
            ILGenerator.Emit(OpCodes.Pop);

        new StatementGenerator(scope, forStatement.Body).Emit();

        ILGenerator.MarkLabel(continueLabel);
        ILGenerator.Emit(OpCodes.Ldloc, indexLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Add);
        ILGenerator.Emit(OpCodes.Stloc, indexLocal);

        ILGenerator.Emit(OpCodes.Br, beginLabel);
        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitEnumeratorForLoop(
        BoundForStatement forStatement,
        Scope scope,
        ILLabel beginLabel,
        ILLabel continueLabel,
        ILLabel endLabel,
        IMethodSymbol getEnumeratorMethod,
        IMethodSymbol moveNextMethod,
        IMethodSymbol currentGetter)
    {
        EmitGetEnumeratorInvocation(scope, forStatement.Collection, getEnumeratorMethod);

        var enumeratorType = getEnumeratorMethod.ReturnType;
        var enumeratorClrType = ResolveClrType(enumeratorType);
        var enumeratorLocal = ILGenerator.DeclareLocal(enumeratorClrType);
        ILGenerator.Emit(OpCodes.Stloc, enumeratorLocal);

        var loopLocal = forStatement.Local;
        var elementType = loopLocal?.Type ?? forStatement.Iteration.ElementType;
        IILocal? elementLocal = null;
        if (loopLocal is not null)
        {
            elementLocal = ILGenerator.DeclareLocal(ResolveClrType(elementType));
            scope.AddLocal(loopLocal, elementLocal);
        }

        ILGenerator.MarkLabel(beginLabel);

        EmitEnumeratorCall(enumeratorLocal, enumeratorType, moveNextMethod);
        ILGenerator.Emit(OpCodes.Brfalse, endLabel);

        EmitEnumeratorCall(enumeratorLocal, enumeratorType, currentGetter);

        var sourceElementType = currentGetter.ReturnType;
        if (!SymbolEqualityComparer.Default.Equals(sourceElementType, elementType))
        {
            var conversion = Compilation.ClassifyConversion(sourceElementType, elementType);
            if (conversion.Exists)
                EmitConversion(sourceElementType, elementType, conversion);
        }

        if (elementLocal is not null)
            ILGenerator.Emit(OpCodes.Stloc, elementLocal);
        else
            ILGenerator.Emit(OpCodes.Pop);

        new StatementGenerator(scope, forStatement.Body).Emit();

        ILGenerator.MarkLabel(continueLabel);
        ILGenerator.Emit(OpCodes.Br, beginLabel);
        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitGetEnumeratorInvocation(
        Scope scope,
        BoundExpression collection,
        IMethodSymbol getEnumeratorMethod)
    {
        BoundInvocationExpression invocation;
        if (getEnumeratorMethod.IsExtensionMethod)
        {
            invocation = new BoundInvocationExpression(getEnumeratorMethod, [collection], receiver: null, extensionReceiver: null);
        }
        else
        {
            invocation = new BoundInvocationExpression(getEnumeratorMethod, [], collection);
        }

        new ExpressionGenerator(scope, collection).EmitInvocationExpressionBase(invocation);
    }

    private void EmitEnumeratorCall(IILocal enumeratorLocal, ITypeSymbol enumeratorType, IMethodSymbol method)
    {
        if (enumeratorType.IsValueType)
        {
            ILGenerator.Emit(OpCodes.Ldloca, enumeratorLocal);
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(method));
            return;
        }

        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        var callOpCode = method.IsVirtual || method.ContainingType?.TypeKind == TypeKind.Interface
            ? OpCodes.Callvirt
            : OpCodes.Call;
        ILGenerator.Emit(callOpCode, GetMethodInfo(method));
    }

    private void EmitTryStatement(BoundTryStatement tryStatement)
    {
        ILGenerator.BeginExceptionBlock();

        var exitLabel = MethodBodyGenerator.GetOrCreateReturnLabel();
        var exceptionBaseType = Compilation.GetSpecialType(SpecialType.System_Exception);

        static bool IsExceptionLike(ITypeSymbol type, ITypeSymbol exceptionBaseType)
        {
            for (var current = type; current is not null; current = current.BaseType)
            {
                if (SymbolEqualityComparer.Default.Equals(current, exceptionBaseType))
                    return true;
            }

            return false;
        }

        var tryScope = new Scope(this);
        tryScope.SetExceptionExitLabel(exitLabel);
        tryScope.MarkAsInsideExceptionHandler();
        new StatementGenerator(tryScope, tryStatement.TryBlock).Emit();

        foreach (var catchClause in tryStatement.CatchClauses)
        {
            var requestedCatchType = catchClause.ExceptionType;
            if (requestedCatchType.TypeKind == TypeKind.Error)
                requestedCatchType = exceptionBaseType;

            var emittedCatchType = IsExceptionLike(requestedCatchType, exceptionBaseType)
                ? requestedCatchType
                : exceptionBaseType;

            ILGenerator.BeginCatchBlock(ResolveClrType(emittedCatchType));

            var catchScope = new Scope(this);
            catchScope.SetExceptionExitLabel(exitLabel);
            catchScope.MarkAsInsideExceptionHandler();

            if (!IsExceptionLike(requestedCatchType, exceptionBaseType))
            {
                ILGenerator.Emit(OpCodes.Pop);
                ILGenerator.Emit(OpCodes.Rethrow);
                continue;
            }

            if (catchClause.Local is { } localSymbol)
            {
                var localType = localSymbol.Type;
                if (localType.TypeKind == TypeKind.Error)
                    localType = exceptionBaseType;

                if (!SymbolEqualityComparer.Default.Equals(localType, emittedCatchType))
                {
                    var conversion = Compilation.ClassifyConversion(emittedCatchType, localType);
                    if (!conversion.Exists)
                    {
                        ILGenerator.Emit(OpCodes.Pop);
                        ILGenerator.Emit(OpCodes.Rethrow);
                        continue;
                    }

                    if (!conversion.IsIdentity)
                        EmitConversion(emittedCatchType, localType, conversion);
                }

                var localBuilder = ILGenerator.DeclareLocal(ResolveClrType(localType));
                catchScope.AddLocal(localSymbol, localBuilder);
                ILGenerator.Emit(OpCodes.Stloc, localBuilder);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Pop);
            }

            new StatementGenerator(catchScope, catchClause.Block).Emit();
        }

        if (tryStatement.FinallyBlock is { } finallyBlock)
        {
            ILGenerator.BeginFinallyBlock();
            var finallyScope = new Scope(this);
            finallyScope.SetExceptionExitLabel(exitLabel);
            finallyScope.MarkAsInsideExceptionHandler();
            new StatementGenerator(finallyScope, finallyBlock).Emit();
        }

        ILGenerator.EndExceptionBlock();
    }

    private void EmitBlockStatement(BoundBlockStatement blockStatement)
    {
        var scope = new Scope(this, blockStatement.LocalsToDispose);
        var emitILScope = blockStatement.IntroduceILScope && !IsInsideExceptionHandler;

        if (emitILScope)
            ILGenerator.BeginScope();
        try
        {
            foreach (var s in blockStatement.Statements)
                new StatementGenerator(scope, s).Emit();

            EmitDispose(blockStatement.LocalsToDispose);
        }
        finally
        {
            if (emitILScope)
                ILGenerator.EndScope();
        }
    }

    private void EmitLabeledStatement(BoundLabeledStatement labeledStatement)
    {
        if (Parent is not Scope scope)
            throw new InvalidOperationException("Labeled statements require an enclosing scope.");

        MethodBodyGenerator.RegisterLabelScope(labeledStatement.Label, scope);

        var ilLabel = MethodBodyGenerator.GetOrCreateLabel(labeledStatement.Label);
        ILGenerator.MarkLabel(ilLabel);
        ILGenerator.Emit(OpCodes.Nop);

        new StatementGenerator(scope, labeledStatement.Statement).Emit();
    }

    private void EmitGotoStatement(BoundGotoStatement gotoStatement)
    {
        if (Parent is not Scope scope)
            throw new InvalidOperationException("Goto statements require an enclosing scope.");

        var targetScope = MethodBodyGenerator.GetLabelScope(gotoStatement.Target);
        EmitScopeDisposals(scope, targetScope);

        var ilLabel = MethodBodyGenerator.GetOrCreateLabel(gotoStatement.Target);
        ILGenerator.Emit(OpCodes.Br, ilLabel);
    }

    private void EmitConditionalGotoStatement(BoundConditionalGotoStatement conditionalGotoStatement)
    {
        if (Parent is not Scope scope)
            throw new InvalidOperationException("Conditional goto statements require an enclosing scope.");

        var skipLabel = ILGenerator.DefineLabel();
        new ExpressionGenerator(scope, conditionalGotoStatement.Condition).Emit();

        if (conditionalGotoStatement.JumpIfTrue)
            ILGenerator.Emit(OpCodes.Brfalse, skipLabel);
        else
            ILGenerator.Emit(OpCodes.Brtrue, skipLabel);

        var targetScope = MethodBodyGenerator.GetLabelScope(conditionalGotoStatement.Target);
        EmitScopeDisposals(scope, targetScope);

        var targetLabel = MethodBodyGenerator.GetOrCreateLabel(conditionalGotoStatement.Target);
        ILGenerator.Emit(OpCodes.Br, targetLabel);

        ILGenerator.MarkLabel(skipLabel);
    }

    private void EmitScopeDisposals(Scope startScope, Scope? targetScope)
    {
        var current = startScope;

        while (current is not null && !ReferenceEquals(current, targetScope))
        {
            EmitDispose(current.LocalsToDispose);
            current = current.Parent as Scope;
        }
    }

    private void EmitLoadElement(ITypeSymbol elementType)
    {
        var clrType = ResolveClrType(elementType);

        if (clrType.IsValueType)
            ILGenerator.Emit(OpCodes.Ldelem, clrType);
        else
            ILGenerator.Emit(OpCodes.Ldelem_Ref);
    }

    private void EmitDeclarationStatement(BoundLocalDeclarationStatement localDeclarationStatement)
    {
        foreach (var declarator in localDeclarationStatement.Declarators)
        {
            EmitDeclarator(localDeclarationStatement, declarator);
        }
    }

    private void EmitDeclarator(BoundLocalDeclarationStatement localDeclarationStatement, BoundVariableDeclarator declarator)
    {
        var localSymbol = declarator.Local;
        var localBuilder = GetLocal(localSymbol);

        if (declarator.FixedPinnedLocal is not null && declarator.FixedAddressInitializer is not null)
        {
            EmitFixedDeclarator(localSymbol, localBuilder, declarator);
            return;
        }

        if (declarator.Initializer is not null)
        {
            // If the local is hoisted into the shared outer-method closure (reference-based capture),
            // store the initializer value directly into the closure field instead of a stack slot.
            // We load the closure *before* emitting the initializer so the IL stack is:
            //   [closure_ref] [value] → stfld  (no temp needed).
            if (localBuilder is null &&
                MethodBodyGenerator.TryGetCapturedField(localSymbol, out var hoistedField, out var fromSm))
            {
                var initType = declarator.Initializer.Type;
                if (initType?.SpecialType is not (SpecialType.System_Void or SpecialType.System_Unit))
                {
                    if (!fromSm)
                        MethodBodyGenerator.EmitLoadClosure();
                    else
                        ILGenerator.Emit(OpCodes.Ldarg_0);

                    new ExpressionGenerator(this, declarator.Initializer, preserveResult: true).Emit2();

                    var exprType = declarator.Initializer.Type;

                    // Apply implicit conversion if the expression and local types differ.
                    if (exprType is not null && localSymbol.Type is not null &&
                        !SymbolEqualityComparer.Default.Equals(exprType, localSymbol.Type))
                    {
                        var conv = Compilation.ClassifyConversion(exprType, localSymbol.Type);
                        if (conv.Exists && !conv.IsIdentity)
                            EmitConversion(exprType, localSymbol.Type, conv);
                    }

                    ILGenerator.Emit(OpCodes.Stfld, hoistedField);
                }
                return;
            }

            var initType2 = declarator.Initializer.Type;
            var discardValue = localBuilder is null && initType2?.SpecialType is (SpecialType.System_Void or SpecialType.System_Unit);
            var preserveResult = !discardValue;
            if (localBuilder is null &&
                localSymbol is SourceFunctionValueSymbol &&
                declarator.Initializer is BoundFunctionExpression)
            {
                // Function-value aliases keep method semantics and avoid delegate materialization.
                preserveResult = false;
            }

            new ExpressionGenerator(this, declarator.Initializer, preserveResult).Emit2();

            var expressionType = declarator.Initializer.Type;

            // If the local wasn't declared (e.g., the initializer returns early or is a discard),
            // there's nothing to store.
            if (localBuilder is null)
            {
                if (expressionType is null ||
                    expressionType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit)
                {
                    return;
                }

                ILGenerator.Emit(OpCodes.Pop);
                return;
            }

            if (localSymbol.Type is NullableTypeSymbol nullableLocal && nullableLocal.UnderlyingType.IsValueType)
            {
                var localClr = ResolveClrType(localSymbol.Type);
                if (expressionType?.TypeKind == TypeKind.Null)
                {
                    ILGenerator.Emit(OpCodes.Pop);
                    ILGenerator.Emit(OpCodes.Ldloca, localBuilder);
                    ILGenerator.Emit(OpCodes.Initobj, localClr);
                    return;
                }

                if (expressionType is NullableTypeSymbol)
                {
                    ILGenerator.Emit(OpCodes.Stloc, localBuilder);
                    return;
                }

                var underlyingClr = ResolveClrType(nullableLocal.UnderlyingType);
                var temp = ILGenerator.DeclareLocal(underlyingClr);
                ILGenerator.Emit(OpCodes.Stloc, temp);
                ILGenerator.Emit(OpCodes.Ldloca, localBuilder);
                ILGenerator.Emit(OpCodes.Ldloc, temp);

                var ctor = GetNullableConstructor(localClr, underlyingClr);
                ILGenerator.Emit(OpCodes.Call, ctor);
                return;
            }

            var finalType = expressionType;

            if (expressionType is not null &&
                localSymbol.Type is not null &&
                !SymbolEqualityComparer.Default.Equals(expressionType, localSymbol.Type))
            {
                var conversion = Compilation.ClassifyConversion(expressionType, localSymbol.Type);
                if (conversion.Exists && !conversion.IsIdentity)
                {
                    EmitConversion(expressionType, localSymbol.Type, conversion);
                    finalType = localSymbol.Type;
                }
            }

            if (finalType is not null &&
                localSymbol.Type is not null &&
                finalType.IsValueType &&
                (localSymbol.Type.SpecialType is SpecialType.System_Object || localSymbol.Type is ITypeUnionSymbol))
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(finalType));
            }

            ILGenerator.Emit(OpCodes.Stloc, localBuilder);
            return;
        }

        if (localBuilder is null || localSymbol.Type is null || !localSymbol.Type.IsValueType)
            return;

        // Value-type locals that are later addressed (ldloca) must be explicitly initialized
        // to satisfy IL verification in all control-flow shapes.
        ILGenerator.Emit(OpCodes.Ldloca, localBuilder);
        ILGenerator.Emit(OpCodes.Initobj, ResolveClrType(localSymbol.Type));
    }

    private void EmitFixedDeclarator(ILocalSymbol localSymbol, IILocal? localBuilder, BoundVariableDeclarator declarator)
    {
        if (localBuilder is null || declarator.FixedPinnedLocal is null || declarator.FixedAddressInitializer is null)
            return;

        var pinnedLocalBuilder = GetLocal(declarator.FixedPinnedLocal);
        if (pinnedLocalBuilder is null)
            return;

        new ExpressionGenerator(this, declarator.FixedAddressInitializer, preserveResult: true).Emit2();
        ILGenerator.Emit(OpCodes.Stloc, pinnedLocalBuilder);
        ILGenerator.Emit(OpCodes.Ldloc, pinnedLocalBuilder);
        ILGenerator.Emit(OpCodes.Conv_U);
        ILGenerator.Emit(OpCodes.Stloc, localBuilder);
    }

}
