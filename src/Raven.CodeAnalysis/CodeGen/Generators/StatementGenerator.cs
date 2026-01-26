using System;
using System.Collections;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

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
        }
    }

    private void EmitIfStatement(BoundIfStatement ifStatement)
    {
        var scope = new Scope(this);
        var hasElse = ifStatement.ElseNode is not null;
        var endLabel = ILGenerator.DefineLabel();
        var elseLabel = hasElse
            ? ILGenerator.DefineLabel()
            : endLabel;

        new ExpressionGenerator(scope, ifStatement.Condition)
            .EmitBranchOpForCondition(ifStatement.Condition, elseLabel);

        new StatementGenerator(scope, ifStatement.ThenNode).Emit();

        if (hasElse)
        {
            ILGenerator.Emit(OpCodes.Br, endLabel);

            ILGenerator.MarkLabel(elseLabel);
            new StatementGenerator(new Scope(this), ifStatement.ElseNode!).Emit();
        }

        ILGenerator.MarkLabel(endLabel);
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
        var returnType = MethodSymbol.ReturnType;
        var expression = returnStatement.Expression;
        ITypeSymbol? expressionType = expression?.Type;
        IILocal? resultTemp = null;

        var isVoidLikeReturn = returnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit;
        var hasExceptionExit = TryGetExceptionExitLabel(out _);

        if (expression is not null)
        {
            var preserveResult = !isVoidLikeReturn;
            var info = new ExpressionGenerator(this, expression, preserveResult).Emit2();

            if (preserveResult && localsToDispose.Length > 0 && expressionType is not null)
            {
                var clrType = ResolveClrType(expressionType);
                resultTemp = SpillValueToLocalIfNeeded(clrType, info);
            }
        }

        EmitDispose(localsToDispose);

        if (expression is not null && !isVoidLikeReturn)
        {
            if (resultTemp is not null)
                ILGenerator.Emit(OpCodes.Ldloc, resultTemp);

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

        if (hasExceptionExit)
        {
            ILGenerator.Emit(OpCodes.Leave, MethodBodyGenerator.GetOrCreateReturnLabel());
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ret);
        }
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
            ILGenerator.Emit(OpCodes.Call, setResultMethod.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));
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

        // Expression is an invocation expression that returns unit. 
        if (expression is BoundInvocationExpression { Type.SpecialType: SpecialType.System_Unit } expr)
        {
            // Emit method call without emitting unit value.
            new ExpressionGenerator(this, expression).EmitInvocationExpressionBase(expr);

            // No pop instruction required.
            return;
        }

        new ExpressionGenerator(this, expression).Emit();

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
            var range = iteration.Range ?? forStatement.Collection as BoundRangeExpression
                ?? throw new InvalidOperationException("Range iteration requires a range expression.");

            EmitRangeForLoop(forStatement, scope, beginLabel, continueLabel, endLabel, range);
            return;
        }

        new ExpressionGenerator(scope, forStatement.Collection).Emit();

        switch (iteration.Kind)
        {
            case ForIterationKind.Array:
                Debug.Assert(iteration.ArrayType is not null, "Missing array type for array iteration.");
                EmitArrayForLoop(forStatement, scope, beginLabel, continueLabel, endLabel, iteration.ArrayType!);
                break;

            case ForIterationKind.Generic:
                Debug.Assert(iteration.EnumerableInterface is not null && iteration.EnumeratorInterface is not null,
                    "Missing generic interfaces for generic iteration.");
                EmitGenericEnumeratorForLoop(
                    forStatement,
                    scope,
                    beginLabel,
                    continueLabel,
                    endLabel,
                    iteration.EnumerableInterface!,
                    iteration.EnumeratorInterface!);
                break;

            default:
                EmitNonGenericEnumeratorForLoop(forStatement, scope, beginLabel, continueLabel, endLabel);
                break;
        }
    }

    private void EmitRangeForLoop(
        BoundForStatement forStatement,
        Scope scope,
        ILLabel beginLabel,
        ILLabel continueLabel,
        ILLabel endLabel,
        BoundRangeExpression range)
    {
        Debug.Assert(range.Left is not null && range.Right is not null, "Range iteration requires explicit bounds.");

        var intClrType = ResolveClrType(Compilation.GetSpecialType(SpecialType.System_Int32));

        var startLocal = ILGenerator.DeclareLocal(intClrType);
        new ExpressionGenerator(scope, range.Left!.Value).Emit();
        ILGenerator.Emit(OpCodes.Stloc, startLocal);

        var endLocal = ILGenerator.DeclareLocal(intClrType);
        new ExpressionGenerator(scope, range.Right!.Value).Emit();
        ILGenerator.Emit(OpCodes.Stloc, endLocal);

        var loopLocal = forStatement.Local;
        var elementType = loopLocal?.Type ?? forStatement.Iteration.ElementType;
        IILocal? elementLocal = null;
        if (loopLocal is not null)
        {
            elementLocal = ILGenerator.DeclareLocal(ResolveClrType(elementType));
            scope.AddLocal(loopLocal, elementLocal);
        }

        ILGenerator.MarkLabel(beginLabel);

        ILGenerator.Emit(OpCodes.Ldloc, startLocal);
        ILGenerator.Emit(OpCodes.Ldloc, endLocal);
        ILGenerator.Emit(OpCodes.Bge, endLabel);

        ILGenerator.Emit(OpCodes.Ldloc, startLocal);
        if (elementLocal is not null)
            ILGenerator.Emit(OpCodes.Stloc, elementLocal);
        else
            ILGenerator.Emit(OpCodes.Pop);

        new StatementGenerator(scope, forStatement.Body).Emit();

        ILGenerator.MarkLabel(continueLabel);
        ILGenerator.Emit(OpCodes.Ldloc, startLocal);
        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Add);
        ILGenerator.Emit(OpCodes.Stloc, startLocal);
        ILGenerator.Emit(OpCodes.Br, beginLabel);
        ILGenerator.MarkLabel(endLabel);
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

    private void EmitGenericEnumeratorForLoop(
        BoundForStatement forStatement,
        Scope scope,
        ILLabel beginLabel,
        ILLabel continueLabel,
        ILLabel endLabel,
        INamedTypeSymbol enumerableInterface,
        INamedTypeSymbol enumeratorInterface)
    {
        var enumerableClrType = ResolveClrType(enumerableInterface);
        ILGenerator.Emit(OpCodes.Castclass, enumerableClrType);

        var getEnumerator = enumerableInterface
            .GetMembers(nameof(IEnumerable.GetEnumerator))
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Parameters.Length == 0)
            ?? throw new InvalidOperationException("Missing IEnumerable<T>.GetEnumerator method.");
        ILGenerator.Emit(OpCodes.Callvirt, getEnumerator.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));

        var enumeratorType = getEnumerator.ReturnType as INamedTypeSymbol
            ?? throw new InvalidOperationException("Generic enumerator must return a named type.");
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

        var moveNext = enumeratorInterface
            .GetMembers(nameof(IEnumerator.MoveNext))
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Parameters.Length == 0)
            ?? Compilation.GetTypeByMetadataName("System.Collections.IEnumerator")
                ?.GetMembers(nameof(IEnumerator.MoveNext))
                .OfType<IMethodSymbol>()
                .FirstOrDefault(m => m.Parameters.Length == 0)
            ?? throw new InvalidOperationException("Missing IEnumerator.MoveNext method.");
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, moveNext.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));
        ILGenerator.Emit(OpCodes.Brfalse, endLabel);

        var currentProperty = enumeratorInterface
            .GetMembers(nameof(IEnumerator.Current))
            .OfType<IPropertySymbol>()
            .FirstOrDefault()
            ?? throw new InvalidOperationException("Missing IEnumerator<T>.Current property.");
        var currentGetter = currentProperty.GetMethod
            ?? throw new InvalidOperationException("Missing IEnumerator<T>.Current getter.");
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, currentGetter.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));
        if (elementLocal is not null)
            ILGenerator.Emit(OpCodes.Stloc, elementLocal);
        else
            ILGenerator.Emit(OpCodes.Pop);

        new StatementGenerator(scope, forStatement.Body).Emit();

        ILGenerator.MarkLabel(continueLabel);
        ILGenerator.Emit(OpCodes.Br, beginLabel);
        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitNonGenericEnumeratorForLoop(
        BoundForStatement forStatement,
        Scope scope,
        ILLabel beginLabel,
        ILLabel continueLabel,
        ILLabel endLabel)
    {
        var enumerable = Compilation.GetTypeByMetadataName("System.Collections.IEnumerable");
        var enumerableClrType = ResolveClrType(enumerable);
        ILGenerator.Emit(OpCodes.Castclass, enumerableClrType);

        var getEnumerator = enumerable
            .GetMembers(nameof(IEnumerable.GetEnumerator))
            .OfType<IMethodSymbol>()
            .First();
        ILGenerator.Emit(OpCodes.Callvirt, getEnumerator.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));

        var enumeratorType = getEnumerator.ReturnType;
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

        var moveNext = enumeratorType.GetMembers(nameof(IEnumerator.MoveNext))!.OfType<IMethodSymbol>().First();
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, moveNext.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));
        ILGenerator.Emit(OpCodes.Brfalse, endLabel);

        var currentProp = enumeratorType
            .GetMembers(nameof(IEnumerator.Current))
            .OfType<IPropertySymbol>()
            .First()
            .GetMethod!;
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, currentProp.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));

        var localClr = ResolveClrType(elementType);
        if (localClr.IsValueType)
            ILGenerator.Emit(OpCodes.Unbox_Any, localClr);
        else
            ILGenerator.Emit(OpCodes.Castclass, localClr);
        if (elementLocal is not null)
            ILGenerator.Emit(OpCodes.Stloc, elementLocal);
        else
            ILGenerator.Emit(OpCodes.Pop);

        new StatementGenerator(scope, forStatement.Body).Emit();

        ILGenerator.MarkLabel(continueLabel);
        ILGenerator.Emit(OpCodes.Br, beginLabel);
        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitTryStatement(BoundTryStatement tryStatement)
    {
        ILGenerator.BeginExceptionBlock();

        var exitLabel = MethodBodyGenerator.GetOrCreateReturnLabel();

        var tryScope = new Scope(this);
        tryScope.SetExceptionExitLabel(exitLabel);
        new StatementGenerator(tryScope, tryStatement.TryBlock).Emit();

        foreach (var catchClause in tryStatement.CatchClauses)
        {
            var catchType = catchClause.ExceptionType;
            if (catchType.TypeKind == TypeKind.Error)
                catchType = Compilation.GetSpecialType(SpecialType.System_Object);

            ILGenerator.BeginCatchBlock(ResolveClrType(catchType));

            var catchScope = new Scope(this);
            catchScope.SetExceptionExitLabel(exitLabel);

            if (catchClause.Local is { } localSymbol)
            {
                var localType = localSymbol.Type;
                if (localType.TypeKind == TypeKind.Error)
                    localType = Compilation.GetSpecialType(SpecialType.System_Object);

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
            new StatementGenerator(finallyScope, finallyBlock).Emit();
        }

        ILGenerator.EndExceptionBlock();
    }

    private void EmitBlockStatement(BoundBlockStatement blockStatement)
    {
        var scope = new Scope(this, blockStatement.LocalsToDispose);
        foreach (var s in blockStatement.Statements)
            new StatementGenerator(scope, s).Emit();

        EmitDispose(blockStatement.LocalsToDispose);
    }

    private void EmitLabeledStatement(BoundLabeledStatement labeledStatement)
    {
        if (Parent is not Scope scope)
            throw new InvalidOperationException("Labeled statements require an enclosing scope.");

        MethodBodyGenerator.RegisterLabelScope(labeledStatement.Label, scope);

        var ilLabel = MethodBodyGenerator.GetOrCreateLabel(labeledStatement.Label);
        ILGenerator.MarkLabel(ilLabel);

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

        if (declarator.Initializer is not null)
        {
            new ExpressionGenerator(this, declarator.Initializer).Emit();

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
        }
    }

}
