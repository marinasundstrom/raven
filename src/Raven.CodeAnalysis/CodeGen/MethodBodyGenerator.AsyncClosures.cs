using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal partial class MethodBodyGenerator
{
    private BoundBlockStatement PrepareAsyncMethodClosure(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        BoundBlockStatement body)
    {
        if (_lambdaClosure is not null ||
            stateMachine.AsyncMethod is not SourceMethodSymbol sourceMethod ||
            stateMachine.OriginalBody is not { } originalBody)
        {
            return body;
        }

        var captures = new HoistedLocalsCollector(sourceMethod);
        captures.Visit(originalBody);
        if (captures.CapturedSymbols.Count == 0)
            return body;

        var hostGenerator = MethodGenerator.TypeGenerator.CodeGen
            .GetOrCreateTypeGenerator(sourceMethod.ContainingType!);
        _hoistedSymbols = captures.CapturedSymbols;
        _outerMethodClosure = hostGenerator.EnsureSharedMethodClosure(
            sourceMethod, captures.CapturedSymbols.ToImmutableArray(),
            captures.LambdaSymbols, captures.LocalFunctionSymbols);

        var codeGen = MethodGenerator.TypeGenerator.CodeGen;
        var closureType = _outerMethodClosure.GetRuntimeType(codeGen);
        var constructor = closureType == _outerMethodClosure.TypeBuilder
            ? _outerMethodClosure.Constructor
            : TypeBuilder.GetConstructor(closureType, _outerMethodClosure.Constructor);
        var closureField = TypeBuilder.DefineField("<>sharedClosure", closureType, FieldAttributes.Private);
        _outerMethodClosureLocal = ILGenerator.DeclareLocal(closureType);

        // MoveNext can be entered repeatedly. Retain the same reference in the state
        // machine, including when the builder copies its value-type state machine.
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldfld, closureField);
        ILGenerator.Emit(OpCodes.Stloc, _outerMethodClosureLocal);
        var initialized = ILGenerator.DefineLabel();
        ILGenerator.Emit(OpCodes.Ldloc, _outerMethodClosureLocal);
        ILGenerator.Emit(OpCodes.Brtrue, initialized);
        ILGenerator.Emit(OpCodes.Newobj, constructor);
        ILGenerator.Emit(OpCodes.Stloc, _outerMethodClosureLocal);
        ILGenerator.Emit(OpCodes.Ldarg_0);
        ILGenerator.Emit(OpCodes.Ldloc, _outerMethodClosureLocal);
        ILGenerator.Emit(OpCodes.Stfld, closureField);

        foreach (var captured in captures.CapturedSymbols)
        {
            if (captured is ILocalSymbol || !_outerMethodClosure.TryGetField(captured, out var target))
                continue;

            IFieldSymbol? source = captured switch
            {
                IParameterSymbol parameter when stateMachine.ParameterFieldMap.TryGetValue(parameter, out var field) => field,
                ITypeSymbol => stateMachine.ThisField,
                IParameterSymbol { Name: "self" } => stateMachine.ThisField,
                _ => null
            };
            if (source is null)
                throw new InvalidOperationException($"No async storage for captured symbol '{captured}'.");

            ILGenerator.Emit(OpCodes.Ldloc, _outerMethodClosureLocal);
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Ldfld, codeGen.RuntimeSymbolResolver.GetFieldInfo(source));
            ILGenerator.Emit(OpCodes.Stfld, closureType == _outerMethodClosure.TypeBuilder
                ? target
                : TypeBuilder.GetField(closureType, target));
        }
        ILGenerator.MarkLabel(initialized);

        // Async lowering selected suspension storage before emission planned the
        // shared closure. Redirect those accesses to the original local symbol so
        // the normal capture emitter uses the same field as every callback.
        var locals = new Dictionary<IFieldSymbol, ILocalSymbol>(SymbolEqualityComparer.Default);
        foreach (var captured in captures.CapturedSymbols)
        {
            if (captured is ILocalSymbol local && stateMachine.TryGetHoistedLocalField(local, out var field))
                locals.Add(field, local);
        }
        return (BoundBlockStatement)new AsyncCapturedLocalRewriter(locals).VisitBlockStatement(body)!;
    }

    private sealed class AsyncCapturedLocalRewriter(Dictionary<IFieldSymbol, ILocalSymbol> locals) : BoundTreeRewriter
    {
        public override BoundExpression? VisitExpression(BoundExpression? node)
            => node is BoundFunctionExpression ? node : base.VisitExpression(node);

        public override BoundNode? VisitFunctionStatement(BoundFunctionStatement node) => node;

        public override BoundNode? VisitFieldAccess(BoundFieldAccess node)
            => locals.TryGetValue(node.Field, out var local)
                ? new BoundLocalAccess(local, node.Reason)
                : base.VisitFieldAccess(node);

        public override BoundNode? VisitMemberAccessExpression(BoundMemberAccessExpression node)
            => node.Member is IFieldSymbol field && locals.TryGetValue(field, out var local)
                ? new BoundLocalAccess(local, node.Reason)
                : base.VisitMemberAccessExpression(node);

        public override BoundNode? VisitFieldAssignmentExpression(BoundFieldAssignmentExpression node)
            => locals.TryGetValue(node.Field, out var local)
                ? new BoundLocalAssignmentExpression(local, new BoundLocalAccess(local),
                    VisitExpression(node.Right)!, node.UnitType)
                : base.VisitFieldAssignmentExpression(node);

        public override BoundNode? VisitAddressOfExpression(BoundAddressOfExpression node)
            => node.Symbol is IFieldSymbol field && locals.TryGetValue(field, out var local)
                ? new BoundAddressOfExpression(new BoundLocalAccess(local))
                : base.VisitAddressOfExpression(node);
    }
}
