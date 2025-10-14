using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class AsyncLowerer
{
    public static AsyncMethodAnalysis Analyze(SourceMethodSymbol method, BoundBlockStatement body)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        if (!method.IsAsync)
            return new AsyncMethodAnalysis(requiresStateMachine: false, containsAwait: false);

        var finder = new AwaitExpressionFinder();
        finder.VisitBlockStatement(body);

        method.SetContainsAwait(finder.FoundAwait);

        var requiresStateMachine = finder.FoundAwait;
        return new AsyncMethodAnalysis(requiresStateMachine, finder.FoundAwait);
    }

    public static BoundBlockStatement Rewrite(SourceMethodSymbol method, BoundBlockStatement body)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));
        if (body is null)
            throw new ArgumentNullException(nameof(body));

        var analysis = Analyze(method, body);
        if (!analysis.RequiresStateMachine)
            return body;

        var compilation = GetCompilation(method);

        if (method.AsyncStateMachine is null)
        {
            var stateMachine = compilation.CreateAsyncStateMachine(method);
            method.SetAsyncStateMachine(stateMachine);
        }

        var asyncStateMachine = method.AsyncStateMachine;
        if (asyncStateMachine is null)
            throw new InvalidOperationException("Async state machine not created.");

        if (asyncStateMachine.OriginalBody is null)
            asyncStateMachine.SetOriginalBody(body);

        if (asyncStateMachine.MoveNextBody is null)
        {
            var moveNextBody = CreateMoveNextBody(compilation, asyncStateMachine);
            asyncStateMachine.SetMoveNextBody(moveNextBody);
        }

        if (asyncStateMachine.SetStateMachineBody is null)
        {
            var setStateMachineBody = CreateSetStateMachineBody(asyncStateMachine);
            if (setStateMachineBody is not null)
                asyncStateMachine.SetSetStateMachineBody(setStateMachineBody);
        }

        return RewriteMethodBody(compilation, method, asyncStateMachine);
    }

    public static bool ShouldRewrite(SourceMethodSymbol method, BoundBlockStatement body)
    {
        return Analyze(method, body).RequiresStateMachine;
    }

    private static BoundBlockStatement CreateMoveNextBody(
        Compilation compilation,
        SynthesizedAsyncStateMachineTypeSymbol stateMachine)
    {
        var originalBody = stateMachine.OriginalBody ?? new BoundBlockStatement(Array.Empty<BoundStatement>());

        var awaitRewriter = new AwaitLoweringRewriter(stateMachine);
        var rewrittenBody = awaitRewriter.Rewrite(originalBody);

        var entryLabel = CreateLabel(stateMachine, "state");

        var tryStatements = new List<BoundStatement>();
        tryStatements.AddRange(CreateStateDispatchStatements(compilation, stateMachine, entryLabel, awaitRewriter.Dispatches));

        var entryStatements = new List<BoundStatement>(rewrittenBody.Statements);
        entryStatements.AddRange(CreateCompletionStatements(stateMachine));
        var entryBlock = new BoundBlockStatement(entryStatements, rewrittenBody.LocalsToDispose);

        tryStatements.Add(new BoundLabeledStatement(entryLabel, entryBlock));

        var tryBlock = new BoundBlockStatement(tryStatements);

        var catchClauses = ImmutableArray<BoundCatchClause>.Empty;
        var catchClause = CreateExceptionCatchClause(compilation, stateMachine);
        if (catchClause is not null)
            catchClauses = ImmutableArray.Create(catchClause);

        var tryStatement = new BoundTryStatement(tryBlock, catchClauses, finallyBlock: null);
        return new BoundBlockStatement(new BoundStatement[] { tryStatement });
    }

    private static BoundBlockStatement RewriteMethodBody(
        Compilation compilation,
        SourceMethodSymbol method,
        SynthesizedAsyncStateMachineTypeSymbol stateMachine)
    {
        var constructedStateMachine = stateMachine.CreateConstructedInstance(method);

        method.SetConstructedAsyncStateMachine(constructedStateMachine);

        if (method.ReturnType is INamedTypeSymbol methodReturn &&
            methodReturn.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            methodReturn.TypeArguments.Length == 1 &&
            constructedStateMachine.BuilderField.Type is INamedTypeSymbol builderType &&
            builderType.OriginalDefinition.SpecialType == SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T)
        {
            var awaitedType = builderType switch
            {
                ConstructedNamedTypeSymbol constructed when constructed.TypeArguments.Length == 1 => constructed.TypeArguments[0],
                _ when builderType.TypeArguments.Length == 1 => builderType.TypeArguments[0],
                _ => null
            };

            if (awaitedType is not null &&
                !SymbolEqualityComparer.Default.Equals(methodReturn.TypeArguments[0], awaitedType) &&
                methodReturn.OriginalDefinition is INamedTypeSymbol taskDefinition)
            {
                var closedTask = taskDefinition.Construct(awaitedType);
                method.SetReturnType(closedTask);
            }
        }

        var statements = new List<BoundStatement>();

        var asyncLocal = new SourceLocalSymbol(
            "<>async",
            constructedStateMachine.Type,
            isMutable: true,
            method,
            method.ContainingType,
            method.ContainingNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>());

        var creation = new BoundObjectCreationExpression(
            constructedStateMachine.Constructor,
            Array.Empty<BoundExpression>());
        var declarator = new BoundVariableDeclarator(asyncLocal, creation);
        statements.Add(new BoundLocalDeclarationStatement(new[] { declarator }));

        if (constructedStateMachine.ThisField is not null)
        {
            var receiver = new BoundLocalAccess(asyncLocal);
            var value = new BoundSelfExpression(constructedStateMachine.ThisField.Type);
            var assignment = new BoundFieldAssignmentExpression(receiver, constructedStateMachine.ThisField, value, requiresReceiverAddress: true);
            statements.Add(new BoundAssignmentStatement(assignment));
        }

        foreach (var parameter in method.Parameters)
        {
            if (!constructedStateMachine.ParameterFieldMap.TryGetValue(parameter, out var field))
                continue;

            var receiver = new BoundLocalAccess(asyncLocal);
            var value = new BoundParameterAccess(parameter);
            var assignment = new BoundFieldAssignmentExpression(receiver, field, value, requiresReceiverAddress: true);
            statements.Add(new BoundAssignmentStatement(assignment));
        }

        var stateReceiver = new BoundLocalAccess(asyncLocal);
        var initialState = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            -1,
            constructedStateMachine.StateField.Type);
        var stateAssignment = new BoundFieldAssignmentExpression(stateReceiver, constructedStateMachine.StateField, initialState, requiresReceiverAddress: true);
        statements.Add(new BoundAssignmentStatement(stateAssignment));

        var builderInitialization = CreateBuilderInitializationStatement(asyncLocal, constructedStateMachine);
        if (builderInitialization is not null)
            statements.Add(builderInitialization);

        var builderStartStatement = CreateBuilderStartStatement(asyncLocal, constructedStateMachine);
        if (builderStartStatement is not null)
        {
            statements.Add(builderStartStatement);
        }
        else
        {
            var moveNextInvocation = new BoundInvocationExpression(
                constructedStateMachine.MoveNextMethod,
                Array.Empty<BoundExpression>(),
                receiver: new BoundLocalAccess(asyncLocal),
                requiresReceiverAddress: true);
            statements.Add(new BoundExpressionStatement(moveNextInvocation));
        }

        BoundExpression? returnExpression = null;
        if (method.ReturnType.SpecialType != SpecialType.System_Void)
        {
            returnExpression = CreateReturnExpression(method, constructedStateMachine, asyncLocal);
        }

        statements.Add(new BoundReturnStatement(returnExpression));

        return new BoundBlockStatement(statements);
    }

    private static BoundCatchClause? CreateExceptionCatchClause(Compilation compilation, SynthesizedAsyncStateMachineTypeSymbol stateMachine)
    {
        var exceptionType = compilation.GetSpecialType(SpecialType.System_Exception);

        var exceptionLocal = new SourceLocalSymbol(
            "<>ex",
            exceptionType,
            isMutable: true,
            stateMachine.MoveNextMethod,
            stateMachine,
            stateMachine.ContainingNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>());

        var statements = new List<BoundStatement>
        {
            CreateStateAssignment(stateMachine, -2)
        };

        statements.AddRange(CreateDisposeStatements(stateMachine, EnumerateReverse(stateMachine.HoistedLocalsToDispose)));

        var setException = CreateBuilderSetExceptionStatement(stateMachine, exceptionLocal);
        if (setException is not null)
            statements.Add(setException);

        statements.Add(new BoundReturnStatement(null));

        var catchBlock = new BoundBlockStatement(statements);
        return new BoundCatchClause(exceptionType, exceptionLocal, catchBlock);
    }

    private static IEnumerable<BoundStatement> CreateStateDispatchStatements(
        Compilation compilation,
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        ILabelSymbol entryLabel,
        ImmutableArray<StateDispatch> dispatches)
    {
        var statements = new List<BoundStatement>();

        var stateAccess = new BoundFieldAccess(stateMachine.StateField);
        var initialState = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            -1,
            stateAccess.Type);

        if (!BoundBinaryOperator.TryLookup(compilation, SyntaxKind.EqualsEqualsToken, stateAccess.Type, initialState.Type, out var equals))
            throw new InvalidOperationException("Async lowering requires integer equality operator.");

        var condition = new BoundBinaryExpression(stateAccess, equals, initialState);
        var gotoEntry = new BoundGotoStatement(entryLabel);
        var thenBlock = new BoundBlockStatement(new BoundStatement[] { gotoEntry });

        statements.Add(new BoundIfStatement(condition, thenBlock));

        foreach (var dispatch in dispatches)
        {
            var stateLiteral = new BoundLiteralExpression(
                BoundLiteralExpressionKind.NumericLiteral,
                dispatch.State,
                stateAccess.Type);

            if (!BoundBinaryOperator.TryLookup(compilation, SyntaxKind.EqualsEqualsToken, stateAccess.Type, stateLiteral.Type, out var stateEquals))
                throw new InvalidOperationException("Async lowering requires integer equality operator.");

            var stateCondition = new BoundBinaryExpression(new BoundFieldAccess(stateMachine.StateField), stateEquals, stateLiteral);
            var gotoLabel = new BoundGotoStatement(dispatch.Label);
            var gotoBlock = new BoundBlockStatement(new BoundStatement[] { gotoLabel });

            statements.Add(new BoundIfStatement(stateCondition, gotoBlock));
        }

        statements.Add(new BoundGotoStatement(entryLabel));

        return statements;
    }

    private static IEnumerable<BoundStatement> CreateCompletionStatements(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine)
    {
        yield return CreateStateAssignment(stateMachine, -2);

        var setResult = CreateBuilderSetResultStatement(stateMachine, expression: null);
        if (setResult is not null)
            yield return setResult;

        yield return new BoundReturnStatement(null);
    }

    private static BoundStatement? CreateBuilderSetResultStatement(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        BoundExpression? expression)
    {
        var builderField = stateMachine.BuilderField;
        if (builderField.Type is not INamedTypeSymbol builderType)
            return null;

        var setResultMethod = FindSetResultMethod(builderType);
        if (setResultMethod is null)
            return null;

        var arguments = Array.Empty<BoundExpression>();

        if (setResultMethod.Parameters.Length == 1)
        {
            BoundExpression? argument = expression;

            if (IsEffectivelyVoidExpression(argument))
                argument = null;

            if (argument is null)
            {
                argument = CreateDefaultValueExpression(setResultMethod.Parameters[0].Type);
                if (argument is null)
                    return null;
            }

            arguments = new[] { argument };
        }
        else if (setResultMethod.Parameters.Length != 0)
        {
            return null;
        }

        if (setResultMethod.Parameters.Length == 0 && expression is not null && !IsEffectivelyVoidExpression(expression))
            return null;

        var receiver = new BoundMemberAccessExpression(new BoundSelfExpression(stateMachine), builderField);
        var invocation = new BoundInvocationExpression(setResultMethod, arguments, receiver, requiresReceiverAddress: true);
        return new BoundExpressionStatement(invocation);
    }

    private static bool IsEffectivelyVoidExpression(BoundExpression? expression)
    {
        if (expression is null)
            return true;

        switch (expression)
        {
            case BoundUnitExpression:
                return true;

            case BoundLiteralExpression literal when literal.Value is null:
                return true;

            case BoundCastExpression cast when cast.Conversion.IsIdentity && IsEffectivelyVoidExpression(cast.Expression):
                return true;

            case BoundAsExpression asExpression when asExpression.Conversion.IsIdentity && IsEffectivelyVoidExpression(asExpression.Expression):
                return true;

            case BoundMemberAccessExpression memberAccess when IsCompletedTaskAccess(memberAccess):
                return true;
        }

        return false;
    }

    private static bool IsCompletedTaskAccess(BoundMemberAccessExpression memberAccess)
    {
        if (memberAccess.Member is not ISymbol member)
            return false;

        if (member is not (IPropertySymbol or IFieldSymbol))
            return false;

        if (member.Name != "CompletedTask")
            return false;

        return member.ContainingType?.SpecialType == SpecialType.System_Threading_Tasks_Task;
    }

    private static BoundExpression? CreateDefaultValueExpression(ITypeSymbol type)
    {
        if (type.SpecialType == SpecialType.System_Void)
            return null;

        if (type.IsReferenceType)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, type);

        if (type.SpecialType == SpecialType.System_Boolean)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.FalseLiteral, false, type);

        if (type.SpecialType == SpecialType.System_Int32)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0, type);

        if (type.SpecialType == SpecialType.System_Double)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0d, type);

        if (type.SpecialType == SpecialType.System_Single)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0f, type);

        if (type.SpecialType == SpecialType.System_Int64)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0L, type);

        if (type.SpecialType == SpecialType.System_Decimal)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0m, type);

        if (type.SpecialType == SpecialType.System_Int16)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, (short)0, type);

        if (type.SpecialType == SpecialType.System_Byte)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, (byte)0, type);

        if (type.SpecialType == SpecialType.System_UInt16)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, (ushort)0, type);

        if (type.SpecialType == SpecialType.System_UInt32)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0u, type);

        if (type.SpecialType == SpecialType.System_UInt64)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0ul, type);

        return null;
    }

    private static IMethodSymbol? FindSetResultMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetResult"))
        {
            if (member is IMethodSymbol method)
                return SubstituteBuilderMethodIfNeeded(builderType, method);
        }

        return null;
    }

    private static LabelSymbol CreateLabel(SynthesizedAsyncStateMachineTypeSymbol stateMachine, string name)
    {
        return new LabelSymbol(
            name,
            stateMachine.MoveNextMethod,
            stateMachine,
            stateMachine.ContainingNamespace,
            new[] { Location.None },
            Array.Empty<SyntaxReference>());
    }

    private static BoundAssignmentStatement CreateStateAssignment(SynthesizedAsyncStateMachineTypeSymbol stateMachine, int state)
    {
        var literal = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            state,
            stateMachine.StateField.Type);

        var assignment = CreateStateMachineFieldAssignment(stateMachine, stateMachine.StateField, literal);
        return new BoundAssignmentStatement(assignment);
    }

    private static BoundFieldAssignmentExpression CreateStateMachineFieldAssignment(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        SourceFieldSymbol field,
        BoundExpression value)
    {
        return new BoundFieldAssignmentExpression(
            new BoundSelfExpression(stateMachine),
            field,
            value,
            requiresReceiverAddress: true);
    }

    private static BoundStatement? CreateBuilderSetExceptionStatement(SynthesizedAsyncStateMachineTypeSymbol stateMachine, ILocalSymbol exceptionLocal)
    {
        var builderField = stateMachine.BuilderField;
        if (builderField.Type is not INamedTypeSymbol builderType)
            return null;

        var setExceptionMethod = FindSetExceptionMethod(builderType);
        if (setExceptionMethod is null)
            return null;

        var builderAccess = new BoundMemberAccessExpression(new BoundSelfExpression(stateMachine), builderField);
        var exceptionAccess = new BoundLocalAccess(exceptionLocal);

        var invocation = new BoundInvocationExpression(
            setExceptionMethod,
            new BoundExpression[] { exceptionAccess },
            builderAccess,
            requiresReceiverAddress: true);

        return new BoundExpressionStatement(invocation);
    }

    private static IEnumerable<BoundStatement> CreateDisposeStatements(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        IEnumerable<SourceFieldSymbol> fields)
    {
        var compilation = stateMachine.Compilation;
        var disposableType = compilation.GetSpecialType(SpecialType.System_IDisposable);
        if (disposableType.TypeKind == TypeKind.Error)
            yield break;

        IMethodSymbol? disposeMethod = null;
        foreach (var member in disposableType.GetMembers(nameof(IDisposable.Dispose)))
        {
            if (member is IMethodSymbol { Parameters.Length: 0 } method)
            {
                disposeMethod = method;
                break;
            }
        }

        if (disposeMethod is null)
            yield break;

        foreach (var field in fields)
        {
            yield return CreateDisposeStatement(stateMachine, field, disposeMethod);
        }
    }

    private static IEnumerable<SourceFieldSymbol> EnumerateReverse(ImmutableArray<SourceFieldSymbol> fields)
    {
        if (fields.IsDefaultOrEmpty)
            yield break;

        for (var i = fields.Length - 1; i >= 0; i--)
            yield return fields[i];
    }

    private static BoundStatement CreateDisposeStatement(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        SourceFieldSymbol field,
        IMethodSymbol disposeMethod)
    {
        var compilation = stateMachine.Compilation;
        var receiver = new BoundMemberAccessExpression(new BoundSelfExpression(stateMachine), field);
        var invocation = new BoundInvocationExpression(disposeMethod, Array.Empty<BoundExpression>(), receiver);
        var disposeCall = new BoundExpressionStatement(invocation);

        BoundStatement? clearStatement = null;
        var defaultValue = CreateDefaultValueExpression(field.Type);
        if (defaultValue is not null)
        {
            var assignment = CreateStateMachineFieldAssignment(stateMachine, field, defaultValue);
            clearStatement = new BoundAssignmentStatement(assignment);
        }

        if (field.Type.IsReferenceType)
        {
            var nullLiteral = new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, field.Type);

            if (BoundBinaryOperator.TryLookup(compilation, SyntaxKind.NotEqualsToken, field.Type, field.Type, out var notEquals))
            {
                var access = new BoundMemberAccessExpression(new BoundSelfExpression(stateMachine), field);
                var condition = new BoundBinaryExpression(access, notEquals, nullLiteral);

                var statements = clearStatement is null
                    ? new BoundStatement[] { disposeCall }
                    : new BoundStatement[] { disposeCall, clearStatement };

                return new BoundIfStatement(condition, new BoundBlockStatement(statements));
            }
        }

        if (clearStatement is null)
            return disposeCall;

        return new BoundBlockStatement(new BoundStatement[] { disposeCall, clearStatement });
    }

    private static IMethodSymbol? FindSetExceptionMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetException"))
        {
            if (member is IMethodSymbol { Parameters.Length: 1 } method)
                return SubstituteBuilderMethodIfNeeded(builderType, method);
        }

        return null;
    }

    private static IMethodSymbol? FindAwaitOnCompletedMethod(INamedTypeSymbol builderType)
    {
        return FindAwaitMethod(builderType, "AwaitUnsafeOnCompleted")
            ?? FindAwaitMethod(builderType, "AwaitOnCompleted");
    }

    private static IMethodSymbol? FindAwaitMethod(INamedTypeSymbol builderType, string name)
    {
        foreach (var member in builderType.GetMembers(name))
        {
            if (member is IMethodSymbol { Parameters.Length: 2 } method)
                return SubstituteBuilderMethodIfNeeded(builderType, method);
        }

        return null;
    }

    private static IMethodSymbol? SubstituteBuilderMethodIfNeeded(INamedTypeSymbol builderType, IMethodSymbol method)
    {
        if (builderType is ConstructedNamedTypeSymbol constructed &&
            method is not SubstitutedMethodSymbol &&
            !SymbolEqualityComparer.Default.Equals(method.ContainingType, constructed))
        {
            return new SubstitutedMethodSymbol(method, constructed);
        }

        return method;
    }

    private static IPropertySymbol? SubstituteBuilderPropertyIfNeeded(INamedTypeSymbol builderType, IPropertySymbol property)
    {
        if (builderType is ConstructedNamedTypeSymbol constructed &&
            property is not SubstitutedPropertySymbol &&
            !SymbolEqualityComparer.Default.Equals(property.ContainingType, constructed))
        {
            return new SubstitutedPropertySymbol(property, constructed);
        }

        return property;
    }

    private sealed class AwaitLoweringRewriter : BoundTreeRewriter
    {
        private readonly SynthesizedAsyncStateMachineTypeSymbol _stateMachine;
        private readonly List<StateDispatch> _dispatches = new();
        private readonly Dictionary<ILocalSymbol, SourceFieldSymbol> _hoistedLocals = new(SymbolEqualityComparer.Default);
        private ImmutableDictionary<ILocalSymbol, bool> _hoistableLocals = ImmutableDictionary<ILocalSymbol, bool>.Empty;
        private int _nextState;
        private int _nextHoistedLocalId;
        private int _nextAwaitResultId;
        private int _nextAwaiterLocalId;

        public AwaitLoweringRewriter(SynthesizedAsyncStateMachineTypeSymbol stateMachine)
        {
            _stateMachine = stateMachine ?? throw new ArgumentNullException(nameof(stateMachine));
            _nextHoistedLocalId = DetermineInitialHoistedLocalId(stateMachine);
            _nextAwaitResultId = 0;
            _nextAwaiterLocalId = 0;
        }

        public ImmutableArray<StateDispatch> Dispatches => _dispatches.ToImmutableArray();

        public BoundBlockStatement Rewrite(BoundBlockStatement body)
        {
            if (body is null)
                throw new ArgumentNullException(nameof(body));

            _hoistableLocals = AwaitCaptureWalker.Analyze(body);

            foreach (var local in _hoistableLocals.Keys)
                AddHoistedLocal(local);

            return (BoundBlockStatement)VisitBlockStatement(body)!;
        }

        private static int DetermineInitialHoistedLocalId(SynthesizedAsyncStateMachineTypeSymbol stateMachine)
        {
            const string prefix = "<>local";
            var next = 0;

            foreach (var field in stateMachine.HoistedLocals)
            {
                if (!field.Name.StartsWith(prefix, StringComparison.Ordinal))
                    continue;

                if (int.TryParse(field.Name.Substring(prefix.Length), out var parsed) && parsed >= next)
                    next = parsed + 1;
            }

            return next;
        }

        public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
        {
            if (node is null)
                return null;

            var statements = new List<BoundStatement>();
            var hoistedDisposables = CollectHoistedDisposables(node.LocalsToDispose);

            foreach (var statement in node.Statements)
            {
                var rewritten = VisitStatement(statement);

                if (rewritten is BoundBlockStatement block && block.LocalsToDispose.Length == 0)
                {
                    statements.AddRange(block.Statements);
                }
                else
                {
                    statements.Add(rewritten);
                }
            }

            if (hoistedDisposables.Count > 0)
            {
                var disposeStatements = CreateDisposeStatements(
                    _stateMachine,
                    EnumerateReverse(hoistedDisposables));

                statements.AddRange(disposeStatements);
            }

            var localsToDispose = FilterLocalsToDispose(node.LocalsToDispose);
            return new BoundBlockStatement(statements, localsToDispose);
        }

        private List<SourceFieldSymbol> CollectHoistedDisposables(ImmutableArray<ILocalSymbol> locals)
        {
            var result = new List<SourceFieldSymbol>();
            if (_hoistableLocals.Count == 0 || locals.IsDefaultOrEmpty)
                return result;

            foreach (var local in locals)
            {
                if (!_hoistableLocals.TryGetValue(local, out var requiresDispose) || !requiresDispose)
                    continue;

                if (_hoistedLocals.TryGetValue(local, out var field))
                    result.Add(field);
            }

            return result;
        }

        private static IEnumerable<SourceFieldSymbol> EnumerateReverse(List<SourceFieldSymbol> fields)
        {
            for (var i = fields.Count - 1; i >= 0; i--)
                yield return fields[i];
        }

        public override BoundNode? VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
        {
            if (node is null)
                return null;

            var hoistedStatements = new List<BoundStatement>();
            var remainingDeclarators = new List<BoundVariableDeclarator>();

            foreach (var declarator in node.Declarators)
            {
                var initializer = VisitExpression(declarator.Initializer) ?? declarator.Initializer;

                if (_hoistedLocals.TryGetValue(declarator.Local, out var field))
                {
                    if (initializer is not null)
                    {
                        var receiver = new BoundSelfExpression(_stateMachine);
                        var assignment = new BoundFieldAssignmentExpression(receiver, field, initializer, requiresReceiverAddress: true);
                        hoistedStatements.Add(new BoundAssignmentStatement(assignment));
                    }
                }
                else
                {
                    if (!ReferenceEquals(initializer, declarator.Initializer))
                        remainingDeclarators.Add(new BoundVariableDeclarator(declarator.Local, initializer));
                    else
                        remainingDeclarators.Add(declarator);
                }
            }

            if (remainingDeclarators.Count == 0)
            {
                return hoistedStatements.Count switch
                {
                    0 => new BoundBlockStatement(Array.Empty<BoundStatement>()),
                    1 => hoistedStatements[0],
                    _ => new BoundBlockStatement(hoistedStatements),
                };
            }

            var declaration = new BoundLocalDeclarationStatement(remainingDeclarators, node.IsUsing);
            if (hoistedStatements.Count == 0)
                return declaration;

            hoistedStatements.Add(declaration);
            return new BoundBlockStatement(hoistedStatements);
        }

        public override BoundNode? VisitExpressionStatement(BoundExpressionStatement node)
        {
            if (node is null)
                return null;

            var expression = VisitExpression(node.Expression) ?? node.Expression;

            if (expression is BoundBlockExpression blockExpression)
                return new BoundBlockStatement(blockExpression.Statements.ToArray(), blockExpression.LocalsToDispose);

            if (!ReferenceEquals(expression, node.Expression))
                return new BoundExpressionStatement(expression);

            return node;
        }

        public override BoundNode? VisitAssignmentStatement(BoundAssignmentStatement node)
        {
            if (node is null)
                return null;

            var expression = Visit(node.Expression) as BoundAssignmentExpression ?? node.Expression;
            if (!ReferenceEquals(expression, node.Expression))
                return new BoundAssignmentStatement(expression);

            return node;
        }

        public override BoundNode? VisitIfStatement(BoundIfStatement node)
        {
            if (node is null)
                return null;

            var condition = VisitExpression(node.Condition) ?? node.Condition;
            var thenStatement = VisitStatement(node.ThenNode);
            var elseStatement = node.ElseNode is null ? null : VisitStatement(node.ElseNode);

            if (!ReferenceEquals(condition, node.Condition) ||
                !ReferenceEquals(thenStatement, node.ThenNode) ||
                !ReferenceEquals(elseStatement, node.ElseNode))
            {
                return new BoundIfStatement(condition, thenStatement, elseStatement);
            }

            return node;
        }

        public override BoundNode? VisitWhileStatement(BoundWhileStatement node)
        {
            if (node is null)
                return null;

            var condition = VisitExpression(node.Condition) ?? node.Condition;
            var body = VisitStatement(node.Body);

            if (!ReferenceEquals(condition, node.Condition) ||
                !ReferenceEquals(body, node.Body))
            {
                return new BoundWhileStatement(condition, body);
            }

            return node;
        }

        public override BoundNode? VisitForStatement(BoundForStatement node)
        {
            if (node is null)
                return null;

            var collection = VisitExpression(node.Collection) ?? node.Collection;
            var body = VisitStatement(node.Body);

            if (!ReferenceEquals(collection, node.Collection) ||
                !ReferenceEquals(body, node.Body))
            {
                return new BoundForStatement(node.Local, node.Iteration, collection, body);
            }

            return node;
        }

        public override BoundNode? VisitTryStatement(BoundTryStatement node)
        {
            if (node is null)
                return null;

            var tryBlock = (BoundBlockStatement)VisitBlockStatement(node.TryBlock)!;
            var changed = !ReferenceEquals(tryBlock, node.TryBlock);

            var catchClauses = node.CatchClauses;
            ImmutableArray<BoundCatchClause> rewrittenCatchClauses = catchClauses;

            if (!catchClauses.IsDefaultOrEmpty && catchClauses.Length > 0)
            {
                var builder = ImmutableArray.CreateBuilder<BoundCatchClause>(catchClauses.Length);

                foreach (var clause in catchClauses)
                {
                    var rewrittenClause = RewriteCatchClause(clause);
                    builder.Add(rewrittenClause);

                    if (!ReferenceEquals(rewrittenClause, clause))
                        changed = true;
                }

                rewrittenCatchClauses = builder.MoveToImmutable();
            }

            BoundBlockStatement? finallyBlock = null;
            if (node.FinallyBlock is not null)
            {
                finallyBlock = (BoundBlockStatement)VisitBlockStatement(node.FinallyBlock)!;
                if (!ReferenceEquals(finallyBlock, node.FinallyBlock))
                    changed = true;
            }

            if (changed)
                return new BoundTryStatement(tryBlock, rewrittenCatchClauses, finallyBlock);

            return node;
        }

        private BoundCatchClause RewriteCatchClause(BoundCatchClause clause)
        {
            if (clause is null)
                throw new ArgumentNullException(nameof(clause));

            var block = (BoundBlockStatement)VisitBlockStatement(clause.Block)!;
            if (!ReferenceEquals(block, clause.Block))
                return new BoundCatchClause(clause.ExceptionType, clause.Local, block);

            return clause;
        }

        public override BoundNode? VisitReturnStatement(BoundReturnStatement node)
        {
            if (node is null)
                return null;

            var expression = VisitExpression(node.Expression) ?? node.Expression;

            var statements = new List<BoundStatement>();
            ImmutableArray<ILocalSymbol> localsToDispose = ImmutableArray<ILocalSymbol>.Empty;
            BoundExpression? resultExpression = expression;

            if (expression is BoundBlockExpression blockExpression)
            {
                localsToDispose = blockExpression.LocalsToDispose;
                var blockStatements = blockExpression.Statements.ToArray();

                if (blockStatements.Length > 0 &&
                    blockStatements[^1] is BoundExpressionStatement resultStatement)
                {
                    for (var i = 0; i < blockStatements.Length - 1; i++)
                        statements.Add(blockStatements[i]);

                    resultExpression = resultStatement.Expression;
                }
                else
                {
                    statements.AddRange(blockStatements);
                    resultExpression = null;
                }
            }

            if (IsEffectivelyVoidExpression(resultExpression))
                resultExpression = null;

            statements.Add(CreateStateAssignment(_stateMachine, -2));

            var setResult = CreateBuilderSetResultStatement(_stateMachine, resultExpression);
            if (setResult is not null)
                statements.Add(setResult);

            statements.Add(new BoundReturnStatement(null));

            if (!localsToDispose.IsDefaultOrEmpty && localsToDispose.Length > 0)
                return new BoundBlockStatement(statements, localsToDispose);

            return new BoundBlockStatement(statements);
        }

        public override BoundNode? VisitAwaitExpression(BoundAwaitExpression node)
        {
            if (node is null)
                return null;

            var expression = VisitExpression(node.Expression) ?? node.Expression;
            if (!ReferenceEquals(expression, node.Expression))
            {
                node = new BoundAwaitExpression(
                    expression,
                    node.ResultType,
                    node.AwaiterType,
                    node.GetAwaiterMethod,
                    node.GetResultMethod,
                    node.IsCompletedProperty);
            }

            return LowerAwaitExpressionToBlock(node);
        }

        public override BoundNode? VisitTryExpression(BoundTryExpression node)
        {
            if (node is null)
                return null;

            var expression = VisitExpression(node.Expression) ?? node.Expression;

            if (!ReferenceEquals(expression, node.Expression))
            {
                var type = node.Type ?? _stateMachine.Compilation.ErrorTypeSymbol;
                return new BoundTryExpression(expression, node.ExceptionType, type);
            }

            return node;
        }

        public override BoundExpression? VisitExpression(BoundExpression? node)
        {
            if (node is null)
                return null;

            switch (node)
            {
                case BoundAwaitExpression awaitExpression:
                    return (BoundExpression?)VisitAwaitExpression(awaitExpression);

                case BoundBinaryExpression binaryExpression:
                {
                    var left = VisitExpression(binaryExpression.Left) ?? binaryExpression.Left;
                    var right = VisitExpression(binaryExpression.Right) ?? binaryExpression.Right;

                    if (!ReferenceEquals(left, binaryExpression.Left) || !ReferenceEquals(right, binaryExpression.Right))
                        return new BoundBinaryExpression(left, binaryExpression.Operator, right);

                    return binaryExpression;
                }

                case BoundUnaryExpression unaryExpression:
                {
                    var operand = VisitExpression(unaryExpression.Operand) ?? unaryExpression.Operand;
                    if (!ReferenceEquals(operand, unaryExpression.Operand))
                        return new BoundUnaryExpression(unaryExpression.Operator, operand);

                    return unaryExpression;
                }

                case BoundInvocationExpression invocationExpression:
                {
                    var receiver = VisitExpression(invocationExpression.Receiver) ?? invocationExpression.Receiver;
                    var extensionReceiver = invocationExpression.ExtensionReceiver is null
                        ? null
                        : VisitExpression(invocationExpression.ExtensionReceiver) ?? invocationExpression.ExtensionReceiver;

                    var originalArguments = invocationExpression.Arguments.ToArray();
                    var rewrittenArguments = new BoundExpression[originalArguments.Length];
                    var changed = !ReferenceEquals(receiver, invocationExpression.Receiver) ||
                        !ReferenceEquals(extensionReceiver, invocationExpression.ExtensionReceiver);

                    for (var i = 0; i < originalArguments.Length; i++)
                    {
                        var rewritten = VisitExpression(originalArguments[i]) ?? originalArguments[i];
                        rewrittenArguments[i] = rewritten;
                        if (!ReferenceEquals(rewritten, originalArguments[i]))
                            changed = true;
                    }

                    if (changed)
                        return new BoundInvocationExpression(
                            invocationExpression.Method,
                            rewrittenArguments,
                            receiver,
                            extensionReceiver,
                            invocationExpression.RequiresReceiverAddress);

                    return invocationExpression;
                }

                case BoundObjectCreationExpression objectCreationExpression:
                {
                    var receiver = VisitExpression(objectCreationExpression.Receiver) ?? objectCreationExpression.Receiver;
                    var originalArguments = objectCreationExpression.Arguments.ToArray();
                    var rewrittenArguments = new BoundExpression[originalArguments.Length];
                    var changed = !ReferenceEquals(receiver, objectCreationExpression.Receiver);

                    for (var i = 0; i < originalArguments.Length; i++)
                    {
                        var rewritten = VisitExpression(originalArguments[i]) ?? originalArguments[i];
                        rewrittenArguments[i] = rewritten;
                        if (!ReferenceEquals(rewritten, originalArguments[i]))
                            changed = true;
                    }

                    if (changed)
                        return new BoundObjectCreationExpression(objectCreationExpression.Constructor, rewrittenArguments, receiver);

                    return objectCreationExpression;
                }

                case BoundCastExpression castExpression:
                {
                    var operand = VisitExpression(castExpression.Expression) ?? castExpression.Expression;
                    if (!ReferenceEquals(operand, castExpression.Expression))
                        return new BoundCastExpression(operand, castExpression.Type, castExpression.Conversion);

                    return castExpression;
                }

                case BoundAsExpression asExpression:
                {
                    var operand = VisitExpression(asExpression.Expression) ?? asExpression.Expression;
                    if (!ReferenceEquals(operand, asExpression.Expression))
                        return new BoundAsExpression(operand, asExpression.Type, asExpression.Conversion);

                    return asExpression;
                }

                case BoundParenthesizedExpression parenthesizedExpression:
                {
                    var inner = VisitExpression(parenthesizedExpression.Expression) ?? parenthesizedExpression.Expression;
                    if (!ReferenceEquals(inner, parenthesizedExpression.Expression))
                        return new BoundParenthesizedExpression(inner);

                    return parenthesizedExpression;
                }

                case BoundConditionalAccessExpression conditionalAccessExpression:
                {
                    var receiver = VisitExpression(conditionalAccessExpression.Receiver) ?? conditionalAccessExpression.Receiver;
                    var whenNotNull = VisitExpression(conditionalAccessExpression.WhenNotNull) ?? conditionalAccessExpression.WhenNotNull;

                    if (!ReferenceEquals(receiver, conditionalAccessExpression.Receiver) ||
                        !ReferenceEquals(whenNotNull, conditionalAccessExpression.WhenNotNull))
                    {
                        return new BoundConditionalAccessExpression(receiver, whenNotNull, conditionalAccessExpression.Type);
                    }

                    return conditionalAccessExpression;
                }

                case BoundIfExpression ifExpression:
                {
                    var condition = VisitExpression(ifExpression.Condition) ?? ifExpression.Condition;
                    var thenBranch = VisitExpression(ifExpression.ThenBranch) ?? ifExpression.ThenBranch;
                    var elseBranch = VisitExpression(ifExpression.ElseBranch) ?? ifExpression.ElseBranch;

                    if (!ReferenceEquals(condition, ifExpression.Condition) ||
                        !ReferenceEquals(thenBranch, ifExpression.ThenBranch) ||
                        !ReferenceEquals(elseBranch, ifExpression.ElseBranch))
                    {
                        return new BoundIfExpression(condition, thenBranch, elseBranch);
                    }

                    return ifExpression;
                }

                case BoundTupleExpression tupleExpression:
                {
                    var originalElements = tupleExpression.Elements.ToArray();
                    var rewrittenElements = new BoundExpression[originalElements.Length];
                    var changed = false;

                    for (var i = 0; i < originalElements.Length; i++)
                    {
                        var rewritten = VisitExpression(originalElements[i]) ?? originalElements[i];
                        rewrittenElements[i] = rewritten;
                        if (!ReferenceEquals(rewritten, originalElements[i]))
                            changed = true;
                    }

                    if (changed)
                        return new BoundTupleExpression(rewrittenElements, tupleExpression.Type);

                    return tupleExpression;
                }

                case BoundCollectionExpression collectionExpression:
                {
                    var originalElements = collectionExpression.Elements.ToArray();
                    var rewrittenElements = new BoundExpression[originalElements.Length];
                    var changed = false;

                    for (var i = 0; i < originalElements.Length; i++)
                    {
                        var rewritten = VisitExpression(originalElements[i]) ?? originalElements[i];
                        rewrittenElements[i] = rewritten;
                        if (!ReferenceEquals(rewritten, originalElements[i]))
                            changed = true;
                    }

                    if (changed)
                        return new BoundCollectionExpression(collectionExpression.Type!, rewrittenElements, collectionExpression.CollectionSymbol, collectionExpression.Reason);

                    return collectionExpression;
                }

                case BoundBlockExpression blockExpression:
                {
                    var statements = new List<BoundStatement>();
                    var changed = false;

                    foreach (var statement in blockExpression.Statements)
                    {
                        var rewritten = VisitStatement(statement);
                        statements.Add(rewritten);
                        if (!ReferenceEquals(rewritten, statement))
                            changed = true;
                    }

                    if (changed)
                        return new BoundBlockExpression(statements, blockExpression.UnitType, blockExpression.LocalsToDispose);

                    return blockExpression;
                }
            }

            return base.VisitExpression(node);
        }

        public override BoundNode? VisitLocalAccess(BoundLocalAccess node)
        {
            if (node is null)
                return null;

            if (_hoistedLocals.TryGetValue(node.Local, out var field))
                return new BoundFieldAccess(field, node.Reason);

            return node;
        }

        public override BoundNode? VisitParameterAccess(BoundParameterAccess node)
        {
            if (node is null)
                return null;

            if (_stateMachine.ParameterFieldMap.TryGetValue(node.Parameter, out var field))
                return new BoundMemberAccessExpression(new BoundSelfExpression(_stateMachine), field, node.Reason);

            return node;
        }

        public override BoundNode? VisitVariableExpression(BoundVariableExpression node)
        {
            if (node is null)
                return null;

            if (_hoistedLocals.TryGetValue(node.Variable, out var field))
                return new BoundFieldAccess(field, node.Reason);

            return node;
        }

        public override BoundNode? VisitLocalAssignmentExpression(BoundLocalAssignmentExpression node)
        {
            if (node is null)
                return null;

            var right = VisitExpression(node.Right) ?? node.Right;

            if (_hoistedLocals.TryGetValue(node.Local, out var field))
                return CreateStateMachineFieldAssignment(_stateMachine, field, right);

            if (!ReferenceEquals(right, node.Right))
                return new BoundLocalAssignmentExpression(node.Local, right);

            return node;
        }

        private BoundExpression LowerAwaitExpressionToBlock(BoundAwaitExpression awaitExpression)
        {
            var statements = new List<BoundStatement>();
            SourceLocalSymbol? resultLocal = null;

            var resultType = awaitExpression.ResultType ?? _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit);

            if (resultType.SpecialType is not SpecialType.System_Void and not SpecialType.System_Unit)
            {
                resultLocal = CreateAwaitResultLocal(resultType);
                var declarator = new BoundVariableDeclarator(resultLocal, initializer: null);
                statements.Add(new BoundLocalDeclarationStatement(new[] { declarator }));
            }

            statements.AddRange(LowerAwaitExpression(awaitExpression, getResult =>
            {
                if (resultLocal is null)
                    return new BoundStatement[] { new BoundExpressionStatement(getResult) };

                var assignment = new BoundLocalAssignmentExpression(resultLocal, getResult);
                return new BoundStatement[] { new BoundExpressionStatement(assignment) };
            }));

            BoundExpression resultExpression;
            if (resultLocal is not null)
            {
                resultExpression = new BoundLocalAccess(resultLocal);
            }
            else
            {
                var unitType = _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit);
                resultExpression = new BoundUnitExpression(unitType);
            }

            statements.Add(new BoundExpressionStatement(resultExpression));

            var unit = _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit);
            return new BoundBlockExpression(statements, unit);
        }

        private IEnumerable<BoundStatement> LowerAwaitExpression(
            BoundAwaitExpression awaitExpression,
            Func<BoundInvocationExpression, IEnumerable<BoundStatement>> createResumeStatements)
        {
            var state = _nextState++;
            var awaiterField = _stateMachine.AddHoistedLocal($"<>awaiter{state}", awaitExpression.AwaiterType);
            var resumeLabel = CreateLabel(_stateMachine, $"state{state}");
            _dispatches.Add(new StateDispatch(state, resumeLabel));

            var awaiterStore = CreateAwaiterStoreStatement(awaitExpression, awaiterField);
            yield return awaiterStore;

            var condition = CreateIsCompletedAccess(awaitExpression, awaiterField);

            var scheduleStatements = new List<BoundStatement>
            {
                CreateStateAssignment(_stateMachine, state),
                CreateAwaitOnCompletedStatement(awaitExpression, awaiterField),
                new BoundReturnStatement(null)
            };

            var ifStatement = new BoundIfStatement(
                condition,
                new BoundBlockStatement(Array.Empty<BoundStatement>()),
                new BoundBlockStatement(scheduleStatements));

            yield return ifStatement;

            var resumeStatements = new List<BoundStatement>
            {
                CreateStateAssignment(_stateMachine, -1)
            };

            var awaiterLocal = CreateAwaiterLocal(awaitExpression.AwaiterType);
            var awaiterDeclarator = new BoundVariableDeclarator(awaiterLocal, initializer: null);
            resumeStatements.Add(new BoundLocalDeclarationStatement(new[] { awaiterDeclarator }));

            var awaiterFieldAccess = new BoundMemberAccessExpression(new BoundSelfExpression(_stateMachine), awaiterField);
            var captureAwaiter = new BoundLocalAssignmentExpression(awaiterLocal, awaiterFieldAccess);
            resumeStatements.Add(new BoundExpressionStatement(captureAwaiter));

            var awaiterDefaultLocal = CreateAwaiterLocal(awaitExpression.AwaiterType);
            var awaiterDefaultDeclarator = new BoundVariableDeclarator(
                awaiterDefaultLocal,
                new BoundDefaultValueExpression(awaiterField.Type));
            resumeStatements.Add(new BoundLocalDeclarationStatement(new[] { awaiterDefaultDeclarator }));

            var clearAssignment = new BoundFieldAssignmentExpression(
                new BoundSelfExpression(_stateMachine),
                awaiterField,
                new BoundLocalAccess(awaiterDefaultLocal),
                requiresReceiverAddress: true);
            resumeStatements.Add(new BoundAssignmentStatement(clearAssignment));

            var awaiterAccess = new BoundLocalAccess(awaiterLocal);
            var getResult = CreateGetResultInvocation(awaitExpression, awaiterAccess);
            resumeStatements.AddRange(createResumeStatements(getResult));

            yield return new BoundLabeledStatement(resumeLabel, new BoundBlockStatement(resumeStatements));
        }

        private SourceLocalSymbol CreateAwaitResultLocal(ITypeSymbol type)
        {
            var name = $"<>awaitResult{_nextAwaitResultId++}";
            var substitutedType = _stateMachine.SubstituteMethodType(type);
            return new SourceLocalSymbol(
                name,
                substitutedType,
                isMutable: true,
                _stateMachine.MoveNextMethod,
                _stateMachine,
                _stateMachine.ContainingNamespace,
                new[] { Location.None },
                Array.Empty<SyntaxReference>());
        }

        private SourceLocalSymbol CreateAwaiterLocal(ITypeSymbol type)
        {
            var name = $"<>awaiterLocal{_nextAwaiterLocalId++}";
            var substitutedType = _stateMachine.SubstituteMethodType(type);
            return new SourceLocalSymbol(
                name,
                substitutedType,
                isMutable: true,
                _stateMachine.MoveNextMethod,
                _stateMachine,
                _stateMachine.ContainingNamespace,
                new[] { Location.None },
                Array.Empty<SyntaxReference>());
        }

        private BoundStatement CreateAwaiterStoreStatement(BoundAwaitExpression awaitExpression, SourceFieldSymbol awaiterField)
        {
            var getAwaiter = new BoundInvocationExpression(
                awaitExpression.GetAwaiterMethod,
                Array.Empty<BoundExpression>(),
                awaitExpression.Expression);

            var receiver = new BoundSelfExpression(_stateMachine);
            var assignment = new BoundFieldAssignmentExpression(receiver, awaiterField, getAwaiter, requiresReceiverAddress: true);
            return new BoundAssignmentStatement(assignment);
        }

        private BoundExpression CreateIsCompletedAccess(BoundAwaitExpression awaitExpression, SourceFieldSymbol awaiterField)
        {
            var awaiterAccess = new BoundMemberAccessExpression(new BoundSelfExpression(_stateMachine), awaiterField);
            return new BoundMemberAccessExpression(awaiterAccess, awaitExpression.IsCompletedProperty);
        }

        private BoundInvocationExpression CreateGetResultInvocation(BoundAwaitExpression awaitExpression, BoundExpression awaiterReceiver)
        {
            return new BoundInvocationExpression(
                awaitExpression.GetResultMethod,
                Array.Empty<BoundExpression>(),
                awaiterReceiver,
                requiresReceiverAddress: true);
        }

        private BoundStatement CreateAwaitOnCompletedStatement(BoundAwaitExpression awaitExpression, SourceFieldSymbol awaiterField)
        {
            var builderField = _stateMachine.BuilderField;
            if (builderField.Type is not INamedTypeSymbol builderType)
                throw new InvalidOperationException("Async builder field must be a named type.");

            var awaitMethod = FindAwaitOnCompletedMethod(builderType)
                ?? throw new InvalidOperationException("Async builder is missing AwaitOnCompleted/AwaitUnsafeOnCompleted.");

            if (awaitMethod.IsGenericMethod)
            {
                var awaiterType = _stateMachine.SubstituteMethodType(awaitExpression.AwaiterType);
                awaitMethod = awaitMethod.Construct(awaiterType, _stateMachine);
            }

            var builderAccess = new BoundMemberAccessExpression(new BoundSelfExpression(_stateMachine), builderField);
            var awaiterAddress = new BoundAddressOfExpression(awaiterField, awaiterField.Type, new BoundSelfExpression(_stateMachine));
            var thisAddress = new BoundAddressOfExpression(_stateMachine, _stateMachine);

            var invocation = new BoundInvocationExpression(
                awaitMethod,
                new BoundExpression[] { awaiterAddress, thisAddress },
                builderAccess,
                requiresReceiverAddress: true);

            return new BoundExpressionStatement(invocation);
        }

        private SourceFieldSymbol AddHoistedLocal(ILocalSymbol local)
        {
            if (_hoistedLocals.TryGetValue(local, out var existing))
                return existing;

            var type = local.Type ?? _stateMachine.Compilation.ErrorTypeSymbol;
            var fieldName = $"<>local{_nextHoistedLocalId++}";
            var requiresDispose = _hoistableLocals.TryGetValue(local, out var dispose) && dispose;
            var field = _stateMachine.AddHoistedLocal(fieldName, type, requiresDispose);
            _hoistedLocals.Add(local, field);
            return field;
        }

        private ImmutableArray<ILocalSymbol> FilterLocalsToDispose(ImmutableArray<ILocalSymbol> locals)
        {
            if (_hoistableLocals.Count == 0 || locals.IsDefaultOrEmpty)
                return locals;

            var builder = ImmutableArray.CreateBuilder<ILocalSymbol>();

            foreach (var local in locals)
            {
                if (!_hoistableLocals.ContainsKey(local))
                    builder.Add(local);
            }

            return builder.MoveToImmutable();
        }
    }

    private sealed class AwaitCaptureWalker : BoundTreeWalker
    {
        private readonly Stack<Scope> _scopes = new();
        private readonly Dictionary<ILocalSymbol, bool> _hoisted = new(SymbolEqualityComparer.Default);

        private AwaitCaptureWalker()
        {
        }

        public static ImmutableDictionary<ILocalSymbol, bool> Analyze(BoundBlockStatement body)
        {
            if (body is null)
                throw new ArgumentNullException(nameof(body));

            var walker = new AwaitCaptureWalker();
            walker.VisitBlockStatement(body);

            var builder = ImmutableDictionary.CreateBuilder<ILocalSymbol, bool>(SymbolEqualityComparer.Default);
            foreach (var pair in walker._hoisted)
                builder.Add(pair.Key, pair.Value);

            return builder.ToImmutable();
        }

        public override void VisitBlockStatement(BoundBlockStatement node)
        {
            if (node is null)
                return;

            _scopes.Push(new Scope(node.LocalsToDispose));

            foreach (var statement in node.Statements)
                VisitStatement(statement);

            _scopes.Pop();
        }

        public override void VisitBlockExpression(BoundBlockExpression node)
        {
            if (node is null)
                return;

            _scopes.Push(new Scope(node.LocalsToDispose));

            foreach (var statement in node.Statements)
                VisitStatement(statement);

            _scopes.Pop();
        }

        public override void VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
        {
            if (node is null)
                return;

            foreach (var declarator in node.Declarators)
            {
                VisitExpression(declarator.Initializer);
                DeclareLocal(declarator.Local, node.IsUsing);
            }
        }

        public override void VisitForStatement(BoundForStatement node)
        {
            if (node is null)
                return;

            VisitExpression(node.Collection);

            _scopes.Push(new Scope(ImmutableArray<ILocalSymbol>.Empty));
            if (node.Local is not null)
                DeclareLocal(node.Local, isUsing: false);
            VisitStatement(node.Body);
            _scopes.Pop();
        }

        public override void VisitCatchClause(BoundCatchClause node)
        {
            if (node is null)
                return;

            _scopes.Push(new Scope(node.Block.LocalsToDispose));

            if (node.Local is not null)
                DeclareLocal(node.Local, isUsing: false);

            foreach (var statement in node.Block.Statements)
                VisitStatement(statement);

            _scopes.Pop();
        }

        public override void VisitAwaitExpression(BoundAwaitExpression node)
        {
            if (node is null)
                return;

            VisitExpression(node.Expression);
            CaptureCurrentLocals();
        }

        public override void VisitLambdaExpression(BoundLambdaExpression node)
        {
            // Lambdas are lowered independently.
        }

        public override void VisitFunctionStatement(BoundFunctionStatement node)
        {
            // Local functions are lowered independently.
        }

        private void DeclareLocal(ILocalSymbol local, bool isUsing)
        {
            if (_scopes.Count == 0)
                _scopes.Push(new Scope(ImmutableArray<ILocalSymbol>.Empty));

            var scope = _scopes.Peek();
            scope.Declare(local, isUsing);
        }

        private void CaptureCurrentLocals()
        {
            foreach (var scope in _scopes)
            {
                foreach (var pair in scope.Locals)
                    Merge(pair.Key, pair.Value);
            }
        }

        private void Merge(ILocalSymbol local, bool requiresDispose)
        {
            if (_hoisted.TryGetValue(local, out var existing))
            {
                if (!existing && requiresDispose)
                    _hoisted[local] = true;
            }
            else
            {
                _hoisted.Add(local, requiresDispose);
            }
        }

        private sealed class Scope
        {
            private readonly Dictionary<ILocalSymbol, bool> _locals = new(SymbolEqualityComparer.Default);
            private readonly HashSet<ILocalSymbol> _localsToDispose;

            public Scope(IEnumerable<ILocalSymbol> localsToDispose)
            {
                _localsToDispose = new HashSet<ILocalSymbol>(localsToDispose, SymbolEqualityComparer.Default);
            }

            public void Declare(ILocalSymbol local, bool isUsing)
            {
                var requiresDispose = isUsing || _localsToDispose.Contains(local);
                _locals[local] = requiresDispose;
            }

            public IEnumerable<KeyValuePair<ILocalSymbol, bool>> Locals => _locals;
        }
    }

    private readonly struct StateDispatch
    {
        public StateDispatch(int state, LabelSymbol label)
        {
            State = state;
            Label = label;
        }

        public int State { get; }

        public LabelSymbol Label { get; }
    }

    internal readonly struct AsyncMethodAnalysis
    {
        public AsyncMethodAnalysis(bool requiresStateMachine, bool containsAwait)
        {
            RequiresStateMachine = requiresStateMachine;
            ContainsAwait = containsAwait;
        }

        public bool RequiresStateMachine { get; }

        public bool ContainsAwait { get; }
    }

    private sealed class AwaitExpressionFinder : BoundTreeWalker
    {
        public bool FoundAwait { get; private set; }

        public override void VisitStatement(BoundStatement statement)
        {
            if (FoundAwait)
                return;

            if (statement is BoundFunctionStatement)
                return;

            base.VisitStatement(statement);
        }

        public override void VisitExpression(BoundExpression node)
        {
            if (FoundAwait)
                return;

            base.VisitExpression(node);
        }

        public override void VisitAwaitExpression(BoundAwaitExpression node)
        {
            FoundAwait = true;
        }

        public override void VisitLambdaExpression(BoundLambdaExpression node)
        {
            // Nested lambdas are lowered independently.
        }

        public override void VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
        {
            if (FoundAwait)
                return;

            foreach (var declarator in node.Declarators)
            {
                if (declarator.Initializer is not null)
                    VisitExpression(declarator.Initializer);

                if (FoundAwait)
                    break;
            }
        }
    }

    private static BoundStatement? CreateBuilderInitializationStatement(SourceLocalSymbol asyncLocal, SynthesizedAsyncStateMachineTypeSymbol.ConstructedStateMachine stateMachine)
    {
        var builderField = stateMachine.BuilderField;
        if (builderField.Type is not INamedTypeSymbol builderType)
            return null;

        var createMethod = FindParameterlessStaticMethod(builderType, "Create");
        if (createMethod is null)
            return null;

        var invocation = new BoundInvocationExpression(createMethod, Array.Empty<BoundExpression>());
        var receiver = new BoundLocalAccess(asyncLocal);
        var assignment = new BoundFieldAssignmentExpression(receiver, builderField, invocation, requiresReceiverAddress: true);
        return new BoundAssignmentStatement(assignment);
    }

    private static BoundStatement? CreateBuilderStartStatement(SourceLocalSymbol asyncLocal, SynthesizedAsyncStateMachineTypeSymbol.ConstructedStateMachine stateMachine)
    {
        var builderField = stateMachine.BuilderField;
        if (builderField.Type is not INamedTypeSymbol builderType)
            return null;

        var startMethod = FindStartMethod(builderType);
        if (startMethod is null)
            return null;

        var constructedStart = startMethod.IsGenericMethod
            ? startMethod.Construct(stateMachine.Type)
            : startMethod;

        var builderAccess = new BoundMemberAccessExpression(new BoundLocalAccess(asyncLocal), builderField);
        var stateMachineReference = new BoundAddressOfExpression(asyncLocal, stateMachine.Type);

        var invocation = new BoundInvocationExpression(
            constructedStart,
            new BoundExpression[] { stateMachineReference },
            builderAccess,
            requiresReceiverAddress: true);

        return new BoundExpressionStatement(invocation);
    }

    private static BoundBlockStatement? CreateSetStateMachineBody(SynthesizedAsyncStateMachineTypeSymbol stateMachine)
    {
        var builderField = stateMachine.BuilderField;
        if (builderField.Type is not INamedTypeSymbol builderType)
            return null;

        var setStateMachineMethod = FindSetStateMachineMethod(builderType);
        if (setStateMachineMethod is null)
            return null;

        var builderAccess = new BoundMemberAccessExpression(new BoundSelfExpression(stateMachine), builderField);
        var parameter = stateMachine.SetStateMachineMethod.Parameters.Length > 0
            ? stateMachine.SetStateMachineMethod.Parameters[0]
            : null;

        if (parameter is null)
            return null;

        var invocation = new BoundInvocationExpression(
            setStateMachineMethod,
            new BoundExpression[] { new BoundParameterAccess(parameter) },
            builderAccess,
            requiresReceiverAddress: true);

        var statement = new BoundExpressionStatement(invocation);
        return new BoundBlockStatement(new BoundStatement[] { statement });
    }

    private static BoundExpression CreateReturnExpression(
        SourceMethodSymbol method,
        SynthesizedAsyncStateMachineTypeSymbol.ConstructedStateMachine stateMachine,
        SourceLocalSymbol asyncLocal)
    {
        if (stateMachine.BuilderField.Type is not INamedTypeSymbol builderType)
        {
            return CreateNullLiteral(method.ReturnType);
        }

        var taskProperty = FindTaskProperty(builderType);
        if (taskProperty is null)
            return CreateNullLiteral(method.ReturnType);

        var builderAccess = new BoundMemberAccessExpression(new BoundLocalAccess(asyncLocal), stateMachine.BuilderField);
        var taskAccess = new BoundMemberAccessExpression(builderAccess, taskProperty);

        if (!SymbolEqualityComparer.Default.Equals(taskProperty.Type, method.ReturnType))
        {
            var conversion = new Conversion(isImplicit: true, isIdentity: true);
            return new BoundCastExpression(taskAccess, method.ReturnType, conversion);
        }

        return taskAccess;
    }

    private static IMethodSymbol? FindParameterlessStaticMethod(INamedTypeSymbol type, string name)
    {
        foreach (var member in type.GetMembers(name))
        {
            if (member is IMethodSymbol { Parameters.Length: 0, IsStatic: true } method)
                return SubstituteBuilderMethodIfNeeded(type, method);
        }

        return null;
    }

    private static IPropertySymbol? FindTaskProperty(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers())
        {
            if (member is IPropertySymbol { Name: "Task" } property)
                return SubstituteBuilderPropertyIfNeeded(builderType, property);
        }

        return null;
    }

    private static IMethodSymbol? FindStartMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("Start"))
        {
            if (member is IMethodSymbol { Parameters.Length: 1 } method)
                return SubstituteBuilderMethodIfNeeded(builderType, method);
        }

        return null;
    }

    private static IMethodSymbol? FindSetStateMachineMethod(INamedTypeSymbol builderType)
    {
        foreach (var member in builderType.GetMembers("SetStateMachine"))
        {
            if (member is IMethodSymbol { Parameters.Length: 1 } method)
                return SubstituteBuilderMethodIfNeeded(builderType, method);
        }

        return null;
    }

    private static BoundLiteralExpression CreateNullLiteral(ITypeSymbol type)
    {
        return new BoundLiteralExpression(
            BoundLiteralExpressionKind.NullLiteral,
            null!,
            type);
    }

    private static Compilation GetCompilation(SourceMethodSymbol method)
    {
        if (method.ContainingAssembly is SourceAssemblySymbol sourceAssembly)
            return sourceAssembly.Compilation;

        throw new InvalidOperationException("Async lowering requires a source assembly method.");
    }
}
