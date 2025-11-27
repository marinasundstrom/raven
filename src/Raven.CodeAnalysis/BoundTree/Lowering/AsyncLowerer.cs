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

        var containsAwait = ContainsAwait(body);

        method.SetContainsAwait(containsAwait);

        var requiresStateMachine = containsAwait;
        return new AsyncMethodAnalysis(requiresStateMachine, containsAwait);
    }

    public static bool ContainsAwait(BoundNode node)
    {
        if (node is null)
            throw new ArgumentNullException(nameof(node));

        var finder = new AwaitExpressionFinder();

        switch (node)
        {
            case BoundBlockStatement block:
                finder.VisitBlockStatement(block);
                break;
            case BoundExpression expression:
                finder.VisitExpression(expression);
                break;
            default:
                finder.Visit(node);
                break;
        }

        return finder.FoundAwait;
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
        var context = new MoveNextLoweringContext(compilation, stateMachine);
        var originalBody = stateMachine.OriginalBody ?? new BoundBlockStatement(Array.Empty<BoundStatement>());

        var entryLabel = CreateLabel(stateMachine, "state");

        var awaitRewriter = new AwaitLoweringRewriter(stateMachine, context.BuilderMembers);
        var rewrittenBody = awaitRewriter.Rewrite(originalBody);
        rewrittenBody = StateDispatchInjector.Inject(
            rewrittenBody,
            stateMachine,
            awaitRewriter.Dispatches,
            out var guardEntryLabels);

        var tryStatements = new List<BoundStatement>();
        tryStatements.AddRange(CreateStateDispatchStatements(
            context,
            entryLabel,
            awaitRewriter.Dispatches,
            guardEntryLabels));

        var entryStatements = new List<BoundStatement>(rewrittenBody.Statements);
        if (!stateMachine.HoistedLocalsToDispose.IsDefaultOrEmpty)
        {
            var disposeStatements = CreateDisposeStatements(
                stateMachine,
                EnumerateReverse(stateMachine.HoistedLocalsToDispose));
            entryStatements.AddRange(disposeStatements);
        }

        entryStatements.AddRange(CreateCompletionStatements(context));
        var entryBlock = new BoundBlockStatement(entryStatements, rewrittenBody.LocalsToDispose);

        tryStatements.Add(new BoundLabeledStatement(entryLabel, entryBlock));

        var tryBlock = new BoundBlockStatement(tryStatements);

        var catchClauses = ImmutableArray<BoundCatchClause>.Empty;
        var catchClause = CreateExceptionCatchClause(context);
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
        var statements = new List<BoundStatement>();
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);

        var constructed = stateMachine.GetConstructedMembers(method);
        var stateMachineType = constructed.StateMachineType;
        var moveNextMethod = constructed.MoveNext;
        var stateField = constructed.StateField;
        var builderMembers = constructed.AsyncMethodBuilderMembers;
        var builderField = builderMembers.BuilderField;
        var thisField = constructed.ThisField;
        var parameterFieldMap = constructed.ParameterFields;

        var asyncLocal = new SourceLocalSymbol(
            "<>async",
            stateMachineType,
            isMutable: true,
            method,
            method.ContainingType,
            method.ContainingNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>());

        var declarator = new BoundVariableDeclarator(asyncLocal, initializer: null);
        statements.Add(new BoundLocalDeclarationStatement(new[] { declarator }));

        if (thisField is not null)
        {
            var receiver = new BoundLocalAccess(asyncLocal);
            var value = new BoundSelfExpression(thisField.Type);
            var assignment = new BoundFieldAssignmentExpression(receiver, thisField, value, unitType, requiresReceiverAddress: true);
            statements.Add(new BoundAssignmentStatement(assignment));
        }

        foreach (var parameter in method.Parameters)
        {
            if (!parameterFieldMap.TryGetValue(parameter, out var field))
                continue;

            var receiver = new BoundLocalAccess(asyncLocal);
            var value = new BoundParameterAccess(parameter);
            var assignment = new BoundFieldAssignmentExpression(receiver, field, value, unitType, requiresReceiverAddress: true);
            statements.Add(new BoundAssignmentStatement(assignment));
        }

        var stateReceiver = new BoundLocalAccess(asyncLocal);
        var initialState = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            -1,
            stateField.Type);
        var stateAssignment = new BoundFieldAssignmentExpression(stateReceiver, stateField, initialState, unitType, requiresReceiverAddress: true);
        statements.Add(new BoundAssignmentStatement(stateAssignment));

        var builderInitialization = CreateBuilderInitializationStatement(stateMachine, asyncLocal, builderMembers, unitType);
        if (builderInitialization is not null)
            statements.Add(builderInitialization);

        var builderStartStatement = CreateBuilderStartStatement(stateMachine, asyncLocal, builderMembers, stateMachineType);
        if (builderStartStatement is not null)
        {
            statements.Add(builderStartStatement);
        }
        else
        {
            var moveNextInvocation = new BoundInvocationExpression(
                moveNextMethod,
                Array.Empty<BoundExpression>(),
                receiver: new BoundLocalAccess(asyncLocal),
                requiresReceiverAddress: true);
            statements.Add(new BoundExpressionStatement(moveNextInvocation));
        }

        BoundExpression? returnExpression = null;
        if (method.ReturnType.SpecialType != SpecialType.System_Void)
        {
            returnExpression = CreateReturnExpression(method, builderMembers, asyncLocal);
        }

        statements.Add(new BoundReturnStatement(returnExpression));

        return new BoundBlockStatement(statements);
    }

    private static BoundCatchClause? CreateExceptionCatchClause(MoveNextLoweringContext context)
    {
        var exceptionType = context.Compilation.GetSpecialType(SpecialType.System_Exception);

        var exceptionLocal = new SourceLocalSymbol(
            "<>ex",
            exceptionType,
            isMutable: true,
            context.StateMachine.MoveNextMethod,
            context.StateMachine,
            context.StateMachine.ContainingNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>());

        var statements = new List<BoundStatement>
        {
            CreateStateAssignment(context.StateMachine, -2)
        };

        statements.AddRange(CreateDisposeStatements(context.StateMachine, EnumerateReverse(context.StateMachine.HoistedLocalsToDispose)));

        var setException = CreateBuilderSetExceptionStatement(context.StateMachine, context.BuilderMembers, exceptionLocal);
        if (setException is not null)
            statements.Add(setException);

        statements.Add(new BoundReturnStatement(null));

        var catchBlock = new BoundBlockStatement(statements);
        return new BoundCatchClause(exceptionType, exceptionLocal, catchBlock);
    }

    private static IEnumerable<BoundStatement> CreateStateDispatchStatements(
        MoveNextLoweringContext context,
        ILabelSymbol entryLabel,
        ImmutableArray<StateDispatch> dispatches,
        ImmutableDictionary<int, ILabelSymbol> guardEntryLabels)
    {
        var statements = new List<BoundStatement>();

        var stateField = context.StateMachine.StateField;
        var stateType = stateField.Type;
        if (!BoundBinaryOperator.TryLookup(context.Compilation, SyntaxKind.EqualsEqualsToken, stateType, stateType, out var equals))
            throw new InvalidOperationException("Async lowering requires integer equality operator.");

        statements.Add(CreateStateDispatchStatement(stateField, equals, -1, entryLabel));

        if (!dispatches.IsDefaultOrEmpty)
        {
            foreach (var dispatch in dispatches)
            {
                var targetLabel = guardEntryLabels.TryGetValue(dispatch.State, out var guarded)
                    ? guarded
                    : dispatch.Label;
                statements.Add(CreateStateDispatchStatement(stateField, equals, dispatch.State, targetLabel));
            }
        }

        statements.Add(new BoundGotoStatement(entryLabel));

        return statements;
    }

    private static BoundStatement CreateStateDispatchStatement(
        SourceFieldSymbol stateField,
        BoundBinaryOperator equals,
        int state,
        ILabelSymbol targetLabel)
    {
        var stateAccess = new BoundFieldAccess(stateField);
        var stateLiteral = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            state,
            stateField.Type);

        var condition = new BoundBinaryExpression(stateAccess, equals, stateLiteral);
        var gotoTarget = new BoundGotoStatement(targetLabel);
        var thenBlock = new BoundBlockStatement(new BoundStatement[] { gotoTarget });
        return new BoundIfStatement(condition, thenBlock);
    }

    private static IEnumerable<BoundStatement> CreateCompletionStatements(MoveNextLoweringContext context)
    {
        yield return CreateStateAssignment(context.StateMachine, -2);

        var setResult = CreateBuilderSetResultStatement(context.StateMachine, context.BuilderMembers, expression: null);
        if (setResult is not null)
            yield return setResult;

        yield return new BoundReturnStatement(null);
    }

    private readonly struct MoveNextLoweringContext
    {
        public MoveNextLoweringContext(
            Compilation compilation,
            SynthesizedAsyncStateMachineTypeSymbol stateMachine)
        {
            Compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
            StateMachine = stateMachine ?? throw new ArgumentNullException(nameof(stateMachine));
            BuilderMembers = stateMachine.GetBuilderMembers(stateMachine.AsyncMethod);
        }

        public Compilation Compilation { get; }
        public SynthesizedAsyncStateMachineTypeSymbol StateMachine { get; }
        public SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers BuilderMembers { get; }
    }

    private static BoundStatement? CreateBuilderSetResultStatement(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers builderMembers,
        BoundExpression? expression)
    {
        var setResultMethod = builderMembers.SetResult;
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

        var receiver = new BoundMemberAccessExpression(new BoundSelfExpression(stateMachine), builderMembers.BuilderField);
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
            stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit),
            requiresReceiverAddress: true);
    }

    private static BoundStatement? CreateBuilderSetExceptionStatement(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers builderMembers,
        ILocalSymbol exceptionLocal)
    {
        var setExceptionMethod = builderMembers.SetException;
        if (setExceptionMethod is null)
            return null;

        var builderAccess = new BoundMemberAccessExpression(new BoundSelfExpression(stateMachine), builderMembers.BuilderField);
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

    private sealed class AsyncMethodExpressionSubstituter : BoundTreeRewriter
    {
        private readonly SynthesizedAsyncStateMachineTypeSymbol _stateMachine;
        private readonly INamedTypeSymbol? _builderTypeDefinition;

        private AsyncMethodExpressionSubstituter(SynthesizedAsyncStateMachineTypeSymbol stateMachine)
        {
            _stateMachine = stateMachine ?? throw new ArgumentNullException(nameof(stateMachine));
            _builderTypeDefinition = GetBuilderTypeDefinition(stateMachine.BuilderField.Type);
        }

        public static BoundExpression Substitute(
            SynthesizedAsyncStateMachineTypeSymbol stateMachine,
            BoundExpression expression)
        {
            if (expression is null)
                throw new ArgumentNullException(nameof(expression));

            var substituter = new AsyncMethodExpressionSubstituter(stateMachine);
            return (BoundExpression)(substituter.VisitExpression(expression) ?? expression);
        }

        public override ITypeSymbol VisitType(ITypeSymbol type)
        {
            if (type is null)
                return type;

            if (IsBuilderType(type))
                return type;

            return _stateMachine.SubstituteStateMachineTypeParameters(type);
        }

        public override IMethodSymbol VisitMethod(IMethodSymbol method)
        {
            if (method is null)
                return method;

            if (IsBuilderMethod(method))
                return method;

            return _stateMachine.SubstituteStateMachineTypeParameters(method);
        }

        public override BoundNode? VisitInvocationExpression(BoundInvocationExpression node)
        {
            if (node is null)
                return null;

            var originalArguments = node.Arguments.ToArray();
            var rewrittenArguments = new BoundExpression[originalArguments.Length];
            var changed = false;

            for (var i = 0; i < originalArguments.Length; i++)
            {
                var rewritten = VisitExpression(originalArguments[i]) ?? originalArguments[i];
                rewrittenArguments[i] = rewritten;
                if (!ReferenceEquals(rewritten, originalArguments[i]))
                    changed = true;
            }

            var receiver = VisitExpression(node.Receiver) ?? node.Receiver;
            var extensionReceiver = VisitExpression(node.ExtensionReceiver) ?? node.ExtensionReceiver;
            var method = VisitMethod(node.Method);

            changed |= !ReferenceEquals(receiver, node.Receiver);
            changed |= !ReferenceEquals(extensionReceiver, node.ExtensionReceiver);
            changed |= !SymbolEqualityComparer.Default.Equals(method, node.Method);

            if (!changed)
                return node;

            return new BoundInvocationExpression(
                method,
                rewrittenArguments,
                receiver,
                extensionReceiver,
                node.RequiresReceiverAddress);
        }

        private static INamedTypeSymbol? GetBuilderTypeDefinition(ITypeSymbol builderType)
        {
            if (builderType is INamedTypeSymbol named)
            {
                if (named.IsGenericType && !named.IsUnboundGenericType)
                    return named.ConstructedFrom as INamedTypeSymbol ?? named;

                return named;
            }

            return null;
        }

        private bool IsBuilderType(ITypeSymbol type)
        {
            if (_builderTypeDefinition is null)
                return false;

            if (SymbolEqualityComparer.Default.Equals(type, _builderTypeDefinition))
                return true;

            if (type is INamedTypeSymbol named)
            {
                var definition = named.ConstructedFrom as INamedTypeSymbol ?? named;
                if (SymbolEqualityComparer.Default.Equals(definition, _builderTypeDefinition))
                    return true;

                if (named.OriginalDefinition is INamedTypeSymbol original &&
                    !ReferenceEquals(original, named) &&
                    SymbolEqualityComparer.Default.Equals(original, _builderTypeDefinition))
                {
                    return true;
                }
            }

            return false;
        }

        private bool IsBuilderMethod(IMethodSymbol method)
        {
            if (method is null)
                return false;

            if (method.UnderlyingSymbol is IMethodSymbol underlying && !ReferenceEquals(underlying, method))
                return IsBuilderMethod(underlying);

            if (method.OriginalDefinition is IMethodSymbol original && !ReferenceEquals(original, method))
                return IsBuilderMethod(original);

            if (method.ContainingType is ITypeSymbol containingType && IsBuilderType(containingType))
                return true;

            return false;
        }
    }

    private sealed class AwaitLoweringRewriter : BoundTreeRewriter
    {
        private readonly SynthesizedAsyncStateMachineTypeSymbol _stateMachine;
        private readonly SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers _builderMembers;
        private readonly List<StateDispatch> _dispatches = new();
        private readonly Dictionary<ILocalSymbol, SourceFieldSymbol> _hoistedLocals = new(SymbolEqualityComparer.Default);
        private ImmutableDictionary<ILocalSymbol, bool> _hoistableLocals = ImmutableDictionary<ILocalSymbol, bool>.Empty;
        private int _nextState;
        private int _nextHoistedLocalId;
        private int _nextAwaitResultId;
        private int _nextAwaiterLocalId;
        private readonly Stack<BoundBlockStatement> _tryBlocks = new();
        private readonly Dictionary<BoundBlockStatement, BoundBlockStatement> _blockMap = new(ReferenceEqualityComparer.Instance);

        public AwaitLoweringRewriter(
            SynthesizedAsyncStateMachineTypeSymbol stateMachine,
            SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers builderMembers)
        {
            if (stateMachine is null)
                throw new ArgumentNullException(nameof(stateMachine));

            _stateMachine = stateMachine;
            _builderMembers = builderMembers;
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

            var rewritten = RewriteBlockStatement(body, appendDisposeStatements: false);
            RemapGuardBlocks();
            return rewritten;
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

            return RewriteBlockStatement(node, appendDisposeStatements: true);
        }

        private BoundBlockStatement RewriteBlockStatement(
            BoundBlockStatement node,
            bool appendDisposeStatements)
        {
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

            if (appendDisposeStatements && hoistedDisposables.Count > 0)
            {
                var disposeStatements = CreateDisposeStatements(
                    _stateMachine,
                    EnumerateReverse(hoistedDisposables));

                statements.AddRange(disposeStatements);
            }

            var localsToDispose = FilterLocalsToDispose(node.LocalsToDispose);
            var rewrittenBlock = new BoundBlockStatement(statements, localsToDispose);
            _blockMap[node] = rewrittenBlock;
            return rewrittenBlock;
        }

        private void RemapGuardBlocks()
        {
            for (var i = 0; i < _dispatches.Count; i++)
            {
                var dispatch = _dispatches[i];
                if (dispatch.GuardPath.IsDefaultOrEmpty || dispatch.GuardPath.Length == 0)
                    continue;

                var guards = dispatch.GuardPath;
                var changed = false;
                var builder = ImmutableArray.CreateBuilder<BoundBlockStatement>(guards.Length);

                for (var guardIndex = 0; guardIndex < guards.Length; guardIndex++)
                {
                    var guard = guards[guardIndex];
                    if (_blockMap.TryGetValue(guard, out var mapped))
                    {
                        builder.Add(mapped);
                        if (!ReferenceEquals(mapped, guard))
                            changed = true;
                    }
                    else
                    {
                        builder.Add(guard);
                    }
                }

                if (changed)
                    _dispatches[i] = new StateDispatch(dispatch.State, dispatch.Label, builder.MoveToImmutable());
            }
        }

        private ImmutableArray<BoundBlockStatement> GetCurrentGuardPath()
        {
            if (_tryBlocks.Count == 0)
                return ImmutableArray<BoundBlockStatement>.Empty;

            var guards = _tryBlocks.ToArray();
            Array.Reverse(guards);
            return ImmutableArray.Create(guards);
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
                        var assignment = new BoundFieldAssignmentExpression(
                            receiver,
                            field,
                            initializer,
                            _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit),
                            requiresReceiverAddress: true);
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

            _tryBlocks.Push(node.TryBlock);
            var tryBlock = (BoundBlockStatement)VisitBlockStatement(node.TryBlock)!;
            _tryBlocks.Pop();
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

            var setResult = CreateBuilderSetResultStatement(_stateMachine, _builderMembers, resultExpression);
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
            expression = AsyncMethodExpressionSubstituter.Substitute(_stateMachine, expression);

            var resultType = SubstituteStateMachineTypeParameters(node.ResultType);
            var awaiterType = SubstituteStateMachineTypeParameters(node.AwaiterType);
            var getAwaiter = SubstituteStateMachineTypeParameters(node.GetAwaiterMethod);
            var getResult = SubstituteStateMachineTypeParameters(node.GetResultMethod);
            var isCompleted = SubstituteStateMachineTypeParameters(node.IsCompletedProperty);

            if (!ReferenceEquals(expression, node.Expression) ||
                !SymbolEqualityComparer.Default.Equals(resultType, node.ResultType) ||
                !SymbolEqualityComparer.Default.Equals(awaiterType, node.AwaiterType) ||
                !SymbolEqualityComparer.Default.Equals(getAwaiter, node.GetAwaiterMethod) ||
                !SymbolEqualityComparer.Default.Equals(getResult, node.GetResultMethod) ||
                !SymbolEqualityComparer.Default.Equals(isCompleted, node.IsCompletedProperty))
            {
                node = new BoundAwaitExpression(
                    expression,
                    resultType,
                    awaiterType,
                    getAwaiter,
                    getResult,
                    isCompleted);
            }

            return LowerAwaitExpressionToBlock(node);
        }

        public override BoundNode? VisitTryExpression(BoundTryExpression node)
        {
            if (node is null)
                return null;

            var containsAwait = ContainsAwait(node.Expression);

            BoundBlockStatement? guardPlaceholder = null;
            if (containsAwait)
            {
                guardPlaceholder = new BoundBlockStatement(Array.Empty<BoundStatement>());
                _tryBlocks.Push(guardPlaceholder);
            }

            var expression = VisitExpression(node.Expression) ?? node.Expression;

            if (guardPlaceholder is not null)
                _tryBlocks.Pop();

            if (containsAwait)
            {
                var compilation = _stateMachine.Compilation;
                var resultType = SubstituteStateMachineTypeParameters(node.Type ?? compilation.ErrorTypeSymbol);
                var unitType = compilation.GetSpecialType(SpecialType.System_Unit);

                var convertedExpression = ApplyConversionIfNeeded(expression, resultType, compilation);

                var resultLocal = new SourceLocalSymbol(
                    "$tryExprResult",
                    SubstituteStateMachineTypeParameters(resultType),
                    isMutable: true,
                    _stateMachine.MoveNextMethod,
                    _stateMachine,
                    _stateMachine.ContainingNamespace,
                    new[] { Location.None },
                    Array.Empty<SyntaxReference>());

                var resultDeclarator = new BoundVariableDeclarator(resultLocal, initializer: null);
                var resultDeclaration = new BoundLocalDeclarationStatement(new[] { resultDeclarator });

                var assignment = new BoundLocalAssignmentExpression(resultLocal, convertedExpression, unitType);
                var tryBlock = new BoundBlockStatement(new BoundStatement[]
                {
                    new BoundExpressionStatement(assignment)
                });

                if (guardPlaceholder is not null)
                    _blockMap[guardPlaceholder] = tryBlock;

                var exceptionLocal = new SourceLocalSymbol(
                    "$tryExprException",
                    SubstituteStateMachineTypeParameters(node.ExceptionType),
                    isMutable: true,
                    _stateMachine.MoveNextMethod,
                    _stateMachine,
                    _stateMachine.ContainingNamespace,
                    new[] { Location.None },
                    Array.Empty<SyntaxReference>());

                var catchExpression = ApplyConversionIfNeeded(
                    new BoundLocalAccess(exceptionLocal),
                    resultType,
                    compilation);

                var catchAssignment = new BoundLocalAssignmentExpression(
                    resultLocal,
                    catchExpression,
                    unitType);

                var catchBlock = new BoundBlockStatement(new BoundStatement[]
                {
                    new BoundExpressionStatement(catchAssignment)
                });

                var catchClause = new BoundCatchClause(node.ExceptionType, exceptionLocal, catchBlock);
                var tryStatement = new BoundTryStatement(tryBlock, ImmutableArray.Create(catchClause), finallyBlock: null);

                var blockStatements = new BoundStatement[]
                {
                    resultDeclaration,
                    tryStatement,
                    new BoundExpressionStatement(new BoundLocalAccess(resultLocal))
                };

                return new BoundBlockExpression(blockStatements, unitType);
            }

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
                return new BoundLocalAssignmentExpression(node.Local, right, _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit));

            return node;
        }

        private BoundExpression LowerAwaitExpressionToBlock(BoundAwaitExpression awaitExpression)
        {
            var statements = new List<BoundStatement>();
            SourceLocalSymbol? resultLocal = null;

            var resultType = awaitExpression.ResultType ?? _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit);
            resultType = SubstituteAsyncMethodTypeParameters(resultType);

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

                var assignment = new BoundLocalAssignmentExpression(resultLocal, getResult, _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit));
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
            var awaiterType = SubstituteAsyncMethodTypeParameters(awaitExpression.AwaiterType);
            var awaiterField = _stateMachine.AddHoistedLocal($"<>awaiter{state}", awaiterType);
            var resumeLabel = CreateLabel(_stateMachine, $"state{state}");
            var guardPath = GetCurrentGuardPath();
            _dispatches.Add(new StateDispatch(state, resumeLabel, guardPath));

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

            var awaiterLocal = CreateAwaiterLocal(awaiterType);
            var awaiterDeclarator = new BoundVariableDeclarator(awaiterLocal, initializer: null);
            resumeStatements.Add(new BoundLocalDeclarationStatement(new[] { awaiterDeclarator }));

            var awaiterFieldAccess = new BoundMemberAccessExpression(new BoundSelfExpression(_stateMachine), awaiterField);
            var captureAwaiter = new BoundLocalAssignmentExpression(awaiterLocal, awaiterFieldAccess, _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit));
            resumeStatements.Add(new BoundExpressionStatement(captureAwaiter));

            var awaiterAccess = new BoundLocalAccess(awaiterLocal);
            var getResult = CreateGetResultInvocation(awaitExpression, awaiterAccess);
            resumeStatements.AddRange(createResumeStatements(getResult));
            var unitType = _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit);
            var clearAwaiterField = new BoundFieldAssignmentExpression(
                new BoundSelfExpression(_stateMachine),
                awaiterField,
                new BoundDefaultValueExpression(awaiterField.Type),
                unitType,
                requiresReceiverAddress: true);
            resumeStatements.Add(new BoundAssignmentStatement(clearAwaiterField));

            yield return new BoundLabeledStatement(resumeLabel, new BoundBlockStatement(resumeStatements));
        }

        private SourceLocalSymbol CreateAwaitResultLocal(ITypeSymbol type)
        {
            var name = $"<>awaitResult{_nextAwaitResultId++}";
            type = SubstituteAsyncMethodTypeParameters(type);
            return new SourceLocalSymbol(
                name,
                type,
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
            type = SubstituteAsyncMethodTypeParameters(type);
            return new SourceLocalSymbol(
                name,
                type,
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
            var assignment = new BoundFieldAssignmentExpression(
                receiver,
                awaiterField,
                getAwaiter,
                _stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit),
                requiresReceiverAddress: true);
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
            var awaitMethod = _builderMembers.AwaitOnCompleted
                ?? throw new InvalidOperationException("Async builder is missing AwaitOnCompleted/AwaitUnsafeOnCompleted.");

            if (awaitMethod.IsGenericMethod)
            {
                var awaiterType = SubstituteAsyncMethodTypeParameters(awaitExpression.AwaiterType);
                awaitMethod = awaitMethod.Construct(awaiterType, _stateMachine);
            }

            var builderAccess = new BoundMemberAccessExpression(new BoundSelfExpression(_stateMachine), _builderMembers.BuilderField);
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
            type = SubstituteAsyncMethodTypeParameters(type);
            var fieldName = $"<>local{_nextHoistedLocalId++}";
            var requiresDispose = _hoistableLocals.TryGetValue(local, out var dispose) && dispose;
            var field = _stateMachine.AddHoistedLocal(fieldName, type, requiresDispose);
            _hoistedLocals.Add(local, field);
            return field;
        }

        private ITypeSymbol SubstituteAsyncMethodTypeParameters(ITypeSymbol type)
        {
            if (type is null)
                return _stateMachine.Compilation.ErrorTypeSymbol;

            return _stateMachine.SubstituteAsyncMethodTypeParameters(type);
        }

        private ITypeSymbol SubstituteStateMachineTypeParameters(ITypeSymbol type)
        {
            if (type is null)
                return _stateMachine.Compilation.ErrorTypeSymbol;

            return _stateMachine.SubstituteStateMachineTypeParameters(type);
        }

        private IMethodSymbol SubstituteStateMachineTypeParameters(IMethodSymbol method)
        {
            if (method is null)
                throw new ArgumentNullException(nameof(method));

            return _stateMachine.SubstituteStateMachineTypeParameters(method);
        }

        private IPropertySymbol SubstituteStateMachineTypeParameters(IPropertySymbol property)
        {
            if (property is null)
                throw new ArgumentNullException(nameof(property));

            return _stateMachine.SubstituteStateMachineTypeParameters(property);
        }

        private static BoundExpression ApplyConversionIfNeeded(
            BoundExpression expression,
            ITypeSymbol targetType,
            Compilation compilation)
        {
            if (targetType is null)
                return expression;

            var sourceType = expression.Type ?? compilation.ErrorTypeSymbol;

            if (SymbolEqualityComparer.Default.Equals(sourceType, targetType))
                return expression;

            var conversion = compilation.ClassifyConversion(sourceType, targetType);
            if (!conversion.Exists || conversion.IsIdentity)
                return expression;

            return new BoundCastExpression(expression, targetType, conversion);
        }

        private static bool ContainsAwait(BoundExpression expression)
        {
            var visitor = new AwaitDetector();
            visitor.VisitExpression(expression);
            return visitor.FoundAwait;
        }

        private sealed class AwaitDetector : BoundTreeWalker
        {
            public bool FoundAwait { get; private set; }

            public override void VisitAwaitExpression(BoundAwaitExpression node)
            {
                FoundAwait = true;
            }

            public override void VisitExpression(BoundExpression? node)
            {
                if (FoundAwait || node is null)
                    return;

                base.VisitExpression(node);
            }

            public override void VisitStatement(BoundStatement? node)
            {
                if (FoundAwait || node is null)
                    return;

                base.VisitStatement(node);
            }
        }

        private ImmutableArray<ILocalSymbol> FilterLocalsToDispose(ImmutableArray<ILocalSymbol> locals)
        {
            if (_hoistableLocals.Count == 0 || locals.IsDefaultOrEmpty)
                return locals;

            var builder = ImmutableArray.CreateBuilder<ILocalSymbol>(locals.Length);
            var removedHoistedLocal = false;

            foreach (var local in locals)
            {
                if (_hoistableLocals.ContainsKey(local))
                {
                    removedHoistedLocal = true;
                    continue;
                }

                builder.Add(local);
            }

            if (!removedHoistedLocal)
                return locals;

            return builder.ToImmutable();
        }
    }

        private static class StateDispatchInjector
        {
            public static BoundBlockStatement Inject(
                BoundBlockStatement body,
                SynthesizedAsyncStateMachineTypeSymbol stateMachine,
            ImmutableArray<StateDispatch> dispatches,
            out ImmutableDictionary<int, ILabelSymbol> guardEntryLabels)
        {
            if (body is null)
                throw new ArgumentNullException(nameof(body));

            guardEntryLabels = ImmutableDictionary<int, ILabelSymbol>.Empty;

            if (dispatches.IsDefaultOrEmpty)
                return body;

            var labelMap = dispatches.ToImmutableDictionary(
                d => (ISymbol)d.Label,
                d => d,
                SymbolEqualityComparer.Default);
            var collector = new DispatchCollector(labelMap);
            var blockDispatches = collector.Collect(body);
            var preparedDispatches = PrepareBlockDispatches(
                stateMachine,
                blockDispatches,
                dispatches,
                out guardEntryLabels);

            if (preparedDispatches.Count == 0)
                return body;

            var rewriter = new DispatchInsertionRewriter(stateMachine, preparedDispatches);
            return (BoundBlockStatement)rewriter.VisitBlockStatement(body)!;
        }

        private static Dictionary<BoundNode, BlockDispatchInfo> PrepareBlockDispatches(
            SynthesizedAsyncStateMachineTypeSymbol stateMachine,
            Dictionary<BoundNode, List<StateDispatch>> blockDispatches,
            ImmutableArray<StateDispatch> dispatches,
            out ImmutableDictionary<int, ILabelSymbol> guardEntryLabels)
        {
            if (blockDispatches is null)
                throw new ArgumentNullException(nameof(blockDispatches));

            var guardBlocks = new HashSet<BoundBlockStatement>(ReferenceEqualityComparer.Instance);

            foreach (var dispatch in dispatches)
            {
                if (!dispatch.HasGuards)
                    continue;

                foreach (var guard in dispatch.GuardPath)
                {
                    guardBlocks.Add(guard);

                    if (!blockDispatches.TryGetValue(guard, out var list))
                    {
                        list = new List<StateDispatch>();
                        blockDispatches[guard] = list;
                    }

                    if (!list.Contains(dispatch))
                        list.Add(dispatch);
                }
            }

            var guardEntryByBlock = new Dictionary<BoundBlockStatement, LabelSymbol>(guardBlocks.Count, ReferenceEqualityComparer.Instance);
            var guardId = 0;

            foreach (var guard in guardBlocks)
                guardEntryByBlock[guard] = CreateLabel(stateMachine, $"guard{guardId++}");

            var entryLabelBuilder = ImmutableDictionary.CreateBuilder<int, ILabelSymbol>();

            foreach (var dispatch in dispatches)
            {
                if (!dispatch.HasGuards)
                    continue;

                var firstGuard = dispatch.GuardPath[0];
                if (guardEntryByBlock.TryGetValue(firstGuard, out var entryLabel))
                    entryLabelBuilder[dispatch.State] = entryLabel;
            }

            var result = new Dictionary<BoundNode, BlockDispatchInfo>(blockDispatches.Count, ReferenceEqualityComparer.Instance);

            foreach (var pair in blockDispatches)
            {
                var block = pair.Key;
                var dispatchList = pair.Value
                    .OrderBy(d => d.State)
                    .ToImmutableArray();

                var blockDispatchesBuilder = ImmutableArray.CreateBuilder<BlockDispatch>(dispatchList.Length);
                foreach (var dispatch in dispatchList)
                {
                    var target = ResolveBlockDispatchTarget(dispatch, block, guardEntryByBlock);
                    blockDispatchesBuilder.Add(new BlockDispatch(dispatch.State, target));
                }

                ILabelSymbol? entryLabel = null;
                if (block is BoundBlockStatement blockStatement)
                {
                    if (guardEntryByBlock.TryGetValue(blockStatement, out var guardEntry))
                        entryLabel = guardEntry;
                }

                result[block] = new BlockDispatchInfo(blockDispatchesBuilder.MoveToImmutable(), entryLabel);
            }

            guardEntryLabels = entryLabelBuilder.ToImmutable();
            return result;
        }

        private static ILabelSymbol ResolveBlockDispatchTarget(
            StateDispatch dispatch,
            BoundNode block,
            Dictionary<BoundBlockStatement, LabelSymbol> guardEntryByBlock)
        {
            if (block is BoundBlockStatement blockStatement && dispatch.TryGetGuardIndex(blockStatement, out var guardIndex))
            {
                if (guardIndex < dispatch.GuardPath.Length - 1)
                {
                    var nextGuard = dispatch.GuardPath[guardIndex + 1];
                    if (!guardEntryByBlock.TryGetValue(nextGuard, out var nextEntry))
                        throw new InvalidOperationException("Missing guard entry label for nested protected region.");

                    return nextEntry;
                }

                return dispatch.Label;
            }

            return dispatch.Label;
        }

        private sealed class DispatchCollector : BoundTreeWalker
        {
            private readonly ImmutableDictionary<ISymbol, StateDispatch> _dispatches;
            private readonly Dictionary<BoundNode, List<StateDispatch>> _blockDispatches = new(ReferenceEqualityComparer.Instance);
            private readonly Stack<BoundNode> _blocks = new();

            public DispatchCollector(ImmutableDictionary<ISymbol, StateDispatch> dispatches)
            {
                _dispatches = dispatches;
            }

            public Dictionary<BoundNode, List<StateDispatch>> Collect(BoundBlockStatement root)
            {
                VisitBlockStatement(root);
                return _blockDispatches;
            }

            public override void VisitBlockStatement(BoundBlockStatement node)
            {
                if (node is null)
                    return;

                _blocks.Push(node);
                foreach (var statement in node.Statements)
                    VisitStatement(statement);
                _blocks.Pop();
            }

            public override void VisitBlockExpression(BoundBlockExpression node)
            {
                if (node is null)
                    return;

                _blocks.Push(node);

                foreach (var statement in node.Statements)
                    VisitStatement(statement);

                _blocks.Pop();
            }

            public override void VisitLabeledStatement(BoundLabeledStatement node)
            {
                if (node is null)
                    return;

                if (_dispatches.TryGetValue(node.Label, out var dispatch) &&
                    _blocks.TryPeek(out var block))
                {
                    if (!_blockDispatches.TryGetValue(block, out var list))
                    {
                        list = new List<StateDispatch>();
                        _blockDispatches[block] = list;
                    }

                    list.Add(dispatch);
                }

                base.VisitLabeledStatement(node);
            }
        }

        private sealed class DispatchInsertionRewriter : BoundTreeRewriter
        {
            private readonly SynthesizedAsyncStateMachineTypeSymbol _stateMachine;
            private readonly Dictionary<BoundNode, BlockDispatchInfo> _blockDispatches;

            public DispatchInsertionRewriter(
                SynthesizedAsyncStateMachineTypeSymbol stateMachine,
                Dictionary<BoundNode, BlockDispatchInfo> blockDispatches)
            {
                _stateMachine = stateMachine;
                _blockDispatches = blockDispatches;
            }

            public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
            {
                if (node is null)
                    return null;

                var statements = new List<BoundStatement>();
                foreach (var statement in node.Statements)
                    statements.Add((BoundStatement)VisitStatement(statement));

                if (_blockDispatches.TryGetValue(node, out var dispatchInfo) && dispatchInfo.Dispatches.Length > 0)
                {
                    var insertionIndex = 0;

                    if (dispatchInfo.EntryLabel is not null)
                    {
                        var labeledEntry = new BoundLabeledStatement(
                            dispatchInfo.EntryLabel,
                            new BoundBlockStatement(Array.Empty<BoundStatement>()));
                        statements.Insert(insertionIndex, labeledEntry);
                        insertionIndex++;
                    }

                    var dispatchStatements = CreateDispatchStatements(dispatchInfo.Dispatches);
                    statements.InsertRange(insertionIndex, dispatchStatements);
                }

                return new BoundBlockStatement(statements, node.LocalsToDispose);
            }

            public override BoundNode? VisitBlockExpression(BoundBlockExpression node)
            {
                if (node is null)
                    return null;

                var statements = new List<BoundStatement>();
                foreach (var statement in node.Statements)
                    statements.Add((BoundStatement)VisitStatement(statement));

                if (_blockDispatches.TryGetValue(node, out var dispatchInfo) && dispatchInfo.Dispatches.Length > 0)
                {
                    var insertionIndex = 0;

                    if (dispatchInfo.EntryLabel is not null)
                    {
                        var labeledEntry = new BoundLabeledStatement(
                            dispatchInfo.EntryLabel,
                            new BoundBlockStatement(Array.Empty<BoundStatement>()));
                        statements.Insert(insertionIndex, labeledEntry);
                        insertionIndex++;
                    }

                    var dispatchStatements = CreateDispatchStatements(dispatchInfo.Dispatches);
                    statements.InsertRange(insertionIndex, dispatchStatements);
                }

                return new BoundBlockExpression(statements, node.UnitType, node.LocalsToDispose);
            }

            private IEnumerable<BoundStatement> CreateDispatchStatements(ImmutableArray<BlockDispatch> dispatches)
            {
                var stateType = _stateMachine.StateField.Type;
                if (!BoundBinaryOperator.TryLookup(_stateMachine.Compilation, SyntaxKind.EqualsEqualsToken, stateType, stateType, out var equals))
                    throw new InvalidOperationException("Async lowering requires integer equality operator.");

                foreach (var dispatch in dispatches)
                {
                    var stateAccess = new BoundFieldAccess(_stateMachine.StateField);
                    var stateLiteral = new BoundLiteralExpression(
                        BoundLiteralExpressionKind.NumericLiteral,
                        dispatch.State,
                        stateType);

                    var condition = new BoundBinaryExpression(stateAccess, equals, stateLiteral);
                    var gotoStatement = new BoundGotoStatement(dispatch.Target);
                    var gotoBlock = new BoundBlockStatement(new BoundStatement[] { gotoStatement });
                    yield return new BoundIfStatement(condition, gotoBlock);
                }
            }
        }

        private sealed class BlockDispatchInfo
        {
            public BlockDispatchInfo(ImmutableArray<BlockDispatch> dispatches, ILabelSymbol? entryLabel)
            {
                Dispatches = dispatches;
                EntryLabel = entryLabel;
            }

            public ImmutableArray<BlockDispatch> Dispatches { get; }

            public ILabelSymbol? EntryLabel { get; }
        }

        private readonly struct BlockDispatch
        {
            public BlockDispatch(int state, ILabelSymbol target)
            {
                State = state;
                Target = target ?? throw new ArgumentNullException(nameof(target));
            }

            public int State { get; }

            public ILabelSymbol Target { get; }
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
        public StateDispatch(int state, LabelSymbol label, ImmutableArray<BoundBlockStatement> guardPath)
        {
            State = state;
            Label = label ?? throw new ArgumentNullException(nameof(label));
            GuardPath = guardPath;
        }

        public int State { get; }

        public LabelSymbol Label { get; }

        public ImmutableArray<BoundBlockStatement> GuardPath { get; }

        public bool HasGuards => !GuardPath.IsDefaultOrEmpty && GuardPath.Length > 0;

        public bool TryGetGuardIndex(BoundBlockStatement block, out int index)
        {
            if (block is null || GuardPath.IsDefaultOrEmpty || GuardPath.Length == 0)
            {
                index = -1;
                return false;
            }

            for (var i = 0; i < GuardPath.Length; i++)
            {
                if (ReferenceEquals(GuardPath[i], block))
                {
                    index = i;
                    return true;
                }
            }

            index = -1;
            return false;
        }
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

    private static BoundStatement? CreateBuilderInitializationStatement(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        SourceLocalSymbol asyncLocal,
        SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers builderMembers,
        ITypeSymbol unitType)
    {
        var createMethod = builderMembers.Create;
        if (createMethod is null)
            return null;

        var invocation = new BoundInvocationExpression(createMethod, Array.Empty<BoundExpression>());
        var receiver = new BoundLocalAccess(asyncLocal);
        var assignment = new BoundFieldAssignmentExpression(
            receiver,
            builderMembers.BuilderField,
            invocation,
            unitType,
            requiresReceiverAddress: true);
        return new BoundAssignmentStatement(assignment);
    }

    private static BoundStatement? CreateBuilderStartStatement(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        SourceLocalSymbol asyncLocal,
        SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers builderMembers,
        INamedTypeSymbol stateMachineType)
    {
        var startMethod = builderMembers.Start;
        if (startMethod is null)
            return null;

        var methodTypeParameters = stateMachine.AsyncMethod.TypeParameters;
        INamedTypeSymbol substitutedStateMachineType;

        if (!methodTypeParameters.IsDefaultOrEmpty && methodTypeParameters.Length == stateMachine.TypeParameters.Length)
        {
            var methodArguments = TryCreateMethodTypeArguments(stateMachine)
                ?? methodTypeParameters.Select(static parameter => (ITypeSymbol)parameter).ToImmutableArray();
            var methodViewStateMachine = new ConstructedNamedTypeSymbol(stateMachine, methodArguments);
            substitutedStateMachineType = stateMachine.SubstituteStateMachineTypeParameters(methodViewStateMachine) as INamedTypeSymbol
                ?? methodViewStateMachine;
        }
        else
        {
            substitutedStateMachineType = stateMachine.SubstituteStateMachineTypeParameters(stateMachineType) as INamedTypeSymbol
                ?? stateMachineType;
        }

        var constructedStart = startMethod.IsGenericMethod
            ? startMethod.Construct(substitutedStateMachineType)
            : startMethod;

        var builderAccess = new BoundMemberAccessExpression(new BoundLocalAccess(asyncLocal), builderMembers.BuilderField);
        var stateMachineReference = new BoundAddressOfExpression(asyncLocal, stateMachineType);

        var invocation = new BoundInvocationExpression(
            constructedStart,
            new BoundExpression[] { stateMachineReference },
            builderAccess,
            requiresReceiverAddress: true);

        return new BoundExpressionStatement(invocation);

        static ImmutableArray<ITypeSymbol>? TryCreateMethodTypeArguments(SynthesizedAsyncStateMachineTypeSymbol stateMachine)
        {
            var mappings = stateMachine.TypeParameterMappings;
            if (mappings.IsDefaultOrEmpty || mappings.Length == 0)
                return null;

            var stateMachineParameters = stateMachine.TypeParameters;
            if (stateMachineParameters.IsDefaultOrEmpty || stateMachineParameters.Length == 0)
                return null;

            var mappedArguments = new ITypeSymbol[stateMachineParameters.Length];
            var asyncParameters = stateMachine.AsyncMethod.TypeParameters;
            var mappedCount = 0;

            foreach (var mapping in mappings)
            {
                var ordinal = mapping.StateMachineParameter.Ordinal;
                if ((uint)ordinal >= (uint)mappedArguments.Length)
                    continue;

                var asyncOrdinal = mapping.AsyncParameter.Ordinal;
                if ((uint)asyncOrdinal >= (uint)asyncParameters.Length)
                    continue;

                mappedArguments[ordinal] = asyncParameters[asyncOrdinal];
                mappedCount++;
            }

            if (mappedCount != mappedArguments.Length)
                return null;

            for (var i = 0; i < mappedArguments.Length; i++)
            {
                if (mappedArguments[i] is null)
                    return null;
            }

            return ImmutableArray.Create(mappedArguments);
        }
    }

    private static BoundBlockStatement? CreateSetStateMachineBody(SynthesizedAsyncStateMachineTypeSymbol stateMachine)
    {
        var builderMembers = stateMachine.GetBuilderMembers(stateMachine.AsyncMethod);
        var setStateMachineMethod = builderMembers.SetStateMachine;
        if (setStateMachineMethod is null)
            return null;

        setStateMachineMethod = stateMachine.SubstituteAsyncMethodTypeParameters(setStateMachineMethod);

        var builderAccess = new BoundMemberAccessExpression(new BoundSelfExpression(stateMachine), builderMembers.BuilderField);
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

        var assignment = CreateStateAssignment(stateMachine, -1);
        return new BoundBlockStatement(new BoundStatement[]
        {
            assignment,
            new BoundExpressionStatement(invocation)
        });
    }

    private static BoundExpression CreateReturnExpression(
        SourceMethodSymbol method,
        SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers builderMembers,
        SourceLocalSymbol asyncLocal)
    {
        var taskProperty = builderMembers.TaskProperty;
        if (taskProperty is null)
            return CreateNullLiteral(method.ReturnType);

        var builderAccess = new BoundMemberAccessExpression(new BoundLocalAccess(asyncLocal), builderMembers.BuilderField);
        var taskAccess = new BoundMemberAccessExpression(builderAccess, taskProperty);

        if (!SymbolEqualityComparer.Default.Equals(taskProperty.Type, method.ReturnType))
        {
            var conversion = new Conversion(isImplicit: true, isIdentity: true);
            return new BoundCastExpression(taskAccess, method.ReturnType, conversion);
        }

        return taskAccess;
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
