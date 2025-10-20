using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class IteratorLowerer
{
    public static bool ShouldRewrite(SourceMethodSymbol method, BoundBlockStatement block)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));
        if (block is null)
            throw new ArgumentNullException(nameof(block));

        var finder = new YieldStatementFinder();
        finder.VisitBlockStatement(block);
        return finder.FoundYield;
    }

    public static BoundBlockStatement Rewrite(SourceMethodSymbol method, BoundBlockStatement block)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));
        if (block is null)
            throw new ArgumentNullException(nameof(block));

        var compilation = GetCompilation(method);

        var signature = DetermineIteratorSignature(compilation, method.ReturnType);
        if (signature.Kind == IteratorMethodKind.None)
            return block;

        method.MarkIterator(signature.Kind, signature.ElementType);

        if (method.IteratorStateMachine is null)
        {
            var stateMachine = compilation.CreateIteratorStateMachine(method, signature.Kind, signature.ElementType);
            method.SetIteratorStateMachine(stateMachine);
        }

        var iteratorType = method.IteratorStateMachine;
        if (iteratorType is null)
            throw new InvalidOperationException("Iterator state machine not created.");

        if (iteratorType.MoveNextBody is null)
        {
            var moveNextBody = CreateMoveNextBody(compilation, iteratorType, block);
            iteratorType.SetMoveNextBody(moveNextBody);
        }

        EnsureIteratorHelpers(compilation, iteratorType);

        return RewriteIteratorMethodBody(compilation, method, iteratorType);
    }

    private static BoundBlockStatement CreateMoveNextBody(
        Compilation compilation,
        SynthesizedIteratorTypeSymbol stateMachine,
        BoundBlockStatement body)
    {
        var builder = new MoveNextBuilder(compilation, stateMachine);
        var moveNext = builder.Rewrite(body);

        if (stateMachine.DisposeBody is null && builder.DisposeBody is not null)
            stateMachine.SetDisposeBody(builder.DisposeBody);

        return moveNext;
    }

    private static BoundBlockStatement CreateCurrentGetterBody(SynthesizedIteratorTypeSymbol stateMachine)
    {
        var currentAccess = new BoundFieldAccess(stateMachine.CurrentField);
        var returnStatement = new BoundReturnStatement(currentAccess);
        return new BoundBlockStatement(new[] { returnStatement });
    }

    private static BoundBlockStatement CreateNonGenericCurrentGetterBody(
        Compilation compilation,
        SynthesizedIteratorTypeSymbol stateMachine)
    {
        var currentAccess = new BoundFieldAccess(stateMachine.CurrentField);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var result = ConvertIfNeeded(compilation, currentAccess, objectType);

        var returnStatement = new BoundReturnStatement(result);
        return new BoundBlockStatement(new[] { returnStatement });
    }

    private static BoundBlockStatement CreateDisposeBody(SynthesizedIteratorTypeSymbol stateMachine)
    {
        var statements = new List<BoundStatement>();

        var literal = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            -1,
            stateMachine.StateField.Type);
        var assignment = new BoundFieldAssignmentExpression(
            null,
            stateMachine.StateField,
            literal,
            stateMachine.Compilation.GetSpecialType(SpecialType.System_Unit));
        statements.Add(new BoundAssignmentStatement(assignment));

        statements.Add(new BoundReturnStatement(null));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateResetBody(
        Compilation compilation,
        SynthesizedIteratorTypeSymbol stateMachine)
    {
        var notSupportedType = compilation.GetTypeByMetadataName("System.NotSupportedException") as INamedTypeSymbol;
        var constructor = notSupportedType?
            .Constructors
            .FirstOrDefault(static ctor => !ctor.IsStatic && ctor.Parameters.Length == 0);

        if (constructor is null)
            return new BoundBlockStatement(new[] { new BoundReturnStatement(null) });

        var creation = new BoundObjectCreationExpression(constructor, Array.Empty<BoundExpression>());
        var throwStatement = new BoundThrowStatement(creation);
        return new BoundBlockStatement(new BoundStatement[] { throwStatement });
    }

    private static BoundBlockStatement CreateGenericGetEnumeratorBody(
        Compilation compilation,
        SynthesizedIteratorTypeSymbol stateMachine)
    {
        var statements = new List<BoundStatement>();

        var literal = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            0,
            stateMachine.StateField.Type);
        var assignment = new BoundFieldAssignmentExpression(
            null,
            stateMachine.StateField,
            literal,
            compilation.GetSpecialType(SpecialType.System_Unit));
        statements.Add(new BoundAssignmentStatement(assignment));

        BoundExpression result = new BoundSelfExpression(stateMachine);
        var method = stateMachine.GenericGetEnumeratorMethod!;
        result = ConvertIfNeeded(compilation, result, method.ReturnType);

        statements.Add(new BoundReturnStatement(result));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateNonGenericGetEnumeratorBody(
        Compilation compilation,
        SynthesizedIteratorTypeSymbol stateMachine)
    {
        var method = stateMachine.NonGenericGetEnumeratorMethod!;
        var genericMethod = stateMachine.GenericGetEnumeratorMethod;

        BoundExpression result;
        if (genericMethod is not null)
        {
            result = new BoundInvocationExpression(
                genericMethod,
                Array.Empty<BoundExpression>(),
                receiver: new BoundSelfExpression(stateMachine));
        }
        else
        {
            result = new BoundSelfExpression(stateMachine);
        }

        result = ConvertIfNeeded(compilation, result, method.ReturnType);

        var returnStatement = new BoundReturnStatement(result);
        return new BoundBlockStatement(new[] { returnStatement });
    }

    private static void EnsureIteratorHelpers(Compilation compilation, SynthesizedIteratorTypeSymbol stateMachine)
    {
        if (stateMachine.CurrentGetterBody is null)
        {
            var body = CreateCurrentGetterBody(stateMachine);
            stateMachine.SetCurrentGetterBody(body);
        }

        if (stateMachine.NonGenericCurrentGetterBody is null)
        {
            var body = CreateNonGenericCurrentGetterBody(compilation, stateMachine);
            stateMachine.SetNonGenericCurrentGetterBody(body);
        }

        if (stateMachine.DisposeBody is null)
        {
            var body = CreateDisposeBody(stateMachine);
            stateMachine.SetDisposeBody(body);
        }

        if (stateMachine.ResetBody is null)
        {
            var body = CreateResetBody(compilation, stateMachine);
            stateMachine.SetResetBody(body);
        }

        if (stateMachine.GenericGetEnumeratorMethod is not null && stateMachine.GenericGetEnumeratorBody is null)
        {
            var body = CreateGenericGetEnumeratorBody(compilation, stateMachine);
            stateMachine.SetGenericGetEnumeratorBody(body);
        }

        if (stateMachine.NonGenericGetEnumeratorMethod is not null && stateMachine.NonGenericGetEnumeratorBody is null)
        {
            var body = CreateNonGenericGetEnumeratorBody(compilation, stateMachine);
            stateMachine.SetNonGenericGetEnumeratorBody(body);
        }
    }

    private static BoundExpression ConvertIfNeeded(
        Compilation compilation,
        BoundExpression expression,
        ITypeSymbol targetType)
    {
        if (SymbolEqualityComparer.Default.Equals(expression.Type, targetType))
            return expression;

        var conversion = compilation.ClassifyConversion(expression.Type, targetType);
        if (!conversion.Exists)
        {
            var fromType = expression.Type;
            var isReference = !fromType.IsValueType && targetType.IsReferenceType;
            var isBoxing = fromType.IsValueType && targetType.IsReferenceType;
            conversion = new Conversion(isImplicit: true, isReference: isReference, isBoxing: isBoxing);
        }

        return new BoundCastExpression(expression, targetType, conversion);
    }

    private static BoundBlockStatement RewriteIteratorMethodBody(
        Compilation compilation,
        SourceMethodSymbol method,
        SynthesizedIteratorTypeSymbol stateMachine)
    {
        var statements = new List<BoundStatement>();
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);

        var stateMachineLocal = new SourceLocalSymbol(
            "<>iter",
            stateMachine,
            isMutable: true,
            method,
            method.ContainingType,
            method.ContainingNamespace,
            Array.Empty<Location>(),
            Array.Empty<SyntaxReference>());

        var creation = new BoundObjectCreationExpression(stateMachine.Constructor, Array.Empty<BoundExpression>());
        var declarator = new BoundVariableDeclarator(stateMachineLocal, creation);
        statements.Add(new BoundLocalDeclarationStatement(new[] { declarator }));

        if (stateMachine.ThisField is not null)
        {
            var receiver = new BoundLocalAccess(stateMachineLocal);
            var value = new BoundSelfExpression(stateMachine.ThisField.Type);
            var assignment = new BoundFieldAssignmentExpression(receiver, stateMachine.ThisField, value, unitType);
            statements.Add(new BoundAssignmentStatement(assignment));
        }

        foreach (var parameter in method.Parameters)
        {
            if (!stateMachine.ParameterFieldMap.TryGetValue(parameter, out var field))
                continue;

            var receiver = new BoundLocalAccess(stateMachineLocal);
            var value = new BoundParameterAccess(parameter);
            var assignment = new BoundFieldAssignmentExpression(receiver, field, value, unitType);
            statements.Add(new BoundAssignmentStatement(assignment));
        }

        var stateReceiver = new BoundLocalAccess(stateMachineLocal);
        var initialState = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            0,
            stateMachine.StateField.Type);
        var stateAssignment = new BoundFieldAssignmentExpression(stateReceiver, stateMachine.StateField, initialState, unitType);
        statements.Add(new BoundAssignmentStatement(stateAssignment));

        BoundExpression returnExpression = new BoundLocalAccess(stateMachineLocal);
        var returnType = method.ReturnType;
        returnExpression = ConvertIfNeeded(compilation, returnExpression, returnType);

        statements.Add(new BoundReturnStatement(returnExpression));

        return new BoundBlockStatement(statements);
    }

    private static Compilation GetCompilation(SourceMethodSymbol method)
    {
        if (method.ContainingAssembly is SourceAssemblySymbol sourceAssembly)
            return sourceAssembly.Compilation;

        throw new InvalidOperationException("Iterator lowering requires a source assembly method.");
    }

    private static IteratorSignature DetermineIteratorSignature(Compilation compilation, ITypeSymbol returnType)
    {
        if (returnType is null)
            return new IteratorSignature(IteratorMethodKind.None, compilation.ErrorTypeSymbol);

        var enumerableDefinition = compilation.GetSpecialType(SpecialType.System_Collections_Generic_IEnumerable_T);
        var enumeratorDefinition = compilation.GetSpecialType(SpecialType.System_Collections_Generic_IEnumerator_T);
        var enumerableType = compilation.GetSpecialType(SpecialType.System_Collections_IEnumerable);
        var enumeratorType = compilation.GetSpecialType(SpecialType.System_Collections_IEnumerator);

        if (returnType is INamedTypeSymbol named)
        {
            var definition = named.ConstructedFrom as INamedTypeSymbol ?? named;

            if (MatchesMetadata(definition, enumerableDefinition))
            {
                var element = named.TypeArguments.Length == 1
                    ? named.TypeArguments[0]
                    : compilation.GetSpecialType(SpecialType.System_Object);
                return new IteratorSignature(IteratorMethodKind.Enumerable, element);
            }

            if (MatchesMetadata(definition, enumeratorDefinition))
            {
                var element = named.TypeArguments.Length == 1
                    ? named.TypeArguments[0]
                    : compilation.GetSpecialType(SpecialType.System_Object);
                return new IteratorSignature(IteratorMethodKind.Enumerator, element);
            }

            if (MatchesMetadata(definition, enumerableType))
                return new IteratorSignature(IteratorMethodKind.Enumerable, compilation.GetSpecialType(SpecialType.System_Object));

            if (MatchesMetadata(definition, enumeratorType))
                return new IteratorSignature(IteratorMethodKind.Enumerator, compilation.GetSpecialType(SpecialType.System_Object));
        }

        if (returnType is INamedTypeSymbol namedEnumerable && MatchesMetadata(namedEnumerable, enumerableType))
            return new IteratorSignature(IteratorMethodKind.Enumerable, compilation.GetSpecialType(SpecialType.System_Object));

        if (returnType is INamedTypeSymbol namedEnumerator && MatchesMetadata(namedEnumerator, enumeratorType))
            return new IteratorSignature(IteratorMethodKind.Enumerator, compilation.GetSpecialType(SpecialType.System_Object));

        return new IteratorSignature(IteratorMethodKind.None, compilation.ErrorTypeSymbol);
    }

    private readonly record struct IteratorSignature(IteratorMethodKind Kind, ITypeSymbol ElementType);

    private static bool MatchesMetadata(INamedTypeSymbol candidate, INamedTypeSymbol target)
    {
        if (candidate.MetadataName != target.MetadataName)
            return false;

        return NamespaceEquals(candidate.ContainingNamespace, target.ContainingNamespace);
    }

    private static bool NamespaceEquals(INamespaceSymbol? left, INamespaceSymbol? right)
    {
        if (left is null && right is null)
            return true;

        if (left is null || right is null)
            return false;

        if (left.IsGlobalNamespace && right.IsGlobalNamespace)
            return true;

        if (left.MetadataName != right.MetadataName)
            return false;

        return NamespaceEquals(left.ContainingNamespace, right.ContainingNamespace);
    }

    private sealed class YieldStatementFinder : BoundTreeWalker
    {
        public bool FoundYield { get; private set; }

        public override void VisitStatement(BoundStatement statement)
        {
            if (FoundYield)
                return;

            base.VisitStatement(statement);
        }

        public override void VisitExpression(BoundExpression node)
        {
            if (FoundYield)
                return;

            base.VisitExpression(node);
        }

        public override void VisitYieldReturnStatement(BoundYieldReturnStatement node)
        {
            FoundYield = true;
        }

        public override void VisitYieldBreakStatement(BoundYieldBreakStatement node)
        {
            FoundYield = true;
        }

        public override void VisitLambdaExpression(BoundLambdaExpression node)
        {
            // Nested lambdas are lowered independently.
        }
    }

    private sealed class MoveNextBuilder : BoundTreeRewriter
    {
        private readonly Compilation _compilation;
        private readonly SynthesizedIteratorTypeSymbol _stateMachine;
        private readonly SourceMethodSymbol _moveNextMethod;
        private readonly List<StateEntry> _states = new();
        private readonly Dictionary<ILocalSymbol, SourceFieldSymbol> _hoistedLocals = new(SymbolEqualityComparer.Default);
        private readonly HashSet<string> _hoistedFieldNames = new(StringComparer.Ordinal);
        private readonly Stack<FinallyFrame> _finallyStack = new();
        private readonly Dictionary<int, ImmutableArray<FinallyFrame>> _pendingFinallyStates = new();
        private readonly ITypeSymbol _boolType;
        private readonly ITypeSymbol _unitType;
        private bool _isTopLevelBlock = true;
        private StateEntry _startState;
        private LabelSymbol? _returnLabel;
        private SourceLocalSymbol? _resultLocal;
        private int _nextHoistedLocalId;

        public BoundBlockStatement? DisposeBody { get; private set; }

        public MoveNextBuilder(Compilation compilation, SynthesizedIteratorTypeSymbol stateMachine)
        {
            _compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
            _stateMachine = stateMachine ?? throw new ArgumentNullException(nameof(stateMachine));
            _moveNextMethod = stateMachine.MoveNextMethod ?? throw new ArgumentNullException(nameof(stateMachine.MoveNextMethod));
            _boolType = compilation.GetSpecialType(SpecialType.System_Boolean);
            _unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        }

        public BoundBlockStatement Rewrite(BoundBlockStatement block)
        {
            if (block is null)
                throw new ArgumentNullException(nameof(block));

            HoistLocals(block);
            _startState = AllocateState();
            _returnLabel = CreateLabel("<>Return");
            _resultLocal = CreateResultLocal();

            var rewrittenBody = (BoundBlockStatement)VisitBlockStatement(block);

            var statements = new List<BoundStatement>();
            var resultLocal = _resultLocal ?? throw new InvalidOperationException("Result local not created.");
            var resultDeclarator = new BoundVariableDeclarator(resultLocal, CreateBoolLiteral(false));
            statements.Add(new BoundLocalDeclarationStatement(new[] { resultDeclarator }));

            statements.AddRange(CreateStateDispatch());
            statements.Add(CreateReturnStatement(CreateBoolLiteral(false)));
            statements.AddRange(rewrittenBody.Statements);
            statements.Add(CreateStateAssignment(-1));
            statements.Add(CreateReturnStatement(CreateBoolLiteral(false)));

            var returnLabel = _returnLabel ?? throw new InvalidOperationException("Return label not created.");
            var returnBlock = new BoundBlockStatement(new BoundStatement[]
            {
                new BoundReturnStatement(new BoundLocalAccess(resultLocal)),
            });
            statements.Add(new BoundLabeledStatement(returnLabel, returnBlock));

            DisposeBody = BuildDisposeBody();

            return new BoundBlockStatement(statements);
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
                        var assignment = new BoundFieldAssignmentExpression(null, field, initializer, _unitType);
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

        public override BoundNode? VisitLocalAccess(BoundLocalAccess node)
        {
            if (node is null)
                return null;

            if (_hoistedLocals.TryGetValue(node.Local, out var field))
                return new BoundFieldAccess(field, node.Reason);

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

        public override BoundNode? VisitReturnStatement(BoundReturnStatement node)
        {
            if (node is null)
                return null;

            var expression = node.Expression is null
                ? null
                : VisitExpression(node.Expression) ?? node.Expression;

            return CreateReturnStatement(expression);
        }

        public override BoundNode? VisitLocalAssignmentExpression(BoundLocalAssignmentExpression node)
        {
            if (node is null)
                return null;

            var right = VisitExpression(node.Right) ?? node.Right;

            if (_hoistedLocals.TryGetValue(node.Local, out var field))
                return new BoundFieldAssignmentExpression(null, field, right, _unitType);

            if (!ReferenceEquals(right, node.Right))
                return new BoundLocalAssignmentExpression(node.Local, right, _unitType);

            return node;
        }

        public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
        {
            if (node is null)
                return null;

            var wasTopLevel = _isTopLevelBlock;
            _isTopLevelBlock = false;

            var statements = new List<BoundStatement>();
            foreach (var statement in node.Statements)
            {
                var rewritten = VisitStatement(statement);
                if (rewritten is BoundBlockStatement rewrittenBlock &&
                    !rewrittenBlock.Statements.Any() &&
                    rewrittenBlock.LocalsToDispose.Length == 0)
                {
                    continue;
                }

                statements.Add(rewritten);
            }

            _isTopLevelBlock = wasTopLevel;

            if (wasTopLevel)
            {
                var inner = new BoundBlockStatement(statements, node.LocalsToDispose);
                var labeled = new BoundLabeledStatement(_startState.Label, inner);
                return new BoundBlockStatement(new BoundStatement[] { labeled });
            }

            return new BoundBlockStatement(statements, node.LocalsToDispose);
        }

        public override BoundNode? VisitTryStatement(BoundTryStatement node)
        {
            if (node is null)
                return null;

            FinallyFrame? frame = null;
            if (node.FinallyBlock is not null)
            {
                frame = new FinallyFrame();
                _finallyStack.Push(frame);
            }

            try
            {
                var tryBlock = (BoundBlockStatement)VisitBlockStatement(node.TryBlock);

                var catchBuilder = ImmutableArray.CreateBuilder<BoundCatchClause>(node.CatchClauses.Length);
                foreach (var catchClause in node.CatchClauses)
                {
                    var catchBlock = (BoundBlockStatement)VisitBlockStatement(catchClause.Block);
                    catchBuilder.Add(new BoundCatchClause(catchClause.ExceptionType, catchClause.Local, catchBlock));
                }

                BoundBlockStatement? finallyBlock = null;
                if (node.FinallyBlock is not null)
                {
                    var rewrittenFinally = (BoundBlockStatement)VisitBlockStatement(node.FinallyBlock);
                    finallyBlock = WrapFinallyBlock(rewrittenFinally);

                    if (frame is not null)
                        frame.FinallyBlock = finallyBlock;
                }

                return new BoundTryStatement(tryBlock, catchBuilder.ToImmutable(), finallyBlock);
            }
            finally
            {
                if (frame is not null && _finallyStack.Count > 0 && ReferenceEquals(_finallyStack.Peek(), frame))
                    _finallyStack.Pop();
            }
        }

        public override BoundNode? VisitYieldReturnStatement(BoundYieldReturnStatement node)
        {
            if (node is null)
                return null;

            var resumeState = AllocateState();
            var expression = VisitExpression(node.Expression) ?? node.Expression;
            var currentValue = ApplyElementConversion(expression);

            var assignCurrent = new BoundAssignmentStatement(
                new BoundFieldAssignmentExpression(null, _stateMachine.CurrentField, currentValue, _unitType));

            var assignState = CreateStateAssignment(resumeState.Value);

            var returnTrue = CreateReturnStatement(CreateBoolLiteral(true));
            var resumeLabel = new BoundLabeledStatement(resumeState.Label, new BoundBlockStatement(Array.Empty<BoundStatement>()));

            if (_finallyStack.Count > 0)
            {
                var builder = ImmutableArray.CreateBuilder<FinallyFrame>(_finallyStack.Count);
                foreach (var frame in _finallyStack)
                    builder.Add(frame);

                _pendingFinallyStates[resumeState.Value] = builder.ToImmutable();
            }

            return new BoundBlockStatement(new BoundStatement[]
            {
                assignCurrent,
                assignState,
                returnTrue,
                resumeLabel,
            });
        }

        public override BoundNode? VisitYieldBreakStatement(BoundYieldBreakStatement node)
        {
            if (node is null)
                return null;

            var assignCompleted = CreateStateAssignment(-1);
            var returnFalse = CreateReturnStatement(CreateBoolLiteral(false));

            return new BoundBlockStatement(new BoundStatement[]
            {
                assignCompleted,
                returnFalse,
            });
        }

        private BoundBlockStatement? BuildDisposeBody()
        {
            if (_pendingFinallyStates.Count == 0)
                return null;

            var statements = new List<BoundStatement>();

            foreach (var entry in _pendingFinallyStates.OrderBy(static pair => pair.Key))
            {
                var condition = CreateStateEquals(entry.Key);
                var thenStatements = new List<BoundStatement>
                {
                    CreateStateAssignment(-1),
                };

                foreach (var frame in entry.Value)
                {
                    if (frame.FinallyBlock is null)
                        continue;

                    thenStatements.Add(frame.FinallyBlock);
                    thenStatements.Add(CreateStateAssignment(-1));
                }

                thenStatements.Add(new BoundReturnStatement(null));

                var thenBlock = new BoundBlockStatement(thenStatements);
                statements.Add(new BoundIfStatement(condition, thenBlock));
            }

            statements.Add(CreateStateAssignment(-1));
            statements.Add(new BoundReturnStatement(null));

            return new BoundBlockStatement(statements);
        }

        private BoundStatement CreateReturnStatement(BoundExpression? expression)
        {
            if (_resultLocal is null || _returnLabel is null)
                throw new InvalidOperationException("Return infrastructure not initialized.");

            var value = expression ?? CreateBoolLiteral(false);
            value = ConvertIfNeeded(_compilation, value, _boolType);

            var assignment = new BoundLocalAssignmentExpression(_resultLocal, value, _unitType);
            var assignStatement = new BoundAssignmentStatement(assignment);
            var gotoStatement = new BoundGotoStatement(_returnLabel);

            return new BoundBlockStatement(new BoundStatement[]
            {
                assignStatement,
                gotoStatement,
            });
        }

        private IEnumerable<BoundStatement> CreateStateDispatch()
        {
            foreach (var state in _states)
            {
                var condition = CreateStateEquals(state.Value);
                var thenBlock = new BoundBlockStatement(new BoundStatement[]
                {
                    CreateStateAssignment(-1),
                    new BoundGotoStatement(state.Label),
                });

                yield return new BoundIfStatement(condition, thenBlock);
            }
        }

        private BoundExpression CreateStateEquals(int value)
        {
            var stateAccess = new BoundFieldAccess(_stateMachine.StateField);
            var literal = CreateIntLiteral(value);

            if (!BoundBinaryOperator.TryLookup(_compilation, SyntaxKind.EqualsEqualsToken, stateAccess.Type, literal.Type, out var op))
                throw new InvalidOperationException("Iterator lowering requires integer equality operator.");

            return new BoundBinaryExpression(stateAccess, op, literal);
        }

        private BoundBlockStatement WrapFinallyBlock(BoundBlockStatement block)
        {
            var stateAccess = new BoundFieldAccess(_stateMachine.StateField);
            var zero = CreateIntLiteral(0);

            if (!BoundBinaryOperator.TryLookup(_compilation, SyntaxKind.LessThanToken, stateAccess.Type, zero.Type, out var op))
                throw new InvalidOperationException("Iterator lowering requires integer comparison operator.");

            var condition = new BoundBinaryExpression(stateAccess, op, zero);
            var guard = new BoundIfStatement(condition, block);

            return new BoundBlockStatement(new BoundStatement[] { guard });
        }

        private BoundAssignmentStatement CreateStateAssignment(int value)
        {
            var literal = CreateIntLiteral(value);
            var assignment = new BoundFieldAssignmentExpression(null, _stateMachine.StateField, literal, _unitType);
            return new BoundAssignmentStatement(assignment);
        }

        private LabelSymbol CreateLabel(string name)
        {
            return new LabelSymbol(
                name,
                _moveNextMethod,
                _stateMachine,
                _stateMachine.ContainingNamespace,
                new[] { Location.None },
                Array.Empty<SyntaxReference>());
        }

        private SourceLocalSymbol CreateResultLocal()
        {
            return new SourceLocalSymbol(
                "<>result",
                _boolType,
                isMutable: true,
                _moveNextMethod,
                _stateMachine,
                _stateMachine.ContainingNamespace,
                new[] { Location.None },
                Array.Empty<SyntaxReference>());
        }

        private BoundLiteralExpression CreateIntLiteral(int value)
        {
            return new BoundLiteralExpression(
                BoundLiteralExpressionKind.NumericLiteral,
                value,
                _stateMachine.StateField.Type);
        }

        private BoundLiteralExpression CreateBoolLiteral(bool value)
        {
            return new BoundLiteralExpression(
                value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral,
                value,
                _boolType);
        }

        private BoundExpression ApplyElementConversion(BoundExpression expression)
        {
            var targetType = _stateMachine.ElementType ?? _compilation.ErrorTypeSymbol;
            var sourceType = expression.Type ?? _compilation.ErrorTypeSymbol;

            if (SymbolEqualityComparer.Default.Equals(sourceType, targetType))
                return expression;

            var conversion = _compilation.ClassifyConversion(sourceType, targetType);
            if (!conversion.Exists || conversion.IsIdentity)
                return expression;

            return new BoundCastExpression(expression, targetType, conversion);
        }

        private StateEntry AllocateState()
        {
            var value = _states.Count;
            var label = new LabelSymbol(
                $"<>State_{value}",
                _moveNextMethod,
                _stateMachine,
                _stateMachine.ContainingNamespace,
                new[] { Location.None },
                Array.Empty<SyntaxReference>());

            var entry = new StateEntry(value, label);
            _states.Add(entry);
            return entry;
        }

        private readonly record struct StateEntry(int Value, ILabelSymbol Label);

        private void HoistLocals(BoundBlockStatement body)
        {
            var collector = new HoistableLocalCollector();
            collector.VisitBlockStatement(body);

            foreach (var local in collector.Locals)
            {
                if (_hoistedLocals.ContainsKey(local))
                    continue;

                var type = local.Type ?? _compilation.ErrorTypeSymbol;
                var fieldName = CreateHoistedLocalFieldName();
                var field = _stateMachine.AddHoistedLocal(fieldName, type);
                _hoistedLocals.Add(local, field);
            }
        }

        private string CreateHoistedLocalFieldName()
        {
            string candidate;
            do
            {
                candidate = $"_local{_nextHoistedLocalId++}";
            } while (!_hoistedFieldNames.Add(candidate));

            return candidate;
        }

        private sealed class HoistableLocalCollector : BoundTreeWalker
        {
            private readonly HashSet<ILocalSymbol> _locals = new(SymbolEqualityComparer.Default);

            public IEnumerable<ILocalSymbol> Locals => _locals;

            public override void VisitLocalDeclarationStatement(BoundLocalDeclarationStatement node)
            {
                foreach (var declarator in node.Declarators)
                    _locals.Add(declarator.Local);

                base.VisitLocalDeclarationStatement(node);
            }

            public override void VisitLocalAccess(BoundLocalAccess node)
            {
                _locals.Add(node.Local);
                base.VisitLocalAccess(node);
            }

            public override void VisitVariableExpression(BoundVariableExpression node)
            {
                _locals.Add(node.Variable);
                base.VisitVariableExpression(node);
            }

            public override void VisitLocalAssignmentExpression(BoundLocalAssignmentExpression node)
            {
                _locals.Add(node.Local);
                base.VisitLocalAssignmentExpression(node);
            }

            public override void VisitForStatement(BoundForStatement node)
            {
                if (node.Local is not null)
                    _locals.Add(node.Local);

                base.VisitForStatement(node);
            }
        }

        private sealed class FinallyFrame
        {
            public BoundBlockStatement? FinallyBlock { get; set; }
        }
    }
}
