using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private BoundStatement BindExpressionStatement(ExpressionStatementSyntax expressionStmt)
    {
        var expr = expressionStmt.Parent is BlockStatementSyntax block &&
            IsImplicitReturnTarget(block, expressionStmt) &&
            _containingSymbol is IMethodSymbol method
            ? BindExpressionWithTargetType(expressionStmt.Expression, GetReturnTargetType(method))
            : BindExpression(expressionStmt.Expression, allowReturn: true);

        if (expr is BoundMethodGroupExpression methodGroup && methodGroup.GetConvertedType() is null)
        {
            expr = ReportMethodGroupRequiresDelegate(methodGroup, expressionStmt.Expression);
            CacheBoundNode(expressionStmt.Expression, expr);
        }

        return ExpressionToStatement(expr);
    }

    private BoundStatement BindIfStatement(IfStatementSyntax ifStmt)
    {
        var condition = BindExpression(ifStmt.Condition);
        var entryState = new HashSet<ISymbol>(_nonNullSymbols, SymbolEqualityComparer.Default);
        var thenEntryState = entryState;
        var elseEntryState = entryState;
        var thenExits = IsEarlyExitStatement(ifStmt.ThenStatement);
        var elseExits = ifStmt.ElseClause is not null && IsEarlyExitStatement(ifStmt.ElseClause.Statement);

        var boolType = Compilation.GetSpecialType(SpecialType.System_Boolean);
        var conversion = Compilation.ClassifyConversion(condition.Type, boolType);
        if (!conversion.Exists)
        {
            _diagnostics.ReportCannotConvertFromTypeToType(condition.Type, boolType, ifStmt.Condition.GetLocation());
        }

        if (TryGetNullCheckFlow(condition, out var symbol, out var nonNullWhenTrue, out var nonNullWhenFalse))
        {
            thenEntryState = new HashSet<ISymbol>(entryState, SymbolEqualityComparer.Default);
            elseEntryState = new HashSet<ISymbol>(entryState, SymbolEqualityComparer.Default);

            if (nonNullWhenTrue)
                thenEntryState.Add(symbol);
            else
                thenEntryState.Remove(symbol);

            if (nonNullWhenFalse)
                elseEntryState.Add(symbol);
            else
                elseEntryState.Remove(symbol);
        }

        _nonNullSymbols.Clear();
        _nonNullSymbols.UnionWith(thenEntryState);
        var thenBound = BindStatement(ifStmt.ThenStatement);
        var thenExitState = new HashSet<ISymbol>(_nonNullSymbols, SymbolEqualityComparer.Default);
        BoundStatement? elseBound = null;
        HashSet<ISymbol> elseExitState = elseEntryState;
        if (ifStmt.ElseClause is not null)
        {
            _nonNullSymbols.Clear();
            _nonNullSymbols.UnionWith(elseEntryState);
            elseBound = BindStatement(ifStmt.ElseClause.Statement);
            elseExitState = new HashSet<ISymbol>(_nonNullSymbols, SymbolEqualityComparer.Default);
        }

        if (ifStmt.ElseClause is not null)
        {
            if (thenExits && elseExits)
            {
                _nonNullSymbols.Clear();
                _nonNullSymbols.UnionWith(entryState);
            }
            else if (thenExits)
            {
                _nonNullSymbols.Clear();
                _nonNullSymbols.UnionWith(elseExitState);
            }
            else if (elseExits)
            {
                _nonNullSymbols.Clear();
                _nonNullSymbols.UnionWith(thenExitState);
            }
            else
            {
                _nonNullSymbols.Clear();
                _nonNullSymbols.UnionWith(IntersectFlowStates(thenExitState, elseExitState));
            }
        }
        else if (thenExits)
        {
            _nonNullSymbols.Clear();
            _nonNullSymbols.UnionWith(elseExitState);
        }
        else
        {
            _nonNullSymbols.Clear();
            _nonNullSymbols.UnionWith(IntersectFlowStates(entryState, thenExitState));
        }

        if (_containingSymbol is IMethodSymbol containingMethod &&
            GetReturnTargetType(containingMethod) is { } methodReturnType &&
            methodReturnType.SpecialType is not SpecialType.System_Void and not SpecialType.System_Unit &&
            elseBound is not null &&
            TryGetIgnoredValueExpression(thenBound, out var thenExpression) &&
            TryGetIgnoredValueExpression(elseBound, out var elseExpression) &&
            !HasExpressionErrors(thenExpression) &&
            !HasExpressionErrors(elseExpression) &&
            thenExpression.Type is { SpecialType: not SpecialType.System_Void and not SpecialType.System_Unit } &&
            elseExpression.Type is { SpecialType: not SpecialType.System_Void and not SpecialType.System_Unit })
        {
            // If the if/else is the last statement in a value-returning function body, the
            // implicit-return machinery (ImplicitReturnRewriter / codegen) will insert returns
            // into each branch — no warning needed in that position.
            if (!IsIfStatementImplicitReturn(ifStmt))
            {
                _diagnostics.ReportIfStatementValueIgnored(ifStmt.IfKeyword.GetLocation());
            }
        }

        return new BoundIfStatement(condition, thenBound, elseBound);
    }

    private bool IsIfStatementImplicitReturn(IfStatementSyntax ifStmt)
    {
        if (_containingSymbol is not IMethodSymbol method)
            return false;

        var returnType = GetReturnTargetType(method);
        if (returnType is null ||
            returnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit)
            return false;

        if (ifStmt.Parent is not BlockStatementSyntax block)
            return false;

        if (block.Statements.Count == 0 || block.Statements.LastOrDefault() != ifStmt)
            return false;

        return block.Parent switch
        {
            BaseMethodDeclarationSyntax => true,
            FunctionStatementSyntax => true,
            AccessorDeclarationSyntax => true,
            _ => false,
        };
    }

    private static bool IsEarlyExitStatement(StatementSyntax statement)
    {
        return statement switch
        {
            ReturnStatementSyntax => true,
            ThrowStatementSyntax => true,
            BreakStatementSyntax => true,
            ContinueStatementSyntax => true,
            BlockStatementSyntax block when block.Statements.Count == 1 => IsEarlyExitStatement(block.Statements[0]),
            _ => false
        };
    }

    private static bool TryGetIgnoredValueExpression(BoundStatement statement, out BoundExpression expression)
    {
        switch (statement)
        {
            case BoundExpressionStatement expressionStatement:
                expression = expressionStatement.Expression;
                return true;
            case BoundBlockStatement blockStatement when blockStatement.Statements.Count() == 1:
                return TryGetIgnoredValueExpression(blockStatement.Statements.First(), out expression);
            default:
                expression = null!;
                return false;
        }
    }

    private BoundStatement BindWhileStatement(WhileStatementSyntax whileStmt)
    {
        var condition = BindExpression(whileStmt.Condition);

        var body = BindStatementInLoop(whileStmt.Statement);

        return new BoundWhileStatement(condition, body);
    }

    private BoundStatement BindTryStatement(TryStatementSyntax tryStmt)
    {
        var tryBlock = BindBlockStatement(tryStmt.Block);

        var catchBuilder = ImmutableArray.CreateBuilder<BoundCatchClause>();
        foreach (var catchClause in tryStmt.CatchClauses)
        {
            catchBuilder.Add(BindCatchClause(catchClause));
        }

        BoundBlockStatement? finallyBlock = null;
        if (tryStmt.FinallyClause is { } finallyClause)
            finallyBlock = BindBlockStatement(finallyClause.Block);

        if (_containingSymbol is IMethodSymbol containingMethod &&
            GetReturnTargetType(containingMethod) is { } methodReturnType &&
            methodReturnType.SpecialType is not SpecialType.System_Void and not SpecialType.System_Unit &&
            TryGetIgnoredValueExpression(tryBlock, out var tryExpression) &&
            !HasExpressionErrors(tryExpression) &&
            tryExpression.Type is { SpecialType: not SpecialType.System_Void and not SpecialType.System_Unit } &&
            (catchBuilder.Count == 0 || AllCatchBlocksProduceIgnoredValues(catchBuilder)))
        {
            _diagnostics.ReportTryStatementValueIgnored(tryStmt.TryKeyword.GetLocation());
        }

        if (catchBuilder.Count == 0 && finallyBlock is null)
            return tryBlock;

        return new BoundTryStatement(tryBlock, catchBuilder.ToImmutable(), finallyBlock);
    }

    private bool AllCatchBlocksProduceIgnoredValues(ImmutableArray<BoundCatchClause>.Builder catchBuilder)
    {
        foreach (var catchClause in catchBuilder)
        {
            if (!TryGetIgnoredValueExpression(catchClause.Block, out var catchExpression) ||
                HasExpressionErrors(catchExpression) ||
                catchExpression.Type is not { SpecialType: not SpecialType.System_Void and not SpecialType.System_Unit })
            {
                return false;
            }
        }

        return catchBuilder.Count > 0;
    }

    public Dictionary<string, IMethodSymbol> _functions = new();

    protected static bool HaveSameSignature(IMethodSymbol first, IMethodSymbol second)
    {
        if (first.Parameters.Length != second.Parameters.Length)
            return false;

        if (first.TypeParameters.Length != second.TypeParameters.Length)
            return false;

        for (int i = 0; i < first.Parameters.Length; i++)
        {
            var firstType = first.Parameters[i].Type;
            var secondType = second.Parameters[i].Type;

            if (!AreTypesEquivalent(firstType, secondType, first, second))
                return false;
        }

        return true;

        static bool AreTypesEquivalent(ITypeSymbol firstType, ITypeSymbol secondType, IMethodSymbol firstMethod, IMethodSymbol secondMethod)
        {
            if (SymbolEqualityComparer.Default.Equals(firstType, secondType))
                return true;

            if (firstType is ITypeParameterSymbol firstParam && secondType is ITypeParameterSymbol secondParam)
            {
                if (!SymbolEqualityComparer.Default.Equals(firstParam.ContainingSymbol, firstMethod) ||
                    !SymbolEqualityComparer.Default.Equals(secondParam.ContainingSymbol, secondMethod))
                {
                    return false;
                }

                if (firstParam is SourceTypeParameterSymbol firstSource && secondParam is SourceTypeParameterSymbol secondSource)
                    return firstSource.Ordinal == secondSource.Ordinal;

                return SymbolEqualityComparer.Default.Equals(firstParam, secondParam);
            }

            if (firstType is INamedTypeSymbol firstNamed && secondType is INamedTypeSymbol secondNamed)
            {
                if (!SymbolEqualityComparer.Default.Equals(firstNamed.ConstructedFrom, secondNamed.ConstructedFrom))
                    return false;

                var firstArguments = firstNamed.TypeArguments;
                var secondArguments = secondNamed.TypeArguments;

                if (firstArguments.Length != secondArguments.Length)
                    return false;

                for (int i = 0; i < firstArguments.Length; i++)
                {
                    if (!AreTypesEquivalent(firstArguments[i], secondArguments[i], firstMethod, secondMethod))
                        return false;
                }

                return true;
            }

            if (firstType is IArrayTypeSymbol firstArray && secondType is IArrayTypeSymbol secondArray)
            {
                if (firstArray.Rank != secondArray.Rank)
                    return false;

                return AreTypesEquivalent(firstArray.ElementType, secondArray.ElementType, firstMethod, secondMethod);
            }

            return false;
        }
    }

    public virtual BoundBlockStatement BindBlockStatement(BlockStatementSyntax block)
    {
        if (TryGetCachedBoundNode(block) is BoundStatement cached)
            return (BoundBlockStatement)cached;

        _scopeDepth++;
        var depth = _scopeDepth;

        EnsureLabelsDeclared(block);

        foreach (var stmt in block.Statements)
        {
            if (stmt is FunctionStatementSyntax func)
            {
                var functionBinder = SemanticModel.GetBinder(func, this);
                if (functionBinder is FunctionBinder lfBinder)
                {
                    var symbol = lfBinder.GetMethodSymbol();
                    if (_functions.TryGetValue(symbol.Name, out var existing) && HaveSameSignature(existing, symbol))
                        _diagnostics.ReportFunctionAlreadyDefined(symbol.Name, func.Identifier.GetLocation());
                    else
                        _functions[symbol.Name] = symbol;
                }
            }
        }

        var boundStatements = new List<BoundStatement>(block.Statements.Count);
        foreach (var stmt in block.Statements)
        {
            var bound = BindStatement(stmt);
            boundStatements.Add(bound);
        }

        var localsAtDepth = _localsToDispose
            .Where(l => l.Depth == depth)
            .Select(l => l.Local)
            .ToList();

        if (localsAtDepth.Count > 0)
            _localsToDispose.RemoveAll(l => l.Depth == depth);

        var blockStmt = new BoundBlockStatement(boundStatements.ToArray(), localsAtDepth.ToImmutableArray());
        CacheBoundNode(block, blockStmt);

        ReportUnreachableStatements(block);

        ClearNonNullSymbolsAtDepth(depth);

        foreach (var name in _locals.Where(kvp => kvp.Value.Depth == depth).Select(kvp => kvp.Key).ToList())
            _locals.Remove(name);

        _scopeDepth--;
        return blockStmt;
    }

    public virtual BoundBlockExpression BindBlock(BlockSyntax block, bool allowReturn = true)
    {
        if (TryGetCachedBoundNode(block) is BoundExpression cached)
            return (BoundBlockExpression)cached;

        _scopeDepth++;
        var depth = _scopeDepth;

        if (!allowReturn)
            _expressionContextDepth++;

        EnsureLabelsDeclared(block);

        foreach (var stmt in block.Statements)
        {
            if (stmt is FunctionStatementSyntax func)
            {
                var functionBinder = SemanticModel.GetBinder(func, this);
                if (functionBinder is FunctionBinder lfBinder)
                {
                    var symbol = lfBinder.GetMethodSymbol();
                    if (_functions.TryGetValue(symbol.Name, out var existing) && HaveSameSignature(existing, symbol))
                        _diagnostics.ReportFunctionAlreadyDefined(symbol.Name, func.Identifier.GetLocation());
                    else
                        _functions[symbol.Name] = symbol;
                }
            }
        }

        var boundStatements = new List<BoundStatement>(block.Statements.Count);
        var hasDisallowedReturnInExpressionContext = false;
        foreach (var stmt in block.Statements)
        {
            BoundStatement bound;
            if (!allowReturn && stmt is ReturnStatementSyntax ret)
            {
                hasDisallowedReturnInExpressionContext = true;
                _diagnostics.ReportReturnStatementInExpression(stmt.GetLocation());
                var expr = ret.Expression is null
                    ? BoundFactory.UnitExpression()
                    : BindExpression(ret.Expression);
                bound = new BoundExpressionStatement(expr);
            }
            else
            {
                bound = BindStatement(stmt);
                if (!allowReturn && bound is BoundReturnStatement br)
                {
                    hasDisallowedReturnInExpressionContext = true;
                    _diagnostics.ReportReturnStatementInExpression(stmt.GetLocation());
                    var expr = br.Expression ?? BoundFactory.UnitExpression();
                    bound = new BoundExpressionStatement(expr);
                }
            }
            boundStatements.Add(bound);
        }

        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var localsAtDepth = _localsToDispose
            .Where(l => l.Depth == depth)
            .Select(l => l.Local)
            .ToList();

        if (localsAtDepth.Count > 0)
            _localsToDispose.RemoveAll(l => l.Depth == depth);

        var blockExpr = new BoundBlockExpression(boundStatements.ToArray(), unitType, localsAtDepth.ToImmutableArray());
        CacheBoundNode(block, blockExpr);

        if (allowReturn || !hasDisallowedReturnInExpressionContext)
            ReportUnreachableStatements(block);

        ClearNonNullSymbolsAtDepth(depth);

        foreach (var name in _locals.Where(kvp => kvp.Value.Depth == depth).Select(kvp => kvp.Key).ToList())
            _locals.Remove(name);

        _scopeDepth--;
        if (!allowReturn)
            _expressionContextDepth--;
        return blockExpr;
    }

    private void ReportUnreachableStatements(SyntaxNode block)
    {
        ControlFlowAnalysis? controlFlow = block switch
        {
            BlockStatementSyntax statementBlock => SemanticModel.AnalyzeControlFlowInternal(new ControlFlowRegion(statementBlock), statementBlock, analyzeJumpPoints: false),
            BlockSyntax expressionBlock when expressionBlock.Statements.Count > 0 => SemanticModel.AnalyzeControlFlowInternal(expressionBlock, analyzeJumpPoints: false),
            _ => null
        };

        if (controlFlow is null)
            return;

        foreach (var statement in controlFlow.UnreachableStatements)
            _diagnostics.ReportUnreachableCodeDetected(statement.GetLocation());
    }

    private BoundStatement BindForStatement(ForStatementSyntax forStmt)
    {
        var loopBinder = (BlockBinder)SemanticModel.GetBinder(forStmt, this)!;

        var collection = BindExpression(forStmt.Expression);

        var iteration = ClassifyForIteration(collection, forStmt.Expression);

        ILocalSymbol? local = null;
        if (forStmt.Identifier.Kind is not SyntaxKind.None and not SyntaxKind.UnderscoreToken)
        {
            local = loopBinder.CreateLocalSymbol(forStmt, forStmt.Identifier.ValueText, isMutable: false, iteration.ElementType);
        }

        var body = loopBinder.BindStatementInLoop(forStmt.Body);

        return new BoundForStatement(local, iteration, collection, body);
    }

    private ForIterationInfo ClassifyForIteration(BoundExpression collection, ExpressionSyntax iterationSyntax)
    {
        var collectionType = collection.Type;
        if (collectionType?.ContainsErrorType() == true)
            return ForIterationInfo.ForNonGeneric(Compilation.ErrorTypeSymbol);

        if (collection is BoundRangeExpression range)
        {
            var rangeIteration = ClassifyRangeIteration(range, iterationSyntax);
            if (rangeIteration is not null)
                return rangeIteration;
        }

        if (collectionType is IArrayTypeSymbol arrayType)
            return ForIterationInfo.ForArray(arrayType);

        if (collectionType is not null &&
            TryClassifyForEnumerator(collectionType, out var iteration))
        {
            return iteration;
        }

        var enumerableType = Compilation.GetSpecialType(SpecialType.System_Collections_IEnumerable);
        _diagnostics.ReportCannotConvertFromTypeToType(
            collectionType ?? Compilation.ErrorTypeSymbol,
            enumerableType,
            iterationSyntax.GetLocation());

        return ForIterationInfo.ForNonGeneric(Compilation.ErrorTypeSymbol);
    }

    private ForIterationInfo? ClassifyRangeIteration(BoundRangeExpression range, ExpressionSyntax iterationSyntax)
    {
        var end = range.Right;

        if (end is null)
        {
            _diagnostics.ReportRangeForLoopRequiresEnd(iterationSyntax.GetLocation());
            return ForIterationInfo.ForNonGeneric(Compilation.ErrorTypeSymbol);
        }

        var start = range.Left ?? CreateZeroIndex();

        if (start.IsFromEnd || end.IsFromEnd)
        {
            _diagnostics.ReportRangeForLoopFromEndNotSupported(iterationSyntax.GetLocation());
            return ForIterationInfo.ForNonGeneric(Compilation.ErrorTypeSymbol);
        }

        var normalizedRange = range.Left is null
            ? new BoundRangeExpression(start, end, range.Type)
            : range;

        return ForIterationInfo.ForRange(Compilation, normalizedRange);
    }

    private BoundIndexExpression CreateZeroIndex()
    {
        var intType = Compilation.GetSpecialType(SpecialType.System_Int32);
        var zero = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0, intType);
        return new BoundIndexExpression(zero, isFromEnd: false, GetIndexType());
    }

    private bool TryClassifyForEnumerator(ITypeSymbol collectionType, out ForIterationInfo iteration)
    {
        if (collectionType is INamedTypeSymbol namedType)
        {
            if ((IsGenericIEnumerableType(namedType) ||
                 namedType.SpecialType == SpecialType.System_Collections_IEnumerable) &&
                TryClassifyEnumerableInterface(namedType, out iteration))
            {
                return true;
            }

            if (TryClassifyPatternGetEnumerator(namedType, includeExtensions: false, out iteration))
                return true;

            if (TryClassifyPatternGetEnumerator(namedType, includeExtensions: true, out iteration))
                return true;

            if (TryClassifyEnumerableInterface(namedType, out iteration))
                return true;
        }

        if (collectionType is ITypeParameterSymbol typeParameter)
        {
            foreach (var constraintType in typeParameter.ConstraintTypes)
            {
                if (TryClassifyForEnumerator(constraintType, out iteration))
                    return true;
            }
        }

        iteration = ForIterationInfo.ForNonGeneric(Compilation.ErrorTypeSymbol);
        return false;
    }

    private bool TryClassifyPatternGetEnumerator(
        INamedTypeSymbol receiverType,
        bool includeExtensions,
        out ForIterationInfo iteration)
    {
        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
        ForIterationInfo? fallbackIteration = null;

        IEnumerable<IMethodSymbol> candidates = includeExtensions
            ? LookupExtensionMethods("GetEnumerator", receiverType)
            : receiverType
                .GetMembers("GetEnumerator")
                .OfType<IMethodSymbol>()
                .Where(static method => !method.IsStatic);

        foreach (var getEnumerator in candidates)
        {
            if (!IsSymbolAccessible(getEnumerator))
                continue;

            if (includeExtensions)
            {
                if (!getEnumerator.IsExtensionMethod || getEnumerator.Parameters.Length != 1)
                    continue;
            }
            else if (getEnumerator.Parameters.Length != 0)
            {
                continue;
            }

            if (!TryResolveEnumeratorMembers(getEnumerator.ReturnType, out var moveNextMethod, out var currentGetter))
                continue;

            var candidateIteration = CreatePatternIteration(getEnumerator, moveNextMethod, currentGetter);
            if (!SymbolEqualityComparer.Default.Equals(candidateIteration.ElementType, objectType))
            {
                iteration = candidateIteration;
                return true;
            }

            fallbackIteration ??= candidateIteration;
        }

        if (fallbackIteration is not null)
        {
            iteration = fallbackIteration;
            return true;
        }

        iteration = ForIterationInfo.ForNonGeneric(Compilation.ErrorTypeSymbol);
        return false;
    }

    private bool TryClassifyEnumerableInterface(INamedTypeSymbol collectionType, out ForIterationInfo iteration)
    {
        if (TryGetGenericEnumerableInterface(collectionType, out var genericEnumerable) &&
            TryResolveInterfaceEnumeratorPattern(genericEnumerable, out iteration))
        {
            return true;
        }

        var nonGenericEnumerable = Compilation.GetSpecialType(SpecialType.System_Collections_IEnumerable);
        if (nonGenericEnumerable is INamedTypeSymbol nonGenericEnumerableInterface &&
            ImplementsInterface(collectionType, nonGenericEnumerableInterface) &&
            TryResolveInterfaceEnumeratorPattern(nonGenericEnumerableInterface, out iteration))
        {
            return true;
        }

        iteration = ForIterationInfo.ForNonGeneric(Compilation.ErrorTypeSymbol);
        return false;
    }

    private bool TryResolveInterfaceEnumeratorPattern(
        INamedTypeSymbol enumerableInterface,
        out ForIterationInfo iteration)
    {
        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
        ForIterationInfo? fallbackIteration = null;

        foreach (var getEnumeratorMethod in enumerableInterface
                     .GetMembers("GetEnumerator")
                     .OfType<IMethodSymbol>()
                     .Where(static method => !method.IsStatic && method.Parameters.Length == 0))
        {
            if (!TryResolveEnumeratorMembers(getEnumeratorMethod.ReturnType, out var moveNextMethod, out var currentGetter))
                continue;

            ForIterationInfo candidateIteration;
            if (IsGenericIEnumerableType(enumerableInterface) &&
                enumerableInterface.TypeArguments.Length == 1 &&
                enumerableInterface.TypeArguments[0].TypeKind != TypeKind.Error &&
                getEnumeratorMethod.ReturnType is INamedTypeSymbol enumeratorInterface)
            {
                candidateIteration = ForIterationInfo.ForGeneric(
                    enumerableInterface,
                    enumeratorInterface,
                    getEnumeratorMethod,
                    moveNextMethod,
                    currentGetter);
            }
            else
            {
                candidateIteration = ForIterationInfo.ForNonGeneric(
                    currentGetter.ReturnType,
                    getEnumeratorMethod,
                    moveNextMethod,
                    currentGetter);
            }

            if (!SymbolEqualityComparer.Default.Equals(candidateIteration.ElementType, objectType))
            {
                iteration = candidateIteration;
                return true;
            }

            fallbackIteration ??= candidateIteration;
        }

        if (fallbackIteration is not null)
        {
            iteration = fallbackIteration;
            return true;
        }

        iteration = ForIterationInfo.ForNonGeneric(Compilation.ErrorTypeSymbol);
        return false;
    }

    private ForIterationInfo CreatePatternIteration(
        IMethodSymbol getEnumeratorMethod,
        IMethodSymbol moveNextMethod,
        IMethodSymbol currentGetter)
    {
        if (currentGetter.ReturnType.SpecialType == SpecialType.System_Object)
        {
            return ForIterationInfo.ForNonGeneric(
                currentGetter.ReturnType,
                getEnumeratorMethod,
                moveNextMethod,
                currentGetter);
        }

        return new ForIterationInfo(
            ForIterationKind.Generic,
            currentGetter.ReturnType,
            GetEnumeratorMethod: getEnumeratorMethod,
            MoveNextMethod: moveNextMethod,
            CurrentGetter: currentGetter);
    }

    private bool TryResolveEnumeratorMembers(
        ITypeSymbol enumeratorType,
        out IMethodSymbol moveNextMethod,
        out IMethodSymbol currentGetter)
    {
        if (enumeratorType is not INamedTypeSymbol namedEnumerator)
        {
            moveNextMethod = null!;
            currentGetter = null!;
            return false;
        }

        foreach (var method in namedEnumerator.GetMembers("MoveNext").OfType<IMethodSymbol>())
        {
            if (method.IsStatic || method.Parameters.Length != 0)
                continue;

            if (method.ReturnType.SpecialType != SpecialType.System_Boolean)
                continue;

            if (TryResolveEnumeratorCurrentGetter(namedEnumerator, out currentGetter))
            {
                moveNextMethod = method;
                return true;
            }
        }

        foreach (var interfaceType in namedEnumerator.AllInterfaces.OfType<INamedTypeSymbol>())
        {
            foreach (var method in interfaceType.GetMembers("MoveNext").OfType<IMethodSymbol>())
            {
                if (method.IsStatic || method.Parameters.Length != 0)
                    continue;

                if (method.ReturnType.SpecialType != SpecialType.System_Boolean)
                    continue;

                if (TryResolveEnumeratorCurrentGetter(interfaceType, out currentGetter))
                {
                    moveNextMethod = method;
                    return true;
                }
            }
        }

        moveNextMethod = null!;
        currentGetter = null!;
        return false;
    }

    private static bool TryResolveEnumeratorCurrentGetter(
        INamedTypeSymbol enumeratorType,
        out IMethodSymbol currentGetter)
    {
        IMethodSymbol? objectGetter = null;

        foreach (var property in enumeratorType.GetMembers("Current").OfType<IPropertySymbol>())
        {
            if (property.IsStatic)
                continue;

            if (property.GetMethod is not IMethodSymbol getter)
                continue;

            if (getter.IsStatic || getter.Parameters.Length != 0)
                continue;

            if (getter.ReturnType.SpecialType != SpecialType.System_Object)
            {
                currentGetter = getter;
                return true;
            }

            objectGetter ??= getter;
        }

        if (objectGetter is not null)
        {
            currentGetter = objectGetter;
            return true;
        }

        currentGetter = null!;
        return false;
    }

    private bool TryGetGenericEnumerableInterface(INamedTypeSymbol type, out INamedTypeSymbol enumerableInterface)
    {
        if (IsGenericIEnumerableType(type) &&
            type.TypeArguments.Length == 1)
        {
            enumerableInterface = type;
            return true;
        }

        foreach (var interfaceType in type.AllInterfaces.OfType<INamedTypeSymbol>())
        {
            if (IsGenericIEnumerableType(interfaceType) &&
                interfaceType.TypeArguments.Length == 1)
            {
                enumerableInterface = interfaceType;
                return true;
            }
        }

        enumerableInterface = null!;
        return false;
    }

    private static bool ImplementsInterface(INamedTypeSymbol type, INamedTypeSymbol interfaceType)
    {
        if (SymbolEqualityComparer.Default.Equals(type, interfaceType))
            return true;

        foreach (var implementedInterface in type.AllInterfaces)
        {
            if (SymbolEqualityComparer.Default.Equals(implementedInterface, interfaceType))
                return true;
        }

        return false;
    }

    private static bool IsGenericIEnumerableType(INamedTypeSymbol type)
    {
        if (type.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T)
            return true;

        var definition = type.OriginalDefinition as INamedTypeSymbol
            ?? type.ConstructedFrom as INamedTypeSymbol
            ?? type;

        return definition.MetadataName == "IEnumerable`1" &&
               IsInNamespace(definition.ContainingNamespace, "System.Collections.Generic");
    }

    private static bool IsInNamespace(INamespaceSymbol? namespaceSymbol, string qualifiedNamespace)
    {
        if (namespaceSymbol is null)
            return false;

        var remaining = qualifiedNamespace;

        while (!namespaceSymbol.IsGlobalNamespace)
        {
            var dot = remaining.LastIndexOf('.');
            var segment = dot >= 0 ? remaining[(dot + 1)..] : remaining;

            if (!string.Equals(namespaceSymbol.Name, segment, StringComparison.Ordinal))
                return false;

            if (dot < 0)
                return namespaceSymbol.ContainingNamespace?.IsGlobalNamespace ?? false;

            remaining = remaining[..dot];
            namespaceSymbol = namespaceSymbol.ContainingNamespace;
        }

        return false;
    }

    private BoundCatchClause BindCatchClause(CatchClauseSyntax catchClause)
    {
        ITypeSymbol exceptionBase = Compilation.GetSpecialType(SpecialType.System_Exception);
        var exceptionType = exceptionBase;

        SourceLocalSymbol? localSymbol = null;

        if (catchClause.Declaration is { } declaration)
        {
            var declaredType = ResolveTypeSyntaxOrError(declaration.Type);
            exceptionType = declaredType;

            if (exceptionBase.TypeKind != TypeKind.Error &&
                declaredType.TypeKind != TypeKind.Error &&
                !IsAssignable(exceptionBase, declaredType, out _))
            {
                _diagnostics.ReportCatchTypeMustDeriveFromSystemException(
                    declaredType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    declaration.Type.GetLocation());
            }

            if (declaration.Identifier is { } identifier &&
                !identifier.IsMissing &&
                identifier.Kind == SyntaxKind.IdentifierToken)
            {
                var name = identifier.Text;
                var isShadowingExistingInScope = false;

                if (_locals.TryGetValue(name, out var existing) && existing.Depth == _scopeDepth)
                {
                    var isSameDeclarator = existing.Symbol.DeclaringSyntaxReferences.Any(reference =>
                        reference.SyntaxTree == declaration.SyntaxTree &&
                        reference.Span == declaration.Span);

                    if (isSameDeclarator)
                    {
                        localSymbol = existing.Symbol as SourceLocalSymbol;
                    }
                    else
                    {
                        isShadowingExistingInScope = true;
                    }
                }

                if (!isShadowingExistingInScope && LookupSymbol(name) is ILocalSymbol or IParameterSymbol or IFieldSymbol)
                    isShadowingExistingInScope = true;

                if (isShadowingExistingInScope)
                    _diagnostics.ReportVariableShadowsPreviousDeclaration(name, identifier.GetLocation());

                localSymbol ??= CreateLocalSymbol(declaration, name, isMutable: false, declaredType);
            }
        }

        var block = BindBlockStatement(catchClause.Block);

        if (localSymbol is not null)
            _locals.Remove(localSymbol.Name);

        return new BoundCatchClause(exceptionType, localSymbol, block);
    }

    private BoundStatement ExpressionToStatement(BoundExpression expression)
    {
        return expression switch
        {
            BoundIfExpression ifExpr => new BoundIfStatement(
                ifExpr.Condition,
                ExpressionToStatement(ifExpr.ThenBranch),
                ifExpr.ElseBranch is not null ? ExpressionToStatement(ifExpr.ElseBranch) : null),
            BoundBlockExpression blockExpr => new BoundBlockStatement(blockExpr.Statements, blockExpr.LocalsToDispose),
            BoundAssignmentExpression assignmentExpr => new BoundAssignmentStatement(assignmentExpr),
            _ => new BoundExpressionStatement(expression),
        };
    }

    private BoundStatement BindReturnStatement(ReturnStatementSyntax returnStatement)
    {
        if (returnStatement.Expression is null && IsSynthesizedTopLevelEntryPointContext())
        {
            _diagnostics.ReportExpressionExpected(returnStatement.ReturnKeyword.GetLocation());
        }

        var expr = BindReturnValue(returnStatement.Expression, returnStatement);

        return new BoundReturnStatement(expr);
    }

    private BoundExpression? BindReturnValue(ExpressionSyntax? expressionSyntax, SyntaxNode returnSyntax)
    {
        BoundExpression? expr = null;

        if (expressionSyntax is not null)
        {
            ITypeSymbol? targetType = null;

            if (_containingSymbol is IMethodSymbol targetMethod)
            {
                targetType = GetReturnTargetType(targetMethod);
            }
            else if (_containingSymbol is ILambdaSymbol targetLambda)
            {
                // Lambdas also support target-typed returns (e.g. `return .None`).
                // Use the effective return type (async unwrapped if needed).
                targetType = GetReturnTargetType(targetLambda);
            }

            // Return payloads are context-sensitive; ensure stale non-target-typed cache entries
            // do not block target-typed member bindings like `.Error(...)`.
            RemoveCachedBoundNode(expressionSyntax);
            expr = BindExpressionWithTargetType(expressionSyntax, targetType, allowReturn: false);
        }

        if (_containingSymbol is IMethodSymbol method)
        {
            var skipReturnConversions = method switch
            {
                SourceMethodSymbol { HasAsyncReturnTypeError: true } => true,
                SourceMethodSymbol { ShouldDeferAsyncReturnDiagnostics: true } => true,
                SourceLambdaSymbol { HasAsyncReturnTypeError: true } => true,
                _ => false,
            };

            if (!skipReturnConversions)
            {
                var methodReturnType = method.ReturnType;
                if (methodReturnType is null)
                    return expr;

                if (expr is null)
                {
                    var unit = Compilation.GetSpecialType(SpecialType.System_Unit);
                    if (!IsAssignable(methodReturnType, unit, out _))
                        _diagnostics.ReportCannotConvertFromTypeToType(
                            unit.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            methodReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            returnSyntax.GetLocation());
                }
                else if (method.IsAsync &&
                    AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, methodReturnType) is
                        { SpecialType: SpecialType.System_Unit or SpecialType.System_Void })
                {
                    var methodDisplay = method.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    _diagnostics.ReportAsyncTaskReturnCannotHaveExpression(
                        methodDisplay,
                        expressionSyntax!.GetLocation());
                }
                else
                {
                    var targetType = methodReturnType;

                    if (method.IsAsync &&
                        methodReturnType.TypeKind != TypeKind.Error &&
                        AsyncReturnTypeUtilities.ExtractAsyncResultType(Compilation, methodReturnType) is { } resultType)
                    {
                        targetType = resultType;
                    }

                    if (ShouldAttemptConversion(expr) && targetType.TypeKind != TypeKind.Error)
                    {
                        expr = BindLambdaToDelegateIfNeeded(expr, targetType);

                        if (!IsAssignable(targetType, expr.Type, out var conversion))
                        {
                            _diagnostics.ReportCannotConvertFromTypeToType(
                                expr.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                                targetType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                                expressionSyntax!.GetLocation());
                        }
                        else
                        {
                            expr = ApplyConversion(expr, targetType, conversion, expressionSyntax!);
                        }
                    }
                }
            }

            if (expr is not null)
                expr = ValidateByRefReturnExpression(
                    method,
                    expr,
                    expressionSyntax as SyntaxNode ?? returnSyntax);
        }

        return expr;
    }

    protected BoundExpression ValidateByRefReturnExpression(
        IMethodSymbol method,
        BoundExpression expression,
        SyntaxNode syntax)
    {
        if (method.ReturnType is not RefTypeSymbol refTypeReturnType)
            return expression;

        if (!TryGetAddressOfStorage(expression, out var storage))
            return expression;

        if (storage is BoundLocalAccess localAccess)
            return ReportInvalidByRefReturnStorage(
                localAccess.Local.Name,
                refTypeReturnType,
                syntax,
                isValueParameter: false);

        if (storage is BoundParameterAccess parameterAccess && parameterAccess.Parameter.RefKind == RefKind.None)
            return ReportInvalidByRefReturnStorage(
                parameterAccess.Parameter.Name,
                refTypeReturnType,
                syntax,
                isValueParameter: true);

        return expression;
    }

    private BoundExpression ReportInvalidByRefReturnStorage(
        string sourceName,
        RefTypeSymbol targetType,
        SyntaxNode syntax,
        bool isValueParameter)
    {
        if (isValueParameter)
            _diagnostics.ReportByRefReturnCannotReferenceValueParameter(sourceName, syntax.GetLocation());
        else
            _diagnostics.ReportByRefReturnCannotReferenceLocal(sourceName, syntax.GetLocation());

        return new BoundErrorExpression(targetType, null, BoundExpressionReason.TypeMismatch);
    }

    private static bool TryGetAddressOfStorage(BoundExpression expression, out BoundExpression storage)
    {
        var current = expression;
        while (current is BoundConversionExpression conversion)
            current = conversion.Expression;

        if (current is BoundAddressOfExpression { Storage: BoundExpression storageExpression })
        {
            storage = storageExpression;
            return true;
        }

        storage = null!;
        return false;
    }

    private bool IsSynthesizedTopLevelEntryPointContext()
    {
        return _containingSymbol is SynthesizedMainMethodSymbol or SynthesizedMainAsyncMethodSymbol;
    }

    private BoundStatement BindYieldReturnStatement(YieldReturnStatementSyntax yieldReturn)
    {
        if (_expressionContextDepth > 0)
        {
            var exprInExpressionContext = BindExpression(yieldReturn.Expression);
            return new BoundExpressionStatement(exprInExpressionContext);
        }

        var expression = BindExpression(yieldReturn.Expression);
        var (kind, elementType) = ResolveIteratorInfoForCurrentMethod();

        if (elementType.TypeKind == TypeKind.Error)
            elementType = Compilation.ErrorTypeSymbol;

        if (ShouldAttemptConversion(expression) &&
            expression.Type is { TypeKind: not TypeKind.Error } expressionType &&
            elementType.TypeKind != TypeKind.Error &&
            IsAssignable(elementType, expressionType, out var conversion))
        {
            expression = ApplyConversion(expression, elementType, conversion, yieldReturn.Expression);
        }

        return new BoundYieldReturnStatement(expression, elementType, kind);
    }

    private BoundStatement BindYieldBreakStatement(YieldBreakStatementSyntax yieldBreak)
    {
        if (_expressionContextDepth > 0)
        {
            var unit = BoundFactory.UnitExpression();
            return new BoundExpressionStatement(unit);
        }

        var (kind, elementType) = ResolveIteratorInfoForCurrentMethod();
        if (elementType.TypeKind == TypeKind.Error)
            elementType = Compilation.ErrorTypeSymbol;

        return new BoundYieldBreakStatement(elementType, kind);
    }

    private BoundStatement BindThrowStatement(ThrowStatementSyntax throwStatement)
    {
        var location = throwStatement.ThrowKeyword.GetLocation();

        if (_expressionContextDepth > 0)
            _diagnostics.ReportThrowStatementInExpression(location);

        var exceptionBase = Compilation.GetTypeByMetadataName("System.Exception")
            ?? Compilation.ErrorTypeSymbol;
        var expression = BindThrowValueExpression(BindExpression(throwStatement.Expression), throwStatement.Expression, exceptionBase);

        return new BoundThrowStatement(expression);
    }

    private BoundExpression BindThrowValueExpression(
        BoundExpression expression,
        ExpressionSyntax expressionSyntax,
        ITypeSymbol exceptionBase)
    {
        if (ShouldAttemptConversion(expression) &&
            expression.Type is { TypeKind: not TypeKind.Error } expressionType &&
            exceptionBase.TypeKind != TypeKind.Error)
        {
            if (!IsAssignable(exceptionBase, expressionType, out var conversion))
            {
                _diagnostics.ReportThrowExpressionMustBeException(
                    expressionType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    expressionSyntax.GetLocation());
            }
            else
            {
                expression = ApplyConversion(expression, exceptionBase, conversion, expressionSyntax);
            }
        }

        return expression;
    }

    private BoundStatement BindLabeledStatement(LabeledStatementSyntax labeledStatement)
    {
        if (_expressionContextDepth > 0)
            _diagnostics.ReportLabelInExpression(labeledStatement.Identifier.GetLocation());

        var labelSymbol = DeclareLabelSymbol(labeledStatement);
        var boundStatement = BindStatement(labeledStatement.Statement);
        var bound = new BoundLabeledStatement(labelSymbol, boundStatement);
        CacheBoundNode(labeledStatement, bound);
        return bound;
    }

    private BoundStatement BindGotoStatement(GotoStatementSyntax gotoStatement)
    {
        if (_expressionContextDepth > 0)
            _diagnostics.ReportGotoStatementInExpression(gotoStatement.GotoKeyword.GetLocation());

        var identifier = gotoStatement.Identifier;
        if (identifier.IsMissing)
        {
            var errorSymbol = CreateLabelSymbol(string.Empty, identifier.GetLocation(), gotoStatement.GetReference());
            var boundError = new BoundGotoStatement(errorSymbol);
            CacheBoundNode(gotoStatement, boundError);
            return boundError;
        }

        if (SyntaxFacts.IsReservedWordKind(identifier.Kind))
        {
            var identifierName = identifier.ValueText;
            _diagnostics.ReportReservedWordCannotBeLabel(identifierName, identifier.GetLocation());
            var errorSymbol = CreateLabelSymbol(string.Empty, identifier.GetLocation(), gotoStatement.GetReference());
            var boundError = new BoundGotoStatement(errorSymbol);
            CacheBoundNode(gotoStatement, boundError);
            return boundError;
        }

        var name = identifier.ValueText;
        if (!_labelsByName.TryGetValue(name, out var label))
        {
            _diagnostics.ReportLabelNotFound(name, identifier.GetLocation());
            label = CreateLabelSymbol(name, identifier.GetLocation(), gotoStatement.GetReference());
        }

        var isBackward = false;
        if (_syntaxByLabel.TryGetValue(label, out var labeledSyntax))
        {
            isBackward = labeledSyntax.Span.Start < gotoStatement.Span.Start;
            if (DoesGotoExitUseScope(gotoStatement, labeledSyntax))
                _diagnostics.ReportGotoCannotExitUseScope(identifier.GetLocation());
        }

        SemanticModel?.RegisterGoto(gotoStatement, label);

        var bound = new BoundGotoStatement(label, isBackward);
        CacheBoundNode(gotoStatement, bound);
        return bound;
    }

    private static bool DoesGotoExitUseScope(GotoStatementSyntax gotoStatement, LabeledStatementSyntax targetLabel)
    {
        var targetBlocks = targetLabel.AncestorsAndSelf().OfType<BlockStatementSyntax>().ToHashSet();

        foreach (var sourceBlock in gotoStatement.AncestorsAndSelf().OfType<BlockStatementSyntax>())
        {
            if (targetBlocks.Contains(sourceBlock))
                break;

            if (HasActiveUseDeclarationBeforeJump(sourceBlock, gotoStatement))
                return true;
        }

        return false;
    }

    private static bool HasActiveUseDeclarationBeforeJump(BlockStatementSyntax block, GotoStatementSyntax gotoStatement)
    {
        var containingStatement = gotoStatement.AncestorsAndSelf()
            .OfType<StatementSyntax>()
            .FirstOrDefault(statement => ReferenceEquals(statement.Parent, block));

        if (containingStatement is null)
            return false;

        foreach (var statement in block.Statements)
        {
            if (ReferenceEquals(statement, containingStatement))
                break;

            if (statement is UseDeclarationStatementSyntax)
                return true;
        }

        return false;
    }

    private BoundStatement BindBreakStatement(BreakStatementSyntax breakStatement)
    {
        var location = breakStatement.BreakKeyword.GetLocation();

        if (_expressionContextDepth > 0)
        {
            _diagnostics.ReportBreakStatementInExpression(location);
        }
        else if (_loopDepth == 0)
        {
            _diagnostics.ReportBreakStatementNotWithinLoop(location);
        }

        var bound = new BoundBreakStatement();
        CacheBoundNode(breakStatement, bound);
        return bound;
    }

    private BoundStatement BindContinueStatement(ContinueStatementSyntax continueStatement)
    {
        var location = continueStatement.ContinueKeyword.GetLocation();

        if (_expressionContextDepth > 0)
        {
            _diagnostics.ReportContinueStatementInExpression(location);
        }
        else if (_loopDepth == 0)
        {
            _diagnostics.ReportContinueStatementNotWithinLoop(location);
        }

        var bound = new BoundContinueStatement();
        CacheBoundNode(continueStatement, bound);
        return bound;
    }

    public BoundStatement BindStatementInLoop(StatementSyntax syntax)
    {
        var previous = EnterLoop();
        try
        {
            return BindStatement(syntax);
        }
        finally
        {
            ExitLoop(previous);
        }
    }

    private int EnterLoop()
    {
        var previous = _loopDepth;
        _loopDepth++;
        return previous;
    }

    private void ExitLoop(int previous)
    {
        _loopDepth = previous;
    }

    private void EnsureLabelsDeclared(SyntaxNode node)
    {
        if (!_labelDeclarationNodes.Add(node))
            return;

        var stack = new Stack<SyntaxNode>();
        stack.Push(node);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            if (current is FunctionStatementSyntax)
                continue;

            if (current is LabeledStatementSyntax labeled)
            {
                _ = DeclareLabelSymbol(labeled);
                stack.Push(labeled.Statement);
                continue;
            }

            foreach (var child in current.ChildNodes())
                stack.Push(child);
        }
    }

    private ILabelSymbol DeclareLabelSymbol(LabeledStatementSyntax labeledStatement)
    {
        if (_labelsBySyntax.TryGetValue(labeledStatement, out var existing))
            return existing;

        var identifier = labeledStatement.Identifier;
        if (identifier.IsMissing)
            return CreateLabelSymbol(string.Empty, identifier.GetLocation(), labeledStatement.GetReference());

        if (SyntaxFacts.IsReservedWordKind(identifier.Kind))
        {
            var identifierName = identifier.ValueText;
            _diagnostics.ReportReservedWordCannotBeLabel(identifierName, identifier.GetLocation());
            return CreateLabelSymbol(string.Empty, identifier.GetLocation(), labeledStatement.GetReference());
        }

        var name = identifier.ValueText;

        if (_labelsByName.TryGetValue(name, out var conflict))
        {
            _diagnostics.ReportLabelAlreadyDefined(name, identifier.GetLocation());
            return conflict;
        }

        var symbol = CreateLabelSymbol(name, identifier.GetLocation(), labeledStatement.GetReference());

        _labelsByName[name] = symbol;
        _labelsBySyntax[labeledStatement] = symbol;
        _syntaxByLabel[symbol] = labeledStatement;
        SemanticModel?.RegisterLabel(labeledStatement, symbol);

        return symbol;
    }

    private INamedTypeSymbol GetGenericEnumeratorDefinition()
        => (INamedTypeSymbol)Compilation.GetSpecialType(
            SpecialType.System_Collections_Generic_IEnumerator_T);

    private INamedTypeSymbol GetNonGenericEnumeratorDefinition()
        => (INamedTypeSymbol)Compilation.GetSpecialType(
            SpecialType.System_Collections_IEnumerator);

    private (IteratorMethodKind Kind, ITypeSymbol ElementType) ResolveIteratorInfoForCurrentMethod()
    {
        if (_containingSymbol is not IMethodSymbol method)
            return (IteratorMethodKind.None, Compilation.ErrorTypeSymbol);

        var result = ResolveIteratorInfo(method);

        if (result.Kind != IteratorMethodKind.None && _containingSymbol is SourceMethodSymbol sourceMethod)
            sourceMethod.MarkIterator(result.Kind, result.ElementType);

        return result;
    }

    private (IteratorMethodKind Kind, ITypeSymbol ElementType) ResolveIteratorInfo(IMethodSymbol method)
    {
        var errorType = Compilation.ErrorTypeSymbol;
        var returnType = method.ReturnType;

        if (returnType.SpecialType == SpecialType.System_Collections_IEnumerable)
        {
            var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
            return (IteratorMethodKind.Enumerable, objectType);
        }

        if (returnType.SpecialType == SpecialType.System_Collections_IEnumerator)
        {
            var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
            return (IteratorMethodKind.Enumerator, objectType);
        }

        if (returnType is INamedTypeSymbol named)
        {
            var definition = named;
            if (named.ConstructedFrom is INamedTypeSymbol constructedFrom)
                definition = constructedFrom;

            if (definition.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T)
            {
                var elementType = named.TypeArguments.Length > 0 ? named.TypeArguments[0] : errorType;
                return (IteratorMethodKind.Enumerable, elementType);
            }

            if (definition.SpecialType == SpecialType.System_Collections_Generic_IEnumerator_T)
            {
                var elementType = named.TypeArguments.Length > 0 ? named.TypeArguments[0] : errorType;
                return (IteratorMethodKind.Enumerator, elementType);
            }
        }

        return (IteratorMethodKind.None, errorType);
    }

    private ILabelSymbol CreateLabelSymbol(string name, Location location, SyntaxReference reference)
    {
        return new LabelSymbol(
            name,
            _containingSymbol,
            _containingSymbol.ContainingType as INamedTypeSymbol,
            _containingSymbol?.ContainingNamespace,
            [location],
            [reference]);
    }

    // Helper for lambda return type target-typing (mirrors method rules).
    private static ITypeSymbol GetReturnTargetType(ILambdaSymbol lambda)
    {
        // Mirror method-return target type rules.
        var returnType = lambda.ReturnType;

        if (returnType is ErrorTypeSymbol)
            return returnType;

        if (lambda.IsAsync &&
            returnType is INamedTypeSymbol namedReturn &&
            namedReturn.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            namedReturn.TypeArguments.Length == 1 &&
            namedReturn.TypeArguments[0] is { } resultType)
        {
            return resultType;
        }

        return returnType;
    }
}
