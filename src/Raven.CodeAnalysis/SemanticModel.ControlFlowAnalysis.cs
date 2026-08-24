using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public ControlFlowAnalysis AnalyzeControlFlow(StatementSyntax statement)
    {
        ValidateSyntaxNode(statement, nameof(statement));

        using var semanticAccess = EnterSemanticAccess(CancellationToken.None);

        EnsureControlFlowBindingReady(statement);

        var region = new ControlFlowRegion(statement);
        return AnalyzeControlFlowInternal(region, statement);
    }

    public ControlFlowAnalysis AnalyzeControlFlow(StatementSyntax firstStatement, StatementSyntax lastStatement)
    {
        ValidateSyntaxNode(firstStatement, nameof(firstStatement));
        ValidateSyntaxNode(lastStatement, nameof(lastStatement));

        using var semanticAccess = EnterSemanticAccess(CancellationToken.None);

        if (!IsValidControlFlowRegion(firstStatement, lastStatement))
            return new ControlFlowAnalysis { Succeeded = false };

        var region = new ControlFlowRegion(firstStatement, lastStatement);
        EnsureControlFlowBindingReady(region.EnclosingBlock ?? firstStatement);
        return AnalyzeControlFlowInternal(region, region.EnclosingBlock ?? firstStatement);
    }

    private static bool IsValidControlFlowRegion(StatementSyntax firstStatement, StatementSyntax lastStatement)
    {
        if (!ReferenceEquals(firstStatement.Parent, lastStatement.Parent))
            return false;

        SyntaxList<StatementSyntax> statements = firstStatement.Parent switch
        {
            BlockStatementSyntax block => block.Statements,
            BlockSyntax blockExpression => blockExpression.Statements,
            _ => default
        };

        var startIndex = statements.IndexOf(firstStatement);
        var endIndex = statements.IndexOf(lastStatement);
        return startIndex >= 0 && endIndex >= startIndex;
    }

    internal ControlFlowAnalysis AnalyzeControlFlowInternal(ControlFlowRegion region, StatementSyntax statement, bool analyzeJumpPoints = true)
    {
        var walker = new ControlFlowWalker(this, region, analyzeJumpPoints);
        walker.Analyze(statement);

        return walker.ToResult();
    }

    internal ControlFlowAnalysis AnalyzeControlFlowInternal(BlockSyntax block, bool analyzeJumpPoints = true)
    {
        if (block.Statements.Count == 0)
        {
            return new ControlFlowAnalysis
            {
                StartPointIsReachable = true,
                EndPointIsReachable = true,
                ReturnStatements = ImmutableArray<SyntaxNode>.Empty,
                EntryPoints = ImmutableArray<SyntaxNode>.Empty,
                ExitPoints = ImmutableArray<SyntaxNode>.Empty,
                UnreachableStatements = ImmutableArray<StatementSyntax>.Empty,
                Succeeded = true
            };
        }

        var first = block.Statements[0];
        var last = block.Statements[^1];
        var region = new ControlFlowRegion(first, last);

        var walker = new ControlFlowWalker(this, region, analyzeJumpPoints);
        walker.Analyze(block);

        return walker.ToResult();
    }
}

public sealed class ControlFlowRegion
{
    public StatementSyntax? FirstStatement { get; }
    public StatementSyntax? LastStatement { get; }
    public BlockStatementSyntax? EnclosingBlock { get; }

    private readonly HashSet<SyntaxNode> _containedNodes = new();

    // For a single statement region
    public ControlFlowRegion(StatementSyntax singleStatement)
    {
        FirstStatement = LastStatement = singleStatement;
        EnclosingBlock = singleStatement.Parent as BlockStatementSyntax;

        CollectContainedNodes(singleStatement);
    }

    // For a span of statements within the same block
    public ControlFlowRegion(StatementSyntax first, StatementSyntax last)
    {
        if (first.Parent != last.Parent)
            throw new ArgumentException("Region must be a contiguous set of statements in the same block.");

        FirstStatement = first;
        LastStatement = last;
        EnclosingBlock = first.Parent as BlockStatementSyntax;

        SyntaxList<StatementSyntax> statements = first.Parent switch
        {
            BlockStatementSyntax block => block.Statements,
            BlockSyntax blockExpr => blockExpr.Statements,
            _ => throw new ArgumentException("Region must be a contiguous set of statements in the same block.")
        };

        var startIndex = statements.IndexOf(first);
        var endIndex = statements.IndexOf(last);
        if (startIndex < 0 || endIndex < startIndex)
            throw new ArgumentException("Region statements must occur in source order in the same block.");

        var found = false;
        foreach (var stmt in statements)
        {
            if (stmt == first) found = true;
            if (found)
            {
                CollectContainedNodes(stmt);
                if (stmt == last) break;
            }
        }
    }

    private void CollectContainedNodes(SyntaxNode node)
    {
        _containedNodes.Add(node);
        foreach (var child in node.ChildNodes())
            CollectContainedNodes(child);
    }

    public bool Contains(SyntaxNode node) => _containedNodes.Contains(node);
}

internal sealed partial class ControlFlowWalker : SyntaxWalker
{
    private readonly SemanticModel _semanticModel;
    private readonly ControlFlowRegion? _region;
    private readonly bool _analyzeJumpPoints;
    private readonly List<SyntaxNode> _returnStatements = new();
    private readonly List<StatementSyntax> _unreachableStatements = new();
    private readonly List<SyntaxNode> _entryPoints = new();
    private readonly List<SyntaxNode> _exitPoints = new();
    private bool _endPointIsReachable = true;
    private readonly Stack<LoopContext> _loopContexts = new();

    public ControlFlowWalker(SemanticModel semanticModel, ControlFlowRegion? region = null, bool analyzeJumpPoints = true)
    {
        _semanticModel = semanticModel;
        _region = region;
        _analyzeJumpPoints = analyzeJumpPoints;
    }

    public void Analyze(StatementSyntax statement)
    {
        _endPointIsReachable = AnalyzeStatement(statement, isReachable: true);
    }

    public void Analyze(BlockSyntax block)
    {
        _endPointIsReachable = AnalyzeBlockStatements(block.Statements, isReachable: true);
    }

    private bool AnalyzeStatement(StatementSyntax statement, bool isReachable)
    {
        if (!isReachable)
        {
            MarkUnreachable(statement);
            return false;
        }

        switch (statement)
        {
            case BlockStatementSyntax block:
                return AnalyzeBlock(block, isReachable);
            case IfStatementSyntax ifStatement:
                var beforeIf = AnalyzeRequiredExpression(ifStatement.Condition, isReachable);

                var thenReachable = AnalyzeStatement(ifStatement.ThenStatement, beforeIf);

                var elseReachable = beforeIf;
                if (ifStatement.ElseClause is { } elseClause)
                    elseReachable = AnalyzeStatement(elseClause.Statement, beforeIf);

                _endPointIsReachable = thenReachable || elseReachable;
                return _endPointIsReachable;
            case IfPatternStatementSyntax ifPatternStatement:
                beforeIf = AnalyzeRequiredExpression(ifPatternStatement.Expression, isReachable);

                thenReachable = AnalyzeStatement(ifPatternStatement.ThenStatement, beforeIf);

                elseReachable = beforeIf;
                if (ifPatternStatement.ElseClause is { } ifPatternElseClause)
                    elseReachable = AnalyzeStatement(ifPatternElseClause.Statement, beforeIf);

                _endPointIsReachable = thenReachable || elseReachable;
                return _endPointIsReachable;
            case WhileStatementSyntax whileStatement:
                var whileConditionReachable = AnalyzeRequiredExpression(whileStatement.Condition, isReachable);
                var whileHasReachableBreak = AnalyzeLoopBody(whileStatement, whileStatement.Statement, whileConditionReachable);
                if (!whileConditionReachable)
                {
                    _endPointIsReachable = false;
                    return false;
                }

                _endPointIsReachable = IsConstantTrue(whileStatement.Condition)
                    ? isReachable && whileHasReachableBreak
                    : isReachable;
                return _endPointIsReachable;
            case WhilePatternStatementSyntax whilePatternStatement:
                var whilePatternExpressionReachable = AnalyzeRequiredExpression(whilePatternStatement.Expression, isReachable);
                AnalyzeLoopBody(whilePatternStatement, whilePatternStatement.Statement, whilePatternExpressionReachable);
                _endPointIsReachable = whilePatternExpressionReachable;
                return _endPointIsReachable;
            case ForStatementSyntax forStatement:
                var forExpressionReachable = AnalyzeRequiredExpression(forStatement.Expression, isReachable);
                AnalyzeLoopBody(forStatement, forStatement.Body, forExpressionReachable);
                _endPointIsReachable = forExpressionReachable;
                return _endPointIsReachable;
            case LoopStatementSyntax loopStatement:
                var hasReachableBreak = AnalyzeLoopBody(loopStatement, loopStatement.Statement, isReachable);
                _endPointIsReachable = isReachable && hasReachableBreak;
                return _endPointIsReachable;
            case MatchStatementSyntax matchStatement:
                return AnalyzeMatchStatement(matchStatement, isReachable);
            case LockStatementSyntax lockStatement:
                var lockExpressionReachable = AnalyzeRequiredExpression(lockStatement.Expression, isReachable);
                return AnalyzeStatement(lockStatement.Statement, lockExpressionReachable);
            case UnsafeStatementSyntax unsafeStatement:
                return AnalyzeStatement(unsafeStatement.Block, isReachable);
            case TryStatementSyntax tryStatement:
                return AnalyzeTryStatement(tryStatement, isReachable);
            case UseDeclarationStatementSyntax { InBlockClause.Block: { } inBlock } useDeclaration:
                Visit(useDeclaration.Declaration);
                _endPointIsReachable = AnalyzeStatement(inBlock, isReachable);
                return _endPointIsReachable;
            case LabeledStatementSyntax labeledStatement:
                VisitLabeledStatement(labeledStatement);
                _endPointIsReachable = AnalyzeStatement(labeledStatement.Statement, isReachable);
                return _endPointIsReachable;
            case GotoStatementSyntax gotoStatement:
                VisitGotoStatement(gotoStatement);
                _endPointIsReachable = false;
                return false;
            case BreakStatementSyntax breakStatement:
                base.VisitBreakStatement(breakStatement);
                MarkReachableBreak(breakStatement.Identifier, breakStatement);
                _endPointIsReachable = false;
                return false;
            case ContinueStatementSyntax continueStatement:
                base.VisitContinueStatement(continueStatement);
                _endPointIsReachable = false;
                return false;
            case ReturnStatementSyntax returnStatement:
                VisitReturnStatement(returnStatement);
                _returnStatements.Add(returnStatement);
                _endPointIsReachable = false;
                return false;
            case MacroExpansionStatementSyntax { Keyword.ValueText: "expand" } expandStatement:
                Visit(expandStatement.Expression);
                _returnStatements.Add(expandStatement);
                _endPointIsReachable = false;
                return false;
            case ThrowStatementSyntax throwStatement:
                base.VisitThrowStatement(throwStatement);
                _endPointIsReachable = false;
                return false;
            case ExpressionStatementSyntax expressionStatement:
                base.VisitExpressionStatement(expressionStatement);
                CollectReturnExpressions(expressionStatement.Expression);

                var boundExpression = _semanticModel.TryGetCachedBoundNode(expressionStatement.Expression) as BoundExpression;
                _endPointIsReachable = boundExpression is null || !BoundNodeFacts.IsAbruptExpression(boundExpression);
                return _endPointIsReachable;
            case LocalDeclarationStatementSyntax localDeclaration:
                base.VisitLocalDeclarationStatement(localDeclaration);
                CollectReturnExpressions(localDeclaration.Declaration);
                var boundDeclaration = _semanticModel.TryGetCachedBoundNode(localDeclaration) as BoundLocalDeclarationStatement;
                _endPointIsReachable = boundDeclaration is null ||
                    !boundDeclaration.Declarators.Any(static declarator =>
                        declarator.Initializer is not null && BoundNodeFacts.IsAbruptExpression(declarator.Initializer));
                return _endPointIsReachable;
            default:
                base.Visit(statement);
                _endPointIsReachable = true;
                return true;
        }
    }

    private void CollectReturnExpressions(SyntaxNode node)
    {
        if (node is ReturnExpressionSyntax returnExpression)
            _returnStatements.Add(returnExpression);
        else if (node is MacroExpansionExpressionSyntax { Keyword.ValueText: "expand" } expandExpression)
            _returnStatements.Add(expandExpression);
        else if (node is BreakExpressionSyntax breakExpression)
            MarkReachableBreak(breakExpression.Identifier, breakExpression);

        foreach (var child in node.ChildNodes())
        {
            if (child is FunctionExpressionSyntax or FunctionStatementSyntax)
                continue;

            CollectReturnExpressions(child);
        }
    }

    private bool AnalyzeRequiredExpression(
        ExpressionSyntax expression,
        bool isReachable,
        BoundExpression? knownBoundExpression = null)
    {
        Visit(expression);
        CollectReturnExpressions(expression);

        if (!isReachable)
            return false;

        var boundExpression = knownBoundExpression ??
            _semanticModel.TryGetCachedBoundNode(expression) as BoundExpression ??
            _semanticModel.TryGetCachedBoundNode(
                expression,
                _semanticModel.Compilation.GetSpecialType(SpecialType.System_Boolean)) as BoundExpression ??
            _semanticModel.GetBoundNode(expression);
        return !BoundNodeFacts.IsAbruptExpression(boundExpression);
    }

    private bool AnalyzeBlock(BlockStatementSyntax block, bool isReachable)
        => AnalyzeBlockStatements(block.Statements, isReachable);

    private bool AnalyzeBlockStatements(IEnumerable<StatementSyntax> statements, bool isReachable)
    {
        var currentReachable = isReachable;

        foreach (var statement in statements)
            currentReachable = AnalyzeStatement(statement, currentReachable);

        _endPointIsReachable = currentReachable;
        return currentReachable;
    }

    private bool AnalyzeLoopBody(StatementSyntax loop, StatementSyntax body, bool isReachable)
    {
        var context = new LoopContext(loop);
        _loopContexts.Push(context);

        try
        {
            _ = AnalyzeStatement(body, isReachable);
            return context.HasReachableBreak;
        }
        finally
        {
            _loopContexts.Pop();
        }
    }

    private void MarkReachableBreak(SyntaxToken identifier, SyntaxNode transfer)
    {
        if (_loopContexts.Count == 0)
            return;

        if (identifier.IsMissing || identifier.Kind == SyntaxKind.None)
        {
            _loopContexts.Peek().HasReachableBreak = true;
            return;
        }

        var labeledLoop = transfer.Ancestors().OfType<LabeledStatementSyntax>()
            .FirstOrDefault(label => label.Identifier.ValueText == identifier.ValueText);
        if (labeledLoop is null)
            return;

        var target = UnwrapLabeledStatement(labeledLoop.Statement);
        foreach (var context in _loopContexts)
        {
            if (ReferenceEquals(context.Loop, target))
            {
                context.HasReachableBreak = true;
                return;
            }
        }
    }

    private static StatementSyntax UnwrapLabeledStatement(StatementSyntax statement)
    {
        while (statement is LabeledStatementSyntax labeled)
            statement = labeled.Statement;
        return statement;
    }

    private static bool IsConstantTrue(ExpressionSyntax expression)
        => ConstantValueEvaluator.TryEvaluate(expression, out var value) && value is true;

    private bool AnalyzeMatchStatement(MatchStatementSyntax matchStatement, bool isReachable)
    {
        if (_semanticModel.TryGetCachedBoundNode(matchStatement) is not BoundMatchStatement boundMatch ||
            boundMatch.Arms.IsDefaultOrEmpty ||
            boundMatch.Arms.Length != matchStatement.Arms.Count)
        {
            base.VisitMatchStatement(matchStatement);
            _endPointIsReachable = isReachable;
            return isReachable;
        }

        var scrutineeReachable = AnalyzeRequiredExpression(matchStatement.Expression, isReachable);

        var anyArmCompletes = false;
        for (var index = 0; index < matchStatement.Arms.Count; index++)
        {
            var arm = matchStatement.Arms[index];
            Visit(arm.Pattern);
            Visit(arm.WhenClause);
            anyArmCompletes |= AnalyzeMatchArmExpression(
                arm.Expression,
                boundMatch.Arms[index].Expression,
                scrutineeReachable);
        }

        var evaluator = new MatchExhaustivenessEvaluator(
            _semanticModel.Compilation,
            _semanticModel.TryGetCachedBoundNode);
        var exhaustiveness = evaluator.Evaluate(matchStatement, boundMatch, default);

        _endPointIsReachable = scrutineeReachable && (!exhaustiveness.IsExhaustive || anyArmCompletes);
        return _endPointIsReachable;
    }

    private bool AnalyzeMatchArmExpression(
        ExpressionSyntax expression,
        BoundExpression boundExpression,
        bool isReachable)
    {
        switch (expression)
        {
            case BlockSyntax block:
                return AnalyzeBlockStatements(block.Statements, isReachable);
            case ReturnExpressionSyntax returnExpression:
                Visit(returnExpression.Expression);
                _returnStatements.Add(returnExpression);
                return false;
            case ThrowExpressionSyntax throwExpression:
                Visit(throwExpression.Expression);
                return false;
            case ParenthesizedExpressionSyntax parenthesized:
                return AnalyzeMatchArmExpression(parenthesized.Expression, boundExpression, isReachable);
            default:
                Visit(expression);
                CollectReturnExpressions(expression);
                return !BoundNodeFacts.IsAbruptExpression(boundExpression);
        }
    }

    private bool AnalyzeTryStatement(TryStatementSyntax tryStatement, bool isReachable)
    {
        var boundTryStatement = _semanticModel.TryGetCachedBoundNode(tryStatement) as BoundTryStatement;
        List<(LoopContext Context, bool Before)>? loopBreakStates = null;
        if (tryStatement.FinallyClause is not null && _loopContexts.Count > 0)
        {
            loopBreakStates = new List<(LoopContext Context, bool Before)>(_loopContexts.Count);
            foreach (var context in _loopContexts)
                loopBreakStates.Add((context, context.HasReachableBreak));
        }

        var tryReachable = AnalyzeStatement(tryStatement.Block, isReachable);
        var reachesEnd = tryReachable;

        for (var index = 0; index < tryStatement.CatchClauses.Count; index++)
        {
            var catchClause = tryStatement.CatchClauses[index];
            var catchEntryReachable = isReachable;
            if (catchClause.WhenClause?.Guard is ExpressionSyntax guard)
            {
                var boundGuard = boundTryStatement is not null && index < boundTryStatement.CatchClauses.Length
                    ? boundTryStatement.CatchClauses[index].Guard
                    : null;
                catchEntryReachable = AnalyzeRequiredExpression(guard, catchEntryReachable, boundGuard);
                if (ConstantValueEvaluator.TryEvaluate(guard, out var filterValue) && filterValue is false)
                    catchEntryReachable = false;
            }

            var catchReachable = AnalyzeStatement(catchClause.Block, catchEntryReachable);
            reachesEnd |= catchReachable;
        }

        if (tryStatement.FinallyClause is { } finallyClause)
        {
            bool[]? pendingBreaks = null;
            if (loopBreakStates is not null)
            {
                pendingBreaks = new bool[loopBreakStates.Count];
                for (var index = 0; index < loopBreakStates.Count; index++)
                {
                    var (context, before) = loopBreakStates[index];
                    pendingBreaks[index] = context.HasReachableBreak;
                    context.HasReachableBreak = before;
                }
            }

            var finallyReachesEnd = AnalyzeStatement(finallyClause.Block, isReachable);
            if (finallyReachesEnd && loopBreakStates is not null && pendingBreaks is not null)
            {
                for (var index = 0; index < loopBreakStates.Count; index++)
                    loopBreakStates[index].Context.HasReachableBreak |= pendingBreaks[index];
            }
            else if (!finallyReachesEnd)
            {
                reachesEnd = false;
            }
        }

        _endPointIsReachable = reachesEnd;
        return reachesEnd;
    }

    public override void VisitGotoStatement(GotoStatementSyntax node)
    {
        if (_analyzeJumpPoints && _region is not null)
        {
            var target = _semanticModel.GetLabelTarget(node);

            if (target is not null)
            {
                var gotoInsideRegion = _region.Contains(node) || IsWithinRegionBounds(node);
                var targetInsideRegion = _region.Contains(target) || IsWithinRegionBounds(target);

                if (gotoInsideRegion && !targetInsideRegion)
                {
                    if (!_exitPoints.Contains(node))
                        _exitPoints.Add(node);
                }
                else if (!gotoInsideRegion && targetInsideRegion)
                {
                    if (!_entryPoints.Contains(target))
                        _entryPoints.Add(target);
                }
            }
        }

        base.VisitGotoStatement(node);
    }

    public override void VisitLabeledStatement(LabeledStatementSyntax node)
    {
        if (_analyzeJumpPoints && _region is not null && _region.Contains(node))
        {
            if (_semanticModel.HasExternalGotoToLabel(node, _region))
            {
                if (!_entryPoints.Contains(node))
                    _entryPoints.Add(node);
            }
        }

        base.VisitLabeledStatement(node);
    }

    private bool IsWithinRegionBounds(SyntaxNode node)
    {
        if (_region is null)
            return false;

        var first = _region.FirstStatement;
        var last = _region.LastStatement;

        if (first is null || last is null)
            return false;

        var start = first.Span.Start;
        var end = last.Span.End;

        var span = node.Span;
        return span.Start >= start && span.Start < end && span.End <= end;
    }

    public ControlFlowAnalysis ToResult()
    {
        return new ControlFlowAnalysis
        {
            StartPointIsReachable = true,
            EndPointIsReachable = _endPointIsReachable,
            ReturnStatements = _returnStatements.ToImmutableArray(),
            EntryPoints = _entryPoints.ToImmutableArray(),
            ExitPoints = _exitPoints.ToImmutableArray(),
            UnreachableStatements = _unreachableStatements.ToImmutableArray(),
            Succeeded = true
        };
    }

    private void MarkUnreachable(StatementSyntax statement)
    {
        if (_region is not null && !_region.Contains(statement) && !IsWithinRegionBounds(statement))
            return;

        if (!_unreachableStatements.Contains(statement))
            _unreachableStatements.Add(statement);
    }

    private sealed class LoopContext(StatementSyntax loop)
    {
        public StatementSyntax Loop { get; } = loop;

        public bool HasReachableBreak { get; set; }
    }
}

public partial class SemanticModel
{
    public LabeledStatementSyntax? GetLabelTarget(GotoStatementSyntax gotoStatement)
    {
        EnsureControlFlowBindingReady(gotoStatement);

        if (_gotoTargets.TryGetValue(gotoStatement, out var symbol))
        {
            if (_labelSyntax.TryGetValue(symbol, out var syntax))
                return syntax;
        }

        var identifier = gotoStatement.Identifier;
        if (identifier.IsMissing)
            return null;

        if (_labelsByName.TryGetValue(identifier.Text, out var candidates))
        {
            foreach (var candidate in candidates)
            {
                if (_labelSyntax.TryGetValue(candidate, out var syntax))
                    return syntax;
            }
        }

        return null;
    }

    public bool HasExternalGotoToLabel(LabeledStatementSyntax labeledStatement, ControlFlowRegion region)
    {
        EnsureControlFlowBindingReady(labeledStatement);

        if (!_labelDeclarations.TryGetValue(labeledStatement, out var labelSymbol))
            return false;

        foreach (var entry in _gotoTargets)
        {
            var gotoSyntax = entry.Key;
            var targetSymbol = entry.Value;

            if (!SymbolEqualityComparer.Default.Equals(targetSymbol, labelSymbol))
                continue;

            if (region.Contains(gotoSyntax))
                continue;

            return true;
        }

        return false;
    }

    private void EnsureControlFlowBindingReady(SyntaxNode node)
    {
        EnsureBindingReadyForSemanticQuery();

        var root = GetControlFlowBindingRoot(node);
        if (root is CompilationUnitSyntax compilationUnit)
            EnsureTopLevelCompilationUnitBound(compilationUnit);
        else
            _ = TryGetBoundNodeForSemanticQuery(root, out _);
    }

    private static SyntaxNode GetControlFlowBindingRoot(SyntaxNode node)
    {
        for (var current = node; current is not null; current = current.Parent)
        {
            switch (current)
            {
                case BlockStatementSyntax or BlockSyntax:
                    return current;
                case GlobalStatementSyntax globalStatement:
                    return globalStatement.SyntaxTree.GetRoot() is CompilationUnitSyntax compilationUnit
                        ? compilationUnit
                        : globalStatement;
            }
        }

        return node;
    }
}

public sealed class ControlFlowAnalysis
{
    /// <summary>
    /// Indicates whether a region completes normally. Return true if and only if the end of the last statement in a region is reachable or the region contains no statements.
    /// </summary>
    public bool EndPointIsReachable { get; init; }

    /// <summary>
    /// The set of statements inside the region what are the destination of branches outside the region.
    /// </summary>
    public ImmutableArray<SyntaxNode> EntryPoints { get; init; }

    /// <summary>
    /// The set of statements inside a region that jump to locations outside the region.
    /// </summary>
    public ImmutableArray<SyntaxNode> ExitPoints { get; init; }

    /// <summary>
    /// The set of return statements found within a region.
    /// </summary>
    public ImmutableArray<SyntaxNode> ReturnStatements { get; init; }

    public bool StartPointIsReachable { get; init; }

    /// <summary>
    /// Returns true if and only if analysis was successful. Analysis can fail if the region does not properly span a single expression, a single statement, or a contiguous series of statements within the enclosing block
    /// </summary>
    public bool Succeeded { get; init; }

    /// <summary>
    /// Statements that were determined to be unreachable during analysis.
    /// </summary>
    public ImmutableArray<StatementSyntax> UnreachableStatements { get; init; }
}
