using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public ControlFlowAnalysis AnalyzeControlFlow(StatementSyntax statement)
    {
        EnsureDiagnosticsCollected();

        var region = new ControlFlowRegion(statement);
        return AnalyzeControlFlowInternal(region, statement);
    }

    public ControlFlowAnalysis AnalyzeControlFlow(StatementSyntax firstStatement, StatementSyntax lastStatement)
    {
        EnsureDiagnosticsCollected();

        var region = new ControlFlowRegion(firstStatement, lastStatement);
        return AnalyzeControlFlowInternal(region, region.EnclosingBlock ?? firstStatement);
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
        if (first.Parent != last.Parent || first.Parent is not BlockStatementSyntax block)
            throw new ArgumentException("Region must be a contiguous set of statements in the same block.");

        FirstStatement = first;
        LastStatement = last;
        EnclosingBlock = block;

        var found = false;
        foreach (var stmt in block.Statements)
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
                Visit(ifStatement.Condition);

                var beforeIf = isReachable;

                _ = AnalyzeStatement(ifStatement.ThenStatement, beforeIf);
                var thenReachable = _endPointIsReachable;

                var elseReachable = beforeIf;
                if (ifStatement.ElseStatement is { } elseStatement)
                {
                    _ = AnalyzeStatement(elseStatement, beforeIf);
                    elseReachable = _endPointIsReachable;
                }

                _endPointIsReachable = thenReachable || elseReachable;
                return _endPointIsReachable;
            case WhileStatementSyntax whileStatement:
                Visit(whileStatement.Condition);
                _ = AnalyzeStatement(whileStatement.Statement, isReachable);
                _endPointIsReachable = isReachable;
                return _endPointIsReachable;
            case ForStatementSyntax forStatement:
                Visit(forStatement.Expression);
                _ = AnalyzeStatement(forStatement.Body, isReachable);
                _endPointIsReachable = isReachable;
                return _endPointIsReachable;
            case TryStatementSyntax tryStatement:
                return AnalyzeTryStatement(tryStatement, isReachable);
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
            case ThrowStatementSyntax throwStatement:
                base.VisitThrowStatement(throwStatement);
                _endPointIsReachable = false;
                return false;
            case YieldBreakStatementSyntax yieldBreakStatement:
                base.VisitYieldBreakStatement(yieldBreakStatement);
                _endPointIsReachable = false;
                return false;
            default:
                base.Visit(statement);
                _endPointIsReachable = true;
                return true;
        }
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

    private bool AnalyzeTryStatement(TryStatementSyntax tryStatement, bool isReachable)
    {
        var tryReachable = AnalyzeStatement(tryStatement.Block, isReachable);
        var reachesEnd = tryReachable;

        foreach (var catchClause in tryStatement.CatchClauses)
        {
            var catchReachable = AnalyzeStatement(catchClause.Block, isReachable);
            reachesEnd |= catchReachable;
        }

        if (tryStatement.FinallyClause is { } finallyClause)
            reachesEnd = AnalyzeStatement(finallyClause.Block, reachesEnd);

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
}

public partial class SemanticModel
{
    public LabeledStatementSyntax? GetLabelTarget(GotoStatementSyntax gotoStatement)
    {
        EnsureDiagnosticsCollected();

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
        EnsureDiagnosticsCollected();

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
