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
        var walker = new ControlFlowWalker(this, region);
        walker.Visit(statement); // only need to walk the single node

        return walker.ToResult();
    }

    public ControlFlowAnalysis AnalyzeControlFlow(StatementSyntax firstStatement, StatementSyntax lastStatement)
    {
        EnsureDiagnosticsCollected();

        var region = new ControlFlowRegion(firstStatement, lastStatement);
        var walker = new ControlFlowWalker(this, region);
        walker.Visit(region.EnclosingBlock);

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
    private readonly List<SyntaxNode> _returnStatements = new();
    private readonly List<SyntaxNode> _unreachableStatements = new();
    private readonly List<SyntaxNode> _entryPoints = new();
    private readonly List<SyntaxNode> _exitPoints = new();
    private readonly bool _isReachable = true;
    private bool _startVisited = false;

    public ControlFlowWalker(SemanticModel semanticModel, ControlFlowRegion? region = null)
    {
        _semanticModel = semanticModel;
        _region = region;
    }

    public override void Visit(SyntaxNode? node)
    {
        if (!_startVisited) _startVisited = true;
        if (node is null) return;

        if (_isReachable)
        {
            base.Visit(node);
        }
        else
        {
            _unreachableStatements.Add(node);
        }
    }

    public override void VisitGotoStatement(GotoStatementSyntax node)
    {
        if (_region is not null)
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
        if (_region is not null && _region.Contains(node))
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
            EndPointIsReachable = _isReachable,
            ReturnStatements = _returnStatements.ToImmutableArray(),
            EntryPoints = _entryPoints.ToImmutableArray(),
            ExitPoints = _exitPoints.ToImmutableArray(),
            Succeeded = true
        };
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
}
