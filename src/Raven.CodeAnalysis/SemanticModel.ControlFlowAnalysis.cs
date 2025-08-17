using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public ControlFlowAnalysis AnalyzeControlFlow(StatementSyntax statement)
    {
        var region = new ControlFlowRegion(statement);
        var walker = new ControlFlowWalker(this, region);
        walker.Visit(statement); // only need to walk the single node

        return walker.ToResult();
    }

    public ControlFlowAnalysis AnalyzeControlFlow(StatementSyntax firstStatement, StatementSyntax lastStatement)
    {
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
    public BlockSyntax? EnclosingBlock { get; }

    private readonly HashSet<SyntaxNode> _containedNodes = new();

    // For a single statement region
    public ControlFlowRegion(StatementSyntax singleStatement)
    {
        FirstStatement = LastStatement = singleStatement;
        EnclosingBlock = singleStatement.Parent as BlockSyntax;

        CollectContainedNodes(singleStatement);
    }

    // For a span of statements within the same block
    public ControlFlowRegion(StatementSyntax first, StatementSyntax last)
    {
        if (first.Parent != last.Parent || first.Parent is not BlockSyntax block)
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

    /*
    public override void VisitGotoStatement(GotoStatementSyntax node)
    {
        if (_region is not null)
        {
            var label = node.Identifier.Text;
            var target = _semanticModel.GetLabelTarget(label);

            if (_region.Contains(node) && target is not null && !_region.Contains(target))
            {
                _exitPoints.Add(node);
            }
        }
        base.VisitGotoStatement(node);
    }

    public override void VisitLabeledStatement(LabeledStatementSyntax node)
    {
        if (_region is not null && _region.Contains(node))
        {
            var label = node.Identifier.Text;
            if (_semanticModel.HasExternalGotoToLabel(label, _region))
            {
                _entryPoints.Add(node);
            }
        }
        base.VisitLabeledStatement(node);
    }
    */

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
    /*
    // Stub for label resolution
    public LabeledStatementSyntax? GetLabelTarget(string label)
    {
        // In a real implementation, index all label declarations and return the one matching
        return null;
    }
    */

    // Stub for checking if any goto exists outside the region targeting the label
    public bool HasExternalGotoToLabel(string label, ControlFlowRegion region)
    {
        // Track all gotos globally and match those targeting label where origin is outside the region
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