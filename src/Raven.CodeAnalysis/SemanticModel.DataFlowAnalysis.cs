using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public DataFlowAnalysis AnalyzeDataFlow(ExpressionSyntax expression)
    {
        if (expression is BlockSyntax block && block.Statements.Count > 0)
        {
            var whileExpr = block.Parent?.Parent as WhileExpressionSyntax;
            var st = whileExpr?.Parent as ExpressionStatementSyntax;
            var globalStmt = st?.Parent as GlobalStatementSyntax;

            if (globalStmt is not null && globalStmt.Parent is CompilationUnitSyntax compilationUnit)
            {
                var globalStatements = compilationUnit.Members.OfType<GlobalStatementSyntax>().ToList();
                var index = globalStatements.IndexOf(globalStmt);

                var assignedBefore = new HashSet<ISymbol>();
                for (int i = 0; i < index; i++)
                {
                    var stmt = globalStatements[i].Statement;
                    var preCollector = new AssignmentCollector(this);
                    preCollector.Visit(stmt);
                    assignedBefore.UnionWith(preCollector.Written);
                }

                var walker = new DataFlowWalker(this);
                walker.SetInitialAssigned(assignedBefore);
                walker.Visit(globalStmt.Statement);
                return walker.ToResult();
            }
        }

        var collector = new DataFlowWalker(this);
        collector.Visit(expression);
        return collector.ToResult();
    }

    public DataFlowAnalysis AnalyzeDataFlow(StatementSyntax statement)
    {
        var collector = new DataFlowWalker(this);
        collector.Visit(statement);
        return collector.ToResult();
    }

    public DataFlowAnalysis AnalyzeDataFlow(StatementSyntax firstStatement, StatementSyntax lastStatement)
    {
        if (firstStatement.Parent != lastStatement.Parent || firstStatement.Parent is not BlockStatementSyntax block)
            return new DataFlowAnalysis { Succeeded = false };

        var startIndex = block.Statements.IndexOf(firstStatement);
        var endIndex = block.Statements.IndexOf(lastStatement);

        if (startIndex == -1 || endIndex == -1 || startIndex > endIndex)
            return new DataFlowAnalysis { Succeeded = false };

        var assignedBeforeRegion = new HashSet<ISymbol>();
        for (int i = 0; i < startIndex; i++)
        {
            var stmt = block.Statements[i];
            var preCollector = new AssignmentCollector(this);
            preCollector.Visit(stmt);
            assignedBeforeRegion.UnionWith(preCollector.Written);
        }

        var collector = new DataFlowWalker(this);
        collector.SetInitialAssigned(assignedBeforeRegion);

        for (int i = startIndex; i <= endIndex; i++)
            collector.Visit(block.Statements[i]);

        return collector.ToResult();
    }
}

internal sealed class AssignmentCollector : SyntaxWalker
{
    private readonly SemanticModel _semanticModel;
    public readonly HashSet<ISymbol> Written = new();

    public AssignmentCollector(SemanticModel semanticModel)
    {
        _semanticModel = semanticModel;
    }

    public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
    {
        var bound = _semanticModel.GetBoundNode(node) as BoundAssignmentExpression;
        if (bound?.Symbol is ILocalSymbol local)
            Written.Add(local);

        base.VisitAssignmentExpression(node);
    }

    public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
    {
        var bound = _semanticModel.GetBoundNode(node) as BoundLocalAccess;
        if (bound?.Symbol is ILocalSymbol local)
            Written.Add(local);

        if (node.Initializer is not null)
            Visit(node.Initializer.Value);

        base.VisitVariableDeclarator(node);
    }

    public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        foreach (var declarator in node.Declaration.Declarators)
        {
            var bound = _semanticModel.GetBoundNode(declarator) as BoundLocalAccess;
            if (bound?.Symbol is ILocalSymbol local)
                Written.Add(local);
        }

        base.VisitLocalDeclarationStatement(node);
    }
}

internal sealed class DataFlowWalker : SyntaxWalker
{
    private readonly SemanticModel _semanticModel;

    private readonly HashSet<ISymbol> _variablesDeclared = new();
    private readonly HashSet<ISymbol> _readInside = new();
    private readonly HashSet<ISymbol> _readOutside = new();
    private readonly HashSet<ISymbol> _writtenOutside = new();
    private readonly HashSet<ISymbol> _dataFlowsOut = new();
    private readonly HashSet<ISymbol> _captured = new();

    private HashSet<ISymbol> _writtenInside = new();
    private readonly HashSet<ISymbol> _dataFlowsIn = new();
    private HashSet<ISymbol> _definitelyAssignedOnEntry = new();
    private HashSet<ISymbol> _definitelyAssignedOnExit = new();
    private HashSet<ISymbol> _assignedOnEntry = new();

    public Compilation Compilation => _semanticModel.Compilation;

    public DataFlowWalker(SemanticModel semanticModel)
    {
        _semanticModel = semanticModel;
    }

    public override void VisitBlock(BlockSyntax node)
    {
        _definitelyAssignedOnEntry = _writtenInside.Union(_assignedOnEntry).ToHashSet();

        foreach (var statement in node.Statements)
            Visit(statement);

        _definitelyAssignedOnExit = _writtenInside.ToHashSet();
    }

    public override void VisitBlockStatement(BlockStatementSyntax node)
    {
        _definitelyAssignedOnEntry = _writtenInside.Union(_assignedOnEntry).ToHashSet();

        foreach (var statement in node.Statements)
            Visit(statement);

        _definitelyAssignedOnExit = _writtenInside.ToHashSet();
    }

    public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
    {
        var bound = _semanticModel.GetBoundNode(node) as BoundLocalAccess;
        if (bound?.Symbol is ILocalSymbol symbol)
            _variablesDeclared.Add(symbol);

        base.VisitVariableDeclarator(node);
    }

    public override void VisitBinaryExpression(BinaryExpressionSyntax node)
    {
        Visit(node.LeftHandSide);
        Visit(node.RightHandSide);
        base.VisitBinaryExpression(node);
    }

    public override void VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
    {
        var bound = _semanticModel.GetBoundNode(node);
        var symbol = bound?.GetSymbolInfo().Symbol;

        if (symbol is ILocalSymbol local)
        {
            _readInside.Add(local);
            if (!_writtenInside.Contains(local) && !_assignedOnEntry.Contains(local))
                _dataFlowsIn.Add(local);
        }

        base.VisitMemberAccessExpression(node);
    }

    public override void VisitMemberBindingExpression(MemberBindingExpressionSyntax node)
    {
        var bound = _semanticModel.GetBoundNode(node);
        var symbol = bound?.GetSymbolInfo().Symbol;

        if (symbol is ILocalSymbol local)
        {
            _readInside.Add(local);
            if (!_writtenInside.Contains(local) && !_assignedOnEntry.Contains(local))
                _dataFlowsIn.Add(local);
        }

        base.VisitMemberBindingExpression(node);
    }

    public override void VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        Visit(node.Expression);

        foreach (var arg in node.ArgumentList.Arguments)
            Visit(arg.Expression);

        base.VisitInvocationExpression(node);
    }

    public override void VisitIdentifierName(IdentifierNameSyntax node)
    {
        var bound = _semanticModel.GetBoundNode(node);
        var symbol = bound?.GetSymbolInfo().Symbol;

        if (symbol is ILocalSymbol local)
        {
            _readInside.Add(local);
            if (!_writtenInside.Contains(local) && !_assignedOnEntry.Contains(local))
                _dataFlowsIn.Add(local);
        }
        else if (symbol is IParameterSymbol param)
        {
            _readInside.Add(param);
            if (!_writtenInside.Contains(param) && !_assignedOnEntry.Contains(param))
                _dataFlowsIn.Add(param);
        }

        base.VisitIdentifierName(node);
    }

    public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
    {
        var bound = _semanticModel.GetBoundNode(node) as BoundAssignmentExpression;

        if (bound?.Symbol is ILocalSymbol local)
        {
            _writtenInside.Add(local);
            _dataFlowsOut.Add(local);
        }

        Visit(node.RightHandSide);
    }

    public override void VisitIfExpression(IfExpressionSyntax node)
    {
        Visit(node.Condition);

        var writtenBefore = _writtenInside.ToHashSet();
        Visit(node.Expression);
        var writtenAfterThen = _writtenInside.ToHashSet();

        _writtenInside.Clear();
        foreach (var sym in writtenBefore)
            _writtenInside.Add(sym);

        if (node.ElseClause is not null)
            Visit(node.ElseClause);

        _writtenInside.IntersectWith(writtenAfterThen);
    }

    public override void VisitWhileExpression(WhileExpressionSyntax node)
    {
        Visit(node.Condition);
        var assignedBefore = _writtenInside.Union(_assignedOnEntry).ToHashSet();

        _assignedOnEntry = assignedBefore;
        _writtenInside = assignedBefore.ToHashSet();

        Visit(node.Expression);
    }

    public override void VisitForExpression(ForExpressionSyntax node)
    {
        Visit(node.Expression);
        var assignedBefore = _writtenInside.Union(_assignedOnEntry).ToHashSet();

        _assignedOnEntry = assignedBefore;
        _writtenInside = assignedBefore.ToHashSet();

        Visit(node.Body);
    }

    public void SetInitialAssigned(HashSet<ISymbol> assigned)
    {
        _assignedOnEntry = assigned;
    }

    public DataFlowAnalysis ToResult()
    {
        return new DataFlowAnalysis
        {
            AlwaysAssigned = ImmutableArray<ISymbol>.Empty,
            Captured = _captured.ToImmutableArray(),
            CapturedInside = ImmutableArray<ISymbol>.Empty,
            CapturedOutside = ImmutableArray<ISymbol>.Empty,
            DataFlowsIn = _dataFlowsIn.ToImmutableArray(),
            DataFlowsOut = _dataFlowsOut.ToImmutableArray(),
            DefinitelyAssignedOnEntry = _definitelyAssignedOnEntry.ToImmutableArray(),
            DefinitelyAssignedOnExit = _definitelyAssignedOnExit.ToImmutableArray(),
            ReadInside = _readInside.ToImmutableArray(),
            ReadOutside = _readOutside.ToImmutableArray(),
            VariablesDeclared = _variablesDeclared.ToImmutableArray(),
            WrittenInside = _writtenInside.ToImmutableArray(),
            WrittenOutside = _writtenOutside.ToImmutableArray(),
            Succeeded = true
        };
    }
}

public sealed class DataFlowAnalysis
{
    public ImmutableArray<ISymbol> AlwaysAssigned { get; init; }
    public ImmutableArray<ISymbol> Captured { get; init; }
    public ImmutableArray<ISymbol> CapturedInside { get; init; }
    public ImmutableArray<ISymbol> CapturedOutside { get; init; }
    public ImmutableArray<ISymbol> DataFlowsIn { get; init; }
    public ImmutableArray<ISymbol> DataFlowsOut { get; init; }
    public ImmutableArray<ISymbol> DefinitelyAssignedOnEntry { get; init; }
    public ImmutableArray<ISymbol> DefinitelyAssignedOnExit { get; init; }
    public ImmutableArray<ISymbol> ReadInside { get; init; }
    public ImmutableArray<ISymbol> ReadOutside { get; init; }
    public ImmutableArray<ISymbol> VariablesDeclared { get; init; }
    public ImmutableArray<ISymbol> WrittenInside { get; init; }
    public ImmutableArray<ISymbol> WrittenOutside { get; init; }
    public bool Succeeded { get; init; }
}
