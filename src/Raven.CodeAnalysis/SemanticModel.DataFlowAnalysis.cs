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
        if (firstStatement.Parent != lastStatement.Parent || firstStatement.Parent is not BlockSyntax block)
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

internal sealed class DataFlowWalker : SyntaxWalker
{
    private readonly HashSet<ISymbol> _variablesDeclared = new();
    private readonly HashSet<ISymbol> _readInside = new();
    private readonly HashSet<ISymbol> _readOutside = new();
    private HashSet<ISymbol> _writtenInside = new();
    private readonly HashSet<ISymbol> _writtenOutside = new();
    private HashSet<ISymbol> _dataFlowsIn = new();
    private readonly HashSet<ISymbol> _dataFlowsOut = new();
    private readonly HashSet<ISymbol> _captured = new();
    private HashSet<ISymbol> _definitelyAssignedOnEntry = new();
    private HashSet<ISymbol> _definitelyAssignedOnExit = new();
    private HashSet<ISymbol> _assignedOnEntry = new();

    private readonly SemanticModel _semanticModel;

    public Compilation Compilation => _semanticModel.Compilation;

    public DataFlowWalker(SemanticModel semanticModel)
    {
        _semanticModel = semanticModel;
    }

    public override void VisitCompilationUnit(CompilationUnitSyntax node)
    {
        foreach (var member in node.Members)
        {
            member.Accept(this);
        }

        base.VisitCompilationUnit(node);
    }

    public override void VisitNamespaceDeclaration(NamespaceDeclarationSyntax node)
    {
        foreach (var member in node.Members)
        {
            member.Accept(this);
        }

        base.VisitNamespaceDeclaration(node);
    }

    public override void VisitGlobalStatement(GlobalStatementSyntax node)
    {
        Visit(node.Statement);

        base.VisitGlobalStatement(node);
    }

    public override void VisitExpressionStatement1(ExpressionStatement1Syntax node)
    {
        Visit(node.Expression);

        base.VisitExpressionStatement1(node);
    }

    public override void VisitExpressionStatement2(ExpressionStatement2Syntax node)
    {
        Visit(node.Expression);

        base.VisitExpressionStatement2(node);
    }

    public override void VisitBlock(BlockSyntax node)
    {
        // Combine _writtenInside with _assignedOnEntry to reflect the real entry state
        _definitelyAssignedOnEntry = _writtenInside
            .Union(_assignedOnEntry)
            .ToHashSet();

        foreach (var statement in node.Statements)
            Visit(statement);

        _definitelyAssignedOnExit = _writtenInside.ToHashSet();
    }

    public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
    {
        var binder = Compilation.GetBinder(node);
        var symbol = binder.LookupLocalSymbol(node.Name.Identifier.Text);
        if (symbol != null)
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
        Visit(node.Expression);
        Visit(node.Name);

        base.VisitMemberAccessExpression(node);
    }

    public override void VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        Visit(node.Expression);

        foreach (var argument in node.ArgumentList.Arguments)
        {
            Visit(argument.Expression);
        }

        base.VisitInvocationExpression(node);
    }

    public override void VisitIdentifierName(IdentifierNameSyntax node)
    {
        var symbol = _semanticModel.GetSymbolInfo(node).Symbol;
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
        var leftSymbol = _semanticModel.GetSymbolInfo(node.LeftHandSide).Symbol;
        if (leftSymbol is ILocalSymbol local)
        {
            _writtenInside.Add(local);
            _dataFlowsOut.Add(local);
        }

        Visit(node.RightHandSide);
    }

    public override void VisitIfExpression(IfExpressionSyntax node)
    {
        // Visit condition (may read variables)
        Visit(node.Condition);

        // Clone the current state for branch tracking
        var writtenBefore = _writtenInside.ToHashSet();

        // Visit the 'then' branch
        Visit(node.Expression);

        var writtenAfterThen = _writtenInside.ToHashSet();

        // Restore pre-state for else
        _writtenInside.Clear();
        foreach (var symbol in writtenBefore)
            _writtenInside.Add(symbol);

        // Visit the 'else' branch
        if (node.ElseClause is not null)
            Visit(node.ElseClause);

        // Merge writes from both branches (intersection means definitely assigned after if)
        _writtenInside.IntersectWith(writtenAfterThen);
    }

    public override void VisitWhileExpression(WhileExpressionSyntax node)
    {
        Visit(node.Condition);

        var assignedBefore = _writtenInside.Union(_assignedOnEntry).ToHashSet();

        // Apply that state to both assigned-on-entry and writtenInside before visiting the body
        _assignedOnEntry = assignedBefore;
        _writtenInside = assignedBefore.ToHashSet(); // <-- THIS is the fix

        Visit(node.Statement);
    }

    public override void VisitReturnStatement(ReturnStatementSyntax node)
    {
        // Returns may contain expressions that read values
        if (node.Expression is not null)
            Visit(node.Expression);

        // You could mark the flow as ending here if doing full CFG (future work)
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


internal sealed class AssignmentCollector : SyntaxWalker
{
    private readonly SemanticModel _semanticModel;
    public readonly HashSet<ISymbol> Written = new();

    public AssignmentCollector(SemanticModel semanticModel)
    {
        _semanticModel = semanticModel;
    }

    public override void VisitGlobalStatement(GlobalStatementSyntax node)
    {
        Visit(node.Statement);
        base.VisitGlobalStatement(node);
    }

    public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
    {
        var symbol = _semanticModel.GetSymbolInfo(node.LeftHandSide).Symbol;
        if (symbol is ILocalSymbol local)
            Written.Add(local);

        base.VisitAssignmentExpression(node);
    }

    public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
    {
        var binder = _semanticModel.Compilation.GetBinder(node);
        var symbol = binder.LookupLocalSymbol(node.Name.Identifier.Text);
        if (symbol != null)
            Written.Add(symbol);

        // Visit the initializer (important!)
        if (node.Initializer is not null)
            Visit(node.Initializer.Value);

        base.VisitVariableDeclarator(node);
    }
    public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
    {
        foreach (var declarator in node.Declaration.Declarators)
        {
            var binder = _semanticModel.Compilation.GetBinder(declarator);
            var symbol = binder.LookupLocalSymbol(declarator.Name.Identifier.Text);
            if (symbol != null)
                Written.Add(symbol);
        }

        base.VisitLocalDeclarationStatement(node);
    }
}

public sealed class DataFlowAnalysis
{
    public DataFlowAnalysis()
    {
    }

    /// <summary>
    /// The set of local variables for which a value is always assigned inside a region.
    /// </summary>
    public ImmutableArray<ISymbol> AlwaysAssigned { get; init; }

    /// <summary>
    /// The set of the local variables that have been referenced in anonymous functions and therefore must be moved to a field of a frame class.
    /// </summary>
    public ImmutableArray<ISymbol> Captured { get; init; }

    /// <summary>
    /// The set of variables that are captured inside a region.
    /// </summary>
    public ImmutableArray<ISymbol> CapturedInside { get; init; }

    /// <summary>
    /// The set of variables that are captured outside a region.
    /// </summary>
    public ImmutableArray<ISymbol> CapturedOutside { get; init; }

    /// <summary>
    /// The set of local variables which are assigned a value outside a region that may be used inside the region.
    /// </summary>
    public ImmutableArray<ISymbol> DataFlowsIn { get; init; }

    /// <summary>
    /// The set of local variables which are assigned a value inside a region that may be used outside the region.
    /// </summary>
    public ImmutableArray<ISymbol> DataFlowsOut { get; init; }

    /// <summary>
    /// The set of local variables which are definitely assigned a value when a region is entered.
    /// </summary>
    public ImmutableArray<ISymbol> DefinitelyAssignedOnEntry { get; init; }

    /// <summary>
    /// The set of local variables which are definitely assigned a value when a region is exited.
    /// </summary>
    public ImmutableArray<ISymbol> DefinitelyAssignedOnExit { get; init; }

    /// <summary>
    /// The set of local variables that are read inside a region.
    /// </summary>
    public ImmutableArray<ISymbol> ReadInside { get; init; }

    /// <summary>
    /// The set of the local variables that are read outside a region.
    /// </summary>
    public ImmutableArray<ISymbol> ReadOutside { get; init; }

    /// <summary>
    /// Returns true if and only if analysis was successful. Analysis can fail if the region does not properly span a single expression, a single statement, or a contiguous series of statements within the enclosing block.
    /// </summary>
    public bool Succeeded { get; init; }

    /// <summary>
    /// The set of local variables that are declared within a region. Note that the region must be bounded by a method's body or a field's initializer, so parameter symbols are never included in the result.
    /// </summary>
    public ImmutableArray<ISymbol> VariablesDeclared { get; init; }

    /// <summary>
    /// The set of local variables that are written inside a region.
    /// </summary>
    public ImmutableArray<ISymbol> WrittenInside { get; init; }

    /// <summary>
    /// The set of local variables that are written outside a region.
    /// </summary>
    public ImmutableArray<ISymbol> WrittenOutside { get; init; }
}