using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    public DataFlowAnalysis AnalyzeDataFlow(ExpressionSyntax expression)
    {
        var collector = new DataFlowWalker(this);

        var assignedBefore = GetAssignedBeforeGlobalStatement(expression);
        if (assignedBefore is not null)
            collector.SetInitialAssigned(assignedBefore);

        collector.Visit(expression);
        return collector.ToResult();
    }

    public DataFlowAnalysis AnalyzeDataFlow(StatementSyntax statement)
    {
        var collector = new DataFlowWalker(this);
        var assignedBefore = GetAssignedBeforeGlobalStatement(statement);
        if (assignedBefore is not null)
            collector.SetInitialAssigned(assignedBefore);
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

        var globalAssigned = GetAssignedBeforeGlobalStatement(block);
        if (globalAssigned is not null)
            assignedBeforeRegion.UnionWith(globalAssigned);

        collector.SetInitialAssigned(assignedBeforeRegion);

        for (int i = startIndex; i <= endIndex; i++)
            collector.Visit(block.Statements[i]);

        return collector.ToResult();
    }

    private HashSet<ISymbol>? GetAssignedBeforeGlobalStatement(SyntaxNode node)
    {
        var globalStatement = node.AncestorsAndSelf().OfType<GlobalStatementSyntax>().FirstOrDefault();
        if (globalStatement is null)
            return null;

        if (globalStatement.Parent is not CompilationUnitSyntax compilationUnit)
            return null;

        var globalStatements = compilationUnit.Members.OfType<GlobalStatementSyntax>().ToList();
        var index = globalStatements.IndexOf(globalStatement);
        if (index <= 0)
            return new HashSet<ISymbol>();

        var assignedBefore = new HashSet<ISymbol>();
        for (var i = 0; i < index; i++)
        {
            var statement = globalStatements[i].Statement;
            var collector = new AssignmentCollector(this);
            collector.Visit(statement);
            assignedBefore.UnionWith(collector.Written);
        }

        return assignedBefore;
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

    public override void VisitAssignmentStatement(AssignmentStatementSyntax node)
    {
        if (_semanticModel.GetBoundNode(node) is BoundAssignmentStatement bound)
            CollectAssignedLocals(bound.Expression);

        foreach (var local in DataFlowAnalysisHelpers.GetAssignedLocals(_semanticModel, node.Left))
            Written.Add(local);

        base.VisitAssignmentStatement(node);
    }

    public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
    {
        if (_semanticModel.GetBoundNode(node) is BoundAssignmentExpression bound)
            CollectAssignedLocals(bound);

        foreach (var local in DataFlowAnalysisHelpers.GetAssignedLocals(_semanticModel, node.Left))
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
            if (_semanticModel.GetDeclaredSymbol(declarator) is ILocalSymbol local)
                Written.Add(local);
        }

        base.VisitLocalDeclarationStatement(node);
    }

    private void CollectAssignedLocals(BoundAssignmentExpression? assignment)
    {
        switch (assignment)
        {
            case null:
                return;
            case BoundLocalAssignmentExpression localAssignment:
                Written.Add(localAssignment.Local);
                break;
            case BoundPatternAssignmentExpression patternAssignment:
                CollectAssignedLocals(patternAssignment.Pattern);
                break;
        }
    }

    private void CollectAssignedLocals(BoundPattern pattern)
    {
        switch (pattern)
        {
            case BoundDeclarationPattern declaration when declaration.Designator is BoundSingleVariableDesignator single:
                Written.Add(single.Local);
                break;
            case BoundTuplePattern tuple:
                foreach (var element in tuple.Elements)
                    CollectAssignedLocals(element);
                break;
            case BoundCasePattern casePattern:
                foreach (var argument in casePattern.Arguments)
                    CollectAssignedLocals(argument.Pattern);
                break;
            case BoundBinaryPattern binary:
                CollectAssignedLocals(binary.Left);
                CollectAssignedLocals(binary.Right);
                break;
            case BoundUnaryPattern unary:
                CollectAssignedLocals(unary.Pattern);
                break;
            default:
                foreach (var designator in pattern.GetDesignators())
                {
                    if (designator is BoundSingleVariableDesignator nested)
                        Written.Add(nested.Local);
                }
                break;
        }
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
        if (_semanticModel.GetDeclaredSymbol(node) is ILocalSymbol symbol)
            _variablesDeclared.Add(symbol);

        base.VisitVariableDeclarator(node);
    }

    public override void VisitBinaryExpression(BinaryExpressionSyntax node)
    {
        Visit(node.Left);
        Visit(node.Right);
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
        if (_semanticModel.GetBoundNode(node) is BoundAssignmentExpression bound)
            MarkAssigned(bound);

        foreach (var local in DataFlowAnalysisHelpers.GetAssignedLocals(_semanticModel, node.Left))
        {
            _writtenInside.Add(local);
            _dataFlowsOut.Add(local);
        }

        Visit(node.Right);
    }

    public override void VisitAssignmentStatement(AssignmentStatementSyntax node)
    {
        if (_semanticModel.GetBoundNode(node) is BoundAssignmentStatement bound)
            MarkAssigned(bound.Expression);

        foreach (var local in DataFlowAnalysisHelpers.GetAssignedLocals(_semanticModel, node.Left))
        {
            _writtenInside.Add(local);
            _dataFlowsOut.Add(local);
        }

        Visit(node.Right);
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

    public override void VisitWhileStatement(WhileStatementSyntax node)
    {
        Visit(node.Condition);
        var assignedBefore = _writtenInside.Union(_assignedOnEntry).ToHashSet();

        _assignedOnEntry = assignedBefore;
        _writtenInside = assignedBefore.ToHashSet();

        Visit(node.Statement);
    }

    public override void VisitForStatement(ForStatementSyntax node)
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

    private void MarkAssigned(BoundAssignmentExpression? assignment)
    {
        switch (assignment)
        {
            case null:
                return;
            case BoundLocalAssignmentExpression localAssignment:
                _writtenInside.Add(localAssignment.Local);
                _dataFlowsOut.Add(localAssignment.Local);
                break;
            case BoundPatternAssignmentExpression patternAssignment:
                foreach (var local in GetPatternLocals(patternAssignment.Pattern))
                {
                    _writtenInside.Add(local);
                    _dataFlowsOut.Add(local);
                }
                break;
        }
    }

    private static IEnumerable<ILocalSymbol> GetPatternLocals(BoundPattern pattern)
    {
        switch (pattern)
        {
            case BoundDeclarationPattern declaration when declaration.Designator is BoundSingleVariableDesignator single:
                yield return single.Local;
                yield break;
            case BoundTuplePattern tuple:
                foreach (var element in tuple.Elements)
                {
                    foreach (var local in GetPatternLocals(element))
                        yield return local;
                }
                yield break;
            case BoundCasePattern casePattern:
                foreach (var argument in casePattern.Arguments)
                {
                    foreach (var local in GetPatternLocals(argument.Pattern))
                        yield return local;
                }
                yield break;
            case BoundBinaryPattern binary:
                foreach (var local in GetPatternLocals(binary.Left))
                    yield return local;
                foreach (var local in GetPatternLocals(binary.Right))
                    yield return local;
                yield break;
            case BoundUnaryPattern unary:
                foreach (var local in GetPatternLocals(unary.Pattern))
                    yield return local;
                yield break;
        }

        foreach (var designator in pattern.GetDesignators())
        {
            if (designator is BoundSingleVariableDesignator nested)
                yield return nested.Local;
        }
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

static class DataFlowAnalysisHelpers
{
    public static IEnumerable<ILocalSymbol> GetAssignedLocals(SemanticModel semanticModel, ExpressionOrPatternSyntax left)
    {
        return left switch
        {
            PatternSyntax pattern => GetAssignedLocals(semanticModel, pattern),
            ExpressionSyntax expression => GetAssignedLocals(semanticModel, expression),
            _ => Enumerable.Empty<ILocalSymbol>()
        };
    }

    private static IEnumerable<ILocalSymbol> GetAssignedLocals(SemanticModel semanticModel, PatternSyntax pattern)
    {
        foreach (var identifier in pattern.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>())
        {
            if (semanticModel.GetSymbolInfo(identifier).Symbol is ILocalSymbol local && !string.IsNullOrEmpty(local.Name))
                yield return local;
        }

        foreach (var designation in pattern.DescendantNodesAndSelf().OfType<SingleVariableDesignationSyntax>())
        {
            if (semanticModel.GetDeclaredSymbol(designation) is ILocalSymbol local && !string.IsNullOrEmpty(local.Name))
                yield return local;
        }
    }

    private static IEnumerable<ILocalSymbol> GetAssignedLocals(SemanticModel semanticModel, ExpressionSyntax expression)
    {
        switch (expression)
        {
            case IdentifierNameSyntax identifier when semanticModel.GetSymbolInfo(identifier).Symbol is ILocalSymbol local && !string.IsNullOrEmpty(local.Name):
                yield return local;
                yield break;
            case TupleExpressionSyntax tuple:
                foreach (var argument in tuple.Arguments)
                {
                    foreach (var local in GetAssignedLocals(semanticModel, argument.Expression))
                        yield return local;
                }
                yield break;
            case ParenthesizedExpressionSyntax parenthesized:
                foreach (var local in GetAssignedLocals(semanticModel, parenthesized.Expression))
                    yield return local;
                yield break;
        }
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
