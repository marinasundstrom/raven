using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    private bool CanLowerRangeFor(BoundForStatement node) =>
        _lowerPortableRanges && _containingSymbol.ContainingAssembly is SourceAssemblySymbol &&
        node.Iteration is { Kind: ForIterationKind.Range, RangeStart: not null, RangeEnd: not null, RangeStep: not null } &&
        node.Iteration.ElementType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 &&
        (node.Local is null || SymbolEqualityComparer.Default.Equals(node.Local.Type, node.Iteration.ElementType));

    private BoundStatement LowerRangeForStatement(BoundForStatement node, ILabelSymbol end, ILabelSymbol continueLabel)
    {
        var compilation = GetCompilation();
        var type = node.Iteration.ElementType;
        var unit = compilation.GetSpecialType(SpecialType.System_Unit);
        var current = CreateTempLocal("rangeCurrent", type, isMutable: true);
        var limit = CreateTempLocal("rangeEnd", type, isMutable: false);
        var step = CreateTempLocal("rangeStep", type, isMutable: false);
        var begin = CreateLabel("range_begin");
        var negative = CreateLabel("range_negative");
        var enter = CreateLabel("range_body");
        BoundExpression Read(ILocalSymbol local) => new BoundLocalAccess(local);
        BoundExpression Binary(BoundExpression left, SyntaxKind kind, BoundExpression right)
        {
            if (!BoundBinaryOperator.TryLookup(compilation, kind, left.Type, right.Type, out var op))
                throw new InvalidOperationException("Missing built-in signed range operator");
            return new BoundBinaryExpression(left, op, right);
        }
        BoundExpression Zero() => new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral,
            type.SpecialType == SpecialType.System_Int64 ? (object)0L : 0, type);
        BoundStatement Initialize(ILocalSymbol local, BoundExpression expression) =>
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(local, (BoundExpression)VisitExpression(expression)!)]);
        BoundStatement body;
        _loopStack.Push((end, continueLabel));
        try { body = (BoundStatement)VisitStatement(node.Body)!; }
        finally { _loopStack.Pop(); }
        var statements = new List<BoundStatement>
        {
            Initialize(current, node.Iteration.RangeStart!),
            Initialize(limit, node.Iteration.RangeEnd!),
            Initialize(step, node.Iteration.RangeStep!),
            new BoundConditionalGotoStatement(end, Binary(Read(step), SyntaxKind.EqualsEqualsToken, Zero()), jumpIfTrue: true),
            CreateLabelStatement(begin),
            new BoundConditionalGotoStatement(negative, Binary(Read(step), SyntaxKind.LessThanToken, Zero()), jumpIfTrue: true),
            new BoundConditionalGotoStatement(end, Binary(Read(current),
                node.Iteration.RangeUpperExclusive ? SyntaxKind.LessThanToken : SyntaxKind.LessThanOrEqualsToken, Read(limit)), jumpIfTrue: false),
            new BoundGotoStatement(enter),
            CreateLabelStatement(negative),
            new BoundConditionalGotoStatement(end, Binary(Read(current),
                node.Iteration.RangeUpperExclusive ? SyntaxKind.GreaterThanToken : SyntaxKind.GreaterThanOrEqualsToken, Read(limit)), jumpIfTrue: false),
            CreateLabelStatement(enter)
        };
        if (node.Local is { } local)
            statements.Add(new BoundLocalDeclarationStatement([new BoundVariableDeclarator(local, Read(current))]));
        statements.AddRange([
            body,
            CreateLabelStatement(continueLabel),
            new BoundAssignmentStatement(new BoundLocalAssignmentExpression(current, Read(current),
                Binary(Read(current), SyntaxKind.PlusToken, Read(step)), unit)),
            new BoundGotoStatement(begin, isBackward: true),
            CreateLabelStatement(end)
        ]);
        return new BoundBlockStatement(statements);
    }
}
