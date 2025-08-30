using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class MethodBodyBinder : BlockBinder
{
    private readonly IMethodSymbol _methodSymbol;

    public MethodBodyBinder(IMethodSymbol methodSymbol, Binder parent)
        : base(methodSymbol, parent)
    {
        _methodSymbol = methodSymbol;
    }

    public override BoundBlockStatement BindBlockStatement(BlockStatementSyntax block)
    {
        var bound = base.BindBlockStatement(block);

        if (_methodSymbol.IsNamedConstructor)
        {
            var selfLocal = new SourceLocalSymbol(
                "__self",
                _methodSymbol.ContainingType!,
                isMutable: true,
                _methodSymbol,
                _methodSymbol.ContainingType,
                _methodSymbol.ContainingNamespace,
                [block.GetLocation()],
                [block.GetReference()]);

            var rewriter = new NamedConstructorRewriter(_methodSymbol, selfLocal, Compilation);
            bound = rewriter.Rewrite(bound);
        }

        var unit = Compilation.GetSpecialType(SpecialType.System_Unit);
        if (!SymbolEqualityComparer.Default.Equals(_methodSymbol.ReturnType, unit))
        {
            if (bound.Statements.LastOrDefault() is BoundExpressionStatement exprStmt && exprStmt.Expression.Type is ITypeSymbol t && !IsAssignable(_methodSymbol.ReturnType, t))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    t.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    _methodSymbol.ReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    block.Statements.Last().GetLocation());
            }
        }

        CacheBoundNode(block, bound);
        return bound;
    }

    public override BoundNode GetOrBind(SyntaxNode node)
    {
        if (node is BlockStatementSyntax blockStmt)
            return BindBlockStatement(blockStmt);
        if (node is BlockSyntax block)
            return BindBlock(block, allowReturn: true);

        return base.GetOrBind(node);
    }

    private sealed class NamedConstructorRewriter : BoundTreeRewriter
    {
        private readonly IMethodSymbol _methodSymbol;
        private readonly SourceLocalSymbol _self;

        public NamedConstructorRewriter(IMethodSymbol methodSymbol, SourceLocalSymbol self)
        {
            _methodSymbol = methodSymbol;
            _self = self;
        }

        private readonly Compilation _compilation;

        public NamedConstructorRewriter(IMethodSymbol methodSymbol, SourceLocalSymbol self, Compilation compilation)
        {
            _methodSymbol = methodSymbol;
            _self = self;
            _compilation = compilation;
        }

        public BoundBlockStatement Rewrite(BoundBlockStatement body)
        {
            var statements = VisitList(body.Statements).Cast<BoundStatement>().ToList();

            var ctor = _methodSymbol.ContainingType!.Constructors.First(c => c.Parameters.Length == 0);
            var creation = new BoundObjectCreationExpression(ctor, Array.Empty<BoundExpression>());
            var declarator = new BoundVariableDeclarator(_self, creation);
            var declaration = new BoundLocalDeclarationStatement([declarator]);
            statements.Insert(0, declaration);
            statements.Add(new BoundReturnStatement(new BoundLocalAccess(_self)));

            // Rewrite field and property assignments that implicitly target `self`.
            // The binder leaves such assignments with a null receiver. After introducing
            // the `__self` local, these assignments should explicitly reference it.
            for (var i = 1; i < statements.Count - 1; i++)
            {
                switch (statements[i])
                {
                    case BoundAssignmentStatement assignStmt:
                        switch (assignStmt.Expression)
                        {
                            case BoundFieldAssignmentExpression fieldAssignment when fieldAssignment.Receiver is null:
                                statements[i] = new BoundAssignmentStatement(
                                    new BoundFieldAssignmentExpression(new BoundLocalAccess(_self), fieldAssignment.Field, fieldAssignment.Right));
                                break;
                            case BoundPropertyAssignmentExpression propertyAssignment when propertyAssignment.Receiver is null:
                                statements[i] = new BoundAssignmentStatement(
                                    new BoundPropertyAssignmentExpression(new BoundLocalAccess(_self), propertyAssignment.Property, propertyAssignment.Right));
                                break;
                        }
                        break;
                    case BoundExpressionStatement exprStmt:
                        switch (exprStmt.Expression)
                        {
                            case BoundFieldAssignmentExpression fieldAssignment when fieldAssignment.Receiver is null:
                                statements[i] = new BoundAssignmentStatement(
                                    new BoundFieldAssignmentExpression(new BoundLocalAccess(_self), fieldAssignment.Field, fieldAssignment.Right));
                                break;
                            case BoundPropertyAssignmentExpression propertyAssignment when propertyAssignment.Receiver is null:
                                statements[i] = new BoundAssignmentStatement(
                                    new BoundPropertyAssignmentExpression(new BoundLocalAccess(_self), propertyAssignment.Property, propertyAssignment.Right));
                                break;
                        }
                        break;
                }
            }

            return new BoundBlockStatement(statements);
        }

        public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
        {
            var statements = VisitList(node.Statements).Cast<BoundStatement>().ToList();
            return new BoundBlockStatement(statements);
        }

        public override BoundNode? VisitBlockExpression(BoundBlockExpression node)
        {
            var statements = VisitList(node.Statements).Cast<BoundStatement>().ToList();
            var unitType = _compilation.GetSpecialType(SpecialType.System_Unit);
            return new BoundBlockExpression(statements, unitType);
        }

        public override BoundNode? VisitAssignmentStatement(BoundAssignmentStatement node)
        {
            var expression = (BoundAssignmentExpression)Visit(node.Expression)!;
            return new BoundAssignmentStatement(expression);
        }

        public override BoundNode? VisitExpressionStatement(BoundExpressionStatement node)
        {
            var expression = (BoundExpression)Visit(node.Expression)!;
            return new BoundExpressionStatement(expression);
        }

        public override BoundNode? VisitReturnStatement(BoundReturnStatement node)
        {
            var expression = node.Expression is null ? null : (BoundExpression?)Visit(node.Expression);
            return new BoundReturnStatement(expression);
        }

        public override BoundNode? VisitMemberAccessExpression(BoundMemberAccessExpression node)
        {
            var receiver = node.Receiver is null ? null : (BoundExpression?)Visit(node.Receiver);
            return new BoundMemberAccessExpression(receiver, node.Member);
        }

        public override BoundNode? VisitFieldAssignmentExpression(BoundFieldAssignmentExpression node)
        {
            var receiver = node.Receiver is null ? null : (BoundExpression?)Visit(node.Receiver);
            var right = (BoundExpression)Visit(node.Right)!;
            return new BoundFieldAssignmentExpression(receiver, node.Field, right);
        }

        public override BoundNode? VisitPropertyAssignmentExpression(BoundPropertyAssignmentExpression node)
        {
            var receiver = node.Receiver is null ? null : (BoundExpression?)Visit(node.Receiver);
            var right = (BoundExpression)Visit(node.Right)!;
            return new BoundPropertyAssignmentExpression(receiver, node.Property, right);
        }

        public override BoundNode? VisitBinaryExpression(BoundBinaryExpression node)
        {
            var left = (BoundExpression)Visit(node.Left)!;
            var right = (BoundExpression)Visit(node.Right)!;
            return new BoundBinaryExpression(left, node.Operator, right);
        }

        public override BoundNode? VisitInvocationExpression(BoundInvocationExpression node)
        {
            var receiver = node.Receiver is null ? null : (BoundExpression?)Visit(node.Receiver);
            var args = node.Arguments.Select(a => (BoundExpression)Visit(a)!).ToArray();
            return new BoundInvocationExpression(node.Method, args, receiver);
        }

        public override BoundNode? VisitSelfExpression(BoundSelfExpression node)
        {
            return new BoundLocalAccess(_self);
        }
    }
}
