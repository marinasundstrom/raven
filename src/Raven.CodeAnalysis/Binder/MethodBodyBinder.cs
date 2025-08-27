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

    public override BoundBlockExpression BindBlock(BlockSyntax block, bool allowReturn = true)
    {
        var bound = base.BindBlock(block, allowReturn);

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

            var rewriter = new NamedConstructorRewriter(_methodSymbol, selfLocal);
            bound = rewriter.Rewrite(bound);
        }

        var unit = Compilation.GetSpecialType(SpecialType.System_Unit);
        if (!SymbolEqualityComparer.Default.Equals(_methodSymbol.ReturnType, unit))
        {
            if (bound.Statements.LastOrDefault() is BoundExpressionStatement exprStmt && exprStmt.Expression.Type is ITypeSymbol t && !IsAssignable(_methodSymbol.ReturnType, t))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(t, _methodSymbol.ReturnType, block.Statements.Last().GetLocation());
            }
        }

        CacheBoundNode(block, bound);
        return bound;
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

        public BoundBlockExpression Rewrite(BoundBlockExpression body)
        {
            var statements = VisitList(body.Statements).Cast<BoundStatement>().ToList();

            var ctor = _methodSymbol.ContainingType!.Constructors.First(c => c.Parameters.Length == 0);
            var creation = new BoundObjectCreationExpression(ctor, Array.Empty<BoundExpression>());
            var declarator = new BoundVariableDeclarator(_self, creation);
            var declaration = new BoundLocalDeclarationStatement([declarator]);
            statements.Insert(0, declaration);
            statements.Add(new BoundReturnStatement(new BoundLocalAccess(_self)));

            var or = (BoundFieldAssignmentExpression)((BoundExpressionStatement)statements[1]).Expression;

            statements[1] = new BoundExpressionStatement(
                new BoundFieldAssignmentExpression(new BoundLocalAccess(_self), or.Field, or.Right));

            return new BoundBlockExpression(statements);
        }

        public override BoundNode? VisitBlockExpression(BoundBlockExpression node)
        {
            var statements = VisitList(node.Statements).Cast<BoundStatement>().ToList();
            return new BoundBlockExpression(statements);
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
