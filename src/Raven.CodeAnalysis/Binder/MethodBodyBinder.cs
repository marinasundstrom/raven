using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class MethodBodyBinder : BlockBinder
{
    private readonly IMethodSymbol _methodSymbol;
    private bool _namedConstructorRewritten;

    public MethodBodyBinder(IMethodSymbol methodSymbol, Binder parent)
        : base(methodSymbol, parent)
    {
        _methodSymbol = methodSymbol;
    }

    public override BoundBlockStatement BindBlockStatement(BlockStatementSyntax block)
    {
        var bound = base.BindBlockStatement(block);

        if (_methodSymbol.IsNamedConstructor && !_namedConstructorRewritten)
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
            _namedConstructorRewritten = true;
        }

        var unit = Compilation.GetSpecialType(SpecialType.System_Unit);
        var skipTrailingExpressionCheck = ShouldSkipTrailingExpressionCheck(unit);

        if (!skipTrailingExpressionCheck &&
            !SymbolEqualityComparer.Default.Equals(_methodSymbol.ReturnType, unit))
        {
            if (bound.Statements.LastOrDefault() is BoundExpressionStatement exprStmt && exprStmt.Expression.Type is ITypeSymbol t && !IsAssignable(_methodSymbol.ReturnType, t, out _))
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

    private bool ShouldSkipTrailingExpressionCheck(ITypeSymbol unitType)
    {
        if (!_methodSymbol.IsAsync)
            return false;

        var returnType = _methodSymbol.ReturnType;

        if (returnType.SpecialType == SpecialType.System_Threading_Tasks_Task)
            return true;

        if (returnType is INamedTypeSymbol named &&
            named.OriginalDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            named.TypeArguments.Length == 1)
        {
            var resultType = named.TypeArguments[0];
            if (resultType is not null && SymbolEqualityComparer.Default.Equals(resultType, unitType))
                return true;
        }

        return false;
    }

    public override BoundNode GetOrBind(SyntaxNode node)
    {
        if (TryGetCachedBoundNode(node) is BoundNode cached)
            return cached;

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

        private BoundLocalAccess CreateSelfAccess() => new BoundLocalAccess(_self);

        private BoundExpression? RewriteReceiver(BoundExpression? receiver, ISymbol? member)
        {
            if (receiver is not null)
                return (BoundExpression?)Visit(receiver);

            if (member is null)
                return null;

            return RequiresInstanceReceiver(member) ? CreateSelfAccess() : null;
        }

        private static bool RequiresInstanceReceiver(ISymbol member)
        {
            return member switch
            {
                IMethodSymbol method => !method.IsStatic,
                IPropertySymbol property => !property.IsStatic,
                IFieldSymbol field => !field.IsStatic,
                _ => false,
            };
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

            return new BoundBlockStatement(statements, body.LocalsToDispose);
        }

        public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
        {
            var statements = VisitList(node.Statements).Cast<BoundStatement>().ToList();
            return new BoundBlockStatement(statements, node.LocalsToDispose);
        }

        public override BoundNode? VisitBlockExpression(BoundBlockExpression node)
        {
            var statements = VisitList(node.Statements).Cast<BoundStatement>().ToList();
            return new BoundBlockExpression(statements, node.UnitType, node.LocalsToDispose);
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
            var receiver = RewriteReceiver(node.Receiver, node.Member);
            return new BoundMemberAccessExpression(receiver, node.Member, node.Reason);
        }

        public override BoundNode? VisitFieldAssignmentExpression(BoundFieldAssignmentExpression node)
        {
            var receiver = RewriteReceiver(node.Receiver, node.Field);
            var right = (BoundExpression)Visit(node.Right)!;
            return new BoundFieldAssignmentExpression(receiver, node.Field, right);
        }

        public override BoundNode? VisitPropertyAssignmentExpression(BoundPropertyAssignmentExpression node)
        {
            var receiver = RewriteReceiver(node.Receiver, node.Property);
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
            var receiver = RewriteReceiver(node.Receiver, node.Method);
            var args = node.Arguments.Select(a => (BoundExpression)Visit(a)!).ToArray();
            BoundExpression? extensionReceiver = null;
            if (node.ExtensionReceiver is not null)
            {
                extensionReceiver = ReferenceEquals(node.ExtensionReceiver, node.Receiver)
                    ? receiver
                    : (BoundExpression?)Visit(node.ExtensionReceiver);
            }

            return new BoundInvocationExpression(
                node.Method,
                args,
                receiver,
                extensionReceiver,
                node.RequiresReceiverAddress);
        }

        public override BoundNode? VisitFieldAccess(BoundFieldAccess node)
        {
            if (node.Field.IsStatic)
                return node;

            return new BoundMemberAccessExpression(CreateSelfAccess(), node.Field, node.Reason);
        }

        public override BoundNode? VisitPropertyAccess(BoundPropertyAccess node)
        {
            if (node.Property.IsStatic)
                return node;

            return new BoundMemberAccessExpression(CreateSelfAccess(), node.Property, node.Reason);
        }

        public override BoundNode? VisitSelfExpression(BoundSelfExpression node)
        {
            return new BoundLocalAccess(_self);
        }
    }
}
