using System;
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
        return FinalizeMethodBody(block, bound, () => base.BindBlockStatement(block));
    }

    private BoundBlockStatement BindArrowExpressionClause(ArrowExpressionClauseSyntax clause)
    {
        if (TryGetCachedBoundNode(clause) is BoundBlockStatement cached)
            return cached;

        var bound = BindArrowExpressionClauseCore(clause);
        return FinalizeMethodBody(clause, bound, () => BindArrowExpressionClauseCore(clause));
    }

    private BoundBlockStatement BindArrowExpressionClauseCore(ArrowExpressionClauseSyntax clause)
    {
        var expression = BindExpression(clause.Expression);

        if (expression is BoundBlockExpression blockExpression)
            return new BoundBlockStatement(blockExpression.Statements, blockExpression.LocalsToDispose);

        BoundStatement statement = _methodSymbol.ReturnType.SpecialType == SpecialType.System_Unit
            ? new BoundExpressionStatement(expression)
            : new BoundReturnStatement(expression);

        return new BoundBlockStatement([statement]);
    }

    private BoundBlockStatement FinalizeMethodBody(SyntaxNode bodySyntax, BoundBlockStatement bound, Func<BoundBlockStatement> rebind)
    {
        if (_methodSymbol is SourceMethodSymbol
            {
                RequiresAsyncReturnTypeInference: true,
                AsyncReturnTypeInferenceComplete: false
            } sourceMethod)
        {
            var inferredReturnType = AsyncReturnTypeUtilities.InferAsyncReturnType(Compilation, bound);
            sourceMethod.SetReturnType(inferredReturnType);
            sourceMethod.CompleteAsyncReturnTypeInference();

            RemoveCachedBoundNode(bodySyntax);
            bound = rebind();
        }

        if (_methodSymbol.IsNamedConstructor && !_namedConstructorRewritten)
        {
            var selfLocal = new SourceLocalSymbol(
                "__self",
                _methodSymbol.ContainingType!,
                isMutable: true,
                _methodSymbol,
                _methodSymbol.ContainingType,
                _methodSymbol.ContainingNamespace,
                [bodySyntax.GetLocation()],
                [bodySyntax.GetReference()]);

            var rewriter = new NamedConstructorRewriter(_methodSymbol, selfLocal, Compilation);
            bound = rewriter.Rewrite(bound);
            _namedConstructorRewritten = true;
        }

        var unit = Compilation.UnitTypeSymbol;
        var skipTrailingExpressionCheck = ShouldSkipTrailingExpressionCheck(unit);

        if (!skipTrailingExpressionCheck &&
            !SymbolEqualityComparer.Default.Equals(_methodSymbol.ReturnType, unit))
        {
            if (bound.Statements.LastOrDefault() is BoundExpressionStatement exprStmt && exprStmt.Expression.Type is ITypeSymbol t && !IsAssignable(_methodSymbol.ReturnType, t, out _))
            {
                _diagnostics.ReportCannotConvertFromTypeToType(
                    t.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    _methodSymbol.ReturnType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                    bodySyntax.GetLocation());
            }
        }

        if (_methodSymbol is SourceMethodSymbol { IsAsync: true } asyncMethod)
            AnalyzeAsyncBody(bodySyntax, asyncMethod, bound);

        CacheBoundNode(bodySyntax, bound);
        return bound;
    }

    private void AnalyzeAsyncBody(SyntaxNode bodySyntax, SourceMethodSymbol asyncMethod, BoundBlockStatement bound)
    {
        var containsAwait = AsyncLowerer.ContainsAwait(bound) ||
            Compilation.ContainsAwaitExpressionOutsideNestedFunctions(bodySyntax);
        asyncMethod.SetContainsAwait(containsAwait);

        if (containsAwait)
            return;

        var memberDescription = AsyncDiagnosticUtilities.GetAsyncMemberDescription(asyncMethod);
        var location = AsyncDiagnosticUtilities.GetAsyncKeywordLocation(asyncMethod, bodySyntax);

        _diagnostics.ReportAsyncLacksAwait(memberDescription, location);
    }

    private bool ShouldSkipTrailingExpressionCheck(ITypeSymbol unitType)
    {
        if (_methodSymbol is SourceMethodSymbol { ShouldDeferAsyncReturnDiagnostics: true })
            return true;

        if (!_methodSymbol.IsAsync)
            return false;

        var returnType = _methodSymbol.ReturnType;

        if (returnType is ErrorTypeSymbol)
            return false;

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
        if (node is ArrowExpressionClauseSyntax arrow)
            return BindArrowExpressionClause(arrow);

        return base.GetOrBind(node);
    }

    private sealed class NamedConstructorRewriter : BoundTreeRewriter
    {
        private readonly IMethodSymbol _methodSymbol;
        private readonly SourceLocalSymbol _self;
        private readonly Compilation _compilation;

        public NamedConstructorRewriter(IMethodSymbol methodSymbol, SourceLocalSymbol self, Compilation compilation)
        {
            _methodSymbol = methodSymbol;
            _self = self;
            _compilation = compilation;
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
            var factory = _compilation.BoundNodeFactory;
            for (var i = 1; i < statements.Count - 1; i++)
            {
                switch (statements[i])
                {
                    case BoundAssignmentStatement assignStmt:
                        switch (assignStmt.Expression)
                        {
                            case BoundFieldAssignmentExpression fieldAssignment when fieldAssignment.Receiver is null:
                                statements[i] = factory.CreateAssignmentStatement(
                                    factory.CreateFieldAssignmentExpression(CreateSelfAccess(), fieldAssignment.Field, fieldAssignment.Right));
                                break;
                            case BoundPropertyAssignmentExpression propertyAssignment when propertyAssignment.Receiver is null:
                                statements[i] = factory.CreateAssignmentStatement(
                                    factory.CreatePropertyAssignmentExpression(CreateSelfAccess(), propertyAssignment.Property, propertyAssignment.Right));
                                break;
                        }
                        break;
                    case BoundExpressionStatement exprStmt:
                        switch (exprStmt.Expression)
                        {
                            case BoundFieldAssignmentExpression fieldAssignment when fieldAssignment.Receiver is null:
                                statements[i] = factory.CreateAssignmentStatement(
                                    factory.CreateFieldAssignmentExpression(CreateSelfAccess(), fieldAssignment.Field, fieldAssignment.Right));
                                break;
                            case BoundPropertyAssignmentExpression propertyAssignment when propertyAssignment.Receiver is null:
                                statements[i] = factory.CreateAssignmentStatement(
                                    factory.CreatePropertyAssignmentExpression(CreateSelfAccess(), propertyAssignment.Property, propertyAssignment.Right));
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
            return _compilation.BoundNodeFactory.CreateFieldAssignmentExpression(receiver, node.Field, right);
        }

        public override BoundNode? VisitPropertyAssignmentExpression(BoundPropertyAssignmentExpression node)
        {
            var receiver = RewriteReceiver(node.Receiver, node.Property);
            var right = (BoundExpression)Visit(node.Right)!;
            return _compilation.BoundNodeFactory.CreatePropertyAssignmentExpression(receiver, node.Property, right);
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
