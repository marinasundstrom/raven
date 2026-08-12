using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class MacroBodyBinder : BlockBinder
{
    public MacroBodyBinder(IMacroDeclarationSymbol macro, Binder parent)
        : base(macro, parent)
    {
    }

    public override BoundStatement BindStatement(StatementSyntax statement)
    {
        if (statement is not MacroExpansionStatementSyntax contribution)
            return base.BindStatement(statement);

        var expression = BindExpression(contribution.Expression);
        BoundStatement bound = contribution.Keyword.ValueText == "expand"
            ? new BoundReturnStatement(expression)
            : new BoundExpressionStatement(expression);
        CacheBoundNode(statement, bound);
        return bound;
    }

    public override BoundExpression BindExpression(ExpressionSyntax expression)
    {
        if (expression is not MacroExpansionExpressionSyntax contribution)
            return base.BindExpression(expression);

        var operand = base.BindExpression(contribution.Expression);
        BoundExpression bound = contribution.Keyword.ValueText == "expand"
            ? new BoundReturnExpression(operand, Compilation.UnitTypeSymbol)
            : new BoundBlockExpression(
                [
                    new BoundExpressionStatement(operand),
                    new BoundExpressionStatement(BoundFactory.UnitExpression())
                ],
                Compilation.UnitTypeSymbol,
                introduceILScope: false);
        CacheBoundNode(expression, bound);
        return bound;
    }
}
