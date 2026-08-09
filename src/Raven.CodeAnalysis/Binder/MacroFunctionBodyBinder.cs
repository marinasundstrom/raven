using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class MacroFunctionBodyBinder : BlockBinder
{
    public MacroFunctionBodyBinder(IMacroFunctionSymbol macroFunction, Binder parent)
        : base(macroFunction, parent)
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
}
