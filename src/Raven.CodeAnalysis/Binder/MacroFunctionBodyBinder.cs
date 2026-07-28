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

        var bound = new BoundExpressionStatement(BindExpression(contribution.Expression));
        CacheBoundNode(statement, bound);
        return bound;
    }
}
