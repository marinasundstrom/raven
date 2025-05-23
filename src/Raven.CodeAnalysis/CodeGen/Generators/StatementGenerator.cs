using System.Reflection.Emit;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class StatementGenerator : Generator
{
    private readonly StatementSyntax _statement;

    public StatementGenerator(Generator parent, StatementSyntax statement) : base(parent)
    {
        _statement = statement;
    }

    public override void Generate()
    {
        switch (_statement)
        {
            case ReturnStatementSyntax returnStatement:
                GenerateReturnStatement(returnStatement);
                break;

            case ExpressionStatementSyntax expressionStatement:
                GenerateExpressionStatement(expressionStatement);
                break;

            case LocalDeclarationStatementSyntax localDeclarationStatement:
                GenerateDeclarationStatement(localDeclarationStatement);
                break;
        }
    }

    private void GenerateReturnStatement(ReturnStatementSyntax returnStatement)
    {
        if (returnStatement.Expression is ExpressionSyntax expression)
        {
            new ExpressionGenerator(this, expression).Generate();
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void GenerateExpressionStatement(ExpressionStatementSyntax expressionStatement)
    {
        new ExpressionGenerator(this, expressionStatement.Expression).Generate();

        var symbol = GetSymbolInfo(expressionStatement.Expression).Symbol;

        if (expressionStatement.Expression is InvocationExpressionSyntax invocationExpression)
        {
            symbol = ((IMethodSymbol)symbol).ReturnType;
        }

        // TODO: Handle the case that Pop is required. If not Void, and not assigned anywhere.

        if (symbol is not null && symbol?.UnwrapType()?.SpecialType != SpecialType.System_Void)
        {
            // The value is not used, pop it from the stack.

            ILGenerator.Emit(OpCodes.Pop);
        }
    }

    private void GenerateDeclarationStatement(LocalDeclarationStatementSyntax localDeclarationStatement)
    {
        foreach (var declarator in localDeclarationStatement.Declaration.Declarators)
        {
            GenerateDeclarator(localDeclarationStatement, declarator);
        }
    }

    private void GenerateDeclarator(LocalDeclarationStatementSyntax localDeclarationStatement, VariableDeclaratorSyntax declarator)
    {
        if (declarator.Initializer is not null)
        {
            var localSymbol = GetDeclaredSymbol<ILocalSymbol>(declarator);

            new ExpressionGenerator(this, declarator.Initializer.Value).Generate();

            var localBuilder = GetLocal(localSymbol);

            ILGenerator.Emit(OpCodes.Stloc, localBuilder);
        }
    }
}