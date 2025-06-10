using System.Reflection.Emit;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class StatementGenerator : Generator
{
    private readonly BoundStatement _statement;

    public StatementGenerator(Generator parent, BoundStatement statement) : base(parent)
    {
        _statement = statement;
    }

    public override void Generate()
    {
        switch (_statement)
        {
            case BoundReturnStatement returnStatement:
                GenerateReturnStatement(returnStatement);
                break;

            case BoundExpressionStatement expressionStatement:
                GenerateExpressionStatement(expressionStatement);
                break;

            case BoundLocalDeclarationStatement localDeclarationStatement:
                GenerateDeclarationStatement(localDeclarationStatement);
                break;
        }
    }

    private void GenerateReturnStatement(BoundReturnStatement returnStatement)
    {
        new ExpressionGenerator(this, returnStatement.Expression).Generate();

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void GenerateExpressionStatement(BoundExpressionStatement expressionStatement)
    {
        var expression = expressionStatement.Expression;

        new ExpressionGenerator(this, expression).Generate();

        ISymbol? symbol = expressionStatement.Symbol;

        if (expressionStatement.Expression is BoundInvocationExpression invocationExpression)
        {
            symbol = ((IMethodSymbol)expressionStatement.Symbol).ReturnType;
        }

        // TODO: Handle the case that Pop is required. If not Void, and not assigned anywhere.

        if (symbol is not null && symbol?.UnwrapType()?.SpecialType is not SpecialType.System_Void)
        {
            // The value is not used, pop it from the stack.

            ILGenerator.Emit(OpCodes.Pop);
        }
    }

    private void GenerateDeclarationStatement(BoundLocalDeclarationStatement localDeclarationStatement)
    {
        foreach (var declarator in localDeclarationStatement.Declarators)
        {
            GenerateDeclarator(localDeclarationStatement, declarator);
        }
    }

    private void GenerateDeclarator(BoundLocalDeclarationStatement localDeclarationStatement, BoundVariableDeclarator declarator)
    {
        if (declarator.Initializer is not null)
        {
            new ExpressionGenerator(this, declarator.Initializer).Generate();

            var localBuilder = GetLocal(declarator.Local);

            var s = declarator.Initializer.Type;

            var localSymbol = declarator.Local;

            if (s.TypeKind is TypeKind.Struct && (localSymbol.Type.SpecialType is SpecialType.System_Object || localSymbol.Type is IUnionTypeSymbol))
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(s));
            }

            ILGenerator.Emit(OpCodes.Stloc, localBuilder);
        }
    }
}