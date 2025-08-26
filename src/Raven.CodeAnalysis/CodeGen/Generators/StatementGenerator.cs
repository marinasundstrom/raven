using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class StatementGenerator : Generator
{
    private readonly BoundStatement _statement;

    public StatementGenerator(Generator parent, BoundStatement statement) : base(parent)
    {
        _statement = statement;
    }

    public override void Emit()
    {
        switch (_statement)
        {
            case BoundReturnStatement returnStatement:
                EmitReturnStatement(returnStatement);
                break;

            case BoundExpressionStatement expressionStatement:
                EmitExpressionStatement(expressionStatement);
                break;

            case BoundLocalDeclarationStatement localDeclarationStatement:
                EmitDeclarationStatement(localDeclarationStatement);
                break;
        }
    }

    private void EmitReturnStatement(BoundReturnStatement returnStatement)
    {
        if (returnStatement.Expression is { } expr && expr is not BoundUnitExpression)
        {
            new ExpressionGenerator(this, expr).Emit();
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitExpressionStatement(BoundExpressionStatement expressionStatement)
    {
        var expression = expressionStatement.Expression;

        new ExpressionGenerator(this, expression).Emit();

        ISymbol? symbol = expressionStatement.Symbol;

        if (expressionStatement.Expression is BoundInvocationExpression invocationExpression)
        {
            symbol = ((IMethodSymbol)expressionStatement.Symbol).ReturnType;
        }

        // TODO: Handle the case that Pop is required. If not Void, and not assigned anywhere.

        var type = symbol?.UnwrapType();
        if (type is not null &&
            type.SpecialType is not SpecialType.System_Void)
        {
            // The value is not used, pop it from the stack.
            ILGenerator.Emit(OpCodes.Pop);
        }
    }

    private void EmitDeclarationStatement(BoundLocalDeclarationStatement localDeclarationStatement)
    {
        foreach (var declarator in localDeclarationStatement.Declarators)
        {
            EmitDeclarator(localDeclarationStatement, declarator);
        }
    }

    private void EmitDeclarator(BoundLocalDeclarationStatement localDeclarationStatement, BoundVariableDeclarator declarator)
    {
        if (declarator.Initializer is not null)
        {
            new ExpressionGenerator(this, declarator.Initializer).Emit();

            var localBuilder = GetLocal(declarator.Local);

            var s = declarator.Initializer.Type;

            var localSymbol = declarator.Local;

            if (s.IsValueType && (localSymbol.Type.SpecialType is SpecialType.System_Object || localSymbol.Type is IUnionTypeSymbol))
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(s));
            }

            ILGenerator.Emit(OpCodes.Stloc, localBuilder);
        }
    }
}
