using System.Collections;
using System.Linq;
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

            case BoundAssignmentStatement assignmentStatement:
                EmitAssignmentStatement(assignmentStatement);
                break;

            case BoundLocalDeclarationStatement localDeclarationStatement:
                EmitDeclarationStatement(localDeclarationStatement);
                break;

            case BoundIfStatement ifStatement:
                EmitIfStatement(ifStatement);
                break;

            case BoundWhileStatement whileStatement:
                EmitWhileStatement(whileStatement);
                break;

            case BoundForStatement forStatement:
                EmitForStatement(forStatement);
                break;

            case BoundBlockStatement blockStatement:
                EmitBlockStatement(blockStatement);
                break;
        }
    }

    private void EmitReturnStatement(BoundReturnStatement returnStatement)
    {
        if (returnStatement.Expression is not null)
        {
            new ExpressionGenerator(this, returnStatement.Expression).Emit();

            var expressionType = returnStatement.Expression.Type;
            var returnType = MethodSymbol.ReturnType;

            if (returnType.SpecialType == SpecialType.System_Unit)
            {
                // The method returns void in IL, so discard the unit value.
                ILGenerator.Emit(OpCodes.Pop);
            }
            else if (expressionType?.IsValueType == true &&
                     (returnType.SpecialType is SpecialType.System_Object || returnType is IUnionTypeSymbol))
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(expressionType));
            }
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void EmitExpressionStatement(BoundExpressionStatement expressionStatement)
    {
        var expression = expressionStatement.Expression;

        // Expression is an invocation expression that returns unit. 
        if (expression is BoundInvocationExpression { Type.SpecialType: SpecialType.System_Unit } expr)
        {
            // Emit method call without emitting unit value.
            new ExpressionGenerator(this, expression).EmitInvocationExpressionBase(expr);

            // No pop instruction required.
            return;
        }

        new ExpressionGenerator(this, expression).Emit();

        // Pop the result
        ILGenerator.Emit(OpCodes.Pop);
    }

    private void EmitAssignmentStatement(BoundAssignmentStatement assignmentStatement)
    {
        new ExpressionGenerator(this, assignmentStatement.Expression).Emit();
    }

    private void EmitIfStatement(BoundIfStatement ifStatement)
    {
        var elseLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        var scope = new Scope(this);
        EmitBranchOpForCondition(ifStatement.Condition, elseLabel, scope);

        new StatementGenerator(scope, ifStatement.ThenNode).Emit();

        ILGenerator.Emit(OpCodes.Br_S, endLabel);
        ILGenerator.MarkLabel(elseLabel);

        if (ifStatement.ElseNode is not null)
        {
            var scope2 = new Scope(this);
            new StatementGenerator(scope2, ifStatement.ElseNode).Emit();
        }

        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitWhileStatement(BoundWhileStatement whileStatement)
    {
        var beginLabel = ILGenerator.DefineLabel();
        var conditionLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Br_S, conditionLabel);

        ILGenerator.MarkLabel(beginLabel);
        ILGenerator.Emit(OpCodes.Nop);

        var scope = new Scope(this);
        new StatementGenerator(scope, whileStatement.Body).Emit();

        ILGenerator.MarkLabel(conditionLabel);
        EmitBranchOpForCondition(whileStatement.Condition, endLabel, scope);
        ILGenerator.Emit(OpCodes.Br_S, beginLabel);

        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitForStatement(BoundForStatement forStatement)
    {
        var beginLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        var scope = new Scope(this);

        new ExpressionGenerator(scope, forStatement.Collection).Emit();

        if (forStatement.Collection.Type is IArrayTypeSymbol)
        {
            var collectionLocal = ILGenerator.DeclareLocal(ResolveClrType(forStatement.Collection.Type));
            ILGenerator.Emit(OpCodes.Stloc, collectionLocal);

            var indexLocal = ILGenerator.DeclareLocal(ResolveClrType(Compilation.GetSpecialType(SpecialType.System_Int32)));
            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            ILGenerator.Emit(OpCodes.Stloc, indexLocal);

            var elementLocal = ILGenerator.DeclareLocal(ResolveClrType(forStatement.Local.Type));
            scope.AddLocal(forStatement.Local, elementLocal);

            ILGenerator.MarkLabel(beginLabel);

            ILGenerator.Emit(OpCodes.Ldloc, indexLocal);
            ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
            ILGenerator.Emit(OpCodes.Ldlen);
            ILGenerator.Emit(OpCodes.Conv_I4);
            ILGenerator.Emit(OpCodes.Bge, endLabel);

            ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
            ILGenerator.Emit(OpCodes.Ldloc, indexLocal);
            EmitLoadElement(forStatement.Local.Type);
            ILGenerator.Emit(OpCodes.Stloc, elementLocal);

            new StatementGenerator(scope, forStatement.Body).Emit();

            ILGenerator.Emit(OpCodes.Ldloc, indexLocal);
            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            ILGenerator.Emit(OpCodes.Add);
            ILGenerator.Emit(OpCodes.Stloc, indexLocal);

            ILGenerator.Emit(OpCodes.Br_S, beginLabel);
            ILGenerator.MarkLabel(endLabel);
        }
        else
        {
            var enumerable = Compilation.GetTypeByMetadataName("System.Collections.IEnumerable");
            var clrType = ResolveClrType(enumerable);
            ILGenerator.Emit(OpCodes.Castclass, clrType);
            var getEnumerator = (PEMethodSymbol)enumerable.GetMembers(nameof(IEnumerable.GetEnumerator)).First()!;
            ILGenerator.Emit(OpCodes.Callvirt, getEnumerator.GetMethodInfo());
            var enumeratorType = getEnumerator.ReturnType;
            var enumeratorLocal = ILGenerator.DeclareLocal(clrType);
            ILGenerator.Emit(OpCodes.Stloc, enumeratorLocal);

            var elementLocal = ILGenerator.DeclareLocal(ResolveClrType(forStatement.Local.Type));
            scope.AddLocal(forStatement.Local, elementLocal);

            ILGenerator.MarkLabel(beginLabel);

            var moveNext = (PEMethodSymbol)enumeratorType.GetMembers(nameof(IEnumerator.MoveNext))!.First();
            ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
            ILGenerator.Emit(OpCodes.Callvirt, moveNext.GetMethodInfo());
            ILGenerator.Emit(OpCodes.Brfalse, endLabel);

            var currentProp = (PEMethodSymbol)enumeratorType.GetMembers(nameof(IEnumerator.Current)).OfType<PEPropertySymbol>().First()!.GetMethod!;
            ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
            ILGenerator.Emit(OpCodes.Callvirt, currentProp.GetMethodInfo());

            var localClr = ResolveClrType(forStatement.Local.Type);
            if (localClr.IsValueType)
                ILGenerator.Emit(OpCodes.Unbox_Any, localClr);
            else
                ILGenerator.Emit(OpCodes.Castclass, localClr);
            ILGenerator.Emit(OpCodes.Stloc, elementLocal);

            new StatementGenerator(scope, forStatement.Body).Emit();

            ILGenerator.Emit(OpCodes.Br_S, beginLabel);
            ILGenerator.MarkLabel(endLabel);
        }
    }

    private void EmitBlockStatement(BoundBlockStatement blockStatement)
    {
        foreach (var s in blockStatement.Statements)
            new StatementGenerator(this, s).Emit();
    }

    private void EmitBranchOpForCondition(BoundExpression expression, Label end, Scope scope)
    {
        if (expression is BoundParenthesizedExpression parenthesizedExpression)
        {
            EmitBranchOpForCondition(parenthesizedExpression.Expression, end, scope);
            return;
        }

        if (expression is BoundBinaryExpression binaryExpression)
        {
            new ExpressionGenerator(scope, binaryExpression.Left).Emit();
            new ExpressionGenerator(scope, binaryExpression.Right).Emit();

            switch (binaryExpression.Operator.OperatorKind)
            {
                case BinaryOperatorKind.Equality:
                    ILGenerator.Emit(OpCodes.Ceq);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case BinaryOperatorKind.Inequality:
                    ILGenerator.Emit(OpCodes.Ceq);
                    ILGenerator.Emit(OpCodes.Ldc_I4_0);
                    ILGenerator.Emit(OpCodes.Ceq);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case BinaryOperatorKind.GreaterThan:
                    ILGenerator.Emit(OpCodes.Cgt);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case BinaryOperatorKind.GreaterThanOrEqual:
                    ILGenerator.Emit(OpCodes.Clt);
                    ILGenerator.Emit(OpCodes.Ldc_I4_0);
                    ILGenerator.Emit(OpCodes.Ceq);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case BinaryOperatorKind.LessThan:
                    ILGenerator.Emit(OpCodes.Clt);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case BinaryOperatorKind.LessThanOrEqual:
                    ILGenerator.Emit(OpCodes.Cgt);
                    ILGenerator.Emit(OpCodes.Ldc_I4_0);
                    ILGenerator.Emit(OpCodes.Ceq);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                default:
                    new ExpressionGenerator(scope, expression).Emit();
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;
            }
            return;
        }

        new ExpressionGenerator(scope, expression).Emit();
        ILGenerator.Emit(OpCodes.Brfalse_S, end);
    }

    private void EmitLoadElement(ITypeSymbol elementType)
    {
        var clrType = ResolveClrType(elementType);

        if (clrType.IsValueType)
            ILGenerator.Emit(OpCodes.Ldelem, clrType);
        else
            ILGenerator.Emit(OpCodes.Ldelem_Ref);
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
            var expressionType = declarator.Initializer.Type;
            var localSymbol = declarator.Local;

            // If the local wasn't declared (e.g., the initializer returns early),
            // there's nothing to store.
            if (localBuilder is null)
                return;

            if (expressionType is not null &&
                localSymbol.Type is not null &&
                expressionType.IsValueType &&
                (localSymbol.Type.SpecialType is SpecialType.System_Object || localSymbol.Type is IUnionTypeSymbol))
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(expressionType));
            }

            ILGenerator.Emit(OpCodes.Stloc, localBuilder);
        }
    }
}
