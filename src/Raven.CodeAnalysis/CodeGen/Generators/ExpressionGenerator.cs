using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class ExpressionGenerator : Generator
{
    private readonly ExpressionSyntax _expression;

    public ExpressionGenerator(Generator parent, ExpressionSyntax expression) : base(parent)
    {
        _expression = expression;
    }

    public override void Generate()
    {
        GenerateExpression(_expression);
    }

    private void GenerateExpression(ExpressionSyntax expression)
    {
        switch (expression)
        {
            case BinaryExpressionSyntax binaryExpression:
                GenerateBinaryExpression(binaryExpression);
                break;

            case MemberAccessExpressionSyntax memberAccessExpression:
                GenerateMemberAccessExpression(memberAccessExpression);
                break;

            case InvocationExpressionSyntax invocationExpression:
                GenerateInvocationExpression(invocationExpression);
                break;

            case IdentifierNameSyntax identifierName:
                GenerateNameExpression(identifierName);
                break;

            case LiteralExpressionSyntax literalExpression:
                GenerateLiteralExpression(literalExpression);
                break;

            case ParenthesizedExpressionSyntax parenthesized:
                GenerateExpression(parenthesized.Expression);
                break;

            case IfExpressionSyntax ifStatementSyntax:
                GenerateIfStatement(ifStatementSyntax);
                break;

            case WhileExpressionSyntax whileStatement:
                GenerateWhileStatement(whileStatement);
                break;

            case BlockSyntax block:
                GenerateBlock(block);
                break;

            case AssignmentExpressionSyntax assignmentExpression:
                GenerateAssignmentExpression(assignmentExpression);
                break;

            case ObjectCreationExpressionSyntax objectCreationExpression:
                GenerateObjectCreationExpression(objectCreationExpression);
                break;

            case CollectionExpressionSyntax collectionExpression:
                GenerateCollectionExpression(collectionExpression);
                break;

            case ElementAccessExpressionSyntax elementAccessExpression:
                GenerateElementAccessExpression(elementAccessExpression);
                break;

            case IsPatternExpressionSyntax isPatternExpression:
                GenerateIsPatternExpression(isPatternExpression);
                break;

            default:
                throw new NotSupportedException("Unsupported expression type");
        }
    }

    private void GenerateIsPatternExpression(IsPatternExpressionSyntax isPatternExpression)
    {
        GenerateExpression(isPatternExpression.Expression); // Push the value of the expression onto the stack

        GeneratePattern(isPatternExpression.Pattern);       // Evaluate the pattern; leaves a boolean on the stack
    }

    private void GeneratePattern(PatternSyntax pattern)
    {
        if (pattern is DeclarationPatternSyntax declarationPattern)
        {
            var typeInfo = GetTypeInfo(declarationPattern.Type);
            var clrType = typeInfo.Type.GetClrType(Compilation);

            GenerateDesignation(declarationPattern.Designation);

            // Assume you already declared the pattern local (e.g., "str")
            var patternLocal = GetLocal(declarationPattern.Designation);

            // Stack: [expr]
            ILGenerator.Emit(OpCodes.Isinst, clrType);          // cast or null
            ILGenerator.Emit(OpCodes.Stloc, patternLocal);      // str = casted or null
            ILGenerator.Emit(OpCodes.Ldloc, patternLocal);      // load str again
            ILGenerator.Emit(OpCodes.Ldnull);                   // compare against null
            ILGenerator.Emit(OpCodes.Cgt_Un);                   // 1 if not null, 0 if null — push result (bool)
        }
    }

    private LocalBuilder GetLocal(VariableDesignationSyntax designation)
    {
        // Create or retrieve a LocalBuilder for the variable name
        if (designation is SingleVariableDesignationSyntax single)
        {
            var symbol = GetDeclaredSymbol<ILocalSymbol>(single);

            return GetLocal(symbol); // assuming `locals` is a dictionary
        }

        throw new NotSupportedException("Unsupported designation");
    }

    private void GenerateDesignation(VariableDesignationSyntax designation)
    {
        if (designation is SingleVariableDesignationSyntax single)
        {
            var symbol = GetDeclaredSymbol<ILocalSymbol>(single);

            var local = ILGenerator.DeclareLocal(symbol.Type.GetClrType(Compilation)); // resolve type
            local.SetLocalSymInfo(single.Identifier.Text);

            AddLocal(symbol, local);
        }
    }

    private void GenerateCollectionExpression(CollectionExpressionSyntax collectionExpression)
    {
        var target = GetSymbolInfo(collectionExpression).Symbol;

        if (target is IArrayTypeSymbol arrayTypeSymbol)
        {
            ILGenerator.Emit(OpCodes.Ldc_I4, collectionExpression.Elements.Count);
            ILGenerator.Emit(OpCodes.Newarr, arrayTypeSymbol.ElementType.GetClrType(Compilation));

            int index = 0;
            foreach (var element in collectionExpression.Elements)
            {
                ILGenerator.Emit(OpCodes.Dup);
                ILGenerator.Emit(OpCodes.Ldc_I4, index);
                GenerateExpression(element.Expression);

                if (!arrayTypeSymbol.ElementType.IsValueType)
                {
                    ILGenerator.Emit(OpCodes.Stelem_Ref);
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Stelem_I4);
                }

                index++;
            }
        }
    }

    private void GenerateElementAccessExpression(ElementAccessExpressionSyntax elementAccessExpression)
    {
        var target = GetSymbolInfo(elementAccessExpression.Expression).Symbol;

        if (target is ILocalSymbol localSymbol
            && localSymbol.Type is IArrayTypeSymbol arrayTypeSymbol)
        {
            GenerateExpression(elementAccessExpression.Expression);

            foreach (var argument in elementAccessExpression.ArgumentList.Arguments.Reverse())
            {
                GenerateExpression(argument.Expression);
            }

            if (!arrayTypeSymbol.ElementType.IsValueType)
            {
                ILGenerator.Emit(OpCodes.Ldelem_Ref);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldelem_I4);
            }
        }
        else if (target is IParameterSymbol parameterSymbol
                 && parameterSymbol.Type is IArrayTypeSymbol arrayTypeSymbol2)
        {
            GenerateExpression(elementAccessExpression.Expression);

            foreach (var argument in elementAccessExpression.ArgumentList.Arguments.Reverse())
            {
                GenerateExpression(argument.Expression);
            }

            if (!arrayTypeSymbol2.ElementType.IsValueType)
            {
                ILGenerator.Emit(OpCodes.Ldelem_Ref);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldelem_I4);
            }
        }
    }

    private void GenerateObjectCreationExpression(ObjectCreationExpressionSyntax objectCreationExpression)
    {
        foreach (var argument in objectCreationExpression.ArgumentList.Arguments.Reverse())
        {
            GenerateExpression(argument.Expression);
        }

        var target = GetSymbolInfo(objectCreationExpression).Symbol as PEMethodSymbol;

        ILGenerator.Emit(OpCodes.Newobj, target.GetConstructorInfo());
    }

    private void GenerateAssignmentExpression(AssignmentExpressionSyntax assignmentExpression)
    {
        var symbol = GetSymbolInfo(assignmentExpression.LeftHandSide).Symbol;

        if (assignmentExpression.LeftHandSide is ElementAccessExpressionSyntax elementAccessExpression)
        {
            var symbol2 = GetSymbolInfo(elementAccessExpression.Expression).Symbol;

            if (symbol2 is ILocalSymbol localSymbol)
            {
                var localBuilder = GetLocal(localSymbol);
                ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
            }
            else if (symbol2 is IParameterSymbol parameterSymbol)
            {
                var parameterBuilder = MethodGenerator.GetParameterBuilder(parameterSymbol);

                int position = parameterBuilder.Position;

                if (MethodSymbol.IsStatic)
                {
                    position -= 1;
                }

                ILGenerator.Emit(OpCodes.Ldarg, position);
            }

            foreach (var argument in elementAccessExpression.ArgumentList.Arguments)
            {
                GenerateExpression(argument.Expression);
            }

            GenerateExpression(assignmentExpression.RightHandSide);

            var type = symbol2.UnwrapType() as IArrayTypeSymbol;

            if (!type.ElementType.IsValueType)
            {
                ILGenerator.Emit(OpCodes.Stelem_Ref);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Stelem_I4);
            }
        }
        else
        {
            if (symbol is ILocalSymbol localSymbol)
            {
                GenerateExpression(assignmentExpression.RightHandSide);

                var s = GetTypeInfo(assignmentExpression.RightHandSide).Type;

                var localBuilder = GetLocal(localSymbol);

                if (s.IsValueType && localSymbol.Type.SpecialType == SpecialType.System_Object)
                {
                    ILGenerator.Emit(OpCodes.Box, s.GetClrType(Compilation));
                }

                ILGenerator.Emit(OpCodes.Stloc, localBuilder);
            }
            else if (symbol is IParameterSymbol parameterSymbol)
            {
                GenerateExpression(assignmentExpression.RightHandSide);

                var parameterBuilder = MethodGenerator.GetParameterBuilder(parameterSymbol);

                var s = GetTypeInfo(assignmentExpression.RightHandSide).Type;

                if (s.IsValueType && parameterSymbol.Type.SpecialType == SpecialType.System_Object)
                {
                    ILGenerator.Emit(OpCodes.Box, s.GetClrType(Compilation));
                }

                int position = parameterBuilder.Position;

                if (MethodSymbol.IsStatic)
                {
                    position -= 1;
                }

                ILGenerator.Emit(OpCodes.Starg, position);
            }
        }
    }

    private void GenerateBinaryExpression(BinaryExpressionSyntax binaryExpression)
    {
        GenerateExpression(binaryExpression.LeftHandSide);
        GenerateExpression(binaryExpression.RightHandSide);

        var semanticModel = Compilation.GetSemanticModel(binaryExpression.SyntaxTree!);

        var methodSymbol = semanticModel.GetSymbolInfo(binaryExpression).Symbol as IMethodSymbol;

        if (methodSymbol is not null)
        {
            var concatMethod = methodSymbol as PEMethodSymbol;

            ILGenerator.Emit(OpCodes.Call, concatMethod.GetMethodInfo());
            return;
        }

        switch (binaryExpression.Kind)
        {
            case SyntaxKind.AddExpression:
                ILGenerator.Emit(OpCodes.Add);
                break;

            case SyntaxKind.SubtractExpression:
                ILGenerator.Emit(OpCodes.Sub);
                break;

            case SyntaxKind.MultiplyExpression:
                ILGenerator.Emit(OpCodes.Mul);
                break;

            case SyntaxKind.DivideExpression:
                ILGenerator.Emit(OpCodes.Div);
                break;

            case SyntaxKind.ModuloExpression:
                ILGenerator.Emit(OpCodes.Rem);
                break;
        }
    }

    private void GenerateMemberAccessExpression(MemberAccessExpressionSyntax memberAccessExpression)
    {
        var symbol = GetSymbolInfo(memberAccessExpression).Symbol;

        if (symbol is IPropertySymbol propertySymbol)
        {
            // First load the target expression (e.g., the array object)
            GenerateExpression(memberAccessExpression.Expression);

            if (propertySymbol.ContainingType!.SpecialType == SpecialType.System_Array && propertySymbol.Name == "Length")
            {
                ILGenerator.Emit(OpCodes.Ldlen);
                ILGenerator.Emit(OpCodes.Conv_I4);
            }
            else
            {
                var metadataPropertySymbol = propertySymbol as PEPropertySymbol;
                var getMethod = metadataPropertySymbol?.GetMethod as PEMethodSymbol;

                if (getMethod is null)
                    throw new Exception($"Cannot resolve getter for property {propertySymbol.Name}");

                // Value types need address loading
                if (!propertySymbol.IsStatic && propertySymbol.ContainingType.IsValueType)
                {
                    var clrType = propertySymbol.ContainingType.GetClrType(Compilation);
                    var tmp = ILGenerator.DeclareLocal(clrType);
                    ILGenerator.Emit(OpCodes.Stloc, tmp);
                    ILGenerator.Emit(OpCodes.Ldloca, tmp);
                }

                ILGenerator.Emit(OpCodes.Call, getMethod.GetMethodInfo());
            }
        }
        else
        {
            throw new Exception($"Unsupported member access: {memberAccessExpression}");
        }
    }

    private void GenerateInvocationExpression(InvocationExpressionSyntax invocationExpression)
    {
        // Resolve target identifier or access
        // If method or delegate, then invoke

        var target = GetSymbolInfo(invocationExpression).Symbol as PEMethodSymbol;

        if (!target?.IsStatic ?? false)
        {
            // Instance member invocation

            var expr = invocationExpression.Expression;
            if (invocationExpression.Expression is MemberAccessExpressionSyntax e)
            {
                // Get the target Expression. Ignores the Name, which is the method.
                expr = e.Expression;
            }

            var localSymbol = GetSymbolInfo(expr).Symbol as ILocalSymbol;

            if (localSymbol is not null)
            {
                // A local

                var localBuilder = GetLocal(localSymbol);

                if (localSymbol.Type.IsValueType)
                {
                    // Loading the address of the value to the instance.

                    ILGenerator.Emit(OpCodes.Ldloca, localBuilder);
                }
                else
                {
                    // Since it's a reference type, the address is stored in the local.

                    ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
                }
            }
            else
            {
                // It's an expression.

                GenerateExpression(expr);

                if (target.ContainingType.IsValueType)
                {
                    var clrType = target.ContainingType.GetClrType(Compilation);
                    var builder = ILGenerator.DeclareLocal(clrType);
                    //_localBuilders[target] = builder;

                    ILGenerator.Emit(OpCodes.Stloc, builder);
                    ILGenerator.Emit(OpCodes.Ldloca, builder);
                }
            }
        }

        foreach (var argument in invocationExpression.ArgumentList.Arguments.Reverse())
        {
            GenerateExpression(argument.Expression);
        }

        if (target?.IsStatic ?? false)
        {
            ILGenerator.Emit(OpCodes.Call, target.GetMethodInfo());
        }
        else
        {
            if (target.ContainingType.IsValueType)
            {
                ILGenerator.Emit(OpCodes.Call, target.GetMethodInfo());
            }
            else
            {
                ILGenerator.Emit(OpCodes.Callvirt, target.GetMethodInfo());
            }
        }
    }

    private void GenerateNameExpression(IdentifierNameSyntax identifierName)
    {
        // Resolve target identifier or access
        // If local, property, or field, then load

        var symbol = GetSymbolInfo(identifierName).Symbol;

        if (symbol is ILocalSymbol localSymbol)
        {
            var localBuilder = GetLocal(localSymbol);

            ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
        }
        else if (symbol is IParameterSymbol parameterSymbol)
        {
            var parameterBuilder = MethodGenerator.GetParameterBuilder(parameterSymbol);

            int position = parameterBuilder.Position;

            if (MethodSymbol.IsStatic)
            {
                position -= 1;
            }

            ILGenerator.Emit(OpCodes.Ldarg, position);
        }
        else if (symbol is IFieldSymbol fieldSymbol)
        {
            var metadataFieldSymbol = fieldSymbol as PEFieldSymbol;

            if (fieldSymbol.IsLiteral)
            {
                if (fieldSymbol.Type.SpecialType == SpecialType.System_Int32)
                {
                    ILGenerator.Emit(OpCodes.Ldc_I4, (int)metadataFieldSymbol.GetConstantValue()!);
                }
                else
                {
                    throw new Exception("Unsupported constant type");
                }
            }
            else
            {
                if (metadataFieldSymbol.IsStatic)
                {
                    ILGenerator.Emit(OpCodes.Ldsfld, metadataFieldSymbol.GetFieldInfo());
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Ldfld, metadataFieldSymbol.GetFieldInfo());
                }
            }
        }
        else if (symbol is IPropertySymbol propertySymbol)
        {
            if (propertySymbol.ContainingType!.Name == "Array") //.SpecialType == SpecialType.System_Array)
            {
                if (propertySymbol.Name == "Length")
                {
                    ILGenerator.Emit(OpCodes.Ldlen);
                    ILGenerator.Emit(OpCodes.Conv_I4);
                }
            }
            else
            {
                var metadataPropertySymbol = propertySymbol as PEPropertySymbol;
                var getMethod = metadataPropertySymbol.GetMethod as PEMethodSymbol;

                if (!propertySymbol.IsStatic
                    && propertySymbol.ContainingType.IsValueType)
                {
                    var clrType = propertySymbol.ContainingType.GetClrType(Compilation);
                    var builder = ILGenerator.DeclareLocal(clrType);
                    //_localBuilders[symbol] = builder;

                    ILGenerator.Emit(OpCodes.Stloc, builder);
                    ILGenerator.Emit(OpCodes.Ldloca, builder);
                }

                ILGenerator.Emit(OpCodes.Call, getMethod.GetMethodInfo());
            }
        }
    }

    private void GenerateLiteralExpression(LiteralExpressionSyntax literalExpression)
    {
        switch (literalExpression.Kind)
        {
            case SyntaxKind.NumericLiteralExpression:
                {
                    ILGenerator.Emit(OpCodes.Ldc_I4, (int)literalExpression.Token.Value);
                    break;
                }

            case SyntaxKind.StringLiteralExpression:
                {
                    var v = literalExpression.Token.Value;
                    ILGenerator.Emit(OpCodes.Ldstr, (string)v);
                    break;
                }

            case SyntaxKind.TrueLiteralExpression:
                {
                    ILGenerator.Emit(OpCodes.Ldc_I4_1);
                    break;
                }

            case SyntaxKind.FalseLiteralExpression:
                {
                    ILGenerator.Emit(OpCodes.Ldc_I4_0);
                    break;
                }

            case SyntaxKind.NullLiteralExpression:
                {
                    ILGenerator.Emit(OpCodes.Ldnull);
                    break;
                }

            default:
                throw new Exception("Not supported");
        }
    }

    private void GenerateIfStatement(IfExpressionSyntax ifStatementSyntax)
    {
        GenerateExpression(ifStatementSyntax.Condition);

        var elseLabel = ILGenerator.DefineLabel();

        GenerateBranchOpForCondition(ifStatementSyntax.Condition, elseLabel);

        var scope = new Scope(this);
        new ExpressionGenerator(scope, ifStatementSyntax.Expression).Generate();

        if (ifStatementSyntax.ElseClause is ElseClauseSyntax elseClause)
        {
            // Define a label for the end of the 'if' statement
            var endIfLabel = ILGenerator.DefineLabel();

            // Branch to end of 'if' after the 'if' block
            ILGenerator.Emit(OpCodes.Br_S, endIfLabel);

            // Mark the 'else' label
            ILGenerator.MarkLabel(elseLabel);

            // Generate the 'else' block
            var scope2 = new Scope(this);
            new ExpressionGenerator(scope2, elseClause.Expression).Generate();

            // Mark the end of the 'if' statement
            ILGenerator.MarkLabel(endIfLabel);
        }
        else
        {
            // If no 'else' block, mark the 'else' label
            ILGenerator.MarkLabel(elseLabel);
        }
    }

    private void GenerateWhileStatement(WhileExpressionSyntax whileStatementSyntax)
    {
        var beginLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        ILGenerator.MarkLabel(beginLabel);

        GenerateExpression(whileStatementSyntax.Condition);

        GenerateBranchOpForCondition(whileStatementSyntax.Condition, endLabel);

        var scope = new Scope(this);
        new StatementGenerator(scope, whileStatementSyntax.Statement).Generate();

        ILGenerator.Emit(OpCodes.Br_S, beginLabel);

        //End
        ILGenerator.MarkLabel(endLabel);
    }


    private void GenerateBranchOpForCondition(ExpressionSyntax expression, Label end)
    {
        if (expression is ParenthesizedExpressionSyntax parenthesizedExpression)
        {
            GenerateBranchOpForCondition(parenthesizedExpression.Expression, end);
            return;
        }

        if (expression is BinaryExpressionSyntax binaryExpression)
        {
            switch (binaryExpression.Kind)
            {
                case SyntaxKind.EqualsExpression:
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case SyntaxKind.NotEqualsExpression:
                    ILGenerator.Emit(OpCodes.Neg);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case SyntaxKind.GreaterThanExpression:
                    ILGenerator.Emit(OpCodes.Ble_S, end);
                    break;

                case SyntaxKind.LessThanExpression:
                    ILGenerator.Emit(OpCodes.Bge_S, end);
                    break;
            }
        }
        else if (expression is LiteralExpressionSyntax literalExpression)
        {
            if (literalExpression.Kind == SyntaxKind.TrueLiteralExpression)
            {
                ILGenerator.Emit(OpCodes.Brfalse_S, end);
            }
            else if (literalExpression.Kind == SyntaxKind.FalseLiteralExpression)
            {
                ILGenerator.Emit(OpCodes.Neg);
                ILGenerator.Emit(OpCodes.Brfalse_S, end);
            }
        }
        else if (expression is IdentifierNameSyntax identifierName)
        {
            ILGenerator.Emit(OpCodes.Brfalse_S, end);
        }
        else
        {
            ILGenerator.Emit(OpCodes.Brfalse_S, end);
        }
    }

    private void GenerateBlock(BlockSyntax block)
    {
        foreach (var s in block.Statements)
        {
            GenerateStatement(s);
        }
    }

    private void GenerateStatement(StatementSyntax statement)
    {
        new StatementGenerator(this, statement).Generate();
    }
}
