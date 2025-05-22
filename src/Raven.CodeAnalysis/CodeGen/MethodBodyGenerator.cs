using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class MethodBodyGenerator
{
    private readonly IDictionary<ISymbol, LocalBuilder> _localBuilders = new Dictionary<ISymbol, LocalBuilder>(SymbolEqualityComparer.Default);
    private TypeBuilder _typeBuilder;
    private MethodBuilder _methodBuilder;
    private Compilation _compilation;
    private IMethodSymbol _methodSymbol;

    public MethodBodyGenerator(MethodGenerator methodGenerator)
    {
        MethodGenerator = methodGenerator;
    }

    public Compilation Compilation => _compilation ??= MethodGenerator.Compilation;
    public MethodGenerator MethodGenerator { get; }
    public IMethodSymbol MethodSymbol => _methodSymbol ??= MethodGenerator.MethodSymbol;
    public TypeBuilder TypeBuilder => _typeBuilder ??= MethodGenerator.TypeGenerator.TypeBuilder!;
    public MethodBuilder MethodBuilder => _methodBuilder ??= MethodGenerator.MethodBuilder;
    public ILGenerator ILGenerator { get; private set; }

    public IEnumerable<LocalBuilder> GetLocalBuilders() => _localBuilders.Values;

    public void Generate()
    {
        ILGenerator = MethodBuilder.GetILGenerator();

        var syntax = MethodSymbol.DeclaringSyntaxReferences.First().GetSyntax();

        var semanticModel = Compilation.GetSemanticModel(syntax.SyntaxTree);

        foreach (var localDeclStmt in syntax.DescendantNodes().OfType<LocalDeclarationStatementSyntax>())
        {
            foreach (var localDeclarator in localDeclStmt.Declaration.Declarators)
            {
                var localSymbol = GetDeclaredSymbol<ILocalSymbol>(localDeclarator);

                var clrType = localSymbol.Type.GetClrType(_compilation);
                var builder = ILGenerator.DeclareLocal(clrType);
                builder.SetLocalSymInfo(localSymbol.Name);

                _localBuilders[localSymbol] = builder;
            }
        }

        if (syntax is CompilationUnitSyntax compilationUnit)
        {
            var statements = compilationUnit.Members.OfType<GlobalStatementSyntax>()
                .Select(x => x.Statement);

            GenerateIL(statements);
        }
        else if (syntax is MethodDeclarationSyntax methodDeclaration)
        {
            GenerateIL(methodDeclaration.Body.Statements.ToList());
        }
    }

    private void GenerateIL(IEnumerable<StatementSyntax> statements)
    {
        foreach (var statement in statements)
        {
            GenerateStatement(statement);
        }

        ILGenerator.Emit(OpCodes.Nop);
        ILGenerator.Emit(OpCodes.Ret);
    }

    private void GenerateStatement(StatementSyntax statement)
    {
        switch (statement)
        {
            case ReturnStatementSyntax returnStatement:
                GenerateReturnStatement(statement, returnStatement);
                break;

            case ExpressionStatementSyntax expressionStatement:
                GenerateExpressionStatement(statement, expressionStatement);
                break;

            case LocalDeclarationStatementSyntax localDeclarationStatement:
                GenerateDeclarationStatement(statement, localDeclarationStatement);
                break;
        }
    }

    private void GenerateReturnStatement(StatementSyntax statement, ReturnStatementSyntax returnStatement)
    {
        if (returnStatement.Expression is ExpressionSyntax expression)
        {
            GenerateExpression(statement, expression);
        }

        ILGenerator.Emit(OpCodes.Ret);
    }

    private void GenerateBlock(BlockSyntax block)
    {
        foreach (var s in block.Statements)
        {
            GenerateStatement(s);
        }
    }

    private void GenerateIfStatement(StatementSyntax statement, IfExpressionSyntax ifStatementSyntax)
    {
        GenerateExpression(statement, ifStatementSyntax.Condition);

        var elseLabel = ILGenerator.DefineLabel();

        GenerateBranchOpForCondition(ifStatementSyntax.Condition, ILGenerator, elseLabel);

        GenerateExpression(statement, ifStatementSyntax.Expression);

        if (ifStatementSyntax.ElseClause is ElseClauseSyntax elseClause)
        {
            // Define a label for the end of the 'if' statement
            var endIfLabel = ILGenerator.DefineLabel();

            // Branch to end of 'if' after the 'if' block
            ILGenerator.Emit(OpCodes.Br_S, endIfLabel);

            // Mark the 'else' label
            ILGenerator.MarkLabel(elseLabel);

            // Generate the 'else' block
            GenerateExpression(statement, elseClause.Expression);

            // Mark the end of the 'if' statement
            ILGenerator.MarkLabel(endIfLabel);
        }
        else
        {
            // If no 'else' block, mark the 'else' label
            ILGenerator.MarkLabel(elseLabel);
        }
    }

    private static void GenerateBranchOpForCondition(ExpressionSyntax expression, ILGenerator ILGenerator, Label end)
    {
        if (expression is ParenthesizedExpressionSyntax parenthesizedExpression)
        {
            GenerateBranchOpForCondition(parenthesizedExpression.Expression, ILGenerator, end);
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
    }


    private void GenerateWhileStatement(StatementSyntax statement, WhileExpressionSyntax whileStatementSyntax)
    {
        var beginLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        ILGenerator.MarkLabel(beginLabel);

        GenerateExpression(statement, whileStatementSyntax.Condition);

        GenerateBranchOpForCondition(whileStatementSyntax.Condition, ILGenerator, endLabel);

        GenerateStatement(whileStatementSyntax.Statement);

        ILGenerator.Emit(OpCodes.Br_S, beginLabel);

        //End
        ILGenerator.MarkLabel(endLabel);
    }

    private void GenerateExpressionStatement(StatementSyntax statement, ExpressionStatementSyntax expressionStatement)
    {
        GenerateExpression(statement, expressionStatement.Expression);

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

    private void GenerateDeclarationStatement(StatementSyntax statement, LocalDeclarationStatementSyntax localDeclarationStatement)
    {
        foreach (var declarator in localDeclarationStatement.Declaration.Declarators)
        {
            GenerateDeclarator(statement, localDeclarationStatement, declarator);
        }
    }

    private void GenerateDeclarator(StatementSyntax statement, LocalDeclarationStatementSyntax localDeclarationStatement, VariableDeclaratorSyntax declarator)
    {
        if (declarator.Initializer is not null)
        {
            var localSymbol = GetDeclaredSymbol<ILocalSymbol>(declarator);

            GenerateExpression(statement, declarator.Initializer.Value);

            var localBuilder = _localBuilders[localSymbol];

            ILGenerator.Emit(OpCodes.Stloc, localBuilder);
        }
    }

    private void GenerateExpression(StatementSyntax statement, ExpressionSyntax expression)
    {
        switch (expression)
        {
            case BinaryExpressionSyntax binaryExpression:
                GenerateBinaryExpression(statement, binaryExpression);
                break;

            case MemberAccessExpressionSyntax memberAccessExpression:
                GenerateMemberAccessExpression(statement, memberAccessExpression);
                break;

            case InvocationExpressionSyntax invocationExpression:
                GenerateInvocationExpression(statement, invocationExpression);
                break;

            case IdentifierNameSyntax identifierName:
                GenerateNameExpression(identifierName);
                break;

            case LiteralExpressionSyntax literalExpression:
                GenerateLiteralExpression(literalExpression);
                break;

            case ParenthesizedExpressionSyntax parenthesized:
                GenerateExpression(statement, parenthesized.Expression);
                break;

            case IfExpressionSyntax ifStatementSyntax:
                GenerateIfStatement(statement, ifStatementSyntax);
                break;

            case WhileExpressionSyntax whileStatement:
                GenerateWhileStatement(statement, whileStatement);
                break;

            case BlockSyntax block:
                GenerateBlock(block);
                break;

            case AssignmentExpressionSyntax assignmentExpression:
                GenerateAssignmentExpression(statement, assignmentExpression);
                break;

            case ObjectCreationExpressionSyntax objectCreationExpression:
                GenerateObjectCreationExpression(statement, objectCreationExpression);
                break;

            case CollectionExpressionSyntax collectionExpression:
                GenerateCollectionExpression(statement, collectionExpression);
                break;

            case ElementAccessExpressionSyntax elementAccessExpression:
                GenerateElementAccessExpression(statement, elementAccessExpression);
                break;
        }
    }

    private void GenerateCollectionExpression(StatementSyntax statement, CollectionExpressionSyntax collectionExpression)
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
                GenerateExpression(statement, element.Expression);
                ILGenerator.Emit(OpCodes.Stelem_I4);
                index++;
            }
        }
    }

    private void GenerateElementAccessExpression(StatementSyntax statement, ElementAccessExpressionSyntax elementAccessExpression)
    {
        var target = GetSymbolInfo(elementAccessExpression.Expression).Symbol;

        if (target is ILocalSymbol localSymbol
            && localSymbol.Type is IArrayTypeSymbol arrayTypeSymbol)
        {
            GenerateExpression(statement, elementAccessExpression.Expression);

            foreach (var argument in elementAccessExpression.ArgumentList.Arguments.Reverse())
            {
                GenerateExpression(statement, argument.Expression);
            }

            ILGenerator.Emit(OpCodes.Ldelem_I4);
        }
        else if (target is IParameterSymbol parameterSymbol
                 && parameterSymbol.Type is IArrayTypeSymbol arrayTypeSymbol2)
        {
            if (parameterSymbol is PEParameterSymbol parameterSymbol2)
            {
                foreach (var argument in elementAccessExpression.ArgumentList.Arguments.Reverse())
                {
                    GenerateExpression(statement, argument.Expression);
                }

                ILGenerator.Emit(OpCodes.Ldelem_I4);
            }
        }
    }

    private void GenerateObjectCreationExpression(StatementSyntax statement, ObjectCreationExpressionSyntax objectCreationExpression)
    {
        foreach (var argument in objectCreationExpression.ArgumentList.Arguments.Reverse())
        {
            GenerateExpression(statement, argument.Expression);
        }

        var target = GetSymbolInfo(objectCreationExpression).Symbol as PEMethodSymbol;

        ILGenerator.Emit(OpCodes.Newobj, target.GetConstructorInfo());
    }

    private void GenerateAssignmentExpression(StatementSyntax statement, AssignmentExpressionSyntax assignmentExpression)
    {
        var symbol = GetSymbolInfo(assignmentExpression.LeftHandSide).Symbol;

        if (assignmentExpression.LeftHandSide is ElementAccessExpressionSyntax elementAccessExpression)
        {
            var symbol2 = GetSymbolInfo(elementAccessExpression.Expression).Symbol;

            if (symbol2 is ILocalSymbol localSymbol)
            {
                var localBuilder = _localBuilders[localSymbol];
                ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
            }
            else if (symbol2 is IParameterSymbol parameterSymbol)
            {
                if (parameterSymbol is PEParameterSymbol parameterSymbol2)
                {
                    ILGenerator.Emit(OpCodes.Ldarg, parameterSymbol2.GetParameterInfo().Position);
                }
            }

            foreach (var argument in elementAccessExpression.ArgumentList.Arguments)
            {
                GenerateExpression(statement, argument.Expression);
            }

            GenerateExpression(statement, assignmentExpression.RightHandSide);

            ILGenerator.Emit(OpCodes.Stelem_I4);
        }
        else
        {
            if (symbol is ILocalSymbol localSymbol)
            {
                GenerateExpression(statement, assignmentExpression.RightHandSide);

                var localBuilder = _localBuilders[localSymbol];

                ILGenerator.Emit(OpCodes.Stloc, localBuilder);
            }
            else if (symbol is IParameterSymbol parameterSymbol)
            {
                if (parameterSymbol is PEParameterSymbol parameterSymbol2)
                {
                    GenerateExpression(statement, assignmentExpression.RightHandSide);

                    ILGenerator.Emit(OpCodes.Starg, parameterSymbol2.GetParameterInfo().Position);
                }
            }
        }
    }

    private SymbolInfo GetSymbolInfo(SyntaxNode syntaxNode)
    {
        return Compilation
                        .GetSemanticModel(syntaxNode.SyntaxTree)
                        .GetSymbolInfo(syntaxNode);
    }

    private TNode? GetDeclaredSymbol<TNode>(SyntaxNode syntaxNode)
        where TNode : class, ISymbol
    {
        return Compilation
                        .GetSemanticModel(syntaxNode.SyntaxTree)
                        .GetDeclaredSymbol(syntaxNode) as TNode;
    }

    private void GenerateBinaryExpression(StatementSyntax statement, BinaryExpressionSyntax binaryExpression)
    {
        GenerateExpression(statement, binaryExpression.LeftHandSide);
        GenerateExpression(statement, binaryExpression.RightHandSide);

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

    private void GenerateMemberAccessExpression(StatementSyntax statement, MemberAccessExpressionSyntax memberAccessExpression)
    {
        var symbol = GetSymbolInfo(memberAccessExpression).Symbol;

        if (symbol is IPropertySymbol propertySymbol)
        {
            // First load the target expression (e.g., the array object)
            GenerateExpression(statement, memberAccessExpression.Expression);

            if (propertySymbol.ContainingType!.SpecialType == SpecialType.System_Array && propertySymbol.Name == "Length")
            {
                ILGenerator.Emit(OpCodes.Ldlen);
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

    private void GenerateInvocationExpression(StatementSyntax statement, InvocationExpressionSyntax invocationExpression)
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

                var localBuilder = _localBuilders[localSymbol];

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

                GenerateExpression(statement, expr);

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
            GenerateExpression(statement, argument.Expression);
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
            var localBuilder = _localBuilders[localSymbol];

            ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
        }
        else if (symbol is IParameterSymbol parameterSymbol)
        {
            if (parameterSymbol is PEParameterSymbol parameterSymbol2)
            {
                ILGenerator.Emit(OpCodes.Ldarg, parameterSymbol2.GetParameterInfo().Position);
            }
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
                    var v = literalExpression.Token.ValueText;
                    ILGenerator.Emit(OpCodes.Ldstr, v.Substring(1, v.Length - 2));
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

            default:
                throw new Exception("Not supported");
        }
    }
}