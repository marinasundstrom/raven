using System.Reflection;
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
                GenerateIfExpression(ifStatementSyntax);
                break;

            case WhileExpressionSyntax whileStatement:
                GenerateWhileExpression(whileStatement);
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

            case LambdaExpressionSyntax lambdaExpression:
                var info = GetTypeInfo(lambdaExpression);
                var x = info.Type.GetMembers().OfType<IMethodSymbol>().First();
                var z = x.Parameters.First();
                var t = z.Type;

                var y = ResolveClrType(info.Type);
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
            var typeSymbol = typeInfo.Type;
            var clrType = ResolveClrType(typeSymbol);

            GenerateDesignation(declarationPattern.Designation);

            var patternLocal = GetLocal(declarationPattern.Designation);

            // [expr]
            if (typeSymbol.TypeKind is TypeKind.Struct)
            {
                // Reference types can use isinst + cgt.un directly.
                var labelFail = ILGenerator.DefineLabel();
                var labelDone = ILGenerator.DefineLabel();

                ILGenerator.Emit(OpCodes.Dup);                     // keep original value
                ILGenerator.Emit(OpCodes.Isinst, clrType);         // type test
                ILGenerator.Emit(OpCodes.Brfalse_S, labelFail);    // if not, jump to false

                ILGenerator.Emit(OpCodes.Unbox_Any, clrType);      // unbox value
                ILGenerator.Emit(OpCodes.Stloc, patternLocal);     // store into pattern variable
                ILGenerator.Emit(OpCodes.Ldc_I4_1);                // push true
                ILGenerator.Emit(OpCodes.Br_S, labelDone);

                ILGenerator.MarkLabel(labelFail);
                ILGenerator.Emit(OpCodes.Pop);                     // discard original expr
                ILGenerator.Emit(OpCodes.Ldc_I4_0);                // push false

                ILGenerator.MarkLabel(labelDone);
            }
            else
            {
                // Reference type flow — same as before
                ILGenerator.Emit(OpCodes.Isinst, clrType);         // cast or null
                ILGenerator.Emit(OpCodes.Dup);
                ILGenerator.Emit(OpCodes.Stloc, patternLocal);     // assign
                ILGenerator.Emit(OpCodes.Ldnull);
                ILGenerator.Emit(OpCodes.Cgt_Un);                  // bool: not-null
            }
        }
        else if (pattern is UnaryPatternSyntax unaryPatternSyntax)
        {
            GeneratePattern(unaryPatternSyntax.Pattern);

            if (unaryPatternSyntax.IsKind(SyntaxKind.NotPattern))
            {
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq); // logical NOT
            }
            else
            {
                throw new NotSupportedException("Unsupported unary pattern kind");
            }
        }
        else if (pattern is BinaryPatternSyntax binaryPattern)
        {
            var labelFail = ILGenerator.DefineLabel();
            var labelDone = ILGenerator.DefineLabel();

            if (binaryPattern.IsKind(SyntaxKind.AndPattern))
            {
                GeneratePattern(binaryPattern.Left);
                ILGenerator.Emit(OpCodes.Brfalse_S, labelFail);

                GeneratePattern(binaryPattern.Right);
                ILGenerator.Emit(OpCodes.Brfalse_S, labelFail);

                ILGenerator.Emit(OpCodes.Ldc_I4_1);
                ILGenerator.Emit(OpCodes.Br_S, labelDone);

                ILGenerator.MarkLabel(labelFail);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);

                ILGenerator.MarkLabel(labelDone);
            }
            else if (binaryPattern.IsKind(SyntaxKind.OrPattern))
            {
                var labelTrue = ILGenerator.DefineLabel();

                GeneratePattern(binaryPattern.Left);
                ILGenerator.Emit(OpCodes.Brtrue_S, labelTrue);

                GeneratePattern(binaryPattern.Right);
                ILGenerator.Emit(OpCodes.Brtrue_S, labelTrue);

                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Br_S, labelDone);

                ILGenerator.MarkLabel(labelTrue);
                ILGenerator.Emit(OpCodes.Ldc_I4_1);

                ILGenerator.MarkLabel(labelDone);
            }
            else
            {
                throw new NotSupportedException("Unsupported binary pattern kind");
            }
        }
        else
        {
            throw new NotSupportedException("Unsupported pattern");
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

            var local = ILGenerator.DeclareLocal(ResolveClrType(symbol.Type)); // resolve type
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
            ILGenerator.Emit(OpCodes.Newarr, ResolveClrType(arrayTypeSymbol.ElementType));

            int index = 0;
            foreach (var element in collectionExpression.Elements)
            {
                ILGenerator.Emit(OpCodes.Dup);
                ILGenerator.Emit(OpCodes.Ldc_I4, index);
                GenerateExpression(element.Expression);

                if (arrayTypeSymbol.ElementType.TypeKind is not TypeKind.Struct)
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

            if (arrayTypeSymbol.ElementType.TypeKind is not TypeKind.Struct)
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

            if (arrayTypeSymbol2.ElementType.TypeKind is not TypeKind.Struct)
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

        var t = GetSymbolInfo(objectCreationExpression);

        var target = t.Symbol switch
        {
            PEMethodSymbol a => a.GetConstructorInfo(),
            SubstitutedMethodSymbol m => m.GetConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen),
            _ => throw new Exception()
        };

        ILGenerator.Emit(OpCodes.Newobj, target);
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

            if (type.ElementType.TypeKind is not TypeKind.Struct)
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

                if (s.TypeKind is TypeKind.Struct
                    && (localSymbol.Type.SpecialType is SpecialType.System_Object || localSymbol.Type.IsUnion))
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(s));
                }

                ILGenerator.Emit(OpCodes.Stloc, localBuilder);
            }
            else if (symbol is IParameterSymbol parameterSymbol)
            {
                GenerateExpression(assignmentExpression.RightHandSide);

                var parameterBuilder = MethodGenerator.GetParameterBuilder(parameterSymbol);

                var s = GetTypeInfo(assignmentExpression.RightHandSide).Type;

                if (s.TypeKind is TypeKind.Struct && parameterSymbol.Type.SpecialType is SpecialType.System_Object)
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(s));
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

            if (memberAccessExpression.Expression is not null)
            {
                GenerateExpression(memberAccessExpression.Expression);
            }

            if (propertySymbol.ContainingType!.SpecialType is SpecialType.System_Array && propertySymbol.Name == "Length")
            {
                ILGenerator.Emit(OpCodes.Ldlen);
                ILGenerator.Emit(OpCodes.Conv_I4);
            }
            else
            {
                var property = propertySymbol switch
                {
                    PEPropertySymbol pEProperty => pEProperty.GetPropertyInfo(),
                    SubstitutedPropertySymbol substitutedProperty => substitutedProperty.GetPropertyInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen),
                    _ => null
                };

                if (property?.GetMethod is null)
                    throw new Exception($"Cannot resolve getter for property {propertySymbol.Name}");

                // Value types need address loading
                if (!propertySymbol.IsStatic && propertySymbol.ContainingType.TypeKind is TypeKind.Struct)
                {
                    var clrType = ResolveClrType(propertySymbol.ContainingType);
                    var tmp = ILGenerator.DeclareLocal(clrType);
                    ILGenerator.Emit(OpCodes.Stloc, tmp);
                    ILGenerator.Emit(OpCodes.Ldloca, tmp);
                }

                ILGenerator.Emit(OpCodes.Callvirt, property?.GetMethod!);
            }
        }
        else if (symbol is IFieldSymbol fieldSymbol)
        {
            // First load the target expression (e.g., the array object)

            if (memberAccessExpression.Expression is not null)
            {
                GenerateExpression(memberAccessExpression.Expression);
            }

            // Value types need address loading
            if (!fieldSymbol.IsStatic && fieldSymbol.ContainingType.TypeKind is TypeKind.Struct)
            {
                var clrType = ResolveClrType(fieldSymbol.ContainingType);
                var tmp = ILGenerator.DeclareLocal(clrType);
                ILGenerator.Emit(OpCodes.Stloc, tmp);
                ILGenerator.Emit(OpCodes.Ldloca, tmp);
            }

            if (fieldSymbol.IsLiteral)
            {
                var constant = fieldSymbol.GetConstantValue();
                switch (constant)
                {
                    case int i:
                        ILGenerator.Emit(OpCodes.Ldc_I4, i);
                        break;
                    case bool b:
                        ILGenerator.Emit(b ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                        break;
                    case null:
                        ILGenerator.Emit(OpCodes.Ldnull);
                        break;
                    default:
                        throw new NotSupportedException($"Literal value type not supported: {constant?.GetType()}");
                }
            }
            else
            {
                var opCode = fieldSymbol.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld;

                if (fieldSymbol is PEFieldSymbol peFieldSymbol)
                {
                    ILGenerator.Emit(opCode, peFieldSymbol.GetFieldInfo());
                }
                else if (fieldSymbol is SourceFieldSymbol sourceFieldSymbol)
                {
                    ILGenerator.Emit(opCode, (FieldInfo)GetMemberBuilder(sourceFieldSymbol)!);
                }
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

        var target = GetSymbolInfo(invocationExpression).Symbol as IMethodSymbol;

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

                if (localSymbol.Type.TypeKind is TypeKind.Struct)
                {
                    if (target.IsVirtual || target.ContainingType.TypeKind == TypeKind.Interface)
                    {
                        // Loading the address of the value to the instance.

                        ILGenerator.Emit(OpCodes.Ldloca, localBuilder);
                    }
                    else
                    {
                        ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
                        ILGenerator.Emit(OpCodes.Box, localBuilder.LocalType);
                    }
                }
                else
                {
                    // Since it's a reference type, the address is stored in the local.

                    ILGenerator.Emit(OpCodes.Ldloc, localBuilder);

                    if (localBuilder.LocalType.IsValueType)
                    {
                        ILGenerator.Emit(OpCodes.Box, localBuilder.LocalType);
                    }
                }
            }
            else
            {
                // It's an expression.

                GenerateExpression(expr);

                if (target.ContainingType.TypeKind is TypeKind.Struct)
                {
                    var clrType = ResolveClrType(target.ContainingType);
                    var builder = ILGenerator.DeclareLocal(clrType);
                    //_localBuilders[target] = builder;

                    ILGenerator.Emit(OpCodes.Stloc, builder);
                    ILGenerator.Emit(OpCodes.Ldloca, builder);
                }
            }
        }

        var paramSymbols = target.Parameters.Reverse().ToArray();
        int pi = 0;

        foreach (var argument in invocationExpression.ArgumentList.Arguments.Reverse())
        {
            var paramSymbol = paramSymbols[pi];
            var argType = GetTypeInfo(argument.Expression)?.Type;
            var paramType = paramSymbol.Type;

            GenerateExpression(argument.Expression);

            if (argType is { TypeKind: TypeKind.Struct or TypeKind.Enum } && paramType is { TypeKind: not TypeKind.Struct })
            {
                // Box value type before passing it to reference type parameter
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(argType));
            }

            pi++;
        }

        if (target?.IsStatic ?? false)
        {
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(target));
        }
        else if (target.IsVirtual || target.ContainingType.TypeKind == TypeKind.Interface)
        {
            ILGenerator.Emit(OpCodes.Callvirt, GetMethodInfo(target));
        }
        else
        {
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(target));
        }

        if (target.Name == "GetType"
            && target.ContainingType.Name == "Object"
            && target.ContainingNamespace.Name == "System")
        {
            var x = Compilation.ReferencedAssemblySymbols
                .First(x => x.Name == "System.Runtime")
                .GetTypeByMetadataName("System.Reflection.MemberInfo");

            ILGenerator.Emit(OpCodes.Castclass, ResolveClrType(x));
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
                var constant = fieldSymbol.GetConstantValue();
                switch (constant)
                {
                    case int i:
                        ILGenerator.Emit(OpCodes.Ldc_I4, i);
                        break;
                    case bool b:
                        ILGenerator.Emit(b ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                        break;
                    case null:
                        ILGenerator.Emit(OpCodes.Ldnull);
                        break;
                    default:
                        throw new NotSupportedException($"Literal value type not supported: {constant?.GetType()}");
                }
            }
            else
            {
                if (fieldSymbol.IsLiteral)
                {
                    var constant = fieldSymbol.GetConstantValue();
                    switch (constant)
                    {
                        case int i:
                            ILGenerator.Emit(OpCodes.Ldc_I4, i);
                            break;
                        case bool b:
                            ILGenerator.Emit(b ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                            break;
                        case null:
                            ILGenerator.Emit(OpCodes.Ldnull);
                            break;
                        default:
                            throw new NotSupportedException($"Literal value type not supported: {constant?.GetType()}");
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
        }
        else if (symbol is IPropertySymbol propertySymbol)
        {
            if (propertySymbol.ContainingType!.Name == "Array") //.SpecialType is SpecialType.System_Array)
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
                    && propertySymbol.ContainingType.TypeKind is TypeKind.Struct)
                {
                    var clrType = ResolveClrType(propertySymbol.ContainingType);
                    var builder = ILGenerator.DeclareLocal(clrType);
                    //_localBuilders[symbol] = builder;

                    ILGenerator.Emit(OpCodes.Stloc, builder);
                    ILGenerator.Emit(OpCodes.Ldloca, builder);
                }

                ILGenerator.Emit(OpCodes.Callvirt, getMethod.GetMethodInfo());
            }
        }
    }

    private void GenerateLiteralExpression(LiteralExpressionSyntax literalExpression)
    {
        switch (literalExpression.Kind)
        {
            case SyntaxKind.NumericLiteralExpression:
                {
                    if (literalExpression.Token.Value is int)
                    {
                        ILGenerator.Emit(OpCodes.Ldc_I4, (int)literalExpression.Token.Value);
                    }
                    else if (literalExpression.Token.Value is long)
                    {
                        ILGenerator.Emit(OpCodes.Ldc_I8, (long)literalExpression.Token.Value);
                    }
                    else if (literalExpression.Token.Value is float)
                    {
                        ILGenerator.Emit(OpCodes.Ldc_R4, (float)literalExpression.Token.Value);
                    }
                    else if (literalExpression.Token.Value is double)
                    {
                        ILGenerator.Emit(OpCodes.Ldc_R8, (double)literalExpression.Token.Value);
                    }
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

    private void GenerateIfExpression(IfExpressionSyntax ifStatementSyntax)
    {
        var elseLabel = ILGenerator.DefineLabel();

        GenerateBranchOpForCondition(ifStatementSyntax.Condition, elseLabel);

        bool isAssigned = ifStatementSyntax.Parent is ExpressionSyntax or EqualsValueClauseSyntax;

        var ifStatementType = GetTypeInfo(ifStatementSyntax);
        var thenType = GetTypeInfo(ifStatementSyntax.Expression);

        var scope = new Scope(this);
        new ExpressionGenerator(scope, ifStatementSyntax.Expression).Generate();

        if (isAssigned
            && ifStatementType.Type.IsUnion
            && thenType.Type.TypeKind is TypeKind.Struct)
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(thenType.Type));
        }

        if (ifStatementSyntax.ElseClause is ElseClauseSyntax elseClause)
        {
            // Define a label for the end of the 'if' statement
            var endIfLabel = ILGenerator.DefineLabel();

            // Branch to end of 'if' after the 'if' block
            ILGenerator.Emit(OpCodes.Br_S, endIfLabel);

            // Mark the 'else' label
            ILGenerator.MarkLabel(elseLabel);

            var elsType = GetTypeInfo(ifStatementSyntax.ElseClause.Expression);

            // Generate the 'else' block
            var scope2 = new Scope(this);
            new ExpressionGenerator(scope2, elseClause.Expression).Generate();

            if (isAssigned
                && ifStatementType.Type.IsUnion
                && elsType.Type.TypeKind is TypeKind.Struct)
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(elsType.Type));
            }

            // Mark the end of the 'if' statement
            ILGenerator.MarkLabel(endIfLabel);
        }
        else
        {
            // If no 'else' block, mark the 'else' label
            ILGenerator.MarkLabel(elseLabel);
        }
    }

    private void GenerateWhileExpression(WhileExpressionSyntax whileStatementSyntax)
    {
        var beginLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        ILGenerator.MarkLabel(beginLabel);

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
            GenerateExpression(binaryExpression.LeftHandSide);
            GenerateExpression(binaryExpression.RightHandSide);

            switch (binaryExpression.Kind)
            {
                case SyntaxKind.EqualsExpression:
                    ILGenerator.Emit(OpCodes.Ceq); // compare
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case SyntaxKind.NotEqualsExpression:
                    ILGenerator.Emit(OpCodes.Ceq);
                    ILGenerator.Emit(OpCodes.Ldc_I4_0);
                    ILGenerator.Emit(OpCodes.Ceq); // logical NOT
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case SyntaxKind.GreaterThanExpression:
                    ILGenerator.Emit(OpCodes.Cgt);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case SyntaxKind.LessThanExpression:
                    ILGenerator.Emit(OpCodes.Clt);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case SyntaxKind.GreaterThanOrEqualExpression:
                    ILGenerator.Emit(OpCodes.Clt);
                    ILGenerator.Emit(OpCodes.Brtrue_S, end);
                    break;

                case SyntaxKind.LessThanOrEqualExpression:
                    ILGenerator.Emit(OpCodes.Cgt);
                    ILGenerator.Emit(OpCodes.Brtrue_S, end);
                    break;

                default:
                    throw new NotSupportedException($"Unsupported binary condition: {binaryExpression.Kind}");
            }
        }
        else if (expression is LiteralExpressionSyntax literalExpression)
        {
            if (literalExpression.Kind == SyntaxKind.TrueLiteralExpression)
            {
                // If true, do nothing; execution continues
            }
            else if (literalExpression.Kind == SyntaxKind.FalseLiteralExpression)
            {
                ILGenerator.Emit(OpCodes.Br_S, end);
            }
        }
        else if (expression is IdentifierNameSyntax or MemberAccessExpressionSyntax)
        {
            GenerateExpression(expression);
            ILGenerator.Emit(OpCodes.Brfalse_S, end);
        }
        else
        {
            GenerateExpression(expression);
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

    public MethodInfo GetMethodInfo(IMethodSymbol methodSymbol)
    {
        if (methodSymbol is PEMethodSymbol pEMethodSymbol)
            return pEMethodSymbol.GetMethodInfo();

        if (methodSymbol is SubstitutedMethodSymbol substitutedMethod)
            return substitutedMethod.GetMethodInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen);

        if (methodSymbol is SourceMethodSymbol sourceMethodSymbol)
            return MethodGenerator.TypeGenerator.MethodGenerators.First(x => x.MethodSymbol == sourceMethodSymbol).MethodBuilder;

        throw new InvalidOperationException();
    }
}