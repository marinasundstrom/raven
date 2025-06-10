using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal class ExpressionGenerator : Generator
{
    private readonly BoundExpression _expression;

    public ExpressionGenerator(Generator parent, BoundExpression expression) : base(parent)
    {
        _expression = expression;
    }

    public override void Generate()
    {
        GenerateExpression(_expression);
    }

    private void GenerateExpression(BoundExpression expression)
    {
        switch (expression)
        {
            case BoundBinaryExpression binaryExpression:
                GenerateBinaryExpression(binaryExpression);
                break;

            case BoundUnaryExpression unaryExpression:
                GenerateUnaryExpression(unaryExpression);
                break;

            case BoundAddressOfExpression addressOfExpression:
                GenerateAddressOfExpression(addressOfExpression);
                break;

            case BoundParameterAccess parameterAccess:
                GenerateParameterAccess(parameterAccess);
                break;

            case BoundLocalAccess localAccess:
                GenerateLocalAccess(localAccess);
                break;

            case BoundPropertyAccess propertyAccess:
                GeneratePropertyAccess(propertyAccess);
                break;

            case BoundMemberAccessExpression memberAccessExpression:
                GenerateMemberAccessExpression(memberAccessExpression);
                break;

            case BoundInvocationExpression invocationExpression:
                GenerateInvocationExpression(invocationExpression);
                break;

            case BoundLiteralExpression literalExpression:
                GenerateLiteralExpression(literalExpression);
                break;

            case BoundParenthesizedExpression parenthesized:
                GenerateExpression(parenthesized.Expression);
                break;

            case BoundIfExpression ifStatement:
                GenerateIfExpression(ifStatement);
                break;

            case BoundWhileExpression whileStatement:
                GenerateWhileExpression(whileStatement);
                break;

            case BoundBlockExpression block:
                GenerateBlock(block);
                break;

            case BoundAssignmentExpression assignmentExpression:
                GenerateAssignmentExpression(assignmentExpression);
                break;

            case BoundObjectCreationExpression objectCreationExpression:
                GenerateObjectCreationExpression(objectCreationExpression);
                break;

            case BoundCollectionExpression collectionExpression:
                GenerateCollectionExpression(collectionExpression);
                break;

            case BoundArrayAccessExpression boundArrayAccessExpression:
                GenerateArrayAccessExpression(boundArrayAccessExpression);
                break;

            case BoundIndexerAccessExpression boundIndexerAccessExpression:
                GenerateIndexerAccessExpression(boundIndexerAccessExpression);
                break;

            case BoundIsPatternExpression isPatternExpression:
                GenerateIsPatternExpression(isPatternExpression);
                break;

            case BoundTypeExpression:
                break;

            case BoundVoidExpression:
                break;

            case BoundLambdaExpression lambdaExpression:
                var x = lambdaExpression.Type.GetMembers().OfType<IMethodSymbol>().First();
                var z = x.Parameters.First();
                var t = z.Type;

                var y = ResolveClrType(lambdaExpression.Type);
                break;

            default:
                throw new NotSupportedException($"Unsupported expression type: {expression.GetType()}");
        }
    }

    private void GenerateLocalAccess(BoundLocalAccess localAccess)
    {
        ILGenerator.Emit(OpCodes.Ldloc, GetLocal(localAccess.Local));
    }

    private void GenerateParameterAccess(BoundParameterAccess parameterAccess)
    {
        int position = MethodGenerator.GetParameterBuilder(parameterAccess.Parameter).Position;
        if (MethodSymbol.IsStatic)
            position -= 1;

        ILGenerator.Emit(OpCodes.Ldarg, position);
    }

    private void GenerateUnaryExpression(BoundUnaryExpression node)
    {
        var operand = node.Operand;
        var op = node.Operator;

        switch (op.OperatorKind)
        {
            case BoundUnaryOperatorKind.UnaryMinus: // -x
                GenerateExpression(operand);
                ILGenerator.Emit(OpCodes.Neg);
                break;

            case BoundUnaryOperatorKind.UnaryPlus: // +x
                GenerateExpression(operand); // no-op
                break;

            case BoundUnaryOperatorKind.LogicalNot: // !x
                GenerateExpression(operand);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq);
                break;

            case BoundUnaryOperatorKind.BitwiseNot: // ~x
                GenerateExpression(operand);
                ILGenerator.Emit(OpCodes.Not);
                break;

            default:
                throw new NotSupportedException($"Unsupported unary operator");
        }
    }

    private void GenerateAddressOfExpression(BoundAddressOfExpression addressOf)
    {
        switch (addressOf.Symbol)
        {
            case ILocalSymbol local:
                ILGenerator.Emit(OpCodes.Ldloca, GetLocal(local));
                break;

            case IParameterSymbol param:
                int pos = MethodGenerator.GetParameterBuilder(param).Position;
                if (MethodSymbol.IsStatic)
                    pos -= 1;

                ILGenerator.Emit(OpCodes.Ldarga, pos);
                break;

            case IFieldSymbol field when !field.IsStatic:
                if (field.ContainingType.TypeKind is TypeKind.Struct)
                {
                    throw new NotSupportedException("Taking address of a field inside struct requires more handling (like loading enclosing struct by ref).");
                }

                // Assume it's on `this`
                ILGenerator.Emit(OpCodes.Ldarg_0);
                ILGenerator.Emit(OpCodes.Ldflda, ((PEFieldSymbol)field).GetFieldInfo());
                break;

            default:
                throw new NotSupportedException($"Cannot take address of: {addressOf.Symbol}");
        }
    }

    private void GenerateIsPatternExpression(BoundIsPatternExpression isPatternExpression)
    {
        GenerateExpression(isPatternExpression.Expression); // Push the value of the expression onto the stack

        GeneratePattern(isPatternExpression.Pattern);       // Evaluate the pattern; leaves a boolean on the stack
    }

    private void GeneratePattern(BoundPattern pattern)
    {
        if (pattern is BoundDeclarationPattern declarationPattern)
        {
            var typeSymbol = declarationPattern.Type;
            var clrType = ResolveClrType(typeSymbol);

            GenerateDesignation(declarationPattern.Designator);

            var patternLocal = GetLocal(declarationPattern.Designator);

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
        else if (pattern is BoundUnaryPattern unaryPattern)
        {
            GeneratePattern(unaryPattern.Pattern);

            if (unaryPattern.Kind == BoundUnaryPatternKind.Not)
            {
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq); // logical NOT
            }
            else
            {
                throw new NotSupportedException("Unsupported unary pattern kind");
            }
        }
        else if (pattern is BoundBinaryPattern binaryPattern)
        {
            var labelFail = ILGenerator.DefineLabel();
            var labelDone = ILGenerator.DefineLabel();

            if (binaryPattern.Kind == BoundPatternKind.And)
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
            else if (binaryPattern.Kind == BoundPatternKind.Or)
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

    private LocalBuilder GetLocal(BoundDesignator designation)
    {
        // Create or retrieve a LocalBuilder for the variable name
        if (designation is BoundSingleVariableDesignator single)
        {
            return GetLocal(single.Local); // assuming `locals` is a dictionary
        }

        throw new NotSupportedException("Unsupported designation");
    }

    private void GenerateDesignation(BoundDesignator designation)
    {
        if (designation is BoundSingleVariableDesignator single)
        {
            var symbol = single.Local;

            var local = ILGenerator.DeclareLocal(ResolveClrType(symbol.Type)); // resolve type
            local.SetLocalSymInfo(single.Local.Name);

            base.AddLocal(symbol, local);
        }
    }

    private void GenerateCollectionExpression(BoundCollectionExpression collectionExpression)
    {
        var target = collectionExpression.Type;

        if (target is IArrayTypeSymbol arrayTypeSymbol)
        {
            ILGenerator.Emit(OpCodes.Ldc_I4, collectionExpression.Elements.Length);
            ILGenerator.Emit(OpCodes.Newarr, ResolveClrType(arrayTypeSymbol.ElementType));

            int index = 0;
            foreach (var element in collectionExpression.Elements)
            {
                ILGenerator.Emit(OpCodes.Dup);
                ILGenerator.Emit(OpCodes.Ldc_I4, index);

                GenerateExpression(element);

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

    private void GenerateArrayAccessExpression(BoundArrayAccessExpression boundArrayAccessExpression)
    {
        var arrayType = boundArrayAccessExpression.Receiver.Type as IArrayTypeSymbol;

        GenerateExpression(boundArrayAccessExpression.Receiver);

        foreach (var argument in boundArrayAccessExpression.Indices)
        {
            GenerateExpression(argument);
        }

        EmitLoadElement(arrayType.ElementType);
    }

    private void GenerateIndexerAccessExpression(BoundIndexerAccessExpression boundIndexerAccessExpression)
    {
        var indexerProperty = boundIndexerAccessExpression.Symbol as IPropertySymbol;

        GenerateExpression(boundIndexerAccessExpression.Receiver);

        foreach (var argument in boundIndexerAccessExpression.Arguments)
        {
            GenerateExpression(argument);
        }

        var getter = indexerProperty switch
        {
            PEPropertySymbol pe => pe.GetPropertyInfo().GetMethod!,
            SubstitutedPropertySymbol sub => sub.GetPropertyInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen).GetMethod!,
            _ => throw new NotSupportedException("Unsupported indexer")
        };

        ILGenerator.Emit(OpCodes.Callvirt, getter);
    }

    private void EmitLoadElement(ITypeSymbol elementType)
    {
        if (elementType.TypeKind != TypeKind.Struct)
        {
            ILGenerator.Emit(OpCodes.Ldelem_Ref);
        }
        else
        {
            // Fallback: treat all structs as int for now
            ILGenerator.Emit(OpCodes.Ldelem_I4);
        }
    }

    private void GenerateObjectCreationExpression(BoundObjectCreationExpression objectCreationExpression)
    {
        var symbol = objectCreationExpression.Symbol;

        IMethodSymbol constructorSymbol = symbol switch
        {
            PEMethodSymbol a => a,
            SubstitutedMethodSymbol b => b,
            _ => throw new Exception("Unsupported constructor symbol")
        };

        var parameters = constructorSymbol.Parameters.ToArray();
        var arguments = objectCreationExpression.Arguments.ToArray();

        for (int i = 0; i < arguments.Length; i++)
        {
            var param = parameters[i];
            var argument = arguments[i];

            if (param.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
            {
                switch (argument)
                {
                    case BoundAddressOfExpression addr:
                        GenerateAddressOfExpression(addr);
                        break;

                    //case BoundLocal { Symbol: ILocalSymbol local }:
                    //    ILGenerator.Emit(OpCodes.Ldloca, GetLocal(local));
                    //    break;

                    //case BoundParameter { Symbol: IParameterSymbol parameter }:
                    //    ILGenerator.Emit(OpCodes.Ldarga, MethodGenerator.GetParameterBuilder(parameter).Position);
                    //    break;

                    default:
                        throw new NotSupportedException("Invalid argument for ref/out constructor parameter");
                }
            }
            else
            {
                GenerateExpression(argument);

                var argType = argument.Type;
                if (argType is { TypeKind: TypeKind.Struct or TypeKind.Enum } &&
                    param.Type.TypeKind != TypeKind.Struct)
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(argType));
                }
            }
        }

        var constructorInfo = symbol switch
        {
            PEMethodSymbol a => a.GetConstructorInfo(),
            SubstitutedMethodSymbol m => m.GetConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen),
            _ => throw new Exception()
        };

        ILGenerator.Emit(OpCodes.Newobj, constructorInfo);
    }

    private void GenerateAssignmentExpression(BoundAssignmentExpression node)
    {
        switch (node)
        {
            case BoundLocalAssignmentExpression localExpression:
                GenerateExpression(localExpression.Right);

                if (localExpression.Right.Type.TypeKind is TypeKind.Struct && localExpression.Type.SpecialType is SpecialType.System_Object)
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(localExpression.Right.Type));
                }

                ILGenerator.Emit(OpCodes.Stloc, GetLocal(localExpression.Local));
                break;

            case BoundArrayAssignmentExpression array:
                GenerateExpression(array.Left.Receiver);

                foreach (var index in array.Left.Indices)
                    GenerateExpression(index);

                GenerateExpression(array.Right);

                EmitStoreElement(((IArrayTypeSymbol)array.Left.Type).ElementType);
                break;

            case BoundIndexerAssignmentExpression indexer:
                GenerateExpression(indexer.Left.Receiver);

                foreach (var arg in indexer.Left.Arguments)
                    GenerateExpression(arg);

                GenerateExpression(indexer.Right);

                var setter = ((IPropertySymbol)indexer.Left.Symbol!) switch
                {
                    PEPropertySymbol pe => pe.GetPropertyInfo().SetMethod!,
                    SubstitutedPropertySymbol sub => sub.GetPropertyInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen).SetMethod!,
                    _ => throw new NotSupportedException("Unsupported indexer property")
                };

                ILGenerator.Emit(OpCodes.Callvirt, setter);
                break;

            case BoundMemberAssignmentExpression member:
                GenerateExpression(member.Receiver);
                GenerateExpression(member.Right);

                switch (member.Member)
                {
                    case IFieldSymbol field:
                        ILGenerator.Emit(OpCodes.Stfld, (FieldInfo)GetField(field));
                        break;
                    case IPropertySymbol prop:
                        var setterMethod = ((IPropertySymbol)member.Member) switch
                        {
                            PEPropertySymbol pe => pe.GetPropertyInfo().SetMethod!,
                            SubstitutedPropertySymbol sub => sub.GetPropertyInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen).SetMethod!,
                            _ => throw new NotSupportedException("Unsupported member property")
                        };
                        ILGenerator.Emit(OpCodes.Callvirt, setterMethod);
                        break;
                    default:
                        throw new NotSupportedException("Unsupported member assignment target");
                }
                break;

            default:
                throw new NotSupportedException($"Unknown BoundAssignmentExpression: {node.GetType().Name}");
        }
    }

    private FieldInfo GetField(IFieldSymbol fieldSymbol)
    {
        return fieldSymbol switch
        {
            PEFieldSymbol peFieldSymbol => peFieldSymbol.GetFieldInfo(),
            _ => throw new Exception("Unsupported field symbol")
        };
    }

    private void EmitStoreElement(ITypeSymbol elementType)
    {
        if (elementType.TypeKind != TypeKind.Struct)
        {
            ILGenerator.Emit(OpCodes.Stelem_Ref);
        }
        else
        {
            // Default fallback: assume int-like
            ILGenerator.Emit(OpCodes.Stelem_I4);
        }
    }

    private void GenerateBinaryExpression(BoundBinaryExpression binaryExpression)
    {
        GenerateExpression(binaryExpression.Left);
        GenerateExpression(binaryExpression.Right);

        var op = binaryExpression.Operator;

        switch (op.OperatorKind)
        {
            case BinaryOperatorKind.Addition:
                ILGenerator.Emit(OpCodes.Add);
                break;

            case BinaryOperatorKind.Subtraction:
                ILGenerator.Emit(OpCodes.Sub);
                break;

            case BinaryOperatorKind.Multiplication:
                ILGenerator.Emit(OpCodes.Mul);
                break;

            case BinaryOperatorKind.Division:
                ILGenerator.Emit(OpCodes.Div);
                break;

            //case BinaryOperatorKind.Modulo:
            //    ILGenerator.Emit(OpCodes.Rem);
            //    break;

            default:
                throw new InvalidOperationException("Invalid operator kind");
        }
    }

    private void GenerateMemberAccessExpression(BoundMemberAccessExpression memberAccessExpression)
    {
        var symbol = memberAccessExpression.Symbol;

        if (symbol is IPropertySymbol propertySymbol)
        {
            // First load the target expression (e.g., the array object)

            var receiver = memberAccessExpression.Receiver;

            if (receiver is not null && !symbol.IsStatic)
            {
                GenerateExpression(receiver);
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

                if (propertySymbol.IsStatic)
                {
                    ILGenerator.Emit(OpCodes.Call, property?.GetMethod!);
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Callvirt, property?.GetMethod!);
                }
            }
        }
        else if (symbol is IFieldSymbol fieldSymbol)
        {
            // First load the target expression (e.g., the array object)

            var receiver = memberAccessExpression.Receiver;

            if (receiver is not null)
            {
                GenerateExpression(receiver);
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

    private void GenerateInvocationExpression(BoundInvocationExpression invocationExpression)
    {
        var target = invocationExpression.Method;
        var receiver = invocationExpression.Receiver;

        // Emit receiver (for instance methods)
        if (!target.IsStatic)
        {
            if (receiver is BoundLocalAccess { Symbol: ILocalSymbol localSymbol })
            {
                var localBuilder = GetLocal(localSymbol);

                if (localSymbol.Type.TypeKind is TypeKind.Struct)
                {
                    var isGetType = target.Name == "GetType"
                        && target.ContainingType.Name == "Object"
                        && target.ContainingNamespace.Name == "System";

                    if (isGetType)
                    {
                        ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
                        ILGenerator.Emit(OpCodes.Box, localBuilder.LocalType);
                    }
                    else
                    {
                        ILGenerator.Emit(OpCodes.Ldloca, localBuilder);
                    }
                }
                else
                {
                    // For reference types, just load the value
                    ILGenerator.Emit(OpCodes.Ldloc, localBuilder);

                    // Box if the runtime type is a value type (e.g. captured as object)
                    if (localBuilder.LocalType.IsValueType)
                    {
                        ILGenerator.Emit(OpCodes.Box, localBuilder.LocalType);
                    }
                }
            }
            else
            {
                // General case: evaluate the receiver expression
                GenerateExpression(receiver);

                if (target.ContainingType.TypeKind is TypeKind.Struct)
                {
                    var isGetType = target.Name == "GetType"
                        && target.ContainingType.Name == "Object"
                        && target.ContainingNamespace.Name == "System";

                    if (isGetType)
                    {
                        var clrType = ResolveClrType(target.ContainingType);
                        var temp = ILGenerator.DeclareLocal(clrType);
                        ILGenerator.Emit(OpCodes.Stloc, temp);
                        ILGenerator.Emit(OpCodes.Ldloc, temp);
                        ILGenerator.Emit(OpCodes.Box, clrType);
                    }
                    else
                    {
                        var clrType = ResolveClrType(target.ContainingType);
                        var temp = ILGenerator.DeclareLocal(clrType);
                        ILGenerator.Emit(OpCodes.Stloc, temp);
                        ILGenerator.Emit(OpCodes.Ldloca, temp);
                    }
                }
            }
        }

        // Emit arguments (in left-to-right order)
        var paramSymbols = target.Parameters.ToArray();
        var args = invocationExpression.Arguments.ToArray();

        for (int i = 0; i < args.Length; i++)
        {
            var paramSymbol = paramSymbols[i];
            var argument = args[i];

            if (paramSymbol.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
            {
                switch (argument)
                {
                    case BoundAddressOfExpression addressOf:
                        GenerateAddressOfExpression(addressOf);
                        break;

                    case BoundLocalAccess { Symbol: ILocalSymbol local }:
                        ILGenerator.Emit(OpCodes.Ldloca, GetLocal(local));
                        break;

                    default:
                        throw new NotSupportedException($"Unsupported ref/out argument expression: {argument?.GetType().Name}");
                }
            }
            else
            {
                GenerateExpression(argument);

                var argType = argument?.Type;
                var paramType = paramSymbol.Type;

                if (argType is { TypeKind: TypeKind.Struct or TypeKind.Enum } &&
                    paramType.TypeKind != TypeKind.Struct)
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(argType));
                }
            }
        }

        // Determine correct call opcode
        var isValueType = target.ContainingType?.TypeKind is TypeKind.Struct or TypeKind.Enum;
        var isInterfaceCall = target.ContainingType?.TypeKind == TypeKind.Interface;

        if (target.IsStatic)
        {
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(target));
        }
        else if (!isValueType && (target.IsVirtual || isInterfaceCall))
        {
            ILGenerator.Emit(OpCodes.Callvirt, GetMethodInfo(target));
        }
        else
        {
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(target));
        }

        // Special cast for Object.GetType() to System.Reflection.MemberInfo
        if (target.Name == "GetType"
            && target.ContainingType.Name == "Object"
            && target.ContainingNamespace.Name == "System")
        {
            var memberInfo = Compilation.ReferencedAssemblySymbols
                .First(x => x.Name == "System.Runtime")
                .GetTypeByMetadataName("System.Reflection.MemberInfo");

            ILGenerator.Emit(OpCodes.Castclass, ResolveClrType(memberInfo));
        }
    }

    private void GenerateFieldAccess(BoundFieldAccess fieldAccess)
    {
        var fieldSymbol = fieldAccess.Field;
        var metadataFieldSymbol = fieldAccess.Field as PEFieldSymbol;

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

    private void GeneratePropertyAccess(BoundPropertyAccess propertyAccess)
    {
        var propertySymbol = propertyAccess.Property;

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

    private void GenerateLiteralExpression(BoundLiteralExpression literalExpression)
    {
        switch (literalExpression.Kind)
        {
            case BoundLiteralExpressionKind.NumericLiteral:
                {
                    if (literalExpression.Value is int)
                    {
                        ILGenerator.Emit(OpCodes.Ldc_I4, (int)literalExpression.Value);
                    }
                    else if (literalExpression.Value is long)
                    {
                        ILGenerator.Emit(OpCodes.Ldc_I8, (long)literalExpression.Value);
                    }
                    else if (literalExpression.Value is float)
                    {
                        ILGenerator.Emit(OpCodes.Ldc_R4, (float)literalExpression.Value);
                    }
                    else if (literalExpression.Value is double)
                    {
                        ILGenerator.Emit(OpCodes.Ldc_R8, (double)literalExpression.Value);
                    }
                    break;
                }

            case BoundLiteralExpressionKind.StringLiteral:
                {
                    var v = literalExpression.Value;
                    ILGenerator.Emit(OpCodes.Ldstr, (string)v);
                    break;
                }

            case BoundLiteralExpressionKind.TrueLiteral:
                {
                    ILGenerator.Emit(OpCodes.Ldc_I4_1);
                    break;
                }

            case BoundLiteralExpressionKind.FalseLiteral:
                {
                    ILGenerator.Emit(OpCodes.Ldc_I4_0);
                    break;
                }

            case BoundLiteralExpressionKind.NullLiteral:
                {
                    ILGenerator.Emit(OpCodes.Ldnull);
                    break;
                }

            default:
                throw new Exception("Not supported");
        }
    }

    private void GenerateIfExpression(BoundIfExpression ifStatement)
    {
        var elseLabel = ILGenerator.DefineLabel();

        GenerateBranchOpForCondition(ifStatement.Condition, elseLabel);

        var scope = new Scope(this);
        new ExpressionGenerator(scope, ifStatement.ThenBranch).Generate();

        var thenType = ifStatement.ThenBranch.Type;

        if (ifStatement.Type.IsUnion
            && thenType.TypeKind is TypeKind.Struct)
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(thenType));
        }

        if (ifStatement.ElseBranch is not null)
        {
            // Define a label for the end of the 'if' statement
            var endIfLabel = ILGenerator.DefineLabel();

            // Branch to end of 'if' after the 'if' block
            ILGenerator.Emit(OpCodes.Br_S, endIfLabel);

            // Mark the 'else' label
            ILGenerator.MarkLabel(elseLabel);

            // Generate the 'else' block
            var scope2 = new Scope(this);
            new ExpressionGenerator(scope2, ifStatement.ElseBranch).Generate();

            var elseType = ifStatement.ElseBranch.Type;

            if (ifStatement.Type.IsUnion
                && elseType.TypeKind is TypeKind.Struct)
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(elseType));
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

    private void GenerateWhileExpression(BoundWhileExpression whileStatement)
    {
        var beginLabel = ILGenerator.DefineLabel();
        var conditionLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel(); // NEW

        // Jump to condition check first
        ILGenerator.Emit(OpCodes.Br_S, conditionLabel);

        ILGenerator.MarkLabel(beginLabel);
        ILGenerator.Emit(OpCodes.Nop);

        var scope = new Scope(this);
        new ExpressionGenerator(scope, whileStatement.Body).Generate();

        ILGenerator.MarkLabel(conditionLabel);
        GenerateBranchOpForCondition(whileStatement.Condition, endLabel); // ✅ jump out if false

        // If true, loop again
        ILGenerator.Emit(OpCodes.Br_S, beginLabel);

        ILGenerator.MarkLabel(endLabel);
    }

    private void GenerateBranchOpForCondition(BoundExpression expression, Label end)
    {
        if (expression is BoundParenthesizedExpression parenthesizedExpression)
        {
            GenerateBranchOpForCondition(parenthesizedExpression.Expression, end);
            return;
        }

        if (expression is BoundBinaryExpression binaryExpression)
        {
            GenerateExpression(binaryExpression.Left);
            GenerateExpression(binaryExpression.Right);

            switch (binaryExpression.Operator.OperatorKind)
            {
                case BinaryOperatorKind.Equality:
                    ILGenerator.Emit(OpCodes.Ceq); // compare
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case BinaryOperatorKind.Inequality:
                    ILGenerator.Emit(OpCodes.Ceq);
                    ILGenerator.Emit(OpCodes.Ldc_I4_0);
                    ILGenerator.Emit(OpCodes.Ceq); // logical NOT
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case BinaryOperatorKind.GreaterThan:
                    ILGenerator.Emit(OpCodes.Cgt);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case BinaryOperatorKind.LessThan:
                    ILGenerator.Emit(OpCodes.Clt);
                    ILGenerator.Emit(OpCodes.Brfalse_S, end);
                    break;

                case BinaryOperatorKind.GreaterThanOrEqual:
                    ILGenerator.Emit(OpCodes.Clt);
                    ILGenerator.Emit(OpCodes.Brtrue_S, end);
                    break;

                case BinaryOperatorKind.LessThanOrEqual:
                    ILGenerator.Emit(OpCodes.Cgt);
                    ILGenerator.Emit(OpCodes.Brtrue_S, end);
                    break;

                default:
                    throw new NotSupportedException($"Unsupported binary condition");
            }
        }
        else if (expression is BoundLiteralExpression literalExpression)
        {
            if (literalExpression.Kind == BoundLiteralExpressionKind.TrueLiteral)
            {
                // If true, do nothing; execution continues
            }
            else if (literalExpression.Kind == BoundLiteralExpressionKind.FalseLiteral)
            {
                ILGenerator.Emit(OpCodes.Br_S, end);
            }
        }
        else
        {
            // Other kinds of expressions... member access etc.

            GenerateExpression(expression);
            ILGenerator.Emit(OpCodes.Brfalse_S, end);
        }
    }

    private void GenerateBlock(BoundBlockExpression block)
    {
        foreach (var s in block.Statements)
        {
            GenerateStatement(s);
        }
    }

    private void GenerateStatement(BoundStatement statement)
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