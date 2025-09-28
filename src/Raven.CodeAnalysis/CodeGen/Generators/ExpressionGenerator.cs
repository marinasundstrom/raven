using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal class ExpressionGenerator : Generator
{
    private readonly BoundExpression _expression;

    private static readonly MethodInfo GetTypeFromHandleMethod = typeof(Type)
        .GetMethod(nameof(Type.GetTypeFromHandle), BindingFlags.Public | BindingFlags.Static, binder: null, new[] { typeof(RuntimeTypeHandle) }, modifiers: null)
        ?? throw new InvalidOperationException("Failed to resolve Type.GetTypeFromHandle(RuntimeTypeHandle).");

    public ExpressionGenerator(Generator parent, BoundExpression expression) : base(parent)
    {
        _expression = expression;
    }

    public override void Emit()
    {
        EmitExpression(_expression);
    }

    private void EmitExpression(BoundExpression expression)
    {
        switch (expression)
        {
            case BoundBinaryExpression binaryExpression:
                EmitBinaryExpression(binaryExpression);
                break;

            case BoundUnaryExpression unaryExpression:
                EmitUnaryExpression(unaryExpression);
                break;

            case BoundAddressOfExpression addressOfExpression:
                EmitAddressOfExpression(addressOfExpression);
                break;

            case BoundParameterAccess parameterAccess:
                EmitParameterAccess(parameterAccess);
                break;

            case BoundLocalAccess localAccess:
                EmitLocalAccess(localAccess);
                break;

            case BoundPropertyAccess propertyAccess:
                EmitPropertyAccess(propertyAccess);
                break;

            case BoundFieldAccess fieldAccess:
                EmitFieldAccess(fieldAccess);
                break;

            case BoundMemberAccessExpression memberAccessExpression:
                EmitMemberAccessExpression(memberAccessExpression);
                break;
            case BoundConditionalAccessExpression conditionalAccess:
                EmitConditionalAccessExpression(conditionalAccess);
                break;

            case BoundInvocationExpression invocationExpression:
                EmitInvocationExpression(invocationExpression);
                break;

            case BoundLiteralExpression literalExpression:
                EmitLiteralExpression(literalExpression);
                break;

            case BoundParenthesizedExpression parenthesized:
                EmitExpression(parenthesized.Expression);
                break;

            case BoundCastExpression castExpression:
                EmitCastExpression(castExpression);
                break;

            case BoundAsExpression asExpression:
                EmitAsExpression(asExpression);
                break;

            case BoundIfExpression ifStatement:
                EmitIfExpression(ifStatement);
                break;


            case BoundBlockExpression block:
                EmitBlock(block);
                break;

            case BoundTupleExpression tupleExpression:
                EmitTupleExpression(tupleExpression);
                break;

            case BoundAssignmentExpression assignmentExpression:
                EmitAssignmentExpression(assignmentExpression);
                break;

            case BoundObjectCreationExpression objectCreationExpression:
                EmitObjectCreationExpression(objectCreationExpression);
                break;
            case BoundMatchExpression matchExpression:
                EmitMatchExpression(matchExpression);
                break;

            case BoundCollectionExpression collectionExpression:
                EmitCollectionExpression(collectionExpression);
                break;

            case BoundEmptyCollectionExpression emptyCollectionExpression:
                EmitEmptyCollectionExpression(emptyCollectionExpression);
                break;

            case BoundArrayAccessExpression boundArrayAccessExpression:
                EmitArrayAccessExpression(boundArrayAccessExpression);
                break;

            case BoundIndexerAccessExpression boundIndexerAccessExpression:
                EmitIndexerAccessExpression(boundIndexerAccessExpression);
                break;

            case BoundIsPatternExpression isPatternExpression:
                EmitIsPatternExpression(isPatternExpression);
                break;

            case BoundSelfExpression selfExpression:
                EmitSelfExpression(selfExpression);
                break;

            case BoundTypeOfExpression typeOfExpression:
                EmitTypeOfExpression(typeOfExpression);
                break;

            case BoundTypeExpression:
                break;

            case BoundUnitExpression unitExpression:
                EmitUnitExpression(unitExpression);
                break;

            case BoundLambdaExpression lambdaExpression:
                EmitLambdaExpression(lambdaExpression);
                break;

            case BoundDelegateCreationExpression delegateCreation:
                EmitDelegateCreationExpression(delegateCreation);
                break;

            default:
                throw new NotSupportedException($"Unsupported expression type: {expression.GetType()}");
        }
    }

    private void EmitSelfExpression(BoundSelfExpression selfExpression)
    {
        if (TryEmitCapturedVariableLoad(selfExpression.Symbol ?? selfExpression.Type))
            return;

        ILGenerator.Emit(OpCodes.Ldarg_0);
    }

    private void EmitTypeOfExpression(BoundTypeOfExpression typeOfExpression)
    {
        var operandClrType = ResolveClrType(typeOfExpression.OperandType);

        ILGenerator.Emit(OpCodes.Ldtoken, operandClrType);
        ILGenerator.Emit(OpCodes.Call, GetTypeFromHandleMethod);
    }

    private void EmitDelegateCreationExpression(BoundDelegateCreationExpression delegateCreation)
    {
        var method = delegateCreation.Method
            ?? throw new InvalidOperationException("Delegate creation requires a resolved target method.");

        if (delegateCreation.DelegateType is not INamedTypeSymbol delegateTypeSymbol)
            throw new InvalidOperationException("Delegate creation requires a delegate type.");

        if (delegateTypeSymbol is SynthesizedDelegateTypeSymbol synthesized)
            MethodGenerator.TypeGenerator.CodeGen.GetTypeBuilder(synthesized);

        var delegateClrType = ResolveClrType(delegateTypeSymbol);
        var ctor = GetDelegateConstructor(delegateClrType);

        var methodInfo = GetMethodInfo(method);

        if (method.IsStatic)
        {
            ILGenerator.Emit(OpCodes.Ldnull);
            ILGenerator.Emit(OpCodes.Ldftn, methodInfo);
        }
        else
        {
            var receiver = delegateCreation.Receiver
                ?? throw new InvalidOperationException("Instance delegate creation requires a receiver.");

            EmitExpression(receiver);

            if (receiver.Type is { IsValueType: true } receiverType)
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(receiverType));

            if (method.IsVirtual && method.ContainingType is { IsValueType: false })
            {
                ILGenerator.Emit(OpCodes.Dup);
                ILGenerator.Emit(OpCodes.Ldvirtftn, methodInfo);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldftn, methodInfo);
            }
        }

        ILGenerator.Emit(OpCodes.Newobj, ctor);
    }

    private void EmitCapturedLambda(BoundLambdaExpression lambdaExpression, TypeGenerator.LambdaClosure closure, MethodInfo methodInfo, Type delegateType)
    {
        var closureLocal = ILGenerator.DeclareLocal(closure.TypeBuilder);
        ILGenerator.Emit(OpCodes.Newobj, closure.Constructor);
        ILGenerator.Emit(OpCodes.Stloc, closureLocal);

        var capturedSymbols = lambdaExpression.CapturedVariables.ToArray();
        for (var i = 0; i < capturedSymbols.Length; i++)
        {
            var captured = capturedSymbols[i];

            ILGenerator.Emit(OpCodes.Ldloc, closureLocal);
            EmitCapturedValue(captured);
            ILGenerator.Emit(OpCodes.Stfld, closure.GetField(captured));
        }

        var delegateCtor = GetDelegateConstructor(delegateType);

        ILGenerator.Emit(OpCodes.Ldloc, closureLocal);
        ILGenerator.Emit(OpCodes.Ldftn, methodInfo);
        ILGenerator.Emit(OpCodes.Newobj, delegateCtor);
    }

    private void EmitCapturedValue(ISymbol symbol)
    {
        if (TryEmitCapturedClosureField(symbol))
            return;

        switch (symbol)
        {
            case ILocalSymbol localSymbol:
                {
                    var localBuilder = GetLocal(localSymbol)
                        ?? throw new InvalidOperationException($"Missing local builder for captured local '{localSymbol.Name}'.");
                    ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
                    break;
                }
            case IParameterSymbol parameterSymbol:
                {
                    var parameterBuilder = MethodGenerator.GetParameterBuilder(parameterSymbol);
                    var position = parameterBuilder.Position;
                    if (MethodSymbol.IsStatic)
                        position -= 1;
                    ILGenerator.Emit(OpCodes.Ldarg, position);
                    break;
                }
            case ITypeSymbol:
                {
                    if (MethodSymbol.IsStatic)
                        throw new InvalidOperationException("Cannot capture 'self' in a static context.");

                    ILGenerator.Emit(OpCodes.Ldarg_0);
                    break;
                }
            default:
                throw new NotSupportedException($"Capturing symbol '{symbol}' is not supported.");
        }
    }

    private bool TryEmitCapturedClosureField(ISymbol symbol)
    {
        if (!MethodBodyGenerator.TryGetCapturedField(symbol, out var fieldBuilder))
            return false;

        MethodBodyGenerator.EmitLoadClosure();
        ILGenerator.Emit(OpCodes.Ldfld, fieldBuilder);
        return true;
    }

    private bool TryEmitCapturedVariableLoad(ISymbol? symbol)
    {
        if (symbol is null)
            return false;

        if (!MethodBodyGenerator.TryGetCapturedField(symbol, out var fieldBuilder))
            return false;

        MethodBodyGenerator.EmitLoadClosure();
        ILGenerator.Emit(OpCodes.Ldfld, fieldBuilder);

        if (symbol is ILocalSymbol)
            ILGenerator.Emit(OpCodes.Ldfld, MethodBodyGenerator.GetStrongBoxValueField(fieldBuilder.FieldType));

        return true;
    }

    private bool TryEmitCapturedAssignment(ISymbol symbol, BoundExpression value)
    {
        if (!MethodBodyGenerator.TryGetCapturedField(symbol, out var fieldBuilder))
            return false;

        MethodBodyGenerator.EmitLoadClosure();
        if (symbol is ILocalSymbol localSymbol)
        {
            ILGenerator.Emit(OpCodes.Ldfld, fieldBuilder);
            EmitExpression(value);

            if (value.Type is { IsValueType: true } &&
                localSymbol.Type is not null &&
                (localSymbol.Type.SpecialType is SpecialType.System_Object || localSymbol.Type is IUnionTypeSymbol))
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(value.Type));
            }

            var valueField = MethodBodyGenerator.GetStrongBoxValueField(fieldBuilder.FieldType);
            ILGenerator.Emit(OpCodes.Stfld, valueField);
            return true;
        }

        EmitExpression(value);
        ILGenerator.Emit(OpCodes.Stfld, fieldBuilder);
        return true;
    }

    private void EmitLambdaExpression(BoundLambdaExpression lambdaExpression)
    {
        if (lambdaExpression.Symbol is not ILambdaSymbol lambdaSymbol)
            throw new InvalidOperationException("Lambda symbol missing.");

        var typeGenerator = MethodGenerator.TypeGenerator;
        var lambdaGenerator = typeGenerator.GetMethodGenerator(lambdaSymbol);
        var hasCaptures = lambdaExpression.CapturedVariables.Any();
        TypeGenerator.LambdaClosure? closure = null;

        if (lambdaGenerator is null)
        {
            lambdaGenerator = new MethodGenerator(typeGenerator, lambdaSymbol);

            if (hasCaptures && lambdaSymbol is SourceLambdaSymbol sourceLambda)
            {
                closure = typeGenerator.EnsureLambdaClosure(sourceLambda);
                lambdaGenerator.SetLambdaClosure(closure);
            }

            typeGenerator.Add(lambdaSymbol, lambdaGenerator);
            lambdaGenerator.DefineMethodBuilder();

            if (lambdaSymbol is SourceLambdaSymbol sourceLambdaSymbol)
                typeGenerator.CodeGen.AddMemberBuilder(sourceLambdaSymbol, lambdaGenerator.MethodBase);
        }
        else if (hasCaptures && lambdaSymbol is SourceLambdaSymbol existingSource)
        {
            closure = typeGenerator.EnsureLambdaClosure(existingSource);
            lambdaGenerator.SetLambdaClosure(closure);
        }

        if (hasCaptures && closure is null && lambdaSymbol is SourceLambdaSymbol lateSource)
            closure = typeGenerator.EnsureLambdaClosure(lateSource);

        if (!lambdaGenerator.HasEmittedBody)
            lambdaGenerator.EmitLambdaBody(lambdaExpression, closure);

        if (lambdaGenerator.MethodBase is not MethodInfo methodInfo)
            throw new InvalidOperationException("Expected a method-backed lambda.");

        var delegateType = ResolveClrType(lambdaExpression.DelegateType);

        if (hasCaptures)
        {
            if (closure is null && !typeGenerator.TryGetLambdaClosure(lambdaSymbol, out closure))
                throw new InvalidOperationException("Missing closure information for captured lambda.");

            EmitCapturedLambda(lambdaExpression, closure!, methodInfo, delegateType);
            return;
        }

        var ctor = GetDelegateConstructor(delegateType);

        ILGenerator.Emit(OpCodes.Ldnull);
        ILGenerator.Emit(OpCodes.Ldftn, methodInfo);
        ILGenerator.Emit(OpCodes.Newobj, ctor);
    }

    private static ConstructorInfo GetDelegateConstructor(Type delegateType)
    {
        foreach (var ctor in delegateType.GetConstructors())
        {
            var parameters = ctor.GetParameters();

            if (parameters.Length != 2)
                continue;

            var first = parameters[0].ParameterType;
            var second = parameters[1].ParameterType;

            if (string.Equals(first.FullName, "System.Object", StringComparison.Ordinal) &&
                string.Equals(second.FullName, "System.IntPtr", StringComparison.Ordinal))
            {
                return ctor;
            }
        }

        throw new InvalidOperationException($"Delegate '{delegateType}' lacks the expected constructor.");
    }

    private void EmitUnitExpression(BoundUnitExpression unitExpression)
    {
        EmitUnitValue();
    }

    private void EmitTupleExpression(BoundTupleExpression tupleExpression)
    {
        var elements = tupleExpression.Elements;
        var elementTypes = elements.Select(e => e.Type!).ToArray();

        for (int i = 0; i < elements.Count(); i++)
        {
            EmitExpression(elements.ElementAt(i));

            if (elementTypes[i].IsValueType == false)
                continue;

            var clrType = ResolveClrType(elementTypes[i]);

            // Box if needed (e.g. to match generic type constraints in ValueTuple.Create<T>)
            /*if (clrType.IsValueType && clrType.IsGenericType == false)
            {
                ILGenerator.Emit(OpCodes.Box, clrType);
            }*/
        }

        var valueTupleType = Compilation.GetTypeByMetadataName($"System.ValueTuple");

        var createMethod = valueTupleType
            .GetMembers("Create")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m =>
                m.IsStatic &&
                m.Parameters.Length == elements.Count());

        if (createMethod is null)
            throw new InvalidOperationException($"Could not resolve ValueTuple.Create({elements.Count()} args)");

        ILGenerator.Emit(OpCodes.Call, GetMethodInfo(createMethod).MakeGenericMethod(elements.Select(x => ResolveClrType(x.Type)).ToArray()));
    }
    private void EmitLocalAccess(BoundLocalAccess localAccess)
    {
        if (TryEmitCapturedVariableLoad(localAccess.Local))
            return;

        var localBuilder = GetLocal(localAccess.Local);
        if (localBuilder is null)
            throw new InvalidOperationException($"Missing local builder for '{localAccess.Local.Name}'");

        if (MethodBodyGenerator.IsCapturedLocal(localAccess.Local))
        {
            ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
            ILGenerator.Emit(OpCodes.Ldfld, MethodBodyGenerator.GetStrongBoxValueField(localBuilder.LocalType));
            return;
        }

        ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
    }

    private void EmitParameterAccess(BoundParameterAccess parameterAccess)
    {
        if (TryEmitCapturedVariableLoad(parameterAccess.Parameter))
            return;

        int position = MethodGenerator.GetParameterBuilder(parameterAccess.Parameter).Position;
        if (MethodSymbol.IsStatic)
            position -= 1;

        ILGenerator.Emit(OpCodes.Ldarg, position);
    }

    private void EmitCastExpression(BoundCastExpression castExpression)
    {
        new ExpressionGenerator(this, castExpression.Expression).Emit();
        EmitConversion(castExpression.Expression.Type!, castExpression.Type, castExpression.Conversion);
    }

    private void EmitAsExpression(BoundAsExpression asExpression)
    {
        new ExpressionGenerator(this, asExpression.Expression).Emit();
        ILGenerator.Emit(OpCodes.Isinst, ResolveClrType(asExpression.Type));
    }

    private void EmitUnaryExpression(BoundUnaryExpression node)
    {
        var operand = node.Operand;
        var op = node.Operator;

        switch (op.OperatorKind)
        {
            case BoundUnaryOperatorKind.UnaryMinus: // -x
                EmitExpression(operand);
                ILGenerator.Emit(OpCodes.Neg);
                break;

            case BoundUnaryOperatorKind.UnaryPlus: // +x
                EmitExpression(operand); // no-op
                break;

            case BoundUnaryOperatorKind.LogicalNot: // !x
                EmitExpression(operand);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq);
                break;

            case BoundUnaryOperatorKind.BitwiseNot: // ~x
                EmitExpression(operand);
                ILGenerator.Emit(OpCodes.Not);
                break;

            default:
                throw new NotSupportedException($"Unsupported unary operator");
        }
    }

    private void EmitConversion(ITypeSymbol from, ITypeSymbol to, Conversion conversion)
    {
        if (conversion.IsIdentity)
            return;

        if (to is NullableTypeSymbol nullableTo && nullableTo.UnderlyingType.IsValueType)
        {
            EmitNullableConversion(from, nullableTo);
            return;
        }

        if (to is IUnionTypeSymbol unionTo)
        {
            EmitUnionConversion(from, unionTo);
            return;
        }

        if (conversion.IsNumeric)
        {
            EmitNumericConversion(to);
            return;
        }

        if (conversion.IsUnboxing)
        {
            ILGenerator.Emit(OpCodes.Unbox_Any, ResolveClrType(to));
            return;
        }

        if (conversion.IsBoxing)
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(from));
            if (!SymbolEqualityComparer.Default.Equals(from, to))
                ILGenerator.Emit(OpCodes.Castclass, ResolveClrType(to));
            return;
        }

        if (conversion.IsReference)
        {
            ILGenerator.Emit(OpCodes.Castclass, ResolveClrType(to));
            return;
        }

        if (conversion.IsUserDefined && conversion.MethodSymbol is IMethodSymbol m)
        {
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(m));
            return;
        }

        throw new NotSupportedException("Unsupported conversion");
    }

    private void EmitUnionConversion(ITypeSymbol from, IUnionTypeSymbol unionTo)
    {
        var emission = unionTo.GetUnionEmissionInfo(Compilation);
        var targetClrType = ResolveClrType(unionTo);

        if (emission.WrapInNullable)
        {
            EmitNullableUnionConversion(from, unionTo, emission.UnderlyingTypeSymbol, targetClrType);
            return;
        }

        if (from.IsValueType && !targetClrType.IsValueType)
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(from));
    }

    private void EmitNullableUnionConversion(
        ITypeSymbol from,
        IUnionTypeSymbol unionTo,
        ITypeSymbol underlyingSymbol,
        Type nullableClrType)
    {
        if (from.TypeKind == TypeKind.Null)
        {
            EmitDefaultValue(unionTo);
            return;
        }

        if (!SymbolEqualityComparer.Default.Equals(from, underlyingSymbol))
        {
            var underlyingConversion = Compilation.ClassifyConversion(from, underlyingSymbol);
            if (!underlyingConversion.Exists)
                throw new NotSupportedException("Unsupported conversion to nullable union underlying type");

            EmitConversion(from, underlyingSymbol, underlyingConversion);
        }

        var underlyingClrType = ResolveClrType(underlyingSymbol);
        var valueLocal = ILGenerator.DeclareLocal(underlyingClrType);
        var nullableLocal = ILGenerator.DeclareLocal(nullableClrType);

        ILGenerator.Emit(OpCodes.Stloc, valueLocal);
        ILGenerator.Emit(OpCodes.Ldloca, nullableLocal);
        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);

        var ctor = nullableClrType.GetConstructor(new[] { underlyingClrType })
            ?? throw new InvalidOperationException($"Missing Nullable constructor for {nullableClrType}");

        ILGenerator.Emit(OpCodes.Call, ctor);
        ILGenerator.Emit(OpCodes.Ldloc, nullableLocal);
    }

    private void EmitNullableConversion(ITypeSymbol from, NullableTypeSymbol nullableTo)
    {
        if (from is NullableTypeSymbol fromNullable)
        {
            if (SymbolEqualityComparer.Default.Equals(fromNullable.UnderlyingType, nullableTo.UnderlyingType))
                return;

            throw new NotSupportedException("Unsupported nullable conversion");
        }

        var underlying = nullableTo.UnderlyingType;

        var underlyingClr = ResolveClrType(underlying);
        var nullableClr = ResolveClrType(nullableTo);

        var valueLocal = ILGenerator.DeclareLocal(underlyingClr);
        var nullableLocal = ILGenerator.DeclareLocal(nullableClr);

        if (!SymbolEqualityComparer.Default.Equals(from, underlying))
        {
            var underlyingConversion = Compilation.ClassifyConversion(from, underlying);
            EmitConversion(from, underlying, underlyingConversion);
        }

        ILGenerator.Emit(OpCodes.Stloc, valueLocal);
        ILGenerator.Emit(OpCodes.Ldloca, nullableLocal);
        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);

        var ctor = nullableClr.GetConstructor(new[] { underlyingClr })
            ?? throw new InvalidOperationException($"Missing Nullable constructor for {nullableClr}");

        ILGenerator.Emit(OpCodes.Call, ctor);
        ILGenerator.Emit(OpCodes.Ldloc, nullableLocal);
    }

    private void EmitNumericConversion(ITypeSymbol to)
    {
        switch (to.SpecialType)
        {
            case SpecialType.System_Int32:
                ILGenerator.Emit(OpCodes.Conv_I4);
                break;
            case SpecialType.System_Int64:
                ILGenerator.Emit(OpCodes.Conv_I8);
                break;
            case SpecialType.System_Single:
                ILGenerator.Emit(OpCodes.Conv_R4);
                break;
            case SpecialType.System_Double:
                ILGenerator.Emit(OpCodes.Conv_R8);
                break;
            case SpecialType.System_Int16:
                ILGenerator.Emit(OpCodes.Conv_I2);
                break;
            case SpecialType.System_UInt16:
            case SpecialType.System_Char:
                ILGenerator.Emit(OpCodes.Conv_U2);
                break;
            case SpecialType.System_UInt32:
                ILGenerator.Emit(OpCodes.Conv_U4);
                break;
            case SpecialType.System_UInt64:
                ILGenerator.Emit(OpCodes.Conv_U8);
                break;
            case SpecialType.System_SByte:
                ILGenerator.Emit(OpCodes.Conv_I1);
                break;
            case SpecialType.System_Byte:
                ILGenerator.Emit(OpCodes.Conv_U1);
                break;
            default:
                throw new NotSupportedException($"Unsupported numeric conversion to {to.ToDisplayString()}");
        }
    }

    private void EmitAddressOfExpression(BoundAddressOfExpression addressOf)
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
                if (field.ContainingType.IsValueType)
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

    private void EmitIsPatternExpression(BoundIsPatternExpression isPatternExpression)
    {
        EmitExpression(isPatternExpression.Expression); // Push the value of the expression onto the stack

        var expressionType = isPatternExpression.Expression.Type;
        if (expressionType is { IsValueType: true } && expressionType.TypeKind != TypeKind.Error)
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(expressionType));

        EmitPattern(isPatternExpression.Pattern);       // Evaluate the pattern; leaves a boolean on the stack
    }

    private void EmitMatchExpression(BoundMatchExpression matchExpression)
    {
        var scrutineeType = matchExpression.Expression.Type ?? Compilation.GetSpecialType(SpecialType.System_Object);
        if (scrutineeType.TypeKind == TypeKind.Error)
            scrutineeType = Compilation.GetSpecialType(SpecialType.System_Object);

        EmitExpression(matchExpression.Expression);

        var scrutineeClrType = ResolveClrType(scrutineeType);
        if (scrutineeType.IsValueType)
        {
            ILGenerator.Emit(OpCodes.Box, scrutineeClrType);
            scrutineeType = Compilation.GetSpecialType(SpecialType.System_Object);
            scrutineeClrType = ResolveClrType(scrutineeType);
        }

        var scrutineeLocal = ILGenerator.DeclareLocal(scrutineeClrType);
        ILGenerator.Emit(OpCodes.Stloc, scrutineeLocal);

        var resultType = matchExpression.Type ?? Compilation.GetSpecialType(SpecialType.System_Object);
        if (resultType.TypeKind == TypeKind.Error)
            resultType = Compilation.GetSpecialType(SpecialType.System_Object);

        var resultLocal = ILGenerator.DeclareLocal(ResolveClrType(resultType));
        var endLabel = ILGenerator.DefineLabel();

        foreach (var arm in matchExpression.Arms)
        {
            var nextArmLabel = ILGenerator.DefineLabel();
            var scope = new Scope(this);

            ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
            EmitPattern(arm.Pattern, scope);
            ILGenerator.Emit(OpCodes.Brfalse_S, nextArmLabel);

            if (arm.Guard is not null)
            {
                new ExpressionGenerator(scope, arm.Guard).Emit();
                ILGenerator.Emit(OpCodes.Brfalse_S, nextArmLabel);
            }

            new ExpressionGenerator(scope, arm.Expression).Emit();

            var armType = arm.Expression.Type;
            if ((matchExpression.Type?.IsUnion ?? false) && (armType?.IsValueType ?? false))
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(armType));

            ILGenerator.Emit(OpCodes.Stloc, resultLocal);
            ILGenerator.Emit(OpCodes.Br_S, endLabel);

            ILGenerator.MarkLabel(nextArmLabel);
        }

        ILGenerator.MarkLabel(endLabel);
        ILGenerator.Emit(OpCodes.Ldloc, resultLocal);
    }

    private void EmitPattern(BoundPattern pattern, Generator? scope = null)
    {
        scope ??= this;

        if (pattern is BoundDiscardPattern)
        {
            ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            return;
        }

        if (pattern is BoundDeclarationPattern declarationPattern)
        {
            var typeSymbol = declarationPattern.Type;
            var clrType = ResolveClrType(typeSymbol);

            var patternLocal = EmitDesignation(declarationPattern.Designator, scope);

            // [expr]
            if (typeSymbol.IsValueType)
            {
                var labelFail = ILGenerator.DefineLabel();
                var labelDone = ILGenerator.DefineLabel();
                var matchLocal = ILGenerator.DeclareLocal(typeof(object));

                ILGenerator.Emit(OpCodes.Stloc, matchLocal);       // store cast attempt
                ILGenerator.Emit(OpCodes.Ldloc, matchLocal);
                ILGenerator.Emit(OpCodes.Brfalse_S, labelFail);

                ILGenerator.Emit(OpCodes.Ldloc, matchLocal);
                ILGenerator.Emit(OpCodes.Unbox_Any, clrType);      // unbox value
                if (patternLocal is not null)
                {
                    ILGenerator.Emit(OpCodes.Stloc, patternLocal); // store into pattern variable
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Pop);                // discard the unboxed value
                }
                ILGenerator.Emit(OpCodes.Ldc_I4_1);
                ILGenerator.Emit(OpCodes.Br_S, labelDone);

                ILGenerator.MarkLabel(labelFail);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);

                ILGenerator.MarkLabel(labelDone);
            }
            else
            {
                // Reference type flow — same as before
                ILGenerator.Emit(OpCodes.Isinst, clrType);         // cast or null
                if (patternLocal is not null)
                {
                    ILGenerator.Emit(OpCodes.Dup);
                    ILGenerator.Emit(OpCodes.Stloc, patternLocal); // assign
                }
                ILGenerator.Emit(OpCodes.Ldnull);
                ILGenerator.Emit(OpCodes.Cgt_Un);                  // bool: not-null
            }
        }
        else if (pattern is BoundConstantPattern constantPattern)
        {
            EmitConstantPattern(constantPattern);
        }
        else if (pattern is BoundUnaryPattern unaryPattern)
        {
            EmitPattern(unaryPattern.Pattern, scope);

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
                EmitPattern(binaryPattern.Left, scope);
                ILGenerator.Emit(OpCodes.Brfalse_S, labelFail);

                EmitPattern(binaryPattern.Right, scope);
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

                EmitPattern(binaryPattern.Left, scope);
                ILGenerator.Emit(OpCodes.Brtrue_S, labelTrue);

                EmitPattern(binaryPattern.Right, scope);
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

    private void EmitConstantPattern(BoundConstantPattern constantPattern)
    {
        var literal = constantPattern.LiteralType;
        var value = literal.ConstantValue;

        var scrutineeLocal = ILGenerator.DeclareLocal(typeof(object));
        ILGenerator.Emit(OpCodes.Stloc, scrutineeLocal);

        if (value is null)
        {
            ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
            ILGenerator.Emit(OpCodes.Ldnull);
            ILGenerator.Emit(OpCodes.Ceq);
            return;
        }

        var notNullLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
        ILGenerator.Emit(OpCodes.Brtrue_S, notNullLabel);

        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Br_S, endLabel);

        ILGenerator.MarkLabel(notNullLabel);
        ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
        EmitConstantAsObject(literal, value);

        var equalsMethod = Compilation.CoreAssembly.GetType("System.Object").GetMethod(nameof(object.Equals), [Compilation.CoreAssembly.GetType("System.Object")])
            ?? throw new InvalidOperationException("object.Equals(object) not found.");

        ILGenerator.Emit(OpCodes.Callvirt, equalsMethod);
        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitConstantAsObject(LiteralTypeSymbol literal, object value)
    {
        switch (value)
        {
            case string s:
                ILGenerator.Emit(OpCodes.Ldstr, s);
                break;
            case char ch:
                ILGenerator.Emit(OpCodes.Ldc_I4, ch);
                ILGenerator.Emit(OpCodes.Conv_U2);
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(literal.UnderlyingType));
                break;
            default:
                EmitLiteral(value);

                if (value is not null && literal.UnderlyingType.IsValueType)
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(literal.UnderlyingType));

                break;
        }
    }

    private LocalBuilder? EmitDesignation(BoundDesignator designation, Generator scope)
    {
        if (designation is BoundSingleVariableDesignator single)
        {
            var symbol = single.Local;

            var local = ILGenerator.DeclareLocal(ResolveClrType(symbol.Type)); // resolve type
            local.SetLocalSymInfo(single.Local.Name);

            scope.AddLocal(symbol, local);

            return local;
        }

        if (designation is BoundDiscardDesignator)
            return null;

        throw new NotSupportedException("Unsupported designation");
    }

    private void EmitCollectionExpression(BoundCollectionExpression collectionExpression)
    {
        var target = collectionExpression.Type;

        if (target is IArrayTypeSymbol arrayTypeSymbol)
        {
            if (!collectionExpression.Elements.OfType<BoundSpreadElement>().Any())
            {
                ILGenerator.Emit(OpCodes.Ldc_I4, collectionExpression.Elements.Count());
                ILGenerator.Emit(OpCodes.Newarr, ResolveClrType(arrayTypeSymbol.ElementType));

                int index = 0;
                foreach (var element in collectionExpression.Elements)
                {
                    ILGenerator.Emit(OpCodes.Dup);
                    ILGenerator.Emit(OpCodes.Ldc_I4, index);

                    EmitExpression(element);

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
            else
            {
                EmitArrayWithSpreads(arrayTypeSymbol, collectionExpression);
            }
        }
        else if (target is INamedTypeSymbol namedType)
        {
            var ctor = namedType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0);
            if (ctor is null)
                throw new NotSupportedException("Collection type requires a parameterless constructor");

            ConstructorInfo ctorInfo = ctor switch
            {
                SourceMethodSymbol sm => (ConstructorInfo)GetMemberBuilder(sm),
                PEMethodSymbol pem => pem.GetConstructorInfo(),
                SubstitutedMethodSymbol sub => sub.GetConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen),
                _ => throw new NotSupportedException()
            };

            ILGenerator.Emit(OpCodes.Newobj, ctorInfo);
            var collectionLocal = ILGenerator.DeclareLocal(ResolveClrType(namedType));
            ILGenerator.Emit(OpCodes.Stloc, collectionLocal);

            var addMethod = collectionExpression.CollectionSymbol as IMethodSymbol;

            if (addMethod is not null)
            {
                var paramType = addMethod.Parameters[0].Type;
                foreach (var element in collectionExpression.Elements)
                {
                    if (element is BoundSpreadElement spread)
                    {
                        EmitSpreadElement(collectionLocal, spread, paramType, addMethod);
                    }
                    else
                    {
                        ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
                        EmitExpression(element);

                        if (element.Type is { IsValueType: true } && !paramType.IsValueType)
                        {
                            ILGenerator.Emit(OpCodes.Box, ResolveClrType(element.Type));
                        }

                        var isInterfaceCall = addMethod.ContainingType?.TypeKind == TypeKind.Interface;

                        if (!addMethod.ContainingType!.IsValueType && (addMethod.IsVirtual || isInterfaceCall))
                            ILGenerator.Emit(OpCodes.Callvirt, GetMethodInfo(addMethod));
                        else
                            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(addMethod));
                    }
                }
            }

            ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
        }
    }

    private void EmitArrayWithSpreads(IArrayTypeSymbol arrayTypeSymbol, BoundCollectionExpression collectionExpression)
    {
        var elementType = arrayTypeSymbol.ElementType;
        var listTypeDef = (INamedTypeSymbol)Compilation.GetTypeByMetadataName("System.Collections.Generic.List`1")!;
        var listType = (INamedTypeSymbol)listTypeDef.Construct(elementType);

        var ctor = listType.Constructors.First(c => !c.IsStatic && c.Parameters.Length == 0);
        ConstructorInfo ctorInfo = ctor switch
        {
            PEMethodSymbol pem => pem.GetConstructorInfo(),
            SubstitutedMethodSymbol sub => sub.GetConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen),
            SourceMethodSymbol sm => (ConstructorInfo)GetMemberBuilder(sm),
            _ => throw new NotSupportedException()
        };

        ILGenerator.Emit(OpCodes.Newobj, ctorInfo);
        var listLocal = ILGenerator.DeclareLocal(ResolveClrType(listType));
        ILGenerator.Emit(OpCodes.Stloc, listLocal);

        var addMethodInfo = ResolveClrType(listType).GetMethod("Add")!;

        foreach (var element in collectionExpression.Elements)
        {
            if (element is BoundSpreadElement spread)
            {
                EmitSpreadElement(listLocal, spread, elementType, addMethodInfo);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldloc, listLocal);
                EmitExpression(element);

                if (element.Type is { IsValueType: true } && !elementType.IsValueType)
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(element.Type));
                }

                ILGenerator.Emit(OpCodes.Callvirt, addMethodInfo);
            }
        }

        ILGenerator.Emit(OpCodes.Ldloc, listLocal);
        var toArrayInfo = ResolveClrType(listType).GetMethod("ToArray")!;
        ILGenerator.Emit(OpCodes.Callvirt, toArrayInfo);
    }

    private void EmitSpreadElement(LocalBuilder collectionLocal, BoundSpreadElement spread, ITypeSymbol elementType, IMethodSymbol addMethod)
    {
        EmitExpression(spread.Expression);

        var enumerable = (INamedTypeSymbol)Compilation.GetTypeByMetadataName("System.Collections.IEnumerable")!;
        ILGenerator.Emit(OpCodes.Castclass, ResolveClrType(enumerable));
        var getEnumerator = (PEMethodSymbol)enumerable.GetMembers(nameof(IEnumerable.GetEnumerator)).First()!;
        ILGenerator.Emit(OpCodes.Callvirt, getEnumerator.GetMethodInfo());
        var enumeratorType = getEnumerator.ReturnType;
        var enumeratorLocal = ILGenerator.DeclareLocal(ResolveClrType(enumeratorType));
        ILGenerator.Emit(OpCodes.Stloc, enumeratorLocal);

        var loopStart = ILGenerator.DefineLabel();
        var loopEnd = ILGenerator.DefineLabel();

        ILGenerator.MarkLabel(loopStart);
        var moveNext = (PEMethodSymbol)enumeratorType.GetMembers(nameof(IEnumerator.MoveNext))!.First();
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, moveNext.GetMethodInfo());
        ILGenerator.Emit(OpCodes.Brfalse, loopEnd);

        ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
        var currentProp = (PEMethodSymbol)enumeratorType.GetMembers(nameof(IEnumerator.Current)).OfType<PEPropertySymbol>().First()!.GetMethod!;
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, currentProp.GetMethodInfo());

        var clrElement = ResolveClrType(elementType);
        if (elementType.IsValueType)
            ILGenerator.Emit(OpCodes.Unbox_Any, clrElement);
        else
            ILGenerator.Emit(OpCodes.Castclass, clrElement);

        var paramType = addMethod.Parameters[0].Type;
        if (elementType.IsValueType && !paramType.IsValueType)
            ILGenerator.Emit(OpCodes.Box, clrElement);

        var isInterfaceCall = addMethod.ContainingType?.TypeKind == TypeKind.Interface;

        if (!addMethod.ContainingType!.IsValueType && (addMethod.IsVirtual || isInterfaceCall))
            ILGenerator.Emit(OpCodes.Callvirt, GetMethodInfo(addMethod));
        else
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(addMethod));

        ILGenerator.Emit(OpCodes.Br, loopStart);
        ILGenerator.MarkLabel(loopEnd);
    }

    private void EmitSpreadElement(LocalBuilder collectionLocal, BoundSpreadElement spread, ITypeSymbol elementType, MethodInfo addMethodInfo)
    {
        EmitExpression(spread.Expression);

        var enumerable = (INamedTypeSymbol)Compilation.GetTypeByMetadataName("System.Collections.IEnumerable")!;
        ILGenerator.Emit(OpCodes.Castclass, ResolveClrType(enumerable));
        var getEnumerator = (PEMethodSymbol)enumerable.GetMembers(nameof(IEnumerable.GetEnumerator)).First()!;
        ILGenerator.Emit(OpCodes.Callvirt, getEnumerator.GetMethodInfo());
        var enumeratorType = getEnumerator.ReturnType;
        var enumeratorLocal = ILGenerator.DeclareLocal(ResolveClrType(enumeratorType));
        ILGenerator.Emit(OpCodes.Stloc, enumeratorLocal);

        var loopStart = ILGenerator.DefineLabel();
        var loopEnd = ILGenerator.DefineLabel();

        ILGenerator.MarkLabel(loopStart);
        var moveNext = (PEMethodSymbol)enumeratorType.GetMembers(nameof(IEnumerator.MoveNext))!.First();
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, moveNext.GetMethodInfo());
        ILGenerator.Emit(OpCodes.Brfalse, loopEnd);

        ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
        var currentProp = (PEMethodSymbol)enumeratorType.GetMembers(nameof(IEnumerator.Current)).OfType<PEPropertySymbol>().First()!.GetMethod!;
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, currentProp.GetMethodInfo());

        var clrElement = ResolveClrType(elementType);
        if (elementType.IsValueType)
            ILGenerator.Emit(OpCodes.Unbox_Any, clrElement);
        else
            ILGenerator.Emit(OpCodes.Castclass, clrElement);

        ILGenerator.Emit(OpCodes.Callvirt, addMethodInfo);

        ILGenerator.Emit(OpCodes.Br, loopStart);
        ILGenerator.MarkLabel(loopEnd);
    }

    private void EmitEmptyCollectionExpression(BoundEmptyCollectionExpression emptyCollectionExpression)
    {
        var target = emptyCollectionExpression.Type;

        if (target is IArrayTypeSymbol arrayTypeSymbol)
        {
            // TODO: Use Array.Empty<T>() or Enumerable.Empty<T>().

            ILGenerator.Emit(OpCodes.Ldc_I4, 0);
            ILGenerator.Emit(OpCodes.Newarr, ResolveClrType(arrayTypeSymbol.ElementType));
        }
        else if (target is INamedTypeSymbol namedType)
        {
            var ctor = namedType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 0);
            if (ctor is null)
                throw new NotSupportedException("Collection type requires a parameterless constructor");

            ConstructorInfo ctorInfo = ctor switch
            {
                SourceMethodSymbol sm => (ConstructorInfo)GetMemberBuilder(sm),
                PEMethodSymbol pem => pem.GetConstructorInfo(),
                SubstitutedMethodSymbol sub => sub.GetConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen),
                _ => throw new NotSupportedException()
            };

            ILGenerator.Emit(OpCodes.Newobj, ctorInfo);
        }
    }

    private void EmitArrayAccessExpression(BoundArrayAccessExpression boundArrayAccessExpression)
    {
        var arrayType = boundArrayAccessExpression.Receiver.Type as IArrayTypeSymbol;

        EmitExpression(boundArrayAccessExpression.Receiver);

        foreach (var argument in boundArrayAccessExpression.Indices)
        {
            EmitExpression(argument);
        }

        EmitLoadElement(arrayType.ElementType);
    }

    private void EmitIndexerAccessExpression(BoundIndexerAccessExpression boundIndexerAccessExpression)
    {
        var indexerProperty = boundIndexerAccessExpression.Symbol as IPropertySymbol;

        EmitExpression(boundIndexerAccessExpression.Receiver);

        foreach (var argument in boundIndexerAccessExpression.Arguments)
        {
            EmitExpression(argument);
        }

        if (indexerProperty?.GetMethod is null)
            throw new InvalidOperationException("Indexer does not have a getter");

        var getter = GetMethodInfo(indexerProperty.GetMethod);

        ILGenerator.Emit(OpCodes.Callvirt, getter);
    }

    private void EmitLoadElement(ITypeSymbol elementType)
    {
        var clrType = ResolveClrType(elementType);

        if (clrType.IsValueType)
        {
            ILGenerator.Emit(OpCodes.Ldelem, clrType);
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ldelem_Ref);
        }
    }

    private void EmitObjectCreationExpression(BoundObjectCreationExpression objectCreationExpression)
    {
        var symbol = objectCreationExpression.Symbol;

        IMethodSymbol constructorSymbol = symbol switch
        {
            SourceMethodSymbol sm => sm,
            PEMethodSymbol a => a,
            SubstitutedMethodSymbol b => b,
            _ => throw new Exception("Unsupported constructor symbol")
        };

        var parameters = constructorSymbol.Parameters.ToArray();
        var arguments = objectCreationExpression.Arguments.ToArray();

        if (objectCreationExpression.Receiver is not null)
        {
            EmitExpression(objectCreationExpression.Receiver);
        }

        for (int i = 0; i < arguments.Length; i++)
        {
            var param = parameters[i];
            var argument = arguments[i];

            if (param.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
            {
                switch (argument)
                {
                    case BoundAddressOfExpression addr:
                        EmitAddressOfExpression(addr);
                        break;

                    default:
                        throw new NotSupportedException("Invalid argument for ref/out constructor parameter");
                }
            }
            else
            {
                EmitExpression(argument);

                var argType = argument.Type;
                if (argType is { IsValueType: true } &&
                    !param.Type.IsValueType)
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(argType));
                }
            }
        }

        ConstructorInfo constructorInfo = symbol switch
        {
            SourceMethodSymbol sm => (ConstructorInfo)GetMemberBuilder(sm),
            PEMethodSymbol a => a.GetConstructorInfo(),
            SubstitutedMethodSymbol m => m.GetConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen),
            _ => throw new Exception()
        };

        if (objectCreationExpression.Receiver is not null)
        {
            ILGenerator.Emit(OpCodes.Call, constructorInfo);
        }
        else
        {
            ILGenerator.Emit(OpCodes.Newobj, constructorInfo);
        }
    }

    private void EmitAssignmentExpression(BoundAssignmentExpression node)
    {
        switch (node)
        {
            case BoundLocalAssignmentExpression localAssignmentExpression:
                if (TryEmitCapturedAssignment(localAssignmentExpression.Local, localAssignmentExpression.Right))
                    break;

                var localBuilder = GetLocal(localAssignmentExpression.Local);
                if (localBuilder is null)
                    throw new InvalidOperationException($"Missing local builder for '{localAssignmentExpression.Local.Name}'");

                if (MethodBodyGenerator.IsCapturedLocal(localAssignmentExpression.Local))
                {
                    ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
                    EmitExpression(localAssignmentExpression.Right);

                    var rightType = localAssignmentExpression.Right.Type;
                    var localType = localAssignmentExpression.Local.Type;
                    if (rightType is { IsValueType: true } &&
                        localType is not null &&
                        (localType.SpecialType is SpecialType.System_Object || localType is IUnionTypeSymbol))
                    {
                        ILGenerator.Emit(OpCodes.Box, ResolveClrType(rightType));
                    }

                    var valueField = MethodBodyGenerator.GetStrongBoxValueField(localBuilder.LocalType);
                    ILGenerator.Emit(OpCodes.Stfld, valueField);
                    break;
                }

                EmitExpression(localAssignmentExpression.Right);

                if (localAssignmentExpression.Right.Type.IsValueType && localAssignmentExpression.Type.SpecialType is SpecialType.System_Object)
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(localAssignmentExpression.Right.Type));
                }

                ILGenerator.Emit(OpCodes.Stloc, localBuilder);
                break;

            case BoundFieldAssignmentExpression fieldAssignmentExpression:
                {
                    var fieldSymbol = fieldAssignmentExpression.Field;
                    var right = fieldAssignmentExpression.Right;
                    var receiver = fieldAssignmentExpression.Receiver;

                    // Load receiver (unless static)
                    if (!fieldSymbol.IsStatic && receiver is not null)
                    {
                        EmitExpression(receiver);

                        if (fieldSymbol.ContainingType!.IsValueType)
                        {
                            EmitValueTypeAddressIfNeeded(fieldSymbol.ContainingType);
                        }
                    }

                    if (!fieldSymbol.IsStatic && receiver is null)
                    {
                        ILGenerator.Emit(OpCodes.Ldarg_0);
                    }

                    // Emit RHS value
                    EmitExpression(right);

                    // Box if assigning value type to reference type
                    if (right.Type is { IsValueType: true } && !fieldSymbol.Type.IsValueType)
                    {
                        ILGenerator.Emit(OpCodes.Box, ResolveClrType(right.Type));
                    }

                    ILGenerator.Emit(fieldSymbol.IsStatic ? OpCodes.Stsfld : OpCodes.Stfld, (FieldInfo)GetField(fieldSymbol));
                    break;
                }

            case BoundPropertyAssignmentExpression propertyAssignmentExpression:
                {
                    var propertySymbol = (IPropertySymbol)propertyAssignmentExpression.Property;
                    var right = propertyAssignmentExpression.Right;
                    var receiver = propertyAssignmentExpression.Receiver;

                    // Load receiver (unless static)
                    if (!propertySymbol.IsStatic)
                    {
                        if (receiver is not null)
                        {
                            EmitExpression(receiver);

                            if (propertySymbol.ContainingType!.IsValueType)
                                EmitValueTypeAddressIfNeeded(propertySymbol.ContainingType);
                        }
                        else
                        {
                            ILGenerator.Emit(OpCodes.Ldarg_0);
                            if (propertySymbol.ContainingType!.IsValueType)
                                EmitValueTypeAddressIfNeeded(propertySymbol.ContainingType);
                        }
                    }

                    // Emit RHS value
                    EmitExpression(right);

                    // Box if assigning value type to reference type
                    if (right.Type is { IsValueType: true } && !propertySymbol.Type.IsValueType)
                    {
                        ILGenerator.Emit(OpCodes.Box, ResolveClrType(right.Type));
                    }

                    // Resolve setter
                    if (propertySymbol.SetMethod is null)
                        throw new InvalidOperationException($"Property {propertySymbol.Name} does not have a setter");

                    var setter = GetMethodInfo(propertySymbol.SetMethod);

                    ILGenerator.Emit(propertySymbol.IsStatic ? OpCodes.Call : OpCodes.Callvirt, setter);
                    break;
                }

            case BoundArrayAssignmentExpression array:
                EmitExpression(array.Left.Receiver);

                foreach (var index in array.Left.Indices)
                    EmitExpression(index);

                EmitExpression(array.Right);

                EmitStoreElement(((IArrayTypeSymbol)array.Left.Type).ElementType);
                break;

            case BoundIndexerAssignmentExpression indexer:
                EmitExpression(indexer.Left.Receiver);

                foreach (var arg in indexer.Left.Arguments)
                    EmitExpression(arg);

                EmitExpression(indexer.Right);

                var indexerProperty = (IPropertySymbol)indexer.Left.Symbol!;
                if (indexerProperty.SetMethod is null)
                    throw new InvalidOperationException("Indexer does not have a setter");

                var setter2 = GetMethodInfo(indexerProperty.SetMethod);

                ILGenerator.Emit(OpCodes.Callvirt, setter2);
                break;

            default:
                throw new NotSupportedException($"Unknown BoundAssignmentExpression: {node.GetType().Name}");
        }
    }

    private FieldInfo GetField(IFieldSymbol fieldSymbol)
    {
        return fieldSymbol switch
        {
            SourceFieldSymbol sourceFieldSymbol => sourceFieldSymbol.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen),
            PEFieldSymbol peFieldSymbol => peFieldSymbol.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen),
            _ => throw new Exception("Unsupported field symbol")
        };
    }

    private void EmitStoreElement(ITypeSymbol elementType)
    {
        if (!elementType.IsValueType)
        {
            ILGenerator.Emit(OpCodes.Stelem_Ref);
        }
        else
        {
            // Default fallback: assume int-like
            ILGenerator.Emit(OpCodes.Stelem_I4);
        }
    }

    private void EmitBinaryExpression(BoundBinaryExpression binaryExpression)
    {
        EmitExpression(binaryExpression.Left);
        EmitExpression(binaryExpression.Right);

        var op = binaryExpression.Operator;
        var operatorKind = op.OperatorKind & ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);

        switch (operatorKind)
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

            case BinaryOperatorKind.Equality:
                ILGenerator.Emit(OpCodes.Ceq);
                break;

            case BinaryOperatorKind.Inequality:
                ILGenerator.Emit(OpCodes.Ceq);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq);
                break;

            case BinaryOperatorKind.GreaterThan:
                ILGenerator.Emit(OpCodes.Cgt);
                break;

            case BinaryOperatorKind.LessThan:
                ILGenerator.Emit(OpCodes.Clt);
                break;

            case BinaryOperatorKind.GreaterThanOrEqual:
                ILGenerator.Emit(OpCodes.Clt);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq);
                break;

            case BinaryOperatorKind.LessThanOrEqual:
                ILGenerator.Emit(OpCodes.Cgt);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq);
                break;

            default:
                throw new InvalidOperationException($"Invalid operator kind '{op.OperatorKind}'");
        }
    }

    private void EmitMemberAccessExpression(BoundMemberAccessExpression memberAccessExpression, bool receiverAlreadyLoaded = false)
    {
        var symbol = memberAccessExpression.Symbol;
        var receiver = memberAccessExpression.Receiver;

        switch (symbol)
        {
            case IPropertySymbol propertySymbol:
                EmitReceiverIfNeeded(receiver, propertySymbol, receiverAlreadyLoaded);

                if (propertySymbol.ContainingType?.SpecialType == SpecialType.System_Array &&
                    propertySymbol.Name == "Length")
                {
                    ILGenerator.Emit(OpCodes.Ldlen);
                    ILGenerator.Emit(OpCodes.Conv_I4);
                    return;
                }

                if (propertySymbol.GetMethod is null)
                    throw new InvalidOperationException($"Property '{propertySymbol.Name}' does not have a getter.");

                MethodInfo getter = GetMethodInfo(propertySymbol.GetMethod);

                if (!propertySymbol.IsStatic)
                {
                    EmitValueTypeAddressIfNeeded(propertySymbol.ContainingType!);

                    EmitBoxIfNeeded(propertySymbol.ContainingType!, getter);
                }

                ILGenerator.Emit(propertySymbol.IsStatic ? OpCodes.Call : OpCodes.Callvirt, getter);
                break;

            case IFieldSymbol fieldSymbol:
                EmitReceiverIfNeeded(receiver, fieldSymbol, receiverAlreadyLoaded);

                if (!fieldSymbol.IsStatic)
                    EmitValueTypeAddressIfNeeded(fieldSymbol.ContainingType!);

                if (fieldSymbol.IsLiteral)
                {
                    EmitLiteral(fieldSymbol.GetConstantValue());
                }
                else
                {
                    var opCode = fieldSymbol.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld;
                    var fieldInfo = fieldSymbol switch
                    {
                        SourceFieldSymbol sfs => (FieldInfo)GetMemberBuilder(sfs)!,
                        _ => fieldSymbol.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen)
                    };

                    ILGenerator.Emit(opCode, fieldInfo);
                }
                break;

            default:
                throw new Exception($"Unsupported member access: {memberAccessExpression}");
        }
    }

    private void EmitConditionalAccessExpression(BoundConditionalAccessExpression conditional)
    {
        var receiverType = conditional.Receiver.Type;
        var isNullableValue = receiverType.IsNullable() && ((NullableTypeSymbol)receiverType).UnderlyingType.IsValueType;

        var receiverClrType = ResolveClrType(receiverType);
        var local = ILGenerator.DeclareLocal(receiverClrType);

        EmitExpression(conditional.Receiver);
        ILGenerator.Emit(OpCodes.Stloc, local);

        var whenNullLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        if (isNullableValue)
        {
            ILGenerator.Emit(OpCodes.Ldloca, local);
            var hasValue = receiverClrType.GetProperty("HasValue")!.GetGetMethod()!;
            ILGenerator.Emit(OpCodes.Call, hasValue);
            ILGenerator.Emit(OpCodes.Brfalse, whenNullLabel);

            ILGenerator.Emit(OpCodes.Ldloca, local);
            var getValueOrDefault = receiverClrType.GetMethod("GetValueOrDefault", Type.EmptyTypes)!;
            ILGenerator.Emit(OpCodes.Call, getValueOrDefault);
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ldloc, local);
            ILGenerator.Emit(OpCodes.Brfalse, whenNullLabel);
            ILGenerator.Emit(OpCodes.Ldloc, local);
        }

        EmitWhenNotNull(conditional.WhenNotNull);

        ILGenerator.Emit(OpCodes.Br, endLabel);
        ILGenerator.MarkLabel(whenNullLabel);
        EmitDefaultValue(conditional.Type);
        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitWhenNotNull(BoundExpression expression)
    {
        switch (expression)
        {
            case BoundMemberAccessExpression memberAccess:
                EmitMemberAccessExpression(memberAccess, receiverAlreadyLoaded: true);
                break;
            case BoundInvocationExpression invocation:
                EmitInvocationExpression(invocation, receiverAlreadyLoaded: true);
                break;
            default:
                EmitExpression(expression);
                break;
        }
    }

    private void EmitDefaultValue(ITypeSymbol type)
    {
        if (type.IsValueType)
        {
            var clr = ResolveClrType(type);
            var local = ILGenerator.DeclareLocal(clr);
            ILGenerator.Emit(OpCodes.Ldloca, local);
            ILGenerator.Emit(OpCodes.Initobj, clr);
            ILGenerator.Emit(OpCodes.Ldloc, local);
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ldnull);
        }
    }

    private void EmitReceiverIfNeeded(BoundExpression? receiver, ISymbol symbol, bool receiverAlreadyLoaded)
    {
        if (receiverAlreadyLoaded)
            return;
        if (receiver is not null && !symbol.IsStatic)
            EmitExpression(receiver);
    }

    private void EmitBoxIfNeeded(ITypeSymbol type, MethodInfo method)
    {
        if (type.IsValueType && method.DeclaringType == typeof(object))
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(type));
        }
    }

    private void EmitValueTypeAddressIfNeeded(ITypeSymbol type)
    {
        if (type.IsValueType)
        {
            var clrType = ResolveClrType(type);
            var tmp = ILGenerator.DeclareLocal(clrType);
            ILGenerator.Emit(OpCodes.Stloc, tmp);
            ILGenerator.Emit(OpCodes.Ldloca, tmp);
        }
    }

    private void EmitLiteral(object? constant)
    {
        switch (constant)
        {
            case int i:
                ILGenerator.Emit(OpCodes.Ldc_I4, i);
                break;
            case long i:
                ILGenerator.Emit(OpCodes.Ldc_I8, i);
                break;
            case float i:
                ILGenerator.Emit(OpCodes.Ldc_R4, i);
                break;
            case double i:
                ILGenerator.Emit(OpCodes.Ldc_R8, i);
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

    private void EmitInvocationExpression(BoundInvocationExpression invocationExpression, bool receiverAlreadyLoaded = false)
    {
        EmitInvocationExpressionBase(invocationExpression, receiverAlreadyLoaded);

        if (invocationExpression.Type.SpecialType == SpecialType.System_Unit)
        {
            EmitUnitValue();
        }
    }

    private void EmitUnitValue()
    {
        var unitType = MethodGenerator.TypeGenerator.CodeGen.UnitType
            ?? throw new InvalidOperationException("Unit type was not emitted.");
        var valueField = unitType.GetField("Value")
            ?? throw new InvalidOperationException("Unit.Value field missing.");
        ILGenerator.Emit(OpCodes.Ldsfld, valueField);
    }

    public void EmitInvocationExpressionBase(BoundInvocationExpression invocationExpression, bool receiverAlreadyLoaded = false)
    {
        var target = invocationExpression.Method;
        var receiver = invocationExpression.Receiver;

        // Emit receiver (for instance methods)
        if (!target.IsStatic)
        {
            if (!receiverAlreadyLoaded)
                EmitExpression(receiver);

            var receiverType = receiver?.Type;
            var effectiveReceiverType = receiverType;

            if (receiverType is NullableTypeSymbol nullable
                && nullable.UnderlyingType.IsValueType
                && target.ContainingType?.SpecialType != SpecialType.System_Nullable_T)
            {
                if (receiverAlreadyLoaded)
                    effectiveReceiverType = nullable.UnderlyingType;
            }

            if (effectiveReceiverType?.IsValueType == true)
            {
                var clrType = ResolveClrType(effectiveReceiverType);
                var methodDeclaringType = target.ContainingType;

                if (methodDeclaringType.SpecialType == SpecialType.System_Object ||
                    methodDeclaringType.TypeKind == TypeKind.Interface)
                {
                    ILGenerator.Emit(OpCodes.Box, clrType);
                }
                else if (!SymbolEqualityComparer.Default.Equals(effectiveReceiverType, methodDeclaringType))
                {
                    // Defensive fallback: method is on a different type, box to be safe
                    ILGenerator.Emit(OpCodes.Box, clrType);
                }
                else
                {
                    var tmp = ILGenerator.DeclareLocal(clrType);
                    ILGenerator.Emit(OpCodes.Stloc, tmp);
                    ILGenerator.Emit(OpCodes.Ldloca, tmp);
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
                        EmitAddressOfExpression(addressOf);
                        break;
                    case BoundLocalAccess { Symbol: ILocalSymbol local }:
                        ILGenerator.Emit(OpCodes.Ldloca, GetLocal(local));
                        break;
                    default:
                        throw new NotSupportedException($"Unsupported ref/out argument: {argument?.GetType().Name}");
                }
            }
            else
            {
                EmitExpression(argument);

                if (argument?.Type is { IsValueType: true } &&
                    !paramSymbol.Type.IsValueType)
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(argument.Type));
                }
            }
        }

        // Emit the actual call
        var isInterfaceCall = target.ContainingType?.TypeKind == TypeKind.Interface;

        if (target.IsStatic)
        {
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(target));
        }
        else if (!target.ContainingType!.IsValueType && (target.IsVirtual || isInterfaceCall))
        {
            ILGenerator.Emit(OpCodes.Callvirt, GetMethodInfo(target));
        }
        else
        {
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(target));
        }

        // Special cast for Object.GetType() to MemberInfo
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

    private void EmitFieldAccess(BoundFieldAccess fieldAccess)
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
                if (fieldSymbol.IsStatic)
                {
                    ILGenerator.Emit(OpCodes.Ldsfld, GetField(fieldSymbol));
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Ldarg_0);
                    ILGenerator.Emit(OpCodes.Ldfld, GetField(fieldSymbol));
                }
            }
        }
    }

    private void EmitPropertyAccess(BoundPropertyAccess propertyAccess)
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
            if (!propertySymbol.IsStatic)
            {
                ILGenerator.Emit(OpCodes.Ldarg_0);
                if (propertySymbol.ContainingType.IsValueType)
                    EmitValueTypeAddressIfNeeded(propertySymbol.ContainingType);
            }

            if (propertySymbol.GetMethod is null)
                throw new InvalidOperationException($"Property '{propertySymbol.Name}' does not have a getter.");

            MethodInfo getter = GetMethodInfo(propertySymbol.GetMethod);

            ILGenerator.Emit(propertySymbol.IsStatic ? OpCodes.Call : OpCodes.Callvirt, getter);
        }
    }

    private void EmitLiteralExpression(BoundLiteralExpression literalExpression)
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

            case BoundLiteralExpressionKind.CharLiteral:
                {
                    if (literalExpression.Value is char)
                    {
                        ILGenerator.Emit(OpCodes.Ldc_I4, Convert.ToInt32(literalExpression.Value));
                        ILGenerator.Emit(OpCodes.Conv_U2);
                    }
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
                    var convertedType = literalExpression.GetConvertedType();
                    if (convertedType is not null && convertedType.IsValueType)
                    {
                        EmitDefaultValue(convertedType);
                    }
                    else
                    {
                        ILGenerator.Emit(OpCodes.Ldnull);
                    }
                    break;
                }

            default:
                throw new Exception("Not supported");
        }
    }

    private void EmitIfExpression(BoundIfExpression ifStatement)
    {
        var elseLabel = ILGenerator.DefineLabel();

        // Create a scope upfront so any pattern variables introduced in the
        // condition are available within the "then" branch.
        var scope = new Scope(this);
        new ExpressionGenerator(scope, ifStatement.Condition)
            .EmitBranchOpForCondition(ifStatement.Condition, elseLabel);

        new ExpressionGenerator(scope, ifStatement.ThenBranch).Emit();

        var thenType = ifStatement.ThenBranch.Type;

        if ((ifStatement.Type?.IsUnion ?? false)
            && (thenType?.IsValueType ?? false))
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

            // Emit the 'else' block
            var scope2 = new Scope(this);
            new ExpressionGenerator(scope2, ifStatement.ElseBranch).Emit();

            var elseType = ifStatement.ElseBranch.Type;

            if ((ifStatement.Type?.IsUnion ?? false)
                && (elseType?.IsValueType ?? false))
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

    private void EmitBranchOpForCondition(BoundExpression expression, Label end)
    {
        if (expression is BoundParenthesizedExpression parenthesizedExpression)
        {
            EmitBranchOpForCondition(parenthesizedExpression.Expression, end);
            return;
        }

        if (expression is BoundBinaryExpression binaryExpression)
        {
            EmitExpression(binaryExpression.Left);
            EmitExpression(binaryExpression.Right);

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

            EmitExpression(expression);
            ILGenerator.Emit(OpCodes.Brfalse_S, end);
        }
    }

    private void EmitBlock(BoundBlockExpression block)
    {
        var scope = new Scope(this, block.LocalsToDispose);
        var statements = block.Statements.ToArray();

        BoundExpression? resultExpression = null;
        var count = statements.Length;

        if (count > 0 &&
            statements[^1] is BoundExpressionStatement exprStmt &&
            exprStmt.Expression.Type?.SpecialType is not SpecialType.System_Void)
        {
            resultExpression = exprStmt.Expression;
            count--;
        }

        for (int i = 0; i < count; i++)
        {
            var statement = statements[i];
            EmitStatement(statement, scope);
        }

        LocalBuilder? resultTemp = null;
        if (resultExpression is not null)
        {
            new ExpressionGenerator(scope, resultExpression).Emit();

            if (!block.LocalsToDispose.IsDefaultOrEmpty && block.LocalsToDispose.Length > 0)
            {
                var resultType = resultExpression.Type;
                if (resultType is not null)
                {
                    var clrType = ResolveClrType(resultType);
                    resultTemp = ILGenerator.DeclareLocal(clrType);
                    ILGenerator.Emit(OpCodes.Stloc, resultTemp);
                }
            }
        }

        EmitDispose(block.LocalsToDispose);

        if (resultTemp is not null)
            ILGenerator.Emit(OpCodes.Ldloc, resultTemp);
    }

    private void EmitStatement(BoundStatement statement, Scope scope)
    {
        new StatementGenerator(scope, statement).Emit();
    }

    private void EmitStatement(BoundStatement statement)
    {
        new StatementGenerator(this, statement).Emit();
    }

    public MethodInfo GetMethodInfo(IMethodSymbol methodSymbol)
    {
        if (methodSymbol.IsAlias && methodSymbol is IAliasSymbol alias)
            return GetMethodInfo((IMethodSymbol)alias.UnderlyingSymbol);

        if (methodSymbol is PEMethodSymbol pEMethodSymbol)
            return pEMethodSymbol.GetMethodInfo();

        if (methodSymbol is SubstitutedMethodSymbol substitutedMethod)
            return substitutedMethod.GetMethodInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen);

        if (methodSymbol is ConstructedMethodSymbol constructedMethod)
        {
            var definitionInfo = GetMethodInfo(constructedMethod.Definition);

            if (constructedMethod.TypeArguments.IsDefaultOrEmpty || constructedMethod.TypeArguments.Length == 0)
                return definitionInfo;

            MethodInfo genericDefinition;
            if (definitionInfo.IsGenericMethodDefinition)
            {
                genericDefinition = definitionInfo;
            }
            else if (definitionInfo.ContainsGenericParameters)
            {
                genericDefinition = definitionInfo.GetGenericMethodDefinition();
            }
            else
            {
                return definitionInfo;
            }

            if (constructedMethod.TypeArguments.Any(static arg => arg is null))
                throw new InvalidOperationException("Constructed method symbol is missing type arguments.");

            var typeArguments = constructedMethod.TypeArguments
                .Select(arg => ResolveClrType(arg!))
                .ToArray();

            return genericDefinition.MakeGenericMethod(typeArguments);
        }

        if (methodSymbol is SourceMethodSymbol sourceMethodSymbol)
        {
            var m = MethodGenerator.TypeGenerator.CodeGen.GetMemberBuilder(sourceMethodSymbol);
            return (MethodInfo)m;
        }

        throw new InvalidOperationException();
    }
}
