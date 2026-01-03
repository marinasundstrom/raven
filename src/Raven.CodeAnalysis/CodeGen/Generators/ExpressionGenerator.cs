using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;
using System.Text;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal class ExpressionGenerator : Generator
{
    private static readonly DelegateConstructorCacheKeyComparer s_delegateConstructorComparer = new();

    private readonly BoundExpression _expression;
    private readonly bool _preserveResult;
    private readonly Dictionary<DelegateConstructorCacheKey, ConstructorInfo> _delegateConstructorCache = new(s_delegateConstructorComparer);
    private Type[]? _delegateConstructorSignature;

    private static readonly MethodInfo GetTypeFromHandleMethod = typeof(Type)
        .GetMethod(nameof(Type.GetTypeFromHandle), BindingFlags.Public | BindingFlags.Static, binder: null, new[] { typeof(RuntimeTypeHandle) }, modifiers: null)
        ?? throw new InvalidOperationException("Failed to resolve Type.GetTypeFromHandle(RuntimeTypeHandle).");

    private static readonly MethodInfo ConsoleWriteLineString = typeof(Console)
        .GetMethod(nameof(Console.WriteLine), BindingFlags.Public | BindingFlags.Static, new[] { typeof(string) })
        ?? throw new InvalidOperationException("Failed to resolve Console.WriteLine(string).");

    private static readonly MethodInfo ConsoleWriteLineStringObject = typeof(Console)
        .GetMethod(nameof(Console.WriteLine), BindingFlags.Public | BindingFlags.Static, new[] { typeof(string), typeof(object) })
        ?? throw new InvalidOperationException("Failed to resolve Console.WriteLine(string, object).");

    private static readonly MethodInfo StringFormatStringObject = typeof(string)
        .GetMethod(nameof(string.Format), BindingFlags.Public | BindingFlags.Static, new[] { typeof(string), typeof(object) })
        ?? throw new InvalidOperationException("Failed to resolve string.Format(string, object).");

    public ExpressionGenerator(Generator parent, BoundExpression expression, bool preserveResult = true) : base(parent)
    {
        _expression = expression;
        _preserveResult = preserveResult;
    }

    public override void Emit()
    {
        if (!_preserveResult && _expression is BoundAssignmentExpression assignmentExpression)
        {
            EmitAssignmentExpression(assignmentExpression, preserveResult: false);
            return;
        }

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

            case BoundIndexExpression indexExpression:
                EmitIndexExpression(indexExpression);
                break;

            case BoundRangeExpression rangeExpression:
                EmitRangeExpression(rangeExpression);
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

            case BoundDefaultValueExpression defaultValueExpression:
                EmitDefaultValue(defaultValueExpression.Type);
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

            case BoundTryExpression tryExpression:
                EmitTryExpression(tryExpression);
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

            case BoundTypeExpression typeExpression:
                if (!TryEmitDiscriminatedUnionCaseCreation(typeExpression.Type))
                    break;

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

            case BoundMethodGroupExpression methodGroupExpression:
                EmitMethodGroupExpression(methodGroupExpression);
                break;

            case BoundErrorExpression errorExpression:
                EmitErrorExpression(errorExpression);
                break;

            default:
                throw new NotSupportedException($"Unsupported expression type: {expression.GetType()}");
        }
    }

    private bool TryEmitDiscriminatedUnionCaseCreation(ITypeSymbol typeSymbol)
    {
        if (typeSymbol is not INamedTypeSymbol namedType)
            return false;

        if (namedType.TryGetDiscriminatedUnionCase() is null &&
            namedType.ContainingType?.TryGetDiscriminatedUnion() is null)
        {
            return false;
        }

        var parameterlessCtor = namedType.Constructors.FirstOrDefault(static ctor => ctor.Parameters.Length == 0);
        if (parameterlessCtor is null)
            return false;

        EmitObjectCreationExpression(new BoundObjectCreationExpression(parameterlessCtor, ImmutableArray<BoundExpression>.Empty));
        return true;
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

        EmitDelegateCreation(delegateCreation.Receiver, method, delegateTypeSymbol);
    }

    private void EmitMethodGroupExpression(BoundMethodGroupExpression methodGroup)
    {
        var method = methodGroup.SelectedMethod;
        var delegateType = methodGroup.DelegateType ?? methodGroup.DelegateTypeFactory?.Invoke();

        if (method is null ||
            delegateType is not INamedTypeSymbol delegateTypeSymbol ||
            delegateTypeSymbol.TypeKind != TypeKind.Delegate)
        {
            EmitDefaultValue(methodGroup.MethodGroupType);
            return;
        }

        EmitDelegateCreation(methodGroup.Receiver, method, delegateTypeSymbol);
    }

    private void EmitErrorExpression(BoundErrorExpression errorExpression)
    {
        if (!_preserveResult)
            return;

        if (errorExpression.Type.SpecialType == SpecialType.System_Void)
            return;

        EmitDefaultValue(errorExpression.Type);
    }

    private void EmitDelegateCreation(BoundExpression? receiver, IMethodSymbol method, INamedTypeSymbol delegateType)
    {
        if (delegateType is SynthesizedDelegateTypeSymbol synthesized)
            MethodGenerator.TypeGenerator.CodeGen.GetTypeBuilder(synthesized);

        var delegateClrType = ResolveClrType(delegateType);
        var ctor = GetDelegateConstructor(delegateType, delegateClrType);

        var methodInfo = GetMethodInfo(method);

        if (method.IsStatic)
        {
            ILGenerator.Emit(OpCodes.Ldnull);
            ILGenerator.Emit(OpCodes.Ldftn, methodInfo);
        }
        else
        {
            if (receiver is null)
                throw new InvalidOperationException("Instance delegate creation requires a receiver.");

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

    private void EmitCapturedLambda(
        BoundLambdaExpression lambdaExpression,
        TypeGenerator.LambdaClosure closure,
        MethodInfo methodInfo,
        ITypeSymbol delegateTypeSymbol,
        Type delegateType)
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

        var delegateCtor = GetDelegateConstructor(delegateTypeSymbol, delegateType);

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
        if (!MethodBodyGenerator.TryGetCapturedField(symbol, out var fieldBuilder, out var fromStateMachine))
            return false;

        if (fromStateMachine)
            ILGenerator.Emit(OpCodes.Ldarg_0);
        else
            MethodBodyGenerator.EmitLoadClosure();
        ILGenerator.Emit(OpCodes.Ldfld, fieldBuilder);
        return true;
    }

    private bool TryEmitCapturedVariableLoad(ISymbol? symbol)
    {
        if (symbol is null)
            return false;

        if (!MethodBodyGenerator.TryGetCapturedField(symbol, out var fieldBuilder, out var fromStateMachine))
            return false;

        if (fromStateMachine)
            ILGenerator.Emit(OpCodes.Ldarg_0);
        else
            MethodBodyGenerator.EmitLoadClosure();
        ILGenerator.Emit(OpCodes.Ldfld, fieldBuilder);
        return true;
    }

    private bool TryEmitCapturedAssignment(ISymbol symbol, BoundExpression value)
    {
        if (!MethodBodyGenerator.TryGetCapturedField(symbol, out var fieldBuilder, out var fromStateMachine))
            return false;

        if (fromStateMachine)
            ILGenerator.Emit(OpCodes.Ldarg_0);
        else
            MethodBodyGenerator.EmitLoadClosure();
        EmitExpression(value);

        if (symbol is ILocalSymbol localSymbol &&
            value.Type is { IsValueType: true } &&
            localSymbol.Type is not null &&
            (localSymbol.Type.SpecialType is SpecialType.System_Object || localSymbol.Type is ITypeUnionSymbol))
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(value.Type));
        }

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
            lambdaGenerator = new MethodGenerator(typeGenerator, lambdaSymbol, typeGenerator.CodeGen.ILBuilderFactory);

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

        var delegateTypeSymbol = lambdaExpression.DelegateType
            ?? throw new InvalidOperationException("Lambda delegate type missing.");
        var delegateType = ResolveClrType(delegateTypeSymbol);

        if (hasCaptures)
        {
            if (closure is null && !typeGenerator.TryGetLambdaClosure(lambdaSymbol, out closure))
                throw new InvalidOperationException("Missing closure information for captured lambda.");

            EmitCapturedLambda(lambdaExpression, closure!, methodInfo, delegateTypeSymbol, delegateType);
            return;
        }

        var ctor = GetDelegateConstructor(delegateTypeSymbol, delegateType);

        ILGenerator.Emit(OpCodes.Ldnull);
        ILGenerator.Emit(OpCodes.Ldftn, methodInfo);
        ILGenerator.Emit(OpCodes.Newobj, ctor);
    }

    private ConstructorInfo GetDelegateConstructor(ITypeSymbol delegateType, Type delegateClrType)
    {
        var cacheKey = CreateDelegateConstructorCacheKey(delegateType, delegateClrType);

        if (_delegateConstructorCache.TryGetValue(cacheKey, out var ctor))
            return ctor;

        if (delegateType is INamedTypeSymbol namedDelegate)
        {
            ctor = TryResolveDelegateConstructor(namedDelegate);
        }

        if (ctor is null)
        {
            _delegateConstructorSignature ??=
            [
                ResolveClrType(Compilation.GetSpecialType(SpecialType.System_Object)),
                ResolveClrType(Compilation.GetSpecialType(SpecialType.System_IntPtr))
            ];

            ctor = delegateClrType.GetConstructor(
                    BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
                    binder: null,
                    types: _delegateConstructorSignature,
                    modifiers: null)
                ?? throw new InvalidOperationException($"Delegate '{delegateClrType}' lacks the expected constructor.");
        }

        _delegateConstructorCache[cacheKey] = ctor;
        return ctor;
    }

    private DelegateConstructorCacheKey CreateDelegateConstructorCacheKey(ITypeSymbol delegateType, Type delegateClrType)
    {
        if (delegateType is SynthesizedDelegateTypeSymbol synthesized)
        {
            var parameterTypes = synthesized.ParameterTypes.IsDefaultOrEmpty
                ? ImmutableArray<ITypeSymbol>.Empty
                : synthesized.ParameterTypes;
            var refKinds = synthesized.ParameterRefKinds.IsDefaultOrEmpty
                ? ImmutableArray<RefKind>.Empty
                : synthesized.ParameterRefKinds;

            return new DelegateConstructorCacheKey(delegateClrType, synthesized.ReturnType, parameterTypes, refKinds);
        }

        if (delegateType is INamedTypeSymbol namedDelegate && namedDelegate.GetDelegateInvokeMethod() is { } invoke)
        {
            var parameterTypesBuilder = ImmutableArray.CreateBuilder<ITypeSymbol>(invoke.Parameters.Length);
            var refKindsBuilder = ImmutableArray.CreateBuilder<RefKind>(invoke.Parameters.Length);

            foreach (var parameter in invoke.Parameters)
            {
                parameterTypesBuilder.Add(parameter.Type);
                refKindsBuilder.Add(parameter.RefKind);
            }

            return new DelegateConstructorCacheKey(
                delegateClrType,
                invoke.ReturnType,
                parameterTypesBuilder.MoveToImmutable(),
                refKindsBuilder.MoveToImmutable());
        }

        return new DelegateConstructorCacheKey(
            delegateClrType,
            delegateType,
            ImmutableArray<ITypeSymbol>.Empty,
            ImmutableArray<RefKind>.Empty);
    }

    private ConstructorInfo? TryResolveDelegateConstructor(INamedTypeSymbol delegateType)
    {
        foreach (var constructor in delegateType.Constructors)
        {
            if (constructor.MethodKind is not MethodKind.Constructor)
                continue;

            if (!IsDelegateConstructorSignature(constructor))
                continue;

            var ctorInfo = TryGetConstructorInfo(constructor);
            if (ctorInfo is not null)
                return ctorInfo;
        }

        return null;
    }

    private bool IsDelegateConstructorSignature(IMethodSymbol constructor)
    {
        if (constructor.Parameters.Length != 2)
            return false;

        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
        var intPtrType = Compilation.GetSpecialType(SpecialType.System_IntPtr);

        return SymbolEqualityComparer.Default.Equals(constructor.Parameters[0].Type, objectType)
            && SymbolEqualityComparer.Default.Equals(constructor.Parameters[1].Type, intPtrType);
    }

    private ConstructorInfo? TryGetConstructorInfo(IMethodSymbol constructor)
    {
        try
        {
            return constructor.GetClrConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen);
        }
        catch (InvalidOperationException)
        {
            return null;
        }
    }

    private ConstructorInfo? TryGetConstructedConstructorInfo(ConstructedMethodSymbol constructed)
    {
        var definitionCtor = TryGetConstructorInfo(constructed.Definition);
        if (definitionCtor is null)
            return null;

        var declaringType = definitionCtor.DeclaringType;
        if (declaringType is null)
            return definitionCtor;

        if (constructed.TypeArguments.IsDefaultOrEmpty || constructed.TypeArguments.Length == 0)
            return definitionCtor;

        if (!declaringType.IsGenericTypeDefinition && !declaringType.ContainsGenericParameters)
            return definitionCtor;

        var runtimeArguments = constructed.TypeArguments.Select(ResolveClrType).ToArray();
        var constructedType = declaringType.MakeGenericType(runtimeArguments);

        _delegateConstructorSignature ??=
        [
            ResolveClrType(Compilation.GetSpecialType(SpecialType.System_Object)),
            ResolveClrType(Compilation.GetSpecialType(SpecialType.System_IntPtr))
        ];

        return constructedType.GetConstructor(
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
            binder: null,
            types: _delegateConstructorSignature,
            modifiers: null);
    }

    private void EmitUnitExpression(BoundUnitExpression unitExpression)
    {
        if (_preserveResult)
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

    private void EmitIndexExpression(BoundIndexExpression indexExpression)
    {
        if (indexExpression.Type.TypeKind == TypeKind.Error)
        {
            EmitDefaultValue(indexExpression.Type);
            return;
        }

        EmitExpression(indexExpression.Value);

        var indexClrType = ResolveClrType(indexExpression.Type);
        var ctor = indexClrType.GetConstructor(new[] { typeof(int), typeof(bool) })
            ?? throw new InvalidOperationException("Missing System.Index constructor.");

        ILGenerator.Emit(indexExpression.IsFromEnd ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Newobj, ctor);
    }

    private void EmitRangeExpression(BoundRangeExpression rangeExpression)
    {
        if (rangeExpression.Type.TypeKind == TypeKind.Error)
        {
            EmitDefaultValue(rangeExpression.Type);
            return;
        }

        var rangeClrType = ResolveClrType(rangeExpression.Type);

        if (rangeExpression.Left is null && rangeExpression.Right is null)
        {
            var allProperty = rangeClrType.GetProperty(nameof(Range.All), BindingFlags.Public | BindingFlags.Static)
                ?? throw new InvalidOperationException("Missing System.Range.All property.");
            var getter = allProperty.GetMethod ?? throw new InvalidOperationException("Missing System.Range.All getter.");
            ILGenerator.Emit(OpCodes.Call, getter);
            return;
        }

        if (rangeExpression.Left is null && rangeExpression.Right is { } end)
        {
            EmitIndexExpression(end);

            var endAt = rangeClrType.GetMethod(
                nameof(Range.EndAt),
                BindingFlags.Public | BindingFlags.Static,
                binder: null,
                new[] { ResolveClrType(end.Type) },
                modifiers: null)
                ?? throw new InvalidOperationException("Missing System.Range.EndAt method.");

            ILGenerator.Emit(OpCodes.Call, endAt);
            return;
        }

        if (rangeExpression.Left is { } start && rangeExpression.Right is null)
        {
            EmitIndexExpression(start);

            var startAt = rangeClrType.GetMethod(
                nameof(Range.StartAt),
                BindingFlags.Public | BindingFlags.Static,
                binder: null,
                new[] { ResolveClrType(start.Type) },
                modifiers: null)
                ?? throw new InvalidOperationException("Missing System.Range.StartAt method.");

            ILGenerator.Emit(OpCodes.Call, startAt);
            return;
        }

        if (rangeExpression.Left is null || rangeExpression.Right is null)
        {
            EmitDefaultValue(rangeExpression.Type);
            return;
        }

        EmitIndexExpression(rangeExpression.Left);
        EmitIndexExpression(rangeExpression.Right);

        var ctor = rangeClrType.GetConstructor(new[]
        {
            ResolveClrType(rangeExpression.Left.Type),
            ResolveClrType(rangeExpression.Right.Type)
        }) ?? throw new InvalidOperationException("Missing System.Range constructor.");

        ILGenerator.Emit(OpCodes.Newobj, ctor);
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

            case IFieldSymbol field when field.IsStatic:
                ILGenerator.Emit(OpCodes.Ldsflda, GetField(field));
                if (TryGetAsyncInvestigationFieldLabel(field, out var staticFieldLabel))
                    EmitAsyncInvestigationAddressLogPreservingPointer(staticFieldLabel);
                break;

            case IFieldSymbol field:
                if (addressOf.Receiver is not null)
                {
                    if (field.ContainingType?.IsValueType == true && addressOf.Receiver is BoundSelfExpression)
                    {
                        if (MethodSymbol.IsStatic)
                            throw new NotSupportedException($"Cannot take address of instance field '{field.Name}' in a static context.");

                        ILGenerator.Emit(OpCodes.Ldarg_0);
                    }
                    else if (!TryEmitValueTypeReceiverAddress(addressOf.Receiver, addressOf.Receiver.Type, field.ContainingType))
                    {
                        EmitExpression(addressOf.Receiver);

                        if (field.ContainingType?.IsValueType == true)
                            EmitValueTypeAddressIfNeeded(addressOf.Receiver.Type, field.ContainingType);
                    }
                }
                else
                {
                    if (MethodSymbol.IsStatic)
                        throw new NotSupportedException($"Cannot take address of instance field '{field.Name}' in a static context.");

                    ILGenerator.Emit(OpCodes.Ldarg_0);
                }

                ILGenerator.Emit(OpCodes.Ldflda, GetField(field));
                if (TryGetAsyncInvestigationFieldLabel(field, out var fieldLabel))
                    EmitAsyncInvestigationAddressLogPreservingPointer(fieldLabel);
                break;

            case ITypeSymbol:
                if (MethodSymbol.IsStatic)
                    throw new NotSupportedException("Cannot take the address of 'self' in a static context.");

                ILGenerator.Emit(OpCodes.Ldarg_0);
                break;

            case null when addressOf.Storage is BoundArrayAccessExpression arrayAccess:
                EmitArrayElementAddress(arrayAccess);
                break;

            default:
                throw new NotSupportedException($"Cannot take address of: {addressOf.Symbol}");
        }
    }

    private void EmitIsPatternExpression(BoundIsPatternExpression isPatternExpression)
    {
        EmitExpression(isPatternExpression.Expression); // Push the value of the expression onto the stack

        var expressionType = isPatternExpression.Expression.Type;
        if (RequiresValueTypeHandling(expressionType) && expressionType.TypeKind != TypeKind.Error)
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

        var endLabel = ILGenerator.DefineLabel();
        var fallthroughLabel = ILGenerator.DefineLabel();
        var exitLabel = ILGenerator.DefineLabel();

        foreach (var arm in matchExpression.Arms)
        {
            var nextArmLabel = ILGenerator.DefineLabel();
            var scope = new Scope(this);

            ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
            EmitPattern(arm.Pattern, scope);
            ILGenerator.Emit(OpCodes.Brfalse, nextArmLabel);

            if (arm.Guard is not null)
            {
                new ExpressionGenerator(scope, arm.Guard).Emit();
                ILGenerator.Emit(OpCodes.Brfalse, nextArmLabel);
            }

            new ExpressionGenerator(scope, arm.Expression).Emit();

            var armType = arm.Expression.Type;
            if ((matchExpression.Type?.IsTypeUnion ?? false) && (armType?.IsValueType ?? false))
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(armType));

            ILGenerator.Emit(OpCodes.Br, endLabel);

            ILGenerator.MarkLabel(nextArmLabel);
        }

        ILGenerator.Emit(OpCodes.Br, fallthroughLabel);

        ILGenerator.MarkLabel(endLabel);
        ILGenerator.Emit(OpCodes.Br, exitLabel);

        ILGenerator.MarkLabel(fallthroughLabel);

        var exceptionCtor = typeof(InvalidOperationException).GetConstructor(new[] { typeof(string) })
            ?? throw new InvalidOperationException("Failed to resolve InvalidOperationException(string) constructor.");
        ILGenerator.Emit(OpCodes.Ldstr, "Match expression was not exhaustive.");
        ILGenerator.Emit(OpCodes.Newobj, exceptionCtor);
        ILGenerator.Emit(OpCodes.Throw);

        ILGenerator.MarkLabel(exitLabel);
    }

    private static bool RequiresValueTypeHandling(ITypeSymbol typeSymbol)
    {
        return typeSymbol.IsValueType || typeSymbol is ITypeParameterSymbol { IsReferenceType: false };
    }

    private static bool IsKnownReferenceType(ITypeSymbol typeSymbol)
    {
        if (typeSymbol is ITypeParameterSymbol typeParameter)
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0;

        return typeSymbol.IsReferenceType == true;
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
            var isReferencePattern = IsKnownReferenceType(typeSymbol);

            if (!isReferencePattern && clrType.IsGenericParameter)
            {
                var attributes = clrType.GenericParameterAttributes;
                if ((attributes & GenericParameterAttributes.ReferenceTypeConstraint) != 0)
                    isReferencePattern = true;
            }

            var patternLocal = EmitDesignation(declarationPattern.Designator, scope);

            if (!isReferencePattern)
            {
                var labelSuccess = ILGenerator.DefineLabel();
                var labelDone = ILGenerator.DefineLabel();
                var requiresUnbox = clrType.IsValueType;

                if (clrType.IsGenericParameter &&
                    (clrType.GenericParameterAttributes & GenericParameterAttributes.NotNullableValueTypeConstraint) != 0)
                {
                    requiresUnbox = true;
                }

                ILGenerator.Emit(OpCodes.Isinst, clrType);
                ILGenerator.Emit(OpCodes.Dup);
                ILGenerator.Emit(OpCodes.Brtrue, labelSuccess);
                ILGenerator.Emit(OpCodes.Pop);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Br, labelDone);

                ILGenerator.MarkLabel(labelSuccess);
                ILGenerator.Emit(requiresUnbox ? OpCodes.Unbox_Any : OpCodes.Castclass, clrType);
                if (patternLocal is not null)
                {
                    ILGenerator.Emit(OpCodes.Stloc, patternLocal);
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Pop);
                }
                ILGenerator.Emit(OpCodes.Ldc_I4_1);

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
        else if (pattern is BoundCasePattern casePattern)
        {
            var unionClrType = Generator.InstantiateType(ResolveClrType(casePattern.CaseSymbol.Union));
            var caseClrType = Generator.InstantiateType(ResolveClrType(casePattern.CaseSymbol));
            var unionLocal = ILGenerator.DeclareLocal(unionClrType);
            var caseLocal = ILGenerator.DeclareLocal(caseClrType);

            var labelSuccess = ILGenerator.DefineLabel();
            var labelFail = ILGenerator.DefineLabel();
            var labelDone = ILGenerator.DefineLabel();

            ILGenerator.Emit(OpCodes.Isinst, unionClrType);
            ILGenerator.Emit(OpCodes.Dup);
            ILGenerator.Emit(OpCodes.Brtrue, labelSuccess);
            ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            ILGenerator.Emit(OpCodes.Br, labelDone);

            ILGenerator.MarkLabel(labelSuccess);
            ILGenerator.Emit(OpCodes.Unbox_Any, unionClrType);
            ILGenerator.Emit(OpCodes.Stloc, unionLocal);

            ILGenerator.Emit(OpCodes.Ldloca, caseLocal);
            ILGenerator.Emit(OpCodes.Initobj, caseClrType);

            ILGenerator.Emit(OpCodes.Ldloca, unionLocal);
            ILGenerator.Emit(OpCodes.Ldloca, caseLocal);
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(casePattern.TryGetMethod));
            ILGenerator.Emit(OpCodes.Brfalse, labelFail);

            var parameterCount = Math.Min(
                casePattern.CaseSymbol.ConstructorParameters.Length,
                casePattern.Arguments.Length);

            for (var i = 0; i < parameterCount; i++)
            {
                var parameter = casePattern.CaseSymbol.ConstructorParameters[i];
                var propertyName = GetCasePropertyName(parameter.Name);
                var propertySymbol = casePattern.CaseSymbol
                    .GetMembers(propertyName)
                    .OfType<IPropertySymbol>()
                    .FirstOrDefault();

                if (propertySymbol?.GetMethod is null)
                {
                    ILGenerator.Emit(OpCodes.Br, labelFail);
                    break;
                }

                ILGenerator.Emit(OpCodes.Ldloca, caseLocal);
                ILGenerator.Emit(OpCodes.Call, GetMethodInfo(propertySymbol.GetMethod));

                if (RequiresValueTypeHandling(propertySymbol.Type))
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(propertySymbol.Type));

                EmitPattern(casePattern.Arguments[i], scope);
                ILGenerator.Emit(OpCodes.Brfalse, labelFail);
            }

            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            ILGenerator.Emit(OpCodes.Br, labelDone);

            ILGenerator.MarkLabel(labelFail);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);

            ILGenerator.MarkLabel(labelDone);
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
                ILGenerator.Emit(OpCodes.Brfalse, labelFail);

                EmitPattern(binaryPattern.Right, scope);
                ILGenerator.Emit(OpCodes.Brfalse, labelFail);

                ILGenerator.Emit(OpCodes.Ldc_I4_1);
                ILGenerator.Emit(OpCodes.Br, labelDone);

                ILGenerator.MarkLabel(labelFail);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);

                ILGenerator.MarkLabel(labelDone);
            }
            else if (binaryPattern.Kind == BoundPatternKind.Or)
            {
                var labelTrue = ILGenerator.DefineLabel();

                EmitPattern(binaryPattern.Left, scope);
                ILGenerator.Emit(OpCodes.Brtrue, labelTrue);

                EmitPattern(binaryPattern.Right, scope);
                ILGenerator.Emit(OpCodes.Brtrue, labelTrue);

                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Br, labelDone);

                ILGenerator.MarkLabel(labelTrue);
                ILGenerator.Emit(OpCodes.Ldc_I4_1);

                ILGenerator.MarkLabel(labelDone);
            }
            else
            {
                throw new NotSupportedException("Unsupported binary pattern kind");
            }
        }
        else if (pattern is BoundTuplePattern tuplePattern)
        {
            var tupleInterfaceType = Compilation.ResolveRuntimeType("System.Runtime.CompilerServices.ITuple")
                ?? throw new InvalidOperationException("Unable to resolve runtime type for System.Runtime.CompilerServices.ITuple.");
            var lengthGetter = tupleInterfaceType.GetProperty("Length")?.GetMethod;
            var itemGetter = tupleInterfaceType.GetProperty("Item")?.GetMethod;

            if (lengthGetter is null || itemGetter is null)
                throw new NotSupportedException("System.Runtime.CompilerServices.ITuple is required to match tuple patterns.");

            var tupleLocal = ILGenerator.DeclareLocal(tupleInterfaceType);
            var labelFail = ILGenerator.DefineLabel();
            var labelDone = ILGenerator.DefineLabel();

            ILGenerator.Emit(OpCodes.Isinst, tupleInterfaceType);
            ILGenerator.Emit(OpCodes.Stloc, tupleLocal);
            ILGenerator.Emit(OpCodes.Ldloc, tupleLocal);
            ILGenerator.Emit(OpCodes.Brfalse, labelFail);

            ILGenerator.Emit(OpCodes.Ldloc, tupleLocal);
            ILGenerator.Emit(OpCodes.Callvirt, lengthGetter);
            ILGenerator.Emit(OpCodes.Ldc_I4, tuplePattern.Elements.Length);
            ILGenerator.Emit(OpCodes.Bne_Un, labelFail);

            for (var i = 0; i < tuplePattern.Elements.Length; i++)
            {
                ILGenerator.Emit(OpCodes.Ldloc, tupleLocal);
                ILGenerator.Emit(OpCodes.Ldc_I4, i);
                ILGenerator.Emit(OpCodes.Callvirt, itemGetter);
                EmitPattern(tuplePattern.Elements[i], scope);
                ILGenerator.Emit(OpCodes.Brfalse, labelFail);
            }

            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            ILGenerator.Emit(OpCodes.Br, labelDone);

            ILGenerator.MarkLabel(labelFail);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);

            ILGenerator.MarkLabel(labelDone);
        }
        else
        {
            throw new NotSupportedException("Unsupported pattern");
        }
    }

    private static string GetCasePropertyName(string parameterName)
    {
        if (string.IsNullOrEmpty(parameterName))
            return parameterName;

        if (char.IsUpper(parameterName[0]))
            return parameterName;

        Span<char> buffer = stackalloc char[parameterName.Length];
        parameterName.AsSpan().CopyTo(buffer);
        buffer[0] = char.ToUpperInvariant(buffer[0]);
        return new string(buffer);
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
        ILGenerator.Emit(OpCodes.Brtrue, notNullLabel);

        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Br, endLabel);

        ILGenerator.MarkLabel(notNullLabel);
        ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
        EmitConstantAsObject(literal, value);

        var runtimeObjectType = Compilation.ResolveRuntimeType("System.Object")
            ?? throw new InvalidOperationException("Unable to resolve runtime type for System.Object.");
        var equalsMethod = runtimeObjectType.GetMethod(nameof(object.Equals), new[] { runtimeObjectType })
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

    private IILocal? EmitDesignation(BoundDesignator designation, Generator scope)
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

            var ctorInfo = ctor.GetClrConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen);

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
        var ctorInfo = ctor.GetClrConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen);

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

    private void EmitSpreadElement(IILocal collectionLocal, BoundSpreadElement spread, ITypeSymbol elementType, IMethodSymbol addMethod)
    {
        EmitExpression(spread.Expression);

        var enumerable = (INamedTypeSymbol)Compilation.GetTypeByMetadataName("System.Collections.IEnumerable")!;
        ILGenerator.Emit(OpCodes.Castclass, ResolveClrType(enumerable));
        var getEnumerator = enumerable
            .GetMembers(nameof(IEnumerable.GetEnumerator))
            .OfType<IMethodSymbol>()
            .First();
        ILGenerator.Emit(OpCodes.Callvirt, getEnumerator.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));
        var enumeratorType = getEnumerator.ReturnType;
        var enumeratorLocal = ILGenerator.DeclareLocal(ResolveClrType(enumeratorType));
        ILGenerator.Emit(OpCodes.Stloc, enumeratorLocal);

        var loopStart = ILGenerator.DefineLabel();
        var loopEnd = ILGenerator.DefineLabel();

        ILGenerator.MarkLabel(loopStart);
        var moveNext = enumeratorType.GetMembers(nameof(IEnumerator.MoveNext))!.OfType<IMethodSymbol>().First();
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, moveNext.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));
        ILGenerator.Emit(OpCodes.Brfalse, loopEnd);

        ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
        var currentProp = enumeratorType
            .GetMembers(nameof(IEnumerator.Current))
            .OfType<IPropertySymbol>()
            .First()
            .GetMethod!;
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, currentProp.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));

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

    private void EmitSpreadElement(IILocal collectionLocal, BoundSpreadElement spread, ITypeSymbol elementType, MethodInfo addMethodInfo)
    {
        EmitExpression(spread.Expression);

        var enumerable = (INamedTypeSymbol)Compilation.GetTypeByMetadataName("System.Collections.IEnumerable")!;
        ILGenerator.Emit(OpCodes.Castclass, ResolveClrType(enumerable));
        var getEnumerator = enumerable
            .GetMembers(nameof(IEnumerable.GetEnumerator))
            .OfType<IMethodSymbol>()
            .First();
        ILGenerator.Emit(OpCodes.Callvirt, getEnumerator.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));
        var enumeratorType = getEnumerator.ReturnType;
        var enumeratorLocal = ILGenerator.DeclareLocal(ResolveClrType(enumeratorType));
        ILGenerator.Emit(OpCodes.Stloc, enumeratorLocal);

        var loopStart = ILGenerator.DefineLabel();
        var loopEnd = ILGenerator.DefineLabel();

        ILGenerator.MarkLabel(loopStart);
        var moveNext = enumeratorType.GetMembers(nameof(IEnumerator.MoveNext))!.OfType<IMethodSymbol>().First();
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, moveNext.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));
        ILGenerator.Emit(OpCodes.Brfalse, loopEnd);

        ILGenerator.Emit(OpCodes.Ldloc, collectionLocal);
        var currentProp = enumeratorType
            .GetMembers(nameof(IEnumerator.Current))
            .OfType<IPropertySymbol>()
            .First()
            .GetMethod!;
        ILGenerator.Emit(OpCodes.Ldloc, enumeratorLocal);
        ILGenerator.Emit(OpCodes.Callvirt, currentProp.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen));

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

            var ctorInfo = ctor.GetClrConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen);

            ILGenerator.Emit(OpCodes.Newobj, ctorInfo);
        }
    }

    private void EmitArrayAccessExpression(BoundArrayAccessExpression boundArrayAccessExpression)
    {
        var arrayType = boundArrayAccessExpression.Receiver.Type as IArrayTypeSymbol;

        var requiresLength = boundArrayAccessExpression.Indices
            .Any(static argument => argument is BoundIndexExpression { IsFromEnd: true });

        IILocal? arrayLocal = null;
        if (requiresLength)
        {
            EmitExpression(boundArrayAccessExpression.Receiver);
            arrayLocal = ILGenerator.DeclareLocal(ResolveClrType(arrayType));
            ILGenerator.Emit(OpCodes.Stloc, arrayLocal);
            ILGenerator.Emit(OpCodes.Ldloc, arrayLocal);
        }
        else
        {
            EmitExpression(boundArrayAccessExpression.Receiver);
        }

        EmitArrayIndices(boundArrayAccessExpression, arrayLocal);

        EmitLoadElement(arrayType.ElementType);
    }

    private void EmitArrayElementAddress(BoundArrayAccessExpression arrayAccess)
    {
        if (arrayAccess.Receiver.Type is not IArrayTypeSymbol arrayType)
            throw new NotSupportedException("Cannot take the address of a non-array element access.");

        var requiresLength = arrayAccess.Indices
            .Any(static argument => argument is BoundIndexExpression { IsFromEnd: true });

        IILocal? arrayLocal = null;
        if (requiresLength)
        {
            EmitExpression(arrayAccess.Receiver);
            arrayLocal = ILGenerator.DeclareLocal(ResolveClrType(arrayType));
            ILGenerator.Emit(OpCodes.Stloc, arrayLocal);
            ILGenerator.Emit(OpCodes.Ldloc, arrayLocal);
        }
        else
        {
            EmitExpression(arrayAccess.Receiver);
        }

        EmitArrayIndices(arrayAccess, arrayLocal);

        var elementClrType = ResolveClrType(arrayType.ElementType);
        ILGenerator.Emit(OpCodes.Ldelema, elementClrType);
    }

    private void EmitArrayIndices(BoundArrayAccessExpression arrayAccess, IILocal? arrayLocal)
    {
        foreach (var argument in arrayAccess.Indices)
        {
            if (argument is BoundIndexExpression indexExpression)
            {
                if (indexExpression.IsFromEnd)
                {
                    if (arrayLocal is null)
                        throw new InvalidOperationException("Array local is required for from-end indexing.");

                    ILGenerator.Emit(OpCodes.Ldloc, arrayLocal);
                    ILGenerator.Emit(OpCodes.Ldlen);
                    ILGenerator.Emit(OpCodes.Conv_I4);
                    new ExpressionGenerator(this, indexExpression.Value).Emit();
                    ILGenerator.Emit(OpCodes.Sub);
                }
                else
                {
                    new ExpressionGenerator(this, indexExpression.Value).Emit();
                }
            }
            else
            {
                EmitExpression(argument);
            }
        }
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

        var constructorInfo = constructorSymbol.GetClrConstructorInfo(MethodBodyGenerator.MethodGenerator.TypeGenerator.CodeGen);

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
        => EmitAssignmentExpression(node, preserveResult: true);

    private void EmitAssignmentExpression(BoundAssignmentExpression node, bool preserveResult)
    {
        switch (node)
        {
            case BoundLocalAssignmentExpression localAssignmentExpression:
                if (TryEmitCapturedAssignment(localAssignmentExpression.Local, localAssignmentExpression.Right))
                    break;

                var localBuilder = GetLocal(localAssignmentExpression.Local);
                if (localBuilder is null)
                    throw new InvalidOperationException($"Missing local builder for '{localAssignmentExpression.Local.Name}'");

                var rightExpression = localAssignmentExpression.Right;
                EmitExpression(rightExpression);

                var resultType = node.Type;
                var needsResult = preserveResult
                    && resultType is not null
                    && resultType.SpecialType is not SpecialType.System_Unit
                    and not SpecialType.System_Void;

                var needsBox = rightExpression.Type is { IsValueType: true }
                    && resultType?.SpecialType == SpecialType.System_Object;

                if (needsBox)
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(rightExpression.Type));

                if (needsResult)
                    ILGenerator.Emit(OpCodes.Dup);

                ILGenerator.Emit(OpCodes.Stloc, localBuilder);
                break;

            case BoundParameterAssignmentExpression parameterAssignmentExpression:
                EmitParameterAssignmentExpression(parameterAssignmentExpression, preserveResult);
                break;

            case BoundByRefAssignmentExpression byRefAssignmentExpression:
                EmitByRefAssignmentExpression(byRefAssignmentExpression, preserveResult);
                break;

            case BoundFieldAssignmentExpression fieldAssignmentExpression:
                {
                    var fieldSymbol = fieldAssignmentExpression.Field;
                    var right = fieldAssignmentExpression.Right
                        ?? new BoundDefaultValueExpression(fieldSymbol.Type);
                    var receiver = fieldAssignmentExpression.Receiver;
                    var requiresAddress = fieldAssignmentExpression.RequiresReceiverAddress;
                    var containingType = fieldSymbol.ContainingType;
                    var needsReceiverAddress = containingType?.IsValueType == true;
                    var needsAddress = requiresAddress || needsReceiverAddress;
                    var fieldResultType = node.Type;
                    var fieldNeedsResult = preserveResult
                        && fieldResultType is not null
                        && fieldResultType.SpecialType is not SpecialType.System_Unit
                        and not SpecialType.System_Void;
                    var fieldNeedsBox = right.Type is { IsValueType: true }
                        && !fieldSymbol.Type.IsValueType;

                    if (right is BoundDefaultValueExpression && fieldSymbol.Type.IsValueType)
                    {
                        if (fieldSymbol.IsStatic)
                        {
                            ILGenerator.Emit(OpCodes.Ldsflda, GetField(fieldSymbol));
                            if (TryGetAsyncInvestigationFieldLabel(fieldSymbol, out var staticDefaultLabel))
                                EmitAsyncInvestigationAddressLogPreservingPointer(staticDefaultLabel);
                        }
                        else
                        {
                            if (!TryEmitValueTypeReceiverAddress(receiver, receiver?.Type, containingType))
                            {
                                if (receiver is not null)
                                {
                                    EmitExpression(receiver);

                                    if (fieldSymbol.ContainingType!.IsValueType)
                                        EmitValueTypeAddressIfNeeded(receiver?.Type, fieldSymbol.ContainingType);

                                    if (needsReceiverAddress)
                                        EmitValueTypeAddressIfNeeded(containingType!);
                                }
                                else
                                {
                                    ILGenerator.Emit(OpCodes.Ldarg_0);

                                    if (needsReceiverAddress)
                                        EmitValueTypeAddressIfNeeded(containingType!);
                                }
                            }

                            ILGenerator.Emit(OpCodes.Ldflda, GetField(fieldSymbol));
                            if (TryGetAsyncInvestigationFieldLabel(fieldSymbol, out var defaultLabel))
                                EmitAsyncInvestigationAddressLogPreservingPointer(defaultLabel);
                        }

                        ILGenerator.Emit(OpCodes.Initobj, ResolveClrType(fieldSymbol.Type));
                        EmitAsyncInvestigationStore(fieldSymbol);
                        break;
                    }

                    var cacheRightValue = !fieldSymbol.IsStatic && containingType?.IsValueType == true;
                    IILocal? cachedRightLocal = null;

                    if (cacheRightValue)
                    {
                        EmitExpression(right);

                        if (fieldNeedsBox)
                            ILGenerator.Emit(OpCodes.Box, ResolveClrType(right.Type));

                        cachedRightLocal = ILGenerator.DeclareLocal(ResolveClrType(fieldSymbol.Type));
                        ILGenerator.Emit(OpCodes.Stloc, cachedRightLocal);
                    }

                    if (!fieldSymbol.IsStatic)
                    {
                        if (needsAddress)
                        {
                            if (!TryEmitValueTypeReceiverAddress(receiver, receiver?.Type, containingType))
                            {
                                if (receiver is not null)
                                {
                                    EmitExpression(receiver);

                                    if (needsReceiverAddress)
                                        EmitValueTypeAddressIfNeeded(containingType!);
                                }
                                else
                                {
                                    ILGenerator.Emit(OpCodes.Ldarg_0);

                                    if (needsReceiverAddress)
                                        EmitValueTypeAddressIfNeeded(containingType!);
                                }
                            }
                        }
                        else if (receiver is not null)
                        {
                            EmitExpression(receiver);

                            if (fieldSymbol.ContainingType!.IsValueType)
                            {
                                EmitValueTypeAddressIfNeeded(receiver?.Type, fieldSymbol.ContainingType);
                            }
                            if (needsReceiverAddress)
                                EmitValueTypeAddressIfNeeded(containingType!);
                        }
                        else
                        {
                            ILGenerator.Emit(OpCodes.Ldarg_0);

                            if (needsReceiverAddress)
                                EmitValueTypeAddressIfNeeded(containingType!);
                        }
                    }

                    if (!cacheRightValue)
                    {
                        // Emit RHS value
                        EmitExpression(right);

                        // Box if assigning value type to reference type
                        if (fieldNeedsBox)
                        {
                            ILGenerator.Emit(OpCodes.Box, ResolveClrType(right.Type));
                        }

                        if (fieldNeedsResult)
                        {
                            ILGenerator.Emit(OpCodes.Dup);
                        }
                    }
                    else if (cachedRightLocal is not null)
                    {
                        ILGenerator.Emit(OpCodes.Ldloc, cachedRightLocal);
                    }

                    ILGenerator.Emit(fieldSymbol.IsStatic ? OpCodes.Stsfld : OpCodes.Stfld, (FieldInfo)GetField(fieldSymbol));
                    EmitAsyncInvestigationStore(fieldSymbol);

                    if (fieldNeedsResult && cacheRightValue && cachedRightLocal is not null)
                    {
                        ILGenerator.Emit(OpCodes.Ldloc, cachedRightLocal);
                    }
                    break;
                }

            case BoundPropertyAssignmentExpression propertyAssignmentExpression:
                {
                    var propertySymbol = (IPropertySymbol)propertyAssignmentExpression.Property;
                    var right = propertyAssignmentExpression.Right;
                    var receiver = propertyAssignmentExpression.Receiver;
                    var isExtensionProperty = propertySymbol.IsExtensionProperty();

                    // Load receiver (unless static)
                    if (isExtensionProperty)
                    {
                        if (receiver is null)
                            throw new InvalidOperationException($"Extension property '{propertySymbol.Name}' requires a receiver.");

                        EmitExpression(receiver);
                    }
                    else if (!propertySymbol.IsStatic)
                    {
                        if (receiver is not null)
                        {
                            EmitExpression(receiver);

                            if (propertySymbol.ContainingType!.IsValueType)
                                EmitValueTypeAddressIfNeeded(receiver?.Type, propertySymbol.ContainingType);
                        }
                        else
                        {
                            ILGenerator.Emit(OpCodes.Ldarg_0);
                            if (propertySymbol.ContainingType!.IsValueType)
                                EmitValueTypeAddressIfNeeded(receiver?.Type, propertySymbol.ContainingType);
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

                    if (isExtensionProperty)
                        ILGenerator.Emit(OpCodes.Call, setter);
                    else
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

            case BoundPatternAssignmentExpression patternAssignmentExpression:
                EmitPatternAssignmentExpression(patternAssignmentExpression);
                break;

            default:
                throw new NotSupportedException($"Unknown BoundAssignmentExpression: {node.GetType().Name}");
        }

        if (preserveResult && node.Type?.SpecialType == SpecialType.System_Unit)
            EmitUnitValue();
    }

    private void EmitParameterAssignmentExpression(BoundParameterAssignmentExpression node, bool preserveResult)
    {
        var rightExpression = node.Right;
        EmitExpression(rightExpression);

        var resultType = node.Type;
        var needsResult = preserveResult
            && resultType is not null
            && resultType.SpecialType is not SpecialType.System_Unit
            and not SpecialType.System_Void;

        var needsBox = rightExpression.Type is { IsValueType: true }
            && resultType?.SpecialType == SpecialType.System_Object;

        if (needsBox)
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(rightExpression.Type));

        if (needsResult)
            ILGenerator.Emit(OpCodes.Dup);

        var parameterBuilder = MethodGenerator.GetParameterBuilder(node.Parameter);
        var argumentIndex = parameterBuilder.Position;
        if (MethodSymbol.IsStatic)
            argumentIndex -= 1;

        ILGenerator.Emit(OpCodes.Starg, (short)argumentIndex);
    }

    private void EmitByRefAssignmentExpression(BoundByRefAssignmentExpression node, bool preserveResult)
    {
        var reference = node.Reference;
        var rightExpression = node.Right;
        var elementType = node.ElementType;
        var resultType = node.Type;

        var needsResult = preserveResult
            && resultType is not null
            && resultType.SpecialType is not SpecialType.System_Unit
            and not SpecialType.System_Void;

        var needsBox = rightExpression.Type is { IsValueType: true }
            && resultType?.SpecialType == SpecialType.System_Object;

        EmitExpression(reference);
        EmitExpression(rightExpression);

        IILocal? tempLocal = null;
        ITypeSymbol? tempStorageType = null;

        if (needsResult)
        {
            tempStorageType = rightExpression.Type ?? elementType;
            var tempClrType = ResolveClrType(tempStorageType);
            tempLocal = ILGenerator.DeclareLocal(tempClrType);
            ILGenerator.Emit(OpCodes.Dup);
            ILGenerator.Emit(OpCodes.Stloc, tempLocal);
        }

        EmitStoreIndirect(elementType);

        if (needsResult && tempLocal is not null)
        {
            ILGenerator.Emit(OpCodes.Ldloc, tempLocal);

            if (needsBox && tempStorageType is not null)
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(tempStorageType));
        }
    }

    private void EmitStoreIndirect(ITypeSymbol elementType)
    {
        switch (elementType.SpecialType)
        {
            case SpecialType.System_SByte:
            case SpecialType.System_Byte:
            case SpecialType.System_Boolean:
                ILGenerator.Emit(OpCodes.Stind_I1);
                break;
            case SpecialType.System_Int16:
            case SpecialType.System_UInt16:
            case SpecialType.System_Char:
                ILGenerator.Emit(OpCodes.Stind_I2);
                break;
            case SpecialType.System_Int32:
            case SpecialType.System_UInt32:
                ILGenerator.Emit(OpCodes.Stind_I4);
                break;
            case SpecialType.System_Int64:
            case SpecialType.System_UInt64:
                ILGenerator.Emit(OpCodes.Stind_I8);
                break;
            case SpecialType.System_Single:
                ILGenerator.Emit(OpCodes.Stind_R4);
                break;
            case SpecialType.System_Double:
                ILGenerator.Emit(OpCodes.Stind_R8);
                break;
            case SpecialType.System_IntPtr:
            case SpecialType.System_UIntPtr:
                ILGenerator.Emit(OpCodes.Stind_I);
                break;
            default:
                if (elementType.IsValueType)
                {
                    ILGenerator.Emit(OpCodes.Stobj, ResolveClrType(elementType));
                }
                else
                {
                    ILGenerator.Emit(OpCodes.Stind_Ref);
                }

                break;
        }
    }

    private void EmitPatternAssignmentExpression(BoundPatternAssignmentExpression node)
    {
        EmitExpression(node.Right);

        var pattern = node.Pattern;

        if (pattern is null || !pattern.GetDesignators().Any())
        {
            var discardType = GetPatternValueType(node.Right.Type);

            if (discardType is null ||
                discardType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit)
            {
                return;
            }

            ILGenerator.Emit(OpCodes.Pop);
            return;
        }

        var valueType = GetPatternValueType(node.Right.Type) ?? GetPatternValueType(node.Pattern.Type);

        if (valueType is null || valueType.TypeKind == TypeKind.Error)
        {
            ILGenerator.Emit(OpCodes.Pop);
            return;
        }

        var valueLocal = ILGenerator.DeclareLocal(ResolveClrType(valueType));
        ILGenerator.Emit(OpCodes.Stloc, valueLocal);

        EmitPatternAssignment(pattern, valueLocal, valueType);
    }

    private void EmitPatternAssignment(BoundPattern pattern, IILocal valueLocal, ITypeSymbol valueType)
    {
        if (pattern is null)
            return;

        switch (pattern)
        {
            case BoundTuplePattern tuplePattern:
                EmitTuplePatternAssignment(tuplePattern, valueLocal, valueType);
                break;

            case BoundDeclarationPattern declarationPattern:
                EmitDeclarationPatternAssignment(declarationPattern, valueLocal, valueType);
                break;

            case BoundDiscardPattern:
                break;

            default:
                throw new NotSupportedException($"Unsupported pattern assignment: {pattern.GetType().Name}");
        }
    }

    private void EmitTuplePatternAssignment(BoundTuplePattern tuplePattern, IILocal valueLocal, ITypeSymbol valueType)
    {
        if (GetPatternValueType(valueType) is not ITupleTypeSymbol tupleType)
            return;

        var elements = tuplePattern.Elements;
        var tupleElements = tupleType.TupleElements;
        var count = Math.Min(elements.Length, tupleElements.Length);

        for (var i = 0; i < count; i++)
        {
            var elementPattern = elements[i];
            if (elementPattern is null)
                continue;
            if (elementPattern is BoundDiscardPattern)
                continue;

            var tupleElement = tupleElements[i];
            var elementValueType = GetPatternValueType(tupleElement.Type);

            ILGenerator.Emit(OpCodes.Ldloca, valueLocal);
            ILGenerator.Emit(OpCodes.Ldfld, GetField(tupleElement));

            if (elementValueType is null || elementValueType.TypeKind == TypeKind.Error)
            {
                ILGenerator.Emit(OpCodes.Pop);
                continue;
            }

            var elementLocal = ILGenerator.DeclareLocal(ResolveClrType(elementValueType));
            ILGenerator.Emit(OpCodes.Stloc, elementLocal);

            EmitPatternAssignment(elementPattern, elementLocal, elementValueType);
        }
    }

    private void EmitDeclarationPatternAssignment(BoundDeclarationPattern declarationPattern, IILocal valueLocal, ITypeSymbol valueType)
    {
        switch (declarationPattern.Designator)
        {
            case BoundSingleVariableDesignator single:
                EmitStorePatternValue(single.Local, valueLocal, valueType, GetPatternValueType(single.Local.Type));
                break;

            case BoundDiscardDesignator:
                break;

            default:
                throw new NotSupportedException($"Unsupported declaration designator: {declarationPattern.Designator.GetType().Name}");
        }
    }

    private void EmitStorePatternValue(ILocalSymbol localSymbol, IILocal valueLocal, ITypeSymbol sourceType, ITypeSymbol? targetType)
    {
        var normalizedSource = GetPatternValueType(sourceType);
        var normalizedTarget = GetPatternValueType(targetType);

        if (MethodBodyGenerator.TryGetCapturedField(localSymbol, out var fieldBuilder))
        {
            MethodBodyGenerator.EmitLoadClosure();
            var storedType = LoadValueWithConversion(valueLocal, normalizedSource, normalizedTarget);

            if (storedType is not null && storedType.IsValueType &&
                localSymbol.Type is not null &&
                (localSymbol.Type.SpecialType is SpecialType.System_Object || localSymbol.Type is ITypeUnionSymbol))
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(storedType));
            }

            ILGenerator.Emit(OpCodes.Stfld, fieldBuilder);
            return;
        }

        var localBuilder = GetLocal(localSymbol);
        if (localBuilder is null)
            return;

        var finalType = LoadValueWithConversion(valueLocal, normalizedSource, normalizedTarget);

        if (finalType is not null && finalType.IsValueType &&
            localSymbol.Type is not null &&
            (localSymbol.Type.SpecialType is SpecialType.System_Object || localSymbol.Type is ITypeUnionSymbol))
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(finalType));
        }

        ILGenerator.Emit(OpCodes.Stloc, localBuilder);
    }

    private ITypeSymbol? LoadValueWithConversion(IILocal valueLocal, ITypeSymbol? sourceType, ITypeSymbol? targetType)
    {
        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);

        if (sourceType is not null)
            sourceType = GetPatternValueType(sourceType);

        if (targetType is not null)
            targetType = GetPatternValueType(targetType);

        if (sourceType is not null && targetType is not null &&
            !SymbolEqualityComparer.Default.Equals(sourceType, targetType))
        {
            var conversion = Compilation.ClassifyConversion(sourceType, targetType);
            if (conversion.Exists)
            {
                EmitConversion(sourceType, targetType, conversion);
                return targetType;
            }
        }

        return sourceType ?? targetType;
    }

    private static ITypeSymbol? GetPatternValueType(ITypeSymbol? type)
        => type?.UnwrapLiteralType() ?? type;

    private FieldInfo GetField(IFieldSymbol fieldSymbol)
    {
        return fieldSymbol.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen);
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
        var op = binaryExpression.Operator;
        var operatorKind = op.OperatorKind & ~(BinaryOperatorKind.Lifted | BinaryOperatorKind.Checked);

        // Short-circuiting operators must control evaluation order themselves
        if (operatorKind is BinaryOperatorKind.LogicalAnd or BinaryOperatorKind.LogicalOr)
        {
            EmitShortCircuitLogical(operatorKind, binaryExpression.Left, binaryExpression.Right);
            return;
        }

        // All other operators: evaluate both operands then apply the opcode
        EmitExpression(binaryExpression.Left);
        EmitExpression(binaryExpression.Right);

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

            case BinaryOperatorKind.Modulo:
                ILGenerator.Emit(OpCodes.Rem);
                break;

            case BinaryOperatorKind.BitwiseAnd:
                ILGenerator.Emit(OpCodes.And);
                break;

            case BinaryOperatorKind.BitwiseOr:
                ILGenerator.Emit(OpCodes.Or);
                break;

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

    private void EmitShortCircuitLogical(
        BinaryOperatorKind operatorKind,
        BoundExpression left,
        BoundExpression right)
    {
        var skipLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        switch (operatorKind)
        {
            case BinaryOperatorKind.LogicalAnd:
                // if (!left) result = false; else result = right;
                EmitExpression(left);                      // stack: [left]
                ILGenerator.Emit(OpCodes.Brfalse_S, skipLabel);

                // left == true → evaluate right, result = right
                EmitExpression(right);                     // stack: [right]
                ILGenerator.Emit(OpCodes.Br_S, endLabel);

                // left == false → push false
                ILGenerator.MarkLabel(skipLabel);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);

                ILGenerator.MarkLabel(endLabel);
                break;

            case BinaryOperatorKind.LogicalOr:
                // if (left) result = true; else result = right;
                EmitExpression(left);                      // stack: [left]
                ILGenerator.Emit(OpCodes.Brtrue_S, skipLabel);

                // left == false → evaluate right, result = right
                EmitExpression(right);                     // stack: [right]
                ILGenerator.Emit(OpCodes.Br_S, endLabel);

                // left == true → push true
                ILGenerator.MarkLabel(skipLabel);
                ILGenerator.Emit(OpCodes.Ldc_I4_1);

                ILGenerator.MarkLabel(endLabel);
                break;

            default:
                throw new InvalidOperationException($"Unexpected logical operator kind '{operatorKind}'.");
        }
    }

    private void EmitMemberAccessExpression(BoundMemberAccessExpression memberAccessExpression, bool receiverAlreadyLoaded = false)
    {
        var symbol = memberAccessExpression.Symbol;
        var receiver = memberAccessExpression.Receiver;

        switch (symbol)
        {
            case IPropertySymbol propertySymbol:
                if (propertySymbol.IsExtensionProperty())
                {
                    EmitExtensionPropertyAccess(propertySymbol, receiver, receiverAlreadyLoaded);
                    break;
                }

                EmitPropertyReceiver(receiver, propertySymbol, receiverAlreadyLoaded);

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

                if (!propertySymbol.IsStatic && !propertySymbol.ContainingType!.IsValueType)
                    EmitBoxIfNeeded(propertySymbol.ContainingType!, getter);

                ILGenerator.Emit(propertySymbol.IsStatic || propertySymbol.ContainingType!.IsValueType ? OpCodes.Call : OpCodes.Callvirt, getter);
                break;

            case IFieldSymbol fieldSymbol:
                if (!fieldSymbol.IsStatic &&
                    fieldSymbol.ContainingType is { } memberFieldContainingType &&
                    (!SymbolEqualityComparer.Default.Equals(receiver?.Type, memberFieldContainingType)) &&
                    MethodSymbol.ContainingType is SynthesizedAsyncStateMachineTypeSymbol memberStateMachine &&
                    memberStateMachine.GetConstructedMembers(memberStateMachine.AsyncMethod).ThisField is { } memberClosureField &&
                    memberFieldContainingType.Name.Contains("LambdaClosure", StringComparison.Ordinal))
                {
                    ILGenerator.Emit(OpCodes.Ldarg_0);
                    ILGenerator.Emit(OpCodes.Ldfld, GetField(memberClosureField));

                    var memberFieldInfo = fieldSymbol switch
                    {
                        SourceFieldSymbol sfs => (FieldInfo)GetMemberBuilder(sfs)!,
                        _ => fieldSymbol.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen)
                    };

                    ILGenerator.Emit(OpCodes.Ldfld, memberFieldInfo);
                    break;
                }

                if (!fieldSymbol.IsStatic &&
                    fieldSymbol.ContainingType is { } closureContainingType &&
                    receiver is BoundSelfExpression self &&
                    MethodGenerator.LambdaClosure is not null &&
                    !SymbolEqualityComparer.Default.Equals(self.Type, closureContainingType) &&
                    closureContainingType.Name.Contains("LambdaClosure", StringComparison.Ordinal))
                {
                    MethodBodyGenerator.EmitLoadClosure();

                    var closureFieldInfo = fieldSymbol switch
                    {
                        SourceFieldSymbol sfs => (FieldInfo)GetMemberBuilder(sfs)!,
                        _ => fieldSymbol.GetFieldInfo(MethodGenerator.TypeGenerator.CodeGen)
                    };

                    ILGenerator.Emit(OpCodes.Ldfld, closureFieldInfo);
                    break;
                }

                if (!fieldSymbol.IsStatic && fieldSymbol.ContainingType!.IsValueType)
                {
                    var containingType = fieldSymbol.ContainingType!;

                    if (receiverAlreadyLoaded)
                    {
                        EmitValueTypeAddressIfNeeded(receiver?.Type ?? containingType, containingType);
                    }
                    else if (!TryEmitInvocationReceiverAddress(receiver))
                    {
                        if (receiver is not null)
                        {
                            EmitExpression(receiver);
                            EmitValueTypeAddressIfNeeded(receiver.Type, containingType);
                        }
                        else
                        {
                            ILGenerator.Emit(OpCodes.Ldarg_0);
                        }
                    }
                }
                else
                {
                    EmitReceiverIfNeeded(receiver, fieldSymbol, receiverAlreadyLoaded);
                }

                if (fieldSymbol.IsConst)
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
        var isNullableValue = receiverType.IsNullable && receiverType.IsValueType;

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

    private void EmitExtensionPropertyAccess(IPropertySymbol propertySymbol, BoundExpression? receiver, bool receiverAlreadyLoaded)
    {
        if (propertySymbol.GetMethod is null)
            throw new InvalidOperationException($"Property '{propertySymbol.Name}' does not have a getter.");

        if (!receiverAlreadyLoaded)
        {
            if (receiver is null)
                throw new InvalidOperationException($"Extension property '{propertySymbol.Name}' requires a receiver.");

            EmitExpression(receiver);
        }

        ILGenerator.Emit(OpCodes.Call, GetMethodInfo(propertySymbol.GetMethod));
    }

    private void EmitReceiverIfNeeded(BoundExpression? receiver, ISymbol symbol, bool receiverAlreadyLoaded)
    {
        if (receiverAlreadyLoaded)
            return;
        if (receiver is not null && !symbol.IsStatic)
            EmitExpression(receiver);
    }

    private void EmitPropertyReceiver(BoundExpression? receiver, IPropertySymbol propertySymbol, bool receiverAlreadyLoaded)
    {
        if (propertySymbol.IsStatic)
            return;

        if (propertySymbol.ContainingType!.IsValueType)
        {
            if (receiverAlreadyLoaded)
            {
                EmitValueTypeAddressIfNeeded(receiver?.Type, propertySymbol.ContainingType);
                return;
            }

            if (TryEmitInvocationReceiverAddress(receiver))
                return;

            if (receiver is null)
                throw new InvalidOperationException($"Instance property '{propertySymbol.Name}' requires a receiver.");

            EmitExpression(receiver);
            EmitValueTypeAddressIfNeeded(receiver.Type, propertySymbol.ContainingType);
            return;
        }

        EmitReceiverIfNeeded(receiver, propertySymbol, receiverAlreadyLoaded);
    }

    private bool TryEmitValueTypeReceiverAddress(BoundExpression? receiver, ITypeSymbol? runtimeType, ITypeSymbol? declaredType = null)
    {
        var effectiveType = runtimeType ?? declaredType;

        if (effectiveType is null || !effectiveType.IsValueType)
            return TryEmitInvocationReceiverAddress(receiver);

        if (TryEmitInvocationReceiverAddress(receiver))
            return true;

        if (receiver is BoundMemberAccessExpression { Member: IFieldSymbol fieldSymbol } member && !fieldSymbol.IsStatic)
        {
            var containingType = fieldSymbol.ContainingType;

            if (TryEmitValueTypeReceiverAddress(member.Receiver, member.Receiver?.Type, containingType))
            {
                ILGenerator.Emit(OpCodes.Ldflda, GetField(fieldSymbol));
                if (TryGetAsyncInvestigationFieldLabel(fieldSymbol, out var receiverLabel))
                    EmitAsyncInvestigationAddressLogPreservingPointer(receiverLabel);
                return true;
            }
        }

        return false;
    }

    private bool TryEmitInvocationReceiverAddress(BoundExpression? receiver)
    {
        var requiresAddress = MethodSymbol.ContainingType?.IsValueType == true;

        switch (receiver)
        {
            case null:
                if (MethodSymbol.IsStatic)
                    return false;

                ILGenerator.Emit(OpCodes.Ldarg_0);
                return true;

            case BoundSelfExpression selfExpression:
                if (MethodSymbol.IsStatic)
                    return false;

                ILGenerator.Emit(OpCodes.Ldarg_0);
                return true;

            case BoundAddressOfExpression addressOf:
                EmitAddressOfExpression(addressOf);
                return true;

            case BoundLocalAccess localAccess:
                {
                    if (MethodBodyGenerator.TryGetCapturedField(localAccess.Local, out var capturedField))
                    {
                        MethodBodyGenerator.EmitLoadClosure();
                        ILGenerator.Emit(OpCodes.Ldflda, capturedField);
                        return true;
                    }

                    var local = GetLocal(localAccess.Local);
                    if (local is null)
                        return false;

                    ILGenerator.Emit(OpCodes.Ldloca, local);
                    return true;
                }

            case BoundParameterAccess parameterAccess:
                {
                    if (MethodBodyGenerator.TryGetCapturedField(parameterAccess.Parameter, out var capturedField))
                    {
                        MethodBodyGenerator.EmitLoadClosure();
                        ILGenerator.Emit(OpCodes.Ldflda, capturedField);
                        return true;
                    }

                    var parameterBuilder = MethodGenerator.GetParameterBuilder(parameterAccess.Parameter);
                    var position = parameterBuilder.Position;
                    if (MethodSymbol.IsStatic)
                        position -= 1;
                    ILGenerator.Emit(OpCodes.Ldarga, position);
                    return true;
                }

            case BoundMemberAccessExpression memberAccess when memberAccess.Member is IFieldSymbol fieldSymbol:
                {
                    if (fieldSymbol.IsStatic)
                    {
                        ILGenerator.Emit(OpCodes.Ldsflda, GetField(fieldSymbol));
                        if (TryGetAsyncInvestigationFieldLabel(fieldSymbol, out var staticLabel))
                            EmitAsyncInvestigationAddressLogPreservingPointer(staticLabel);
                        return true;
                    }

                    if (!TryEmitInvocationReceiverAddress(memberAccess.Receiver))
                        return false;

                    ILGenerator.Emit(OpCodes.Ldflda, GetField(fieldSymbol));
                    if (TryGetAsyncInvestigationFieldLabel(fieldSymbol, out var instanceLabel))
                        EmitAsyncInvestigationAddressLogPreservingPointer(instanceLabel);
                    return true;
                }
        }

        return false;
    }

    private bool CanReEmitInvocationReceiverAddress(BoundExpression? receiver)
    {
        switch (receiver)
        {
            case null:
                return !MethodSymbol.IsStatic;

            case BoundSelfExpression:
                return !MethodSymbol.IsStatic;

            case BoundAddressOfExpression addressOf:
                return CanReEmitInvocationReceiverAddress(addressOf.Receiver);

            case BoundLocalAccess localAccess:
                if (MethodBodyGenerator.TryGetCapturedField(localAccess.Local, out _))
                    return true;

                return GetLocal(localAccess.Local) is not null;

            case BoundParameterAccess parameterAccess:
                return true;

            case BoundMemberAccessExpression memberAccess when memberAccess.Member is IFieldSymbol fieldSymbol:
                if (fieldSymbol.IsStatic)
                    return true;

                return CanReEmitInvocationReceiverAddress(memberAccess.Receiver);

            default:
                return false;
        }
    }

    private void EmitBoxIfNeeded(ITypeSymbol type, MethodInfo method)
    {
        if (type.IsValueType && method.DeclaringType == typeof(object))
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(type));
        }
    }

    private void EmitValueTypeAddressIfNeeded(ITypeSymbol? runtimeType, ITypeSymbol? declaredType = null)
    {
        runtimeType ??= declaredType;

        if (runtimeType is null || !runtimeType.IsValueType)
            return;

        var clrType = ResolveClrType(runtimeType);

        if (clrType.ContainsGenericParameters && declaredType is not null)
        {
            var declaredClrType = ResolveClrType(declaredType);

            if (!declaredClrType.ContainsGenericParameters)
                clrType = declaredClrType;
        }

        var tmp = ILGenerator.DeclareLocal(clrType);
        ILGenerator.Emit(OpCodes.Stloc, tmp);
        ILGenerator.Emit(OpCodes.Ldloca, tmp);
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

        if (_preserveResult && invocationExpression.Type.SpecialType == SpecialType.System_Unit)
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
        var target = GetInvocationTarget(invocationExpression);
        var receiver = invocationExpression.Receiver;

        // Emit receiver (for instance methods)
        if (!target.IsStatic)
        {
            var requiresAddress = invocationExpression.RequiresReceiverAddress;
            var receiverAddressLoaded = false;

            if (!receiverAlreadyLoaded)
            {
                if (requiresAddress && TryEmitValueTypeReceiverAddress(receiver, receiver?.Type, target.ContainingType))
                {
                    receiverAlreadyLoaded = true;
                    receiverAddressLoaded = true;
                }
                else
                {
                    EmitExpression(receiver);
                }
            }
            else if (requiresAddress)
            {
                receiverAddressLoaded = true;
            }

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
                    ILGenerator.Emit(OpCodes.Box, clrType);
                }
                else if (!receiverAddressLoaded)
                {
                    var tmp = ILGenerator.DeclareLocal(clrType);
                    ILGenerator.Emit(OpCodes.Stloc, tmp);
                    ILGenerator.Emit(OpCodes.Ldloca, tmp);
                    receiverAddressLoaded = true;
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
                if (argument?.Type?.TypeKind == TypeKind.Null &&
                    paramSymbol.Type is NullableTypeSymbol nullableParam &&
                    nullableParam.UnderlyingType.IsValueType)
                {
                    EmitDefaultValue(paramSymbol.Type);
                    continue;
                }

                EmitExpression(argument);

                var argumentType = argument?.Type;
                var paramType = paramSymbol.Type;
                var finalType = argumentType;

                if (argumentType is not null &&
                    paramType is not null &&
                    !SymbolEqualityComparer.Default.Equals(argumentType, paramType))
                {
                    var conversion = Compilation.ClassifyConversion(argumentType, paramType);
                    if (conversion.Exists && !conversion.IsIdentity)
                    {
                        EmitConversion(argumentType, paramType, conversion);
                        finalType = paramType;
                    }
                }

                if (finalType is not null &&
                    RequiresValueTypeHandling(finalType) &&
                    !RequiresValueTypeHandling(paramType))
                {
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(finalType));
                }
            }
        }

        // Emit the actual call
        var isInterfaceCall = target.ContainingType?.TypeKind == TypeKind.Interface;

        var targetMethodInfo = GetMethodInfo(target);

        if (target.IsStatic)
        {
            ILGenerator.Emit(OpCodes.Call, targetMethodInfo);
        }
        else
        {
            var callOpCode = target.ContainingType!.IsValueType
                ? (target.IsVirtual || isInterfaceCall ? OpCodes.Callvirt : OpCodes.Call)
                : OpCodes.Callvirt;

            ILGenerator.Emit(callOpCode, targetMethodInfo);
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

    private IMethodSymbol GetInvocationTarget(BoundInvocationExpression invocationExpression)
    {
        if (TryGetSpecializedAsyncBuilderMethod(invocationExpression, out var specialized))
            return specialized;

        return invocationExpression.Method;
    }

    private bool TryGetSpecializedAsyncBuilderMethod(BoundInvocationExpression invocationExpression, out IMethodSymbol specialized)
    {
        var original = invocationExpression.Method;
        specialized = original;

        if (original.ContainingType is not INamedTypeSymbol containingType)
            return false;

        if (!IsAsyncTaskBuilderType(containingType))
            return false;

        var receiverType = GetAsyncBuilderReceiverType(invocationExpression);
        if (receiverType is null)
            return false;

        if (!IsAsyncTaskBuilderType(receiverType))
            return false;

        if (SymbolEqualityComparer.Default.Equals(receiverType, containingType))
            return false;

        var originalDefinition = original.OriginalDefinition ?? original;

        foreach (var candidate in receiverType.GetMembers(original.Name).OfType<IMethodSymbol>())
        {
            if (!SymbolEqualityComparer.Default.Equals(candidate.OriginalDefinition ?? candidate, originalDefinition))
                continue;

            var aligned = AlignMethodTypeArguments(candidate, original);
            specialized = aligned;
            return !SymbolEqualityComparer.Default.Equals(aligned, original);
        }

        return false;
    }

    private static INamedTypeSymbol? GetAsyncBuilderReceiverType(BoundInvocationExpression invocationExpression)
    {
        if (invocationExpression.Method.IsStatic)
            return null;

        var receiver = invocationExpression.Receiver;

        if (receiver?.Type is INamedTypeSymbol receiverType)
            return receiverType;

        if (receiver is BoundMemberAccessExpression memberAccess)
        {
            if (memberAccess.Member is IFieldSymbol field && field.Type is INamedTypeSymbol fieldType)
                return fieldType;
        }

        return null;
    }

    private static IMethodSymbol AlignMethodTypeArguments(IMethodSymbol candidate, IMethodSymbol original)
    {
        if (!original.IsGenericMethod)
            return candidate;

        var originalArgs = original.TypeArguments;
        if (originalArgs.IsDefaultOrEmpty)
            return candidate;

        if (!candidate.IsGenericMethod)
            return candidate;

        if (candidate.TypeArguments.Length == originalArgs.Length)
        {
            bool matches = true;
            for (var i = 0; i < originalArgs.Length; i++)
            {
                if (!SymbolEqualityComparer.Default.Equals(candidate.TypeArguments[i], originalArgs[i]))
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
                return candidate;
        }

        return candidate.Construct(originalArgs.ToArray());
    }

    private static bool IsAsyncTaskBuilderType(INamedTypeSymbol type)
    {
        var definition = type.ConstructedFrom as INamedTypeSymbol ?? type;

        return definition.SpecialType is SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder
            or SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T;
    }

    private bool TryGetAsyncInvestigationFieldLabel(IFieldSymbol fieldSymbol, out string fieldLabel)
    {
        var options = Compilation.Options.AsyncInvestigation;
        if (!options.IsEnabled)
        {
            fieldLabel = string.Empty;
            return false;
        }

        if (fieldSymbol is not SourceFieldSymbol sourceField)
        {
            fieldLabel = string.Empty;
            return false;
        }

        if (sourceField.ContainingType is not SynthesizedAsyncStateMachineTypeSymbol)
        {
            fieldLabel = string.Empty;
            return false;
        }

        var name = sourceField.Name;
        if (name == "_state" || name == "_builder" || name.StartsWith("<>awaiter", StringComparison.Ordinal))
        {
            fieldLabel = name;
            return true;
        }

        fieldLabel = string.Empty;
        return false;
    }

    private IILocal EnsureAsyncInvestigationPointerLocal()
    {
        return _asyncInvestigationPointerLocal ??= ILGenerator.DeclareLocal(typeof(UIntPtr));
    }

    private void EmitAsyncInvestigationLog(string fieldLabel, AsyncInvestigationOperation operation)
    {
        var options = Compilation.Options.AsyncInvestigation;
        if (!options.IsEnabled)
            return;

        var pointerLocal = EnsureAsyncInvestigationPointerLocal();
        ILGenerator.Emit(OpCodes.Conv_U);
        ILGenerator.Emit(OpCodes.Stloc, pointerLocal);

        var stepLabel = GetAsyncInvestigationStepLabel(options);
        var format = $"{stepLabel}:{fieldLabel}:{GetAsyncInvestigationOperationName(operation)} -> 0x{{0:x}}";
        ILGenerator.Emit(OpCodes.Ldstr, format);
        ILGenerator.Emit(OpCodes.Ldloc, pointerLocal);
        ILGenerator.Emit(OpCodes.Conv_U8);
        ILGenerator.Emit(OpCodes.Box, typeof(ulong));
        ILGenerator.Emit(OpCodes.Call, StringFormatStringObject);
        ILGenerator.Emit(OpCodes.Call, ConsoleWriteLineString);
    }

    private void EmitAsyncInvestigationAddressLogPreservingPointer(string fieldLabel)
    {
        if (!Compilation.Options.AsyncInvestigation.IsEnabled)
            return;

        ILGenerator.Emit(OpCodes.Dup);
        EmitAsyncInvestigationLog(fieldLabel, AsyncInvestigationOperation.Address);
    }

    private string GetAsyncInvestigationStepLabel(AsyncInvestigationOptions options)
    {
        if (options.PointerLabelScope == AsyncInvestigationPointerLabelScope.IncludeAsyncMethodName &&
            MethodSymbol.ContainingType is SourceNamedTypeSymbol { } containingType &&
            containingType is SynthesizedAsyncStateMachineTypeSymbol stateMachine &&
            stateMachine.AsyncMethod is IMethodSymbol asyncMethod)
        {
            var qualifier = SanitizeAsyncInvestigationQualifier(asyncMethod.Name);
            if (!string.IsNullOrEmpty(qualifier))
                return $"{options.StepLabel}/{qualifier}";
        }

        return options.StepLabel;
    }

    private static string SanitizeAsyncInvestigationQualifier(string name)
    {
        if (string.IsNullOrEmpty(name))
            return string.Empty;

        var builder = new StringBuilder(name.Length);
        foreach (var ch in name)
        {
            if (char.IsLetterOrDigit(ch) || ch == '_' || ch == '.')
            {
                builder.Append(ch);
            }
            else
            {
                builder.Append('_');
            }
        }

        return builder.ToString().Trim('_');
    }

    private void EmitAsyncInvestigationStore(IFieldSymbol fieldSymbol)
    {
        if (!TryGetAsyncInvestigationFieldLabel(fieldSymbol, out var label))
            return;

        if (fieldSymbol.IsStatic)
        {
            ILGenerator.Emit(OpCodes.Ldsflda, GetField(fieldSymbol));
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Ldflda, GetField(fieldSymbol));
        }

        EmitAsyncInvestigationLog(label, AsyncInvestigationOperation.Store);
    }

    private static string GetAsyncInvestigationOperationName(AsyncInvestigationOperation operation)
        => operation switch
        {
            AsyncInvestigationOperation.Load => "load",
            AsyncInvestigationOperation.Store => "store",
            AsyncInvestigationOperation.Address => "addr",
            _ => operation.ToString().ToLowerInvariant()
        };

    private enum AsyncInvestigationOperation
    {
        Load,
        Store,
        Address
    }

    private IILocal? _asyncInvestigationPointerLocal;

    private void EmitFieldAccess(BoundFieldAccess fieldAccess)
    {
        var fieldSymbol = fieldAccess.Field;

        if (!fieldSymbol.IsStatic &&
            MethodGenerator.TypeGenerator.TypeSymbol is SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine &&
            asyncStateMachine.GetConstructedMembers(asyncStateMachine.AsyncMethod).ThisField is { } asyncClosureField &&
            fieldSymbol.ContainingType is { } closureContainingType &&
            closureContainingType.Name.Contains("LambdaClosure", StringComparison.Ordinal))
        {
            ILGenerator.Emit(OpCodes.Ldarg_0);
            ILGenerator.Emit(OpCodes.Ldfld, GetField(asyncClosureField));
            ILGenerator.Emit(OpCodes.Ldfld, GetField(fieldSymbol));
            return;
        }
        if (fieldSymbol.IsConst)
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
            return;
        }

        if (fieldSymbol.IsStatic)
        {
            if (TryGetAsyncInvestigationFieldLabel(fieldSymbol, out var staticLabel))
            {
                ILGenerator.Emit(OpCodes.Ldsflda, GetField(fieldSymbol));
                EmitAsyncInvestigationLog(staticLabel, AsyncInvestigationOperation.Load);
            }

            ILGenerator.Emit(OpCodes.Ldsfld, GetField(fieldSymbol));
            return;
        }

        var containingType = fieldSymbol.ContainingType;
        var requiresReceiverAddress = containingType?.IsValueType == true;
        var loadedByReference = false;

        if (requiresReceiverAddress)
        {
            loadedByReference = TryEmitValueTypeReceiverAddress(
                fieldAccess.Receiver,
                fieldAccess.Receiver?.Type,
                containingType);
        }

        if (!loadedByReference)
        {
            var shouldLoadClosureReceiver = false;
            FieldInfo? closureFieldInfo = null;

            if (MethodGenerator.TypeGenerator.TypeSymbol is SynthesizedAsyncStateMachineTypeSymbol stateMachine &&
                stateMachine.GetConstructedMembers(stateMachine.AsyncMethod).ThisField is { } thisField &&
                fieldSymbol.ContainingType is { } fieldContainingType &&
                (SymbolEqualityComparer.Default.Equals(fieldContainingType, thisField.Type) ||
                 fieldContainingType.Name.Contains("LambdaClosure", StringComparison.Ordinal)))
            {
                var receiverType = fieldAccess.Receiver?.Type;
                if (receiverType is null || !SymbolEqualityComparer.Default.Equals(receiverType, fieldContainingType))
                {
                    shouldLoadClosureReceiver = true;
                    closureFieldInfo = GetField(thisField);
                }
            }

            if (shouldLoadClosureReceiver && closureFieldInfo is not null)
            {
                ILGenerator.Emit(OpCodes.Ldarg_0);
                ILGenerator.Emit(OpCodes.Ldfld, closureFieldInfo);

                if (requiresReceiverAddress)
                    EmitValueTypeAddressIfNeeded(fieldSymbol.ContainingType, containingType);
            }
            else if (fieldAccess.Receiver is not null)
            {
                EmitExpression(fieldAccess.Receiver);

                if (requiresReceiverAddress)
                    EmitValueTypeAddressIfNeeded(fieldAccess.Receiver.Type, containingType);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Ldarg_0);

                if (requiresReceiverAddress)
                    EmitValueTypeAddressIfNeeded(null, containingType);
            }
        }

        if (TryGetAsyncInvestigationFieldLabel(fieldSymbol, out var label))
        {
            ILGenerator.Emit(OpCodes.Dup);
            ILGenerator.Emit(OpCodes.Ldflda, GetField(fieldSymbol));
            EmitAsyncInvestigationLog(label, AsyncInvestigationOperation.Load);
        }

        ILGenerator.Emit(OpCodes.Ldfld, GetField(fieldSymbol));
    }

    private void EmitPropertyAccess(BoundPropertyAccess propertyAccess)
    {
        var propertySymbol = propertyAccess.Property;

        if (propertySymbol.IsExtensionProperty())
            throw new InvalidOperationException($"Extension property '{propertySymbol.Name}' requires an explicit receiver.");

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
                    EmitValueTypeAddressIfNeeded(MethodSymbol.ContainingType, propertySymbol.ContainingType);
            }

            if (propertySymbol.GetMethod is null)
                throw new InvalidOperationException($"Property '{propertySymbol.Name}' does not have a getter.");

            MethodInfo getter = GetMethodInfo(propertySymbol.GetMethod);

            var callOpCode = propertySymbol.IsStatic || propertySymbol.ContainingType!.IsValueType
                ? OpCodes.Call
                : OpCodes.Callvirt;

            ILGenerator.Emit(callOpCode, getter);
        }
    }

    private void EmitLiteralExpression(BoundLiteralExpression literalExpression)
    {
        switch (literalExpression.Kind)
        {
            case BoundLiteralExpressionKind.NumericLiteral:
                EmitNumericConstant(literalExpression.Value);
                break;

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

    private void EmitNumericConstant(object value)
    {
        switch (value)
        {
            case byte b:
                ILGenerator.Emit(OpCodes.Ldc_I4, b);
                break;
            case sbyte sb:
                ILGenerator.Emit(OpCodes.Ldc_I4, sb);
                break;
            case short s:
                ILGenerator.Emit(OpCodes.Ldc_I4, s);
                break;
            case ushort us:
                ILGenerator.Emit(OpCodes.Ldc_I4, us);
                break;
            case int i:
                ILGenerator.Emit(OpCodes.Ldc_I4, i);
                break;
            case uint ui:
                ILGenerator.Emit(OpCodes.Ldc_I4, unchecked((int)ui));
                break;
            case long l:
                ILGenerator.Emit(OpCodes.Ldc_I8, l);
                break;
            case ulong ul:
                ILGenerator.Emit(OpCodes.Ldc_I8, unchecked((long)ul));
                break;
            case float f:
                ILGenerator.Emit(OpCodes.Ldc_R4, f);
                break;
            case double d:
                ILGenerator.Emit(OpCodes.Ldc_R8, d);
                break;
            case decimal dec:
                EmitDecimalConstant(dec);
                break;
            case Enum enumValue:
                EmitEnumConstant(enumValue);
                break;
            case DateTime dateTime:
                EmitDateTimeConstant(dateTime);
                break;
            default:
                throw new Exception("Not supported literal value");
        }
    }

    private void EmitDecimalConstant(decimal value)
    {
        var bits = decimal.GetBits(value);
        var isNegative = (bits[3] & unchecked((int)0x80000000)) != 0;
        var scale = (byte)((bits[3] >> 16) & 0x7F);

        ILGenerator.Emit(OpCodes.Ldc_I4, bits[0]);
        ILGenerator.Emit(OpCodes.Ldc_I4, bits[1]);
        ILGenerator.Emit(OpCodes.Ldc_I4, bits[2]);
        ILGenerator.Emit(isNegative ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ldc_I4_S, scale);

        var constructor = typeof(decimal).GetConstructor(new[] { typeof(int), typeof(int), typeof(int), typeof(bool), typeof(byte) })!;
        ILGenerator.Emit(OpCodes.Newobj, constructor);
    }

    private void EmitEnumConstant(Enum value)
    {
        var underlying = Enum.GetUnderlyingType(value.GetType());
        var numericValue = Convert.ChangeType(value, underlying, CultureInfo.InvariantCulture)!;
        EmitNumericConstant(numericValue);
    }

    private void EmitDateTimeConstant(DateTime value)
    {
        var constructor = typeof(DateTime).GetConstructor(new[] { typeof(long), typeof(DateTimeKind) })!;

        ILGenerator.Emit(OpCodes.Ldc_I8, value.Ticks);
        ILGenerator.Emit(OpCodes.Ldc_I4, (int)value.Kind);
        ILGenerator.Emit(OpCodes.Newobj, constructor);
    }

    private void EmitIfExpression(BoundIfExpression ifStatement)
    {
        var elseLabel = ILGenerator.DefineLabel();

        // Create a scope upfront so any pattern variables introduced in the
        // condition are available within the "then" branch.
        var scope = new Scope(this);
        new ExpressionGenerator(scope, ifStatement.Condition)
            .EmitBranchOpForCondition(ifStatement.Condition, elseLabel);

        var resultClrType = ifStatement.Type is not null
            ? ResolveClrType(ifStatement.Type)
            : null;

        new ExpressionGenerator(scope, ifStatement.ThenBranch).Emit();

        var thenType = ifStatement.ThenBranch.Type;

        var thenRequiresBox =
            resultClrType is { IsValueType: false }
            && (thenType?.IsValueType ?? false);

        if (thenRequiresBox)
        {
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(thenType));
        }

        if (ifStatement.ElseBranch is not null)
        {
            // Define a label for the end of the 'if' statement
            var endIfLabel = ILGenerator.DefineLabel();

            // Branch to end of 'if' after the 'if' block
            ILGenerator.Emit(OpCodes.Br, endIfLabel);

            // Mark the 'else' label
            ILGenerator.MarkLabel(elseLabel);

            // Emit the 'else' block
            var scope2 = new Scope(this);
            new ExpressionGenerator(scope2, ifStatement.ElseBranch).Emit();

            var elseType = ifStatement.ElseBranch.Type;

            var elseRequiresBox =
                resultClrType is { IsValueType: false }
                && (elseType?.IsValueType ?? false);

            if (elseRequiresBox)
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

    internal void EmitBranchOpForCondition(BoundExpression expression, ILLabel end)
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
                    ILGenerator.Emit(OpCodes.Brfalse, end);
                    break;

                case BinaryOperatorKind.Inequality:
                    ILGenerator.Emit(OpCodes.Ceq);
                    ILGenerator.Emit(OpCodes.Ldc_I4_0);
                    ILGenerator.Emit(OpCodes.Ceq); // logical NOT
                    ILGenerator.Emit(OpCodes.Brfalse, end);
                    break;

                case BinaryOperatorKind.GreaterThan:
                    ILGenerator.Emit(OpCodes.Cgt);
                    ILGenerator.Emit(OpCodes.Brfalse, end);
                    break;

                case BinaryOperatorKind.LessThan:
                    ILGenerator.Emit(OpCodes.Clt);
                    ILGenerator.Emit(OpCodes.Brfalse, end);
                    break;

                case BinaryOperatorKind.GreaterThanOrEqual:
                    ILGenerator.Emit(OpCodes.Clt);
                    ILGenerator.Emit(OpCodes.Brtrue, end);
                    break;

                case BinaryOperatorKind.LessThanOrEqual:
                    ILGenerator.Emit(OpCodes.Cgt);
                    ILGenerator.Emit(OpCodes.Brtrue, end);
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
                ILGenerator.Emit(OpCodes.Br, end);
            }
        }
        else
        {
            // Other kinds of expressions... member access etc.

            EmitExpression(expression);
            ILGenerator.Emit(OpCodes.Brfalse, end);
        }
    }

    private void EmitBlock(BoundBlockExpression block)
    {
        var scope = new Scope(this, block.LocalsToDispose);
        var statements = block.Statements.ToArray();

        if (statements.Length > 0)
            MethodBodyGenerator.DeclareLocals(scope, statements);

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

        IILocal? resultTemp = null;
        if (resultExpression is not null)
        {
            new ExpressionGenerator(scope, resultExpression).Emit();

            var resultType = resultExpression.Type;
            if (resultType is not null)
            {
                var clrType = ResolveClrType(resultType);
                resultTemp = ILGenerator.DeclareLocal(clrType);
                ILGenerator.Emit(OpCodes.Stloc, resultTemp);
            }
        }

        EmitDispose(block.LocalsToDispose);

        if (resultTemp is not null)
            ILGenerator.Emit(OpCodes.Ldloc, resultTemp);
    }

    private void EmitTryExpression(BoundTryExpression tryExpression)
    {
        var resultType = tryExpression.Type ?? Compilation.ErrorTypeSymbol;
        var resultClrType = ResolveClrType(resultType);
        var resultLocal = ILGenerator.DeclareLocal(resultClrType);

        var exceptionType = tryExpression.ExceptionType;
        if (exceptionType.TypeKind == TypeKind.Error)
            exceptionType = Compilation.GetSpecialType(SpecialType.System_Object);

        var expressionType = tryExpression.Expression.Type ?? Compilation.ErrorTypeSymbol;

        var exitLabel = MethodBodyGenerator.GetOrCreateReturnLabel();

        var tryScope = new Scope(this);
        tryScope.SetExceptionExitLabel(exitLabel);

        if (tryExpression.Expression is BoundBlockExpression blockExpression)
        {
            EmitTryExpressionBlock(tryScope, blockExpression);
        }
        else
        {
            ILGenerator.BeginExceptionBlock();
            new ExpressionGenerator(tryScope, tryExpression.Expression).Emit();
        }

        if (!SymbolEqualityComparer.Default.Equals(expressionType, resultType))
        {
            var expressionConversion = Compilation.ClassifyConversion(expressionType, resultType);
            if (expressionConversion.Exists && !expressionConversion.IsIdentity)
                EmitConversion(expressionType, resultType, expressionConversion);
        }

        ILGenerator.Emit(OpCodes.Stloc, resultLocal);

        ILGenerator.BeginCatchBlock(ResolveClrType(exceptionType));

        var catchConversion = Compilation.ClassifyConversion(exceptionType, resultType);
        if (catchConversion.Exists && !catchConversion.IsIdentity)
            EmitConversion(exceptionType, resultType, catchConversion);

        ILGenerator.Emit(OpCodes.Stloc, resultLocal);

        ILGenerator.EndExceptionBlock();

        ILGenerator.Emit(OpCodes.Ldloc, resultLocal);
    }

    private void EmitTryExpressionBlock(Scope tryScope, BoundBlockExpression blockExpression)
    {
        var blockScope = new Scope(tryScope, blockExpression.LocalsToDispose);
        var statements = blockExpression.Statements.ToArray();

        if (statements.Length > 0)
            MethodBodyGenerator.DeclareLocals(blockScope, statements);

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
            if (statements[i] is BoundLabeledStatement labeled)
                MethodBodyGenerator.RegisterLabelScope(labeled.Label, blockScope);
        }

        ILGenerator.BeginExceptionBlock();

        for (int i = 0; i < count; i++)
        {
            var statement = statements[i];
            if (statement is BoundLabeledStatement labeled)
            {
                MethodBodyGenerator.RegisterLabelScope(labeled.Label, blockScope);
                var ilLabel = MethodBodyGenerator.GetOrCreateLabel(labeled.Label);
                ILGenerator.MarkLabel(ilLabel);
                new StatementGenerator(blockScope, labeled.Statement).Emit();
            }
            else
            {
                new StatementGenerator(blockScope, statement).Emit();
            }
        }

        IILocal? resultTemp = null;
        if (resultExpression is not null)
        {
            new ExpressionGenerator(blockScope, resultExpression).Emit();

            var resultExprType = resultExpression.Type;
            if (resultExprType is not null)
            {
                var clrType = ResolveClrType(resultExprType);
                resultTemp = ILGenerator.DeclareLocal(clrType);
                ILGenerator.Emit(OpCodes.Stloc, resultTemp);
            }
        }

        EmitDispose(blockExpression.LocalsToDispose);

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

    private readonly struct DelegateConstructorCacheKey
    {
        public DelegateConstructorCacheKey(
            Type delegateClrType,
            ITypeSymbol returnType,
            ImmutableArray<ITypeSymbol> parameterTypes,
            ImmutableArray<RefKind> refKinds)
        {
            DelegateClrType = delegateClrType ?? throw new ArgumentNullException(nameof(delegateClrType));
            ReturnType = returnType ?? throw new ArgumentNullException(nameof(returnType));
            ParameterTypes = parameterTypes.IsDefault ? ImmutableArray<ITypeSymbol>.Empty : parameterTypes;
            RefKinds = refKinds.IsDefault ? ImmutableArray<RefKind>.Empty : refKinds;
        }

        public Type DelegateClrType { get; }

        public ITypeSymbol ReturnType { get; }

        public ImmutableArray<ITypeSymbol> ParameterTypes { get; }

        public ImmutableArray<RefKind> RefKinds { get; }
    }

    private sealed class DelegateConstructorCacheKeyComparer : IEqualityComparer<DelegateConstructorCacheKey>
    {
        public bool Equals(DelegateConstructorCacheKey x, DelegateConstructorCacheKey y)
        {
            if (!ReferenceEquals(x.DelegateClrType, y.DelegateClrType))
                return false;

            if (!SymbolEqualityComparer.Default.Equals(x.ReturnType, y.ReturnType))
                return false;

            if (x.ParameterTypes.Length != y.ParameterTypes.Length || x.RefKinds.Length != y.RefKinds.Length)
                return false;

            for (var i = 0; i < x.ParameterTypes.Length; i++)
            {
                if (x.RefKinds[i] != y.RefKinds[i])
                    return false;

                if (!SymbolEqualityComparer.Default.Equals(x.ParameterTypes[i], y.ParameterTypes[i]))
                    return false;
            }

            return true;
        }

        public int GetHashCode(DelegateConstructorCacheKey obj)
        {
            var hash = RuntimeHelpers.GetHashCode(obj.DelegateClrType);
            hash = HashCode.Combine(hash, SymbolEqualityComparer.Default.GetHashCode(obj.ReturnType));

            for (var i = 0; i < obj.ParameterTypes.Length; i++)
            {
                hash = HashCode.Combine(
                    hash,
                    SymbolEqualityComparer.Default.GetHashCode(obj.ParameterTypes[i]),
                    obj.RefKinds[i]);
            }

            return hash;
        }
    }
}
