using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal abstract class Generator
{
    static readonly Dictionary<SyntaxTree, SemanticModel> _semanticModels = new Dictionary<SyntaxTree, SemanticModel>();

    public Generator(Generator? parent = null)
    {
        Parent = parent;
    }

    public Generator? Parent { get; }

    public Compilation Compilation => MethodBodyGenerator.Compilation;

    public MethodGenerator MethodGenerator => MethodBodyGenerator.MethodGenerator;

    public virtual MethodBodyGenerator MethodBodyGenerator => Parent!.MethodBodyGenerator;

    public IMethodSymbol MethodSymbol => MethodBodyGenerator.MethodSymbol;

    public IILBuilder ILGenerator => MethodBodyGenerator.ILGenerator;

    public virtual void Emit()
    {

    }

    public virtual void AddLocal(ILocalSymbol localSymbol, IILocal builder)
    {
        Parent?.AddLocal(localSymbol, builder);
    }

    public virtual IILocal? GetLocal(ILocalSymbol localSymbol)
    {
        return Parent?.GetLocal(localSymbol);
    }

    public virtual IEnumerable<ILocalSymbol> EnumerateLocalsToDispose()
    {
        return Parent?.EnumerateLocalsToDispose() ?? Enumerable.Empty<ILocalSymbol>();
    }

    public virtual bool TryGetExceptionExitLabel(out ILLabel label)
    {
        if (Parent is not null)
            return Parent.TryGetExceptionExitLabel(out label);

        label = default;
        return false;
    }

    public void EmitDispose(ImmutableArray<ILocalSymbol> locals)
    {
        if (locals.IsDefaultOrEmpty || locals.Length == 0)
            return;

        var disposableType = Compilation.GetSpecialType(SpecialType.System_IDisposable);
        if (disposableType.TypeKind == TypeKind.Error)
            return;

        var disposableClr = ResolveClrType(disposableType);
        var disposeMethod = disposableClr.GetMethod(nameof(IDisposable.Dispose), Type.EmptyTypes)
            ?? throw new InvalidOperationException("Missing IDisposable.Dispose method.");

        for (int i = locals.Length - 1; i >= 0; i--)
        {
            EmitDispose(locals[i], disposableClr, disposeMethod);
        }
    }

    private void EmitDispose(ILocalSymbol local, Type disposableClr, MethodInfo disposeMethod)
    {
        if (local.Type is null || local.Type.TypeKind == TypeKind.Error)
            return;

        var localBuilder = GetLocal(local);
        if (localBuilder is null)
            return;

        if (local.Type.IsReferenceType || local.Type.TypeKind == TypeKind.Null)
        {
            var skipLabel = ILGenerator.DefineLabel();
            ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
            ILGenerator.Emit(OpCodes.Brfalse, skipLabel);
            ILGenerator.Emit(OpCodes.Ldloc, localBuilder);
            ILGenerator.Emit(OpCodes.Callvirt, disposeMethod);
            ILGenerator.MarkLabel(skipLabel);
        }
        else
        {
            var clrType = ResolveClrType(local.Type);
            ILGenerator.Emit(OpCodes.Ldloca, localBuilder);
            ILGenerator.Emit(OpCodes.Constrained, clrType);
            ILGenerator.Emit(OpCodes.Callvirt, disposeMethod);
        }
    }

    internal static Type InstantiateType(Type type)
    {
        if (type is TypeBuilder typeBuilder && typeBuilder.ContainsGenericParameters)
        {
            var parameters = typeBuilder.GetGenericArguments();
            return parameters.Length == 0 ? typeBuilder : typeBuilder.MakeGenericType(parameters);
        }

        if (type.IsGenericTypeDefinition)
        {
            var parameters = type.GetGenericArguments();
            return parameters.Length == 0 ? type : type.MakeGenericType(parameters);
        }

        return type;
    }

    protected BoundNode GetBoundNode(SyntaxNode syntaxNode)
    {
        SemanticModel semanticModel = ResolveSemanticModel(syntaxNode);
        return semanticModel.GetBoundNode(syntaxNode);
    }

    protected BoundExpression GetBoundNode(ExpressionSyntax expression)
    {
        SemanticModel semanticModel = ResolveSemanticModel(expression);
        return semanticModel.GetBoundNode(expression) ?? throw new InvalidCastException("Cannot cast {0} to {2}.");
    }

    protected SymbolInfo GetSymbolInfo(SyntaxNode syntaxNode)
    {
        SemanticModel semanticModel = ResolveSemanticModel(syntaxNode);
        return semanticModel.GetSymbolInfo(syntaxNode);
    }

    protected T? GetDeclaredSymbol<T>(SyntaxNode syntaxNode)
        where T : class, ISymbol
    {
        SemanticModel semanticModel = ResolveSemanticModel(syntaxNode);
        return semanticModel.GetDeclaredSymbol(syntaxNode) as T;
    }

    protected TypeInfo GetTypeInfo(ExpressionSyntax expression)
    {
        SemanticModel semanticModel = ResolveSemanticModel(expression);
        return semanticModel.GetTypeInfo(expression);
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(MethodGenerator.TypeGenerator.CodeGen);
    }

    public MemberInfo? GetMemberBuilder(SourceSymbol sourceSymbol) => MethodGenerator.TypeGenerator.CodeGen.GetMemberBuilder(sourceSymbol);

    protected static ConstructorInfo GetNullableConstructor(Type nullableType, Type underlyingType)
    {
        if (nullableType.IsGenericType)
        {
            var definition = nullableType.GetGenericTypeDefinition();
            var isTypeBuilderInstantiation = string.Equals(
                nullableType.GetType().FullName,
                "System.Reflection.Emit.TypeBuilderInstantiation",
                StringComparison.Ordinal);
            if (nullableType.ContainsGenericParameters || definition is TypeBuilder || isTypeBuilderInstantiation)
            {
                var genericArgument = definition.GetGenericArguments()[0];
                var definitionCtor = definition.GetConstructor(new[] { genericArgument });
                if (definitionCtor is not null)
                    return TypeBuilder.GetConstructor(nullableType, definitionCtor);

                throw new InvalidOperationException($"Missing Nullable constructor for {nullableType}");
            }
        }

        var ctor = nullableType.GetConstructor(new[] { underlyingType });
        if (ctor is not null)
            return ctor;

        throw new InvalidOperationException($"Missing Nullable constructor for {nullableType}");
    }

    private SemanticModel ResolveSemanticModel(SyntaxNode syntaxNode)
    {
        var syntaxTree = syntaxNode.SyntaxTree!;

        if (!_semanticModels.TryGetValue(syntaxTree, out var semanticModel))
        {
            semanticModel = Compilation.GetSemanticModel(syntaxTree);
            _semanticModels[syntaxTree] = semanticModel;
        }

        return semanticModel;
    }

    protected void EmitConversion(ITypeSymbol from, ITypeSymbol to, Conversion conversion)
    {
        if (conversion.IsIdentity)
            return;

        if (to is ByRefTypeSymbol && from is IAddressTypeSymbol)
            return;

        var fromClrType = ResolveClrType(from);
        var toClrType = ResolveClrType(to);

        if (conversion.IsUserDefined && !conversion.IsLifted && conversion.MethodSymbol is IMethodSymbol userDefinedMethod)
        {
            var parameterType = userDefinedMethod.Parameters[0].Type;
            if (!SymbolEqualityComparer.Default.Equals(from, parameterType))
            {
                var parameterConversion = Compilation.ClassifyConversion(from, parameterType, includeUserDefined: false);
                if (parameterConversion.Exists && parameterConversion.IsImplicit)
                    EmitConversion(from, parameterType, parameterConversion);
            }

            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(userDefinedMethod));
            return;
        }

        if (to is NullableTypeSymbol nullableReference && !nullableReference.UnderlyingType.IsValueType)
        {
            EmitConversion(from, nullableReference.UnderlyingType, conversion);
            return;
        }

        if (to is NullableTypeSymbol nullableTo && nullableTo.UnderlyingType.IsValueType)
        {
            if (conversion.IsLifted && from is NullableTypeSymbol fromNullable && fromNullable.UnderlyingType.IsValueType)
            {
                EmitLiftedNullableConversion(fromNullable, nullableTo, conversion);
                return;
            }

            EmitNullableConversion(from, nullableTo);
            return;
        }

        if (to is ITypeUnionSymbol unionTo)
        {
            EmitUnionConversion(from, unionTo);
            return;
        }

        if (conversion.IsDiscriminatedUnion)
        {
            if (conversion.MethodSymbol is IMethodSymbol methodSymbol)
            {
                ILGenerator.Emit(OpCodes.Call, GetMethodInfo(methodSymbol));
                return;
            }

            if (to is INamedTypeSymbol toNamed)
            {
                var symbolMatch = toNamed
                    .GetMembers("op_Implicit")
                    .OfType<IMethodSymbol>()
                    .FirstOrDefault(m =>
                        m.Parameters.Length == 1 &&
                        ParameterMatchesSource(m.Parameters[0].Type, from));

                if (symbolMatch is not null)
                {
                    ILGenerator.Emit(OpCodes.Call, GetMethodInfo(symbolMatch));
                    return;
                }
            }

            if (IsDynamicBuilderType(toClrType) || IsDynamicBuilderType(fromClrType))
            {
                if (fromClrType == toClrType)
                    return;

                throw new NotSupportedException("Discriminated-union conversion method not found.");
            }

            var candidate = toClrType.GetMethod(
                               "op_Implicit",
                               BindingFlags.Public | BindingFlags.Static,
                               binder: null,
                               types: new[] { fromClrType },
                               modifiers: null)
                           ?? fromClrType.GetMethod(
                               "op_Implicit",
                               BindingFlags.Public | BindingFlags.Static,
                               binder: null,
                               types: new[] { fromClrType },
                               modifiers: null);

            if (candidate is not null)
            {
                ILGenerator.Emit(OpCodes.Call, candidate);
                return;
            }

            if (fromClrType == toClrType)
                return;
        }

        if (conversion.IsNumeric)
        {
            EmitNumericConversion(to);
            return;
        }

        if (conversion.IsUnboxing)
        {
            if (fromClrType.IsValueType)
            {
                if (toClrType == fromClrType)
                    return;

                if (conversion.IsNumeric)
                {
                    EmitNumericConversion(to);
                    return;
                }
            }

            ILGenerator.Emit(OpCodes.Unbox_Any, toClrType);
            return;
        }

        if (conversion.IsBoxing)
        {
            if (!fromClrType.IsValueType)
                return;

            ILGenerator.Emit(OpCodes.Box, fromClrType);
            if (!SymbolEqualityComparer.Default.Equals(from, to))
                ILGenerator.Emit(OpCodes.Castclass, toClrType);
            return;
        }

        if (conversion.IsReference &&
            from is ITypeParameterSymbol typeParameter &&
            (typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) == 0 &&
            to.SpecialType == SpecialType.System_Object)
        {
            ILGenerator.Emit(OpCodes.Box, fromClrType);
            return;
        }

        if (conversion.IsReference)
        {
            ILGenerator.Emit(OpCodes.Castclass, toClrType);
            return;
        }

        if (conversion.IsPointer)
        {
            ILGenerator.Emit(OpCodes.Conv_U);
            return;
        }

        throw new NotSupportedException("Unsupported conversion");
    }

    private void EmitUnionConversion(ITypeSymbol from, ITypeUnionSymbol unionTo)
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
        ITypeUnionSymbol unionTo,
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

        var ctor = GetNullableConstructor(nullableClrType, underlyingClrType);

        ILGenerator.Emit(OpCodes.Call, ctor);
        ILGenerator.Emit(OpCodes.Ldloc, nullableLocal);
    }

    private void EmitNullableConversion(ITypeSymbol from, NullableTypeSymbol nullableTo)
    {
        if (from.TypeKind == TypeKind.Null)
        {
            EmitDefaultValue(nullableTo);
            return;
        }

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

        var ctor = GetNullableConstructor(nullableClr, underlyingClr);

        ILGenerator.Emit(OpCodes.Call, ctor);
        ILGenerator.Emit(OpCodes.Ldloc, nullableLocal);
    }

    private void EmitLiftedNullableConversion(
        NullableTypeSymbol fromNullable,
        NullableTypeSymbol toNullable,
        Conversion conversion)
    {
        if (SymbolEqualityComparer.Default.Equals(fromNullable.UnderlyingType, toNullable.UnderlyingType) &&
            conversion.IsIdentity)
        {
            return;
        }

        var fromClr = ResolveClrType(fromNullable);
        var toClr = ResolveClrType(toNullable);

        var fromUnderlying = fromNullable.UnderlyingType;
        var toUnderlying = toNullable.UnderlyingType;
        var fromUnderlyingClr = ResolveClrType(fromUnderlying);
        var toUnderlyingClr = ResolveClrType(toUnderlying);

        var fromLocal = ILGenerator.DeclareLocal(fromClr);
        var toLocal = ILGenerator.DeclareLocal(toClr);
        var valueLocal = ILGenerator.DeclareLocal(toUnderlyingClr);

        var hasValueLabel = ILGenerator.DefineLabel();
        var doneLabel = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Stloc, fromLocal);
        ILGenerator.Emit(OpCodes.Ldloca, fromLocal);
        var hasValue = fromClr.GetProperty("HasValue")!.GetGetMethod()!;
        ILGenerator.Emit(OpCodes.Call, hasValue);
        ILGenerator.Emit(OpCodes.Brtrue_S, hasValueLabel);

        ILGenerator.Emit(OpCodes.Ldloca, toLocal);
        ILGenerator.Emit(OpCodes.Initobj, toClr);
        ILGenerator.Emit(OpCodes.Ldloc, toLocal);
        ILGenerator.Emit(OpCodes.Br_S, doneLabel);

        ILGenerator.MarkLabel(hasValueLabel);
        ILGenerator.Emit(OpCodes.Ldloca, fromLocal);
        var getValueOrDefault = fromClr.GetMethod("GetValueOrDefault", Type.EmptyTypes)!;
        ILGenerator.Emit(OpCodes.Call, getValueOrDefault);

        if (!SymbolEqualityComparer.Default.Equals(fromUnderlying, toUnderlying))
        {
            var underlyingConversion = Compilation.ClassifyConversion(fromUnderlying, toUnderlying);
            EmitConversion(fromUnderlying, toUnderlying, underlyingConversion);
        }

        ILGenerator.Emit(OpCodes.Stloc, valueLocal);
        ILGenerator.Emit(OpCodes.Ldloca, toLocal);
        ILGenerator.Emit(OpCodes.Ldloc, valueLocal);

        var ctor = GetNullableConstructor(toClr, toUnderlyingClr);

        ILGenerator.Emit(OpCodes.Call, ctor);
        ILGenerator.Emit(OpCodes.Ldloc, toLocal);
        ILGenerator.MarkLabel(doneLabel);
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

    protected void EmitDefaultValue(ITypeSymbol type)
    {
        if (type is ITypeParameterSymbol typeParameter)
        {
            if ((typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0)
            {
                ILGenerator.Emit(OpCodes.Ldnull);
                return;
            }

            EmitDefaultValueWithInitObj(type);
            return;
        }

        if (type.IsValueType)
        {
            EmitDefaultValueWithInitObj(type);
            return;
        }

        ILGenerator.Emit(OpCodes.Ldnull);
    }

    protected void EmitDefaultValueWithInitObj(ITypeSymbol type)
    {
        var clr = ResolveClrType(type);
        var local = ILGenerator.DeclareLocal(clr);
        ILGenerator.Emit(OpCodes.Ldloca, local);
        ILGenerator.Emit(OpCodes.Initobj, clr);
        ILGenerator.Emit(OpCodes.Ldloc, local);
    }

    public MethodInfo GetMethodInfo(IMethodSymbol methodSymbol)
    {
        return methodSymbol.GetClrMethodInfo(MethodGenerator.TypeGenerator.CodeGen);
    }

    private static bool IsDynamicBuilderType(Type type)
    {
        var fullName = type.FullName;
        return fullName is not null &&
               fullName.StartsWith("System.Reflection.Emit.TypeBuilder", StringComparison.Ordinal);
    }

    private static bool ParameterMatchesSource(ITypeSymbol parameterType, ITypeSymbol sourceType)
    {
        if (SymbolEqualityComparer.Default.Equals(parameterType, sourceType))
            return true;

        if (parameterType.MetadataIdentityEquals(sourceType))
            return true;

        if (parameterType is INamedTypeSymbol parameterNamed &&
            sourceType is INamedTypeSymbol sourceNamed &&
            SymbolEqualityComparer.Default.Equals(parameterNamed.OriginalDefinition, sourceNamed.OriginalDefinition))
        {
            return true;
        }

        return false;
    }
}
