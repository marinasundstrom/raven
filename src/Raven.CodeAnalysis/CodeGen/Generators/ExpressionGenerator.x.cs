using System;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal partial class ExpressionGenerator
{
    private ILocalSymbol? _carrierPayloadSymbol;
    private IILocal? _carrierPayloadLocal;

    private void EmitCarrierConditionalAccessExpression(BoundCarrierConditionalAccessExpression expr, EmitContext context)
    {
        var receiverClrType = Generator.InstantiateType(ResolveClrType(expr.Receiver.Type));

        // Evaluate receiver once
        var receiverInfo = EmitExpression(expr.Receiver);
        var tmp = SpillValueToLocalIfNeeded(receiverClrType, receiverInfo, keepValueOnStack: false);

        switch (expr.CarrierKind)
        {
            case BoundCarrierKind.Result:
                EmitResultCarrierConditionalAccess(expr, receiverClrType, tmp);
                return;

            case BoundCarrierKind.Option:
                EmitOptionCarrierConditionalAccess(expr, receiverClrType, tmp);
                return;

            default:
                throw new NotSupportedException();
        }
    }

    private void EmitResultCarrierConditionalAccess(
    BoundCarrierConditionalAccessExpression expr,
    Type receiverClrType,
    IILocal receiverTmp)
    {
        // Symbol-driven emission avoids open generic locals (!0/!1) from reflection over TypeBuilderInstantiation.
        if (expr.ResultTryGetValueForOkCaseMethod is null ||
            expr.ResultTryGetValueForErrorCaseMethod is null ||
            expr.ResultOkCaseType is null ||
            expr.ResultErrorCaseType is null ||
            expr.ResultOkCtor is null ||
            expr.ResultErrorCtor is null ||
            expr.ResultImplicitFromOk is null ||
            expr.ResultImplicitFromError is null ||
            expr.ReceiverResultOkCaseType is null ||
            expr.ReceiverResultErrorCaseType is null ||
            expr.ReceiverResultOkValueGetter is null ||
            expr.ReceiverResultErrorDataGetter is null)
        {
            throw new InvalidOperationException(
                "Missing carrier symbols for Result conditional access (TryGetValue/Ok/Error/ctors/op_Implicit/receiver symbols).");
        }

        var resultClrType = Generator.InstantiateType(ResolveClrType(expr.Type));
        var payloadClrType = Generator.InstantiateType(ResolveClrType(expr.PayloadType));

        var tryGetOkMI = GetMethodInfo(expr.ResultTryGetValueForOkCaseMethod!);
        var okCaseClrType = CloseNestedCarrierCaseType(
            TryGetOutLocalElementType(tryGetOkMI)
                ?? Generator.InstantiateType(ResolveClrType(expr.ReceiverResultOkCaseType!)),
            tryGetOkMI.DeclaringType ?? receiverClrType);
        var okOutLocal = ILGenerator.DeclareLocal(okCaseClrType);

        var okLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        // if (tmp.TryGetValue(out okOutLocal)) goto okLabel;
        EmitCarrierReceiver(receiverTmp, receiverClrType);
        ILGenerator.Emit(OpCodes.Ldloca_S, okOutLocal);
        ILGenerator.Emit(OpCodes.Call, tryGetOkMI);
        ILGenerator.Emit(OpCodes.Brtrue, okLabel);

        EmitResultErrorWrap_Symbols(expr, receiverTmp, receiverClrType, resultClrType);
        ILGenerator.Emit(OpCodes.Br, endLabel);

        ILGenerator.MarkLabel(okLabel);

        // Extract payload into a local usable by WhenPresent binding.
        var payloadLocalBuilder = ILGenerator.DeclareLocal(payloadClrType);

        if (okCaseClrType == payloadClrType)
        {
            ILGenerator.Emit(OpCodes.Ldloc, okOutLocal);
            ILGenerator.Emit(OpCodes.Stloc, payloadLocalBuilder);
        }
        else
        {
            var valueGetter = GetMethodInfo(expr.ReceiverResultOkValueGetter!);
            ILGenerator.Emit(OpCodes.Ldloca_S, okOutLocal);
            ILGenerator.Emit(OpCodes.Call, valueGetter);
            ILGenerator.Emit(OpCodes.Stloc, payloadLocalBuilder);
        }

        var prevSym = _carrierPayloadSymbol;
        var prevLoc = _carrierPayloadLocal;
        _carrierPayloadSymbol = expr.PayloadLocal;
        _carrierPayloadLocal = payloadLocalBuilder;

        try { EmitExpression(expr.WhenPresent); }
        finally { _carrierPayloadSymbol = prevSym; _carrierPayloadLocal = prevLoc; }

        // Wrap as Result<U,E>.Ok then implicit convert to Result<U,E>
        var okCtor = GetConstructorInfo(expr.ResultOkCtor!);
        ILGenerator.Emit(OpCodes.Newobj, okCtor);
        ILGenerator.Emit(OpCodes.Call, GetMethodInfo(expr.ResultImplicitFromOk!));

        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitResultErrorWrap_Symbols(
    BoundCarrierConditionalAccessExpression expr,
    IILocal receiverTmp,
    Type receiverClrType,
    Type resultClrType)
    {
        // Build Result<U,E>.Error from the receiver's error payload.
        if (expr.ResultTryGetValueForErrorCaseMethod is null ||
            expr.ResultErrorCaseType is null ||
            expr.ResultErrorCtor is null ||
            expr.ResultImplicitFromError is null)
        {
            throw new InvalidOperationException("Missing result error symbols for carrier conditional access.");
        }

        var tryGetErrMI = GetMethodInfo(expr.ResultTryGetValueForErrorCaseMethod);
        var errCaseClrType = CloseNestedCarrierCaseType(
            TryGetOutLocalElementType(tryGetErrMI)
                ?? Generator.InstantiateType(ResolveClrType(expr.ReceiverResultErrorCaseType)),
            tryGetErrMI.DeclaringType ?? resultClrType);
        var errOutLocal = ILGenerator.DeclareLocal(errCaseClrType);

        var gotErrLabel = ILGenerator.DefineLabel();
        var doneLabel = ILGenerator.DefineLabel();

        EmitCarrierReceiver(receiverTmp, receiverClrType);
        ILGenerator.Emit(OpCodes.Ldloca_S, errOutLocal);
        ILGenerator.Emit(OpCodes.Call, tryGetErrMI);
        ILGenerator.Emit(OpCodes.Brtrue, gotErrLabel);

        // If no error, default payload
        var ctor = expr.ResultErrorCtor;
        if (ctor.Parameters.Length == 0)
        {
            ILGenerator.Emit(OpCodes.Newobj, GetConstructorInfo(ctor));
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(expr.ResultImplicitFromError));
            return;
        }

        var paramType = Generator.InstantiateType(ResolveClrType(ctor.Parameters[0].Type));
        if (paramType.IsValueType)
        {
            var tmpDefault = ILGenerator.DeclareLocal(paramType);
            ILGenerator.Emit(OpCodes.Ldloca_S, tmpDefault);
            ILGenerator.Emit(OpCodes.Initobj, paramType);
            ILGenerator.Emit(OpCodes.Ldloc, tmpDefault);
        }
        else
        {
            ILGenerator.Emit(OpCodes.Ldnull);
        }

        ILGenerator.Emit(OpCodes.Br, doneLabel);

        ILGenerator.MarkLabel(gotErrLabel);

        if (ctor.Parameters.Length == 1)
        {
            var getter = expr.ReceiverResultErrorDataGetter!;
            ILGenerator.Emit(OpCodes.Ldloca_S, errOutLocal);
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(getter));

            // If needed, do a simple reference cast. (Binder should ensure types match.)
            var getterClr = Generator.InstantiateType(ResolveClrType(getter.ReturnType));
            var paramClr = paramType;
            if (getterClr != paramClr)
            {
                if (paramClr.IsValueType)
                    ILGenerator.Emit(OpCodes.Unbox_Any, paramClr);
                else
                    ILGenerator.Emit(OpCodes.Castclass, paramClr);
            }
        }

        ILGenerator.MarkLabel(doneLabel);

        ILGenerator.Emit(OpCodes.Newobj, GetConstructorInfo(expr.ResultErrorCtor)!);
        ILGenerator.Emit(OpCodes.Call, GetMethodInfo(expr.ResultImplicitFromError));
    }

    private static Type? TryGetOutLocalElementType(MethodInfo methodInfo)
    {
        var parameters = methodInfo.GetParameters();
        if (parameters.Length != 1)
            return null;

        var parameterType = parameters[0].ParameterType;
        if (!parameterType.IsByRef)
            return null;

        var elementType = parameterType.GetElementType();
        if (elementType is null)
            return null;

        return CloseTypeFromMethodContext(elementType, methodInfo.DeclaringType);
    }

    private static Type CloseTypeFromMethodContext(Type type, Type? declaringType)
    {
        if (type.IsByRef)
            return CloseTypeFromMethodContext(type.GetElementType()!, declaringType).MakeByRefType();

        if (type.IsPointer)
            return CloseTypeFromMethodContext(type.GetElementType()!, declaringType).MakePointerType();

        if (type.IsArray)
        {
            var closedElement = CloseTypeFromMethodContext(type.GetElementType()!, declaringType);
            return type.GetArrayRank() == 1
                ? closedElement.MakeArrayType()
                : closedElement.MakeArrayType(type.GetArrayRank());
        }

        if (type.IsGenericParameter)
        {
            if (type.DeclaringMethod is not null)
                return type;

            if (declaringType is { IsGenericType: true, ContainsGenericParameters: false })
            {
                var typeArguments = declaringType.GetGenericArguments();
                var ordinal = type.GenericParameterPosition;
                if ((uint)ordinal < (uint)typeArguments.Length)
                    return typeArguments[ordinal];
            }

            return type;
        }

        if (!type.IsGenericType)
            return type;

        var definition = type.IsGenericTypeDefinition ? type : type.GetGenericTypeDefinition();
        var arguments = type.GetGenericArguments();
        var substituted = new Type[arguments.Length];
        var changed = false;

        for (var i = 0; i < arguments.Length; i++)
        {
            var updated = CloseTypeFromMethodContext(arguments[i], declaringType);
            substituted[i] = updated;
            if (!ReferenceEquals(updated, arguments[i]))
                changed = true;
        }

        if (!changed)
            return type;

        return definition.MakeGenericType(substituted);
    }

    private static Type CloseNestedCarrierCaseType(Type caseType, Type carrierType)
    {
        if (!caseType.ContainsGenericParameters)
            return caseType;

        if (!carrierType.IsGenericType || carrierType.ContainsGenericParameters)
            return caseType;

        var flags = BindingFlags.Public | BindingFlags.NonPublic;
        var nestedType = carrierType.GetNestedType(caseType.Name, flags);
        if (nestedType is null)
        {
            var caseBaseName = caseType.Name.Split('`')[0];
            nestedType = carrierType.GetNestedTypes(flags)
                .FirstOrDefault(type =>
                    string.Equals(type.Name, caseType.Name, StringComparison.Ordinal) ||
                    string.Equals(type.Name.Split('`')[0], caseBaseName, StringComparison.Ordinal));
        }

        if (nestedType is not null && nestedType.ContainsGenericParameters)
        {
            var nestedDefinition = nestedType.IsGenericTypeDefinition
                ? nestedType
                : nestedType.GetGenericTypeDefinition();
            var carrierArguments = carrierType.GetGenericArguments();
            if (nestedDefinition.GetGenericArguments().Length == carrierArguments.Length)
            {
                nestedType = nestedDefinition.MakeGenericType(carrierArguments);
            }
        }

        return nestedType ?? caseType;
    }

    private void EmitResultOkWrap(Type resultClrType)
    {
        var okCaseType = FindNestedCaseTypeSafe(resultClrType, "Ok");
        var okCtor = FindSingleArgCtorSafe(okCaseType);

        ILGenerator.Emit(OpCodes.Newobj, okCtor);

        var opImplicit = FindImplicitConversionSafe(resultClrType, okCaseType);
        ILGenerator.Emit(OpCodes.Call, opImplicit);
    }

    private void EmitResultErrorWrap(
    BoundCarrierConditionalAccessExpression expr,
    Type receiverClrType,
    IILocal receiverTmp,
    Type resultClrType)
    {
        // Legacy reflection-based implementation (TypeBuilderInstantiation.GetMethods/GetConstructors)
        // is not safe for async state machines. Delegate to the symbol-driven path.
        EmitResultErrorWrap_Symbols(expr, receiverTmp, receiverClrType, resultClrType);
    }

    private static MethodInfo? FindSingleOutBoolMethod(Type t, string name)
    {
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        static MethodInfo? FindOn(Type target, string methodName)
        {
            return target
                .GetMethods(flags)
                .FirstOrDefault(m =>
                    m.Name == methodName &&
                    m.ReturnType == typeof(bool) &&
                    m.GetParameters().Length == 1 &&
                    m.GetParameters()[0].ParameterType.IsByRef);
        }

        static bool HasOpenOutType(MethodInfo m)
        {
            var p = m.GetParameters();
            if (p.Length != 1) return false;
            var pt = p[0].ParameterType;
            if (!pt.IsByRef) return false;
            var et = pt.GetElementType();
            return et is not null && et.ContainsGenericParameters;
        }

        // If we have a constructed generic type, ALWAYS prefer mapping from the generic definition.
        // Reflection over TypeBuilderInstantiation often returns methods whose signatures still reference
        // open generic params (!0/!1) in nested out types, which produces invalid IL.
        if (t.IsGenericType)
        {
            var def = t.GetGenericTypeDefinition();

            MethodInfo? defMethod;
            try { defMethod = FindOn(def, name); }
            catch (NotSupportedException) { defMethod = null; }

            if (defMethod is null)
                return null;

            // If we're already on the definition, return it.
            if (t == def)
                return defMethod;

            try
            {
                var mapped = TypeBuilder.GetMethod(t, defMethod);

                // Defensive: if mapping still leaves open params in the out type, reject.
                if (HasOpenOutType(mapped))
                    return null;

                return mapped;
            }
            catch
            {
                return null;
            }
        }

        // Non-generic receiver: normal reflection is fine.
        try
        {
            var direct = FindOn(t, name);
            if (direct is null)
                return null;

            // If the out type is somehow still open (shouldn't happen), reject.
            if (HasOpenOutType(direct))
                return null;

            return direct;
        }
        catch (NotSupportedException)
        {
            return null;
        }
    }

    private static Type FindNestedCaseType(Type carrierClrType, string caseName)
    {
        var nested = carrierClrType.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic)
            .FirstOrDefault(t => t.Name == caseName)
            ?? carrierClrType.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic)
            .FirstOrDefault(t => t.Name.StartsWith(caseName, StringComparison.Ordinal));

        if (nested is null)
            throw new InvalidOperationException($"Missing nested case '{caseName}' on '{carrierClrType}'.");

        if (nested.IsGenericTypeDefinition && carrierClrType.IsGenericType)
            return nested.MakeGenericType(carrierClrType.GetGenericArguments());

        return nested;
    }

    private void EmitOptionCarrierConditionalAccess(
        BoundCarrierConditionalAccessExpression expr,
        Type receiverClrType,
        IILocal receiverTmp)
    {
        // Option<T>?.X  => Option<U>
        var optionClrType = Generator.InstantiateType(ResolveClrType(expr.Type));          // Option<U>
        var payloadClrType = Generator.InstantiateType(ResolveClrType(expr.PayloadType)); // T (payload local type)

        // Try to find a "has payload" method. Prefer TryGetSome(out ...), then common alternates.
        var tryGetSomeSym = expr.OptionTryGetValueMethod;
        if (tryGetSomeSym is null)
            throw new InvalidOperationException("Missing Option.TryGetValue symbol");

        var tryGetSome = GetMethodInfo(tryGetSomeSym);

        var someCaseSym = expr.OptionSomeCaseType
            ?? throw new InvalidOperationException("Missing Option.Some case symbol");
        var someOutType = Generator.InstantiateType(ResolveClrType(someCaseSym));
        var someOutLocal = ILGenerator.DeclareLocal(someOutType);

        var someLabel = ILGenerator.DefineLabel();
        var endLabel = ILGenerator.DefineLabel();

        // if (tmp.TryGetSome(out someOutLocal)) goto someLabel;
        EmitCarrierReceiver(receiverTmp, receiverClrType);
        ILGenerator.Emit(OpCodes.Ldloca_S, someOutLocal);
        ILGenerator.Emit(OpCodes.Call, tryGetSome);
        ILGenerator.Emit(OpCodes.Brtrue, someLabel);

        // NONE PATH: push Option<U>.None on stack
        var noneCtorSym = expr.OptionNoneCtorOrFactory
            ?? throw new InvalidOperationException("Missing None ctor/factory symbol");
        ILGenerator.Emit(OpCodes.Newobj, GetConstructorInfo(noneCtorSym)!);

        var implNoneSym = expr.OptionImplicitFromNone
            ?? throw new InvalidOperationException("Missing implicit conversion from None");
        ILGenerator.Emit(OpCodes.Call, GetMethodInfo(implNoneSym)); ILGenerator.Emit(OpCodes.Br, endLabel);

        // SOME PATH:
        ILGenerator.MarkLabel(someLabel);

        // Extract payload into a real IL local (so WhenPresent can read it)
        var payloadLocalBuilder = ILGenerator.DeclareLocal(payloadClrType);

        if (someOutType == payloadClrType)
        {
            ILGenerator.Emit(OpCodes.Ldloc, someOutLocal);
            ILGenerator.Emit(OpCodes.Stloc, payloadLocalBuilder);
        }
        else
        {
            var valueGetterSym = expr.OptionSomeValueGetter
     ?? throw new InvalidOperationException("Missing Some.Value getter symbol");

            ILGenerator.Emit(OpCodes.Ldloca_S, someOutLocal);
            ILGenerator.Emit(OpCodes.Call, GetMethodInfo(valueGetterSym));
            ILGenerator.Emit(OpCodes.Stloc, payloadLocalBuilder);
        }

        // Emit WhenPresent, with payload symbol mapped to payloadLocalBuilder
        var prevSym = _carrierPayloadSymbol;
        var prevLoc = _carrierPayloadLocal;
        _carrierPayloadSymbol = expr.PayloadLocal;
        _carrierPayloadLocal = payloadLocalBuilder;

        try { EmitExpression(expr.WhenPresent); }
        finally { _carrierPayloadSymbol = prevSym; _carrierPayloadLocal = prevLoc; }

        // Stack now has U. Wrap into Option<U>.Some and convert to Option<U>.
        var someCtorSym = expr.OptionSomeCtor
            ?? throw new InvalidOperationException("Missing Some ctor symbol");
        ILGenerator.Emit(OpCodes.Newobj, GetConstructorInfo(someCtorSym)!);

        var implSomeSym = expr.OptionImplicitFromSome
            ?? throw new InvalidOperationException("Missing implicit conversion from Some");
        ILGenerator.Emit(OpCodes.Call, GetMethodInfo(implSomeSym));

        ILGenerator.MarkLabel(endLabel);
    }

    private void EmitOptionSomeWrap(Type optionClrType)
    {
        var someCaseType = FindNestedCaseTypeSafe(optionClrType, "Some");
        var someCtor = FindSingleArgCtorSafe(someCaseType);

        ILGenerator.Emit(OpCodes.Newobj, someCtor);

        var opImplicit = FindImplicitConversionSafe(optionClrType, someCaseType);
        ILGenerator.Emit(OpCodes.Call, opImplicit);
    }

    private void EmitOptionNoneWrap(Type optionClrType)
    {
        // Push Option<U>.None (case) then implicitly convert to Option<U>
        var noneCaseType = FindNestedCaseTypeSafe(optionClrType, "None");

        // Prefer a static field/property (common for singleton none), otherwise a parameterless ctor.
        var noneField = noneCaseType.GetField("Value", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)
                      ?? noneCaseType.GetField("Instance", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)
                      ?? noneCaseType.GetField("Default", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static);

        if (noneField is not null)
        {
            ILGenerator.Emit(OpCodes.Ldsfld, noneField);
        }
        else
        {
            var noneProp = noneCaseType.GetProperty("Value", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)
                        ?? noneCaseType.GetProperty("Instance", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)
                        ?? noneCaseType.GetProperty("Default", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static);

            if (noneProp?.GetMethod is not null)
            {
                ILGenerator.Emit(OpCodes.Call, noneProp.GetMethod);
            }
            else
            {
                var noneCtor = FindParameterlessCtorSafe(noneCaseType);
                ILGenerator.Emit(OpCodes.Newobj, noneCtor);
            }
        }

        var opImplicit = FindImplicitConversionSafe(optionClrType, noneCaseType);
        ILGenerator.Emit(OpCodes.Call, opImplicit);
    }

    private static Type[] GetNestedTypesSafe(Type t)
    {
        try { return t.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic); }
        catch (NotSupportedException)
        {
            if (t.IsGenericType)
                return t.GetGenericTypeDefinition().GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic);
            throw;
        }
    }

    private static MethodInfo[] GetMethodsSafe(Type t, BindingFlags flags)
    {
        try { return t.GetMethods(flags); }
        catch (NotSupportedException)
        {
            if (t.IsGenericType)
                return t.GetGenericTypeDefinition().GetMethods(flags);
            throw;
        }
    }

    private static ConstructorInfo[] GetConstructorsSafe(Type t, BindingFlags flags)
    {
        try { return t.GetConstructors(flags); }
        catch (NotSupportedException)
        {
            if (t.IsGenericType)
                return t.GetGenericTypeDefinition().GetConstructors(flags);
            throw;
        }
    }

    private static MethodInfo MapMethod(Type constructedType, MethodInfo definitionMethod)
    {
        return TypeBuilder.GetMethod(constructedType, definitionMethod);
    }

    private static ConstructorInfo MapCtor(Type constructedType, ConstructorInfo definitionCtor)
    {
        return TypeBuilder.GetConstructor(constructedType, definitionCtor);
    }

    private static Type FindNestedCaseTypeSafe(Type carrierClrType, string caseName)
    {
        var nestedTypes = GetNestedTypesSafe(carrierClrType);

        var nested = nestedTypes.FirstOrDefault(t => t.Name == caseName)
            ?? nestedTypes.FirstOrDefault(t => t.Name.StartsWith(caseName, StringComparison.Ordinal));

        if (nested is null)
            throw new InvalidOperationException($"Missing nested case '{caseName}' on '{carrierClrType}'.");

        // If we found it on the generic definition, close it using the constructed arguments.
        if (carrierClrType.IsGenericType)
        {
            var args = carrierClrType.GetGenericArguments();
            if (nested.IsGenericTypeDefinition)
                return nested.MakeGenericType(args);

            if (nested.ContainsGenericParameters && nested.IsGenericType)
                return nested.GetGenericTypeDefinition().MakeGenericType(args);
        }

        return nested;
    }

    private static ConstructorInfo FindSingleArgCtorSafe(Type caseType)
    {
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        try
        {
            var direct = caseType.GetConstructors(flags).FirstOrDefault(c => c.GetParameters().Length == 1);
            if (direct is not null) return direct;
        }
        catch (NotSupportedException) { /* fallthrough */ }

        if (caseType.IsGenericType)
        {
            var def = caseType.GetGenericTypeDefinition();
            var defCtor = GetConstructorsSafe(def, flags).FirstOrDefault(c => c.GetParameters().Length == 1)
                ?? throw new InvalidOperationException($"Missing single-arg ctor on '{caseType}'.");
            return MapCtor(caseType, defCtor);
        }

        throw new InvalidOperationException($"Missing single-arg ctor on '{caseType}'.");
    }

    private static ConstructorInfo FindParameterlessCtorSafe(Type caseType)
    {
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        try
        {
            var direct = caseType.GetConstructors(flags).FirstOrDefault(c => c.GetParameters().Length == 0);
            if (direct is not null) return direct;
        }
        catch (NotSupportedException) { /* fallthrough */ }

        if (caseType.IsGenericType)
        {
            var def = caseType.GetGenericTypeDefinition();
            var defCtor = GetConstructorsSafe(def, flags).FirstOrDefault(c => c.GetParameters().Length == 0)
                ?? throw new InvalidOperationException($"Missing parameterless ctor on '{caseType}'.");
            return MapCtor(caseType, defCtor);
        }

        throw new InvalidOperationException($"Missing parameterless ctor on '{caseType}'.");
    }

    private static MethodInfo FindImplicitConversionSafe(Type carrierClrType, Type fromType)
    {
        const BindingFlags flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static;

        static bool SameTypeShape(Type a, Type b)
        {
            if (a == b)
                return true;

            // Prefer definition comparison for generics.
            if (a.IsGenericType && !a.IsGenericTypeDefinition)
                a = a.GetGenericTypeDefinition();

            if (b.IsGenericType && !b.IsGenericTypeDefinition)
                b = b.GetGenericTypeDefinition();

            if (a == b)
                return true;

            // Nested case structs can come from different reflection contexts; compare by declaring type + name + arity.
            if (a.Name != b.Name)
                return false;

            if (a.IsGenericTypeDefinition != b.IsGenericTypeDefinition)
                return false;

            if (a.IsGenericTypeDefinition && b.IsGenericTypeDefinition)
            {
                if (a.GetGenericArguments().Length != b.GetGenericArguments().Length)
                    return false;
            }

            var da = a.DeclaringType;
            var db = b.DeclaringType;
            if (da is null || db is null)
                return a.FullName == b.FullName;

            // For declaring types, also compare by definition shape.
            if (da.IsGenericType && !da.IsGenericTypeDefinition)
                da = da.GetGenericTypeDefinition();

            if (db.IsGenericType && !db.IsGenericTypeDefinition)
                db = db.GetGenericTypeDefinition();

            return da.Name == db.Name && da.FullName == db.FullName;
        }

        // 1) Direct path: try to resolve on the constructed carrier type.
        try
        {
            var direct = carrierClrType.GetMethods(flags).FirstOrDefault(m =>
                m.Name == "op_Implicit" &&
                m.ReturnType == carrierClrType &&
                m.GetParameters().Length == 1 &&
                m.GetParameters()[0].ParameterType == fromType);

            if (direct is not null)
                return direct;
        }
        catch (NotSupportedException)
        {
            // fallthrough to definition + map
        }

        // 2) Definition + map path.
        if (!carrierClrType.IsGenericType)
            throw new InvalidOperationException("Missing implicit conversion to carrier type.");

        var defCarrier = carrierClrType.GetGenericTypeDefinition();

        // We want the *definition* of the source type (often nested, e.g. Result`2+Error`2).
        var defFrom = fromType;
        if (defFrom.IsGenericType && !defFrom.IsGenericTypeDefinition)
            defFrom = defFrom.GetGenericTypeDefinition();

        // Some nested types returned from constructed contexts may still have open params; compare by shape.
        MethodInfo? def = null;

        foreach (var m in GetMethodsSafe(defCarrier, flags))
        {
            if (m.Name != "op_Implicit")
                continue;

            if (m.GetParameters().Length != 1)
                continue;

            // Return type must be the carrier definition (shape match, not ref equality).
            if (!SameTypeShape(m.ReturnType, defCarrier))
                continue;

            var p0 = m.GetParameters()[0].ParameterType;
            if (SameTypeShape(p0, defFrom))
            {
                def = m;
                break;
            }

            // Extra tolerant fallback: for nested case types, allow matching by name when reflection contexts differ.
            if (p0.DeclaringType == defCarrier && p0.Name == defFrom.Name)
            {
                def = m;
                break;
            }
        }

        if (def is null)
            throw new InvalidOperationException("Missing implicit conversion to carrier type.");

        return MapMethod(carrierClrType, def);
    }

    private void EmitCarrierReceiver(IILocal receiverTmp, Type receiverClrType)
    {
        if (receiverClrType.IsValueType)
            ILGenerator.Emit(OpCodes.Ldloca_S, receiverTmp);
        else
            ILGenerator.Emit(OpCodes.Ldloc, receiverTmp);
    }
}
