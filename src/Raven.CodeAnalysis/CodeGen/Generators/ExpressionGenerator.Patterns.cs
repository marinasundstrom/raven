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

internal partial class ExpressionGenerator
{
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
        // Determine scrutinee type (fall back to object on error)
        var scrutineeType =
            matchExpression.Expression.Type
            ?? Compilation.GetSpecialType(SpecialType.System_Object);

        if (scrutineeType.TypeKind == TypeKind.Error)
            scrutineeType = Compilation.GetSpecialType(SpecialType.System_Object);

        // Emit scrutinee value
        EmitExpression(matchExpression.Expression);

        var scrutineeClrType = ResolveClrType(scrutineeType);

        // We can keep the scrutinee unboxed only when it's a value-type DU
        var isValueTypeDU = IsDiscriminatedUnionValueType(scrutineeType);

        // Store scrutinee into a local (typed when unboxed, object when boxed)
        IILocal scrutineeLocal;

        if (scrutineeClrType.IsValueType && !isValueTypeDU)
        {
            // Non-DU value types: box into object pipeline (EmitPattern expects object-ish)
            ILGenerator.Emit(OpCodes.Box, scrutineeClrType);

            scrutineeLocal = ILGenerator.DeclareLocal(typeof(object));
            ILGenerator.Emit(OpCodes.Stloc, scrutineeLocal);

            scrutineeType = Compilation.GetSpecialType(SpecialType.System_Object);
            scrutineeClrType = typeof(object);
        }
        else
        {
            // Value-type DU can stay unboxed; reference types also stay as-is
            scrutineeLocal = ILGenerator.DeclareLocal(scrutineeClrType);
            ILGenerator.Emit(OpCodes.Stloc, scrutineeLocal);
        }

        var endLabel = ILGenerator.DefineLabel();
        var fallthroughLabel = ILGenerator.DefineLabel();
        var exitLabel = ILGenerator.DefineLabel();

        foreach (var arm in matchExpression.Arms)
        {
            var nextArmLabel = ILGenerator.DefineLabel();
            var scope = new Scope(this);

            // --- Pattern test ---
            if (isValueTypeDU)
            {
                // Try unboxed DU fast path (supports wrappers like not/and/or around a case pattern)
                if (!TryEmitPatternTest_UnboxedValueTypeDU(arm.Pattern, scope, scrutineeLocal, scrutineeClrType))
                {
                    // Fallback to object-pattern pipeline:
                    // load scrutinee and BOX (because EmitPattern expects object-ish input)
                    ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
                    ILGenerator.Emit(OpCodes.Box, scrutineeClrType);

                    EmitPattern(arm.Pattern, scope);
                }
            }
            else
            {
                // Non-unboxed path:
                // scrutineeLocal is either object already, or a reference type, so EmitPattern works
                ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
                EmitPattern(arm.Pattern, scope);
            }

            ILGenerator.Emit(OpCodes.Brfalse, nextArmLabel);

            // --- Guard ---
            if (arm.Guard is not null)
            {
                new ExpressionGenerator(scope, arm.Guard).Emit();
                ILGenerator.Emit(OpCodes.Brfalse, nextArmLabel);
            }

            // --- Arm body ---
            new ExpressionGenerator(scope, arm.Expression).Emit();

            // If the match expression itself is a union, box arm values when needed
            var armType = arm.Expression.Type;
            if ((matchExpression.Type?.IsTypeUnion ?? false) &&
                (armType?.IsValueType ?? false))
            {
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(armType));
            }

            ILGenerator.Emit(OpCodes.Br, endLabel);
            ILGenerator.MarkLabel(nextArmLabel);
        }

        ILGenerator.Emit(OpCodes.Br, fallthroughLabel);

        ILGenerator.MarkLabel(endLabel);
        ILGenerator.Emit(OpCodes.Br, exitLabel);

        ILGenerator.MarkLabel(fallthroughLabel);

        var exceptionCtor = typeof(InvalidOperationException)
            .GetConstructor(new[] { typeof(string) })!;

        ILGenerator.Emit(OpCodes.Ldstr, "Match expression was not exhaustive.");
        ILGenerator.Emit(OpCodes.Newobj, exceptionCtor);
        ILGenerator.Emit(OpCodes.Throw);

        ILGenerator.MarkLabel(exitLabel);
    }

    private bool TryEmitPatternTest_UnboxedValueTypeDU(
        BoundPattern pattern,
        Generator scope,
        IILocal unionLocal,
        Type unionClrType)
    {
        // Direct case pattern (fast path)
        if (pattern is BoundCasePattern cp)
        {
            EmitCasePatternUnboxed(cp, scope, unionLocal, unionClrType);
            return true;
        }

        // not <case>
        if (pattern is BoundUnaryPattern up && up.Kind == BoundUnaryPatternKind.Not)
        {
            if (!TryEmitPatternTest_UnboxedValueTypeDU(up.Pattern, scope, unionLocal, unionClrType))
                return false;

            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            ILGenerator.Emit(OpCodes.Ceq);
            return true;
        }

        // (<case> and <case>) or any nesting of those
        if (pattern is BoundBinaryPattern bp)
        {
            if (bp.Kind == BoundPatternKind.And)
                return TryEmitAnd_UnboxedDU(bp.Left, bp.Right, scope, unionLocal, unionClrType);

            if (bp.Kind == BoundPatternKind.Or)
                return TryEmitOr_UnboxedDU(bp.Left, bp.Right, scope, unionLocal, unionClrType);

            return false;
        }

        // Anything else (property patterns, tuple patterns, declaration patterns, constants etc.)
        // is not supported in the unboxed DU pipeline, fall back to boxed/object pipeline.
        return false;
    }

    private bool TryEmitAnd_UnboxedDU(
        BoundPattern left,
        BoundPattern right,
        Generator scope,
        IILocal unionLocal,
        Type unionClrType)
    {
        if (!IsUnboxedDUCompatible(left) || !IsUnboxedDUCompatible(right))
            return false;

        var labelFail = ILGenerator.DefineLabel();
        var labelDone = ILGenerator.DefineLabel();

        TryEmitPatternTest_UnboxedValueTypeDU(left, scope, unionLocal, unionClrType);
        ILGenerator.Emit(OpCodes.Brfalse, labelFail);

        TryEmitPatternTest_UnboxedValueTypeDU(right, scope, unionLocal, unionClrType);
        ILGenerator.Emit(OpCodes.Brfalse, labelFail);

        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Br, labelDone);

        ILGenerator.MarkLabel(labelFail);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);

        ILGenerator.MarkLabel(labelDone);
        return true;
    }

    private bool TryEmitOr_UnboxedDU(
        BoundPattern left,
        BoundPattern right,
        Generator scope,
        IILocal unionLocal,
        Type unionClrType)
    {
        if (!IsUnboxedDUCompatible(left) || !IsUnboxedDUCompatible(right))
            return false;

        var labelTrue = ILGenerator.DefineLabel();
        var labelDone = ILGenerator.DefineLabel();

        TryEmitPatternTest_UnboxedValueTypeDU(left, scope, unionLocal, unionClrType);
        ILGenerator.Emit(OpCodes.Brtrue, labelTrue);

        TryEmitPatternTest_UnboxedValueTypeDU(right, scope, unionLocal, unionClrType);
        ILGenerator.Emit(OpCodes.Brtrue, labelTrue);

        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Br, labelDone);

        ILGenerator.MarkLabel(labelTrue);
        ILGenerator.Emit(OpCodes.Ldc_I4_1);

        ILGenerator.MarkLabel(labelDone);
        return true;
    }

    private static bool IsUnboxedDUCompatible(BoundPattern pattern)
    {
        // The only patterns we handle without boxing are:
        // - case patterns
        // - unary not over unboxed-compatible patterns
        // - binary and/or over unboxed-compatible patterns
        if (pattern is BoundCasePattern)
            return true;

        if (pattern is BoundUnaryPattern up && up.Kind == BoundUnaryPatternKind.Not)
            return IsUnboxedDUCompatible(up.Pattern);

        if (pattern is BoundBinaryPattern bp &&
            (bp.Kind == BoundPatternKind.And || bp.Kind == BoundPatternKind.Or))
            return IsUnboxedDUCompatible(bp.Left) && IsUnboxedDUCompatible(bp.Right);

        return false;
    }

    private void EmitCasePatternUnboxed(
    BoundCasePattern casePattern,
    Generator scope,
    IILocal unionLocal,
    Type unionClrType)
    {
        var caseClrType =
            Generator.InstantiateType(
                ResolveClrType(casePattern.CaseSymbol));

        var caseLocal = ILGenerator.DeclareLocal(caseClrType);

        var labelFail = ILGenerator.DefineLabel();
        var labelDone = ILGenerator.DefineLabel();

        // init caseLocal
        ILGenerator.Emit(OpCodes.Ldloca, caseLocal);
        ILGenerator.Emit(OpCodes.Initobj, caseClrType);

        // TryGetXxx(ref case)
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

    private static bool IsDiscriminatedUnionValueType(ITypeSymbol type)
    {
        return type is INamedTypeSymbol named &&
               named.IsValueType &&
               named.TryGetDiscriminatedUnion() is not null;
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
                var requiresUnbox = clrType.IsValueType ||
                                    (clrType.IsGenericParameter && !isReferencePattern);

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
            // Preserve scrutinee because EmitPattern(child) consumes it.
            var scrutineeLocal = ILGenerator.DeclareLocal(typeof(object));
            ILGenerator.Emit(OpCodes.Stloc, scrutineeLocal);

            ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
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
            // Preserve scrutinee because each EmitPattern(child) consumes it.
            var scrutineeLocal = ILGenerator.DeclareLocal(typeof(object));
            ILGenerator.Emit(OpCodes.Stloc, scrutineeLocal);

            var labelFail = ILGenerator.DefineLabel();
            var labelDone = ILGenerator.DefineLabel();

            if (binaryPattern.Kind == BoundPatternKind.And)
            {
                // left
                ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
                EmitPattern(binaryPattern.Left, scope);
                ILGenerator.Emit(OpCodes.Brfalse, labelFail);

                // right
                ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
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

                // left
                ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
                EmitPattern(binaryPattern.Left, scope);
                ILGenerator.Emit(OpCodes.Brtrue, labelTrue);

                // right
                ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
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
        else if (pattern is BoundPropertyPattern propertyPattern)
        {
            EmitPropertyPattern(propertyPattern, scope);
        }
        else
        {
            throw new NotSupportedException("Unsupported pattern");
        }
    }

    private void EmitPropertyPattern(BoundPropertyPattern propertyPattern, Generator scope)
    {
        // Stack at entry: [scrutineeObj]
        // We'll always treat the scrutinee as object here (EmitIsPatternExpression boxed value types already).
        var objLocal = ILGenerator.DeclareLocal(typeof(object));
        ILGenerator.Emit(OpCodes.Stloc, objLocal);

        var labelFail = ILGenerator.DefineLabel();
        var labelDone = ILGenerator.DefineLabel();

        // Null fails for property patterns (same semantics as C# for "Foo { ... }").
        ILGenerator.Emit(OpCodes.Ldloc, objLocal);
        ILGenerator.Emit(OpCodes.Brfalse, labelFail);

        // If an explicit type was written (Foo { ... } or Foo { }),
        // emit a type-test using the *narrowed* type.
        if (propertyPattern.NarrowedType is not null)
        {
            var narrowedClrType = ResolveClrType(propertyPattern.NarrowedType);

            ILGenerator.Emit(OpCodes.Ldloc, objLocal);
            ILGenerator.Emit(OpCodes.Isinst, narrowedClrType);
            ILGenerator.Emit(OpCodes.Brfalse, labelFail);
        }

        // Special-case: empty property pattern `{ }` => matches any non-null
        // (plus the explicit type-test above, if present).
        if (propertyPattern.Properties.Length == 0)
        {
            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            ILGenerator.Emit(OpCodes.Br, labelDone);

            ILGenerator.MarkLabel(labelFail);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);

            ILGenerator.MarkLabel(labelDone);
            return;
        }

        // Determine which runtime type we're matching members against
        var lookupType = propertyPattern.ReceiverType;
        var lookupClrType = ResolveClrType(lookupType);

        // Create typed local
        IILocal typedLocal;
        if (lookupClrType == typeof(object))
        {
            typedLocal = ILGenerator.DeclareLocal(typeof(object));
            ILGenerator.Emit(OpCodes.Ldloc, objLocal);
            ILGenerator.Emit(OpCodes.Stloc, typedLocal);
        }
        else
        {
            typedLocal = ILGenerator.DeclareLocal(lookupClrType);
            ILGenerator.Emit(OpCodes.Ldloc, objLocal);
            ILGenerator.Emit(OpCodes.Castclass, lookupClrType);
            ILGenerator.Emit(OpCodes.Stloc, typedLocal);
        }

        // Evaluate each property subpattern
        foreach (var sub in propertyPattern.Properties)
        {
            // Load receiver for member access
            ILGenerator.Emit(OpCodes.Ldloc, typedLocal);

            // Load member value
            switch (sub.Member)
            {
                case IPropertySymbol prop:
                    {
                        if (prop.GetMethod is null)
                        {
                            ILGenerator.Emit(OpCodes.Br, labelFail);
                            break;
                        }

                        var getter = GetMethodInfo(prop.GetMethod);
                        ILGenerator.Emit(OpCodes.Callvirt, getter);
                        break;
                    }

                case IFieldSymbol field:
                    {
                        var fieldInfo = GetField(field);
                        ILGenerator.Emit(OpCodes.Ldfld, fieldInfo);
                        break;
                    }

                default:
                    // Unknown member → fail the whole pattern
                    ILGenerator.Emit(OpCodes.Br, labelFail);
                    break;
            }

            // Box member value if it is a value type (EmitPattern generally operates on object-y values)
            if (RequiresValueTypeHandling(sub.Type) && sub.Type.TypeKind != TypeKind.Error)
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(sub.Type));

            // Now stack: [memberValueObj]
            EmitPattern(sub.Pattern, scope);
            ILGenerator.Emit(OpCodes.Brfalse, labelFail);
        }

        // Success
        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Br, labelDone);

        // Fail
        ILGenerator.MarkLabel(labelFail);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);

        ILGenerator.MarkLabel(labelDone);
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

        // scrutinee is currently on stack (object-ish)
        var scrutineeLocal = ILGenerator.DeclareLocal(typeof(object));
        ILGenerator.Emit(OpCodes.Stloc, scrutineeLocal);

        if (value is null)
        {
            ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
            ILGenerator.Emit(OpCodes.Ldnull);
            ILGenerator.Emit(OpCodes.Ceq);
            return;
        }

        var notNull = ILGenerator.DefineLabel();
        var done = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
        ILGenerator.Emit(OpCodes.Brtrue, notNull);

        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Br, done);

        ILGenerator.MarkLabel(notNull);

        // Fast path for primitives/enums/bool/char/int etc.
        var underlying = literal.UnderlyingType;

        if (underlying.IsValueType && underlying.SpecialType is
            SpecialType.System_Boolean or
            SpecialType.System_Char or
            SpecialType.System_SByte or
            SpecialType.System_Byte or
            SpecialType.System_Int16 or
            SpecialType.System_UInt16 or
            SpecialType.System_Int32 or
            SpecialType.System_UInt32 or
            SpecialType.System_Int64 or
            SpecialType.System_UInt64)
        {
            ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
            ILGenerator.Emit(OpCodes.Unbox_Any, ResolveClrType(underlying));

            EmitLiteral(value); // pushes the constant in its natural IL form
            ILGenerator.Emit(OpCodes.Ceq);

            ILGenerator.MarkLabel(done);
            return;
        }

        // Fallback: object.Equals
        ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
        EmitConstantAsObject(literal, value);

        var runtimeObjectType = typeof(object);
        var equalsMethod = runtimeObjectType.GetMethod(nameof(object.Equals), new[] { runtimeObjectType })!;
        ILGenerator.Emit(OpCodes.Callvirt, equalsMethod);

        ILGenerator.MarkLabel(done);
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
}
