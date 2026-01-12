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
    // ============================================
    // Entry points
    // ============================================

    private void EmitIsPatternExpression(BoundIsPatternExpression e)
    {
        EmitExpression(e.Expression);

        var inputType =
            e.Expression.Type
            ?? Compilation.GetSpecialType(SpecialType.System_Object);

        if (inputType.TypeKind == TypeKind.Error)
            inputType = Compilation.GetSpecialType(SpecialType.System_Object);

        EmitPattern(e.Pattern, inputType, scope: null);
    }

    private void EmitMatchExpression(BoundMatchExpression matchExpression)
    {
        // Determine scrutinee type (fall back to object on error)
        var scrutineeType =
            matchExpression.Expression.Type
            ?? Compilation.GetSpecialType(SpecialType.System_Object);

        if (scrutineeType.TypeKind == TypeKind.Error)
            scrutineeType = Compilation.GetSpecialType(SpecialType.System_Object);

        var scrutineeClrType = ResolveClrType(scrutineeType);

        // Emit scrutinee value
        EmitExpression(matchExpression.Expression);

        // Always keep a typed local for scrutinee (even if we later need boxed too)
        var typedLocal = ILGenerator.DeclareLocal(scrutineeClrType);
        ILGenerator.Emit(OpCodes.Stloc, typedLocal);

        // Value-type DU fast path is orthogonal
        var isValueTypeDU = IsDiscriminatedUnionValueType(scrutineeType);

        // Precompute which arms truly require object semantics.
        // If the scrutinee is a value-type DU and the pattern is DU-compatible,
        // we can skip boxing/isinst entirely and go straight to TryGet*.
        bool ArmNeedsObject(BoundMatchArm a)
        {
            if (isValueTypeDU && IsUnboxedDUCompatible(a.Pattern))
                return false;

            return GetPatternInputRequirement(a.Pattern) == PatternInput.Object;
        }

        var anyArmNeedsObject = matchExpression.Arms.Any(ArmNeedsObject);

        // Lazy boxed scrutinee cache (only created/filled if needed)
        IILocal? boxedLocal = anyArmNeedsObject ? ILGenerator.DeclareLocal(typeof(object)) : null;

        void LoadScrutineeForArm(PatternInput req, out ITypeSymbol inputTypeForEmit)
        {
            if (req == PatternInput.Typed)
            {
                ILGenerator.Emit(OpCodes.Ldloc, typedLocal);
                inputTypeForEmit = scrutineeType;
                return;
            }

            // Object required
            if (boxedLocal is null)
                boxedLocal = ILGenerator.DeclareLocal(typeof(object));

            // Initialize boxedLocal if it hasn't been initialized yet (null)
            var boxedReady = ILGenerator.DefineLabel();

            ILGenerator.Emit(OpCodes.Ldloc, boxedLocal);
            ILGenerator.Emit(OpCodes.Brtrue, boxedReady);

            ILGenerator.Emit(OpCodes.Ldloc, typedLocal);
            if (scrutineeClrType.IsValueType)
                ILGenerator.Emit(OpCodes.Box, scrutineeClrType);

            ILGenerator.Emit(OpCodes.Stloc, boxedLocal);

            ILGenerator.MarkLabel(boxedReady);

            ILGenerator.Emit(OpCodes.Ldloc, boxedLocal);
            inputTypeForEmit = Compilation.GetSpecialType(SpecialType.System_Object);
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
                // DU unboxed fast path first (supports not/and/or around case patterns)
                if (!TryEmitPatternTest_UnboxedValueTypeDU(arm.Pattern, scope, typedLocal, scrutineeClrType))
                {
                    // Fall back to regular pipeline (typed/object depending on requirement)
                    var req = ArmNeedsObject(arm) ? PatternInput.Object : PatternInput.Typed;
                    LoadScrutineeForArm(req, out var inputTypeForEmit);
                    EmitPattern(arm.Pattern, inputTypeForEmit, scope);
                }
            }
            else
            {
                var req = ArmNeedsObject(arm) ? PatternInput.Object : PatternInput.Typed;
                LoadScrutineeForArm(req, out var inputTypeForEmit);
                EmitPattern(arm.Pattern, inputTypeForEmit, scope);
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
            if ((matchExpression.Type?.IsTypeUnion ?? false) && (armType?.IsValueType ?? false))
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

    // ============================================
    // Pattern emission (typed input)
    // ============================================

    private enum PatternInput
    {
        Typed = 0,
        Object = 1,
    }

    private PatternInput GetPatternInputRequirement(BoundPattern pattern)
    {
        switch (pattern)
        {
            // constant patterns can be evaluated in typed form (for value-type scrutinees)
            case BoundConstantPattern:
                return PatternInput.Typed;

            case BoundRelationalPattern:
                return PatternInput.Typed;

            // combinations inherit the "worst" requirement
            case BoundUnaryPattern up:
                return GetPatternInputRequirement(up.Pattern);

            case BoundBinaryPattern bp:
                {
                    var l = GetPatternInputRequirement(bp.Left);
                    var r = GetPatternInputRequirement(bp.Right);
                    return (PatternInput)Math.Max((int)l, (int)r);
                }

            // These need object semantics in your implementation (isinst, ITuple, member lookup pipeline, etc.)
            case BoundDeclarationPattern:
            case BoundTuplePattern:
            case BoundPropertyPattern:
            case BoundCasePattern:
                return PatternInput.Object;

            default:
                return PatternInput.Object;
        }
    }

    private void EmitPattern(BoundPattern pattern, ITypeSymbol inputType, Generator? scope = null)
    {
        scope ??= this;

        // Some patterns require an object reference on stack to work (isinst/ITuple/property pipeline, etc.)
        void EnsureObjectOnStack(ref ITypeSymbol curType)
        {
            if (RequiresValueTypeHandling(curType) && curType.TypeKind != TypeKind.Error)
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(curType));

            curType = Compilation.GetSpecialType(SpecialType.System_Object);
        }

        // Spill the scrutinee into a local of its current IL stack type (avoids forcing object boxing)
        IILocal SpillScrutineeToLocal(ITypeSymbol curType)
        {
            var clr = ResolveClrType(curType);
            var loc = ILGenerator.DeclareLocal(clr);
            ILGenerator.Emit(OpCodes.Stloc, loc);
            return loc;
        }

        if (pattern is BoundRelationalPattern relational)
        {
            EmitRelationalPattern(relational, inputType, scope);
            return;
        }

        if (pattern is BoundDiscardPattern)
        {
            ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            return;
        }

        if (pattern is BoundConstantPattern constantPattern)
        {
            EmitConstantPattern(constantPattern, inputType);
            return;
        }

        if (pattern is BoundDeclarationPattern declarationPattern)
        {
            var typeSymbol = declarationPattern.Type;
            var clrType = ResolveClrType(typeSymbol);

            // Fast path: if the scrutinee is already exactly the declared type, bind directly
            // without boxing or isinst/unbox.any.
            if (inputType.TypeKind != TypeKind.Error)
            {
                var inputClr = ResolveClrType(inputType);
                inputClr = Generator.InstantiateType(inputClr);
                var declaredClr = Generator.InstantiateType(clrType);

                if (inputClr == declaredClr)
                {
                    var local = EmitDesignation(declarationPattern.Designator, scope);
                    if (local is not null)
                        ILGenerator.Emit(OpCodes.Stloc, local);
                    else
                        ILGenerator.Emit(OpCodes.Pop);

                    ILGenerator.Emit(OpCodes.Ldc_I4_1);
                    return;
                }
            }

            // General case: use object semantics
            EnsureObjectOnStack(ref inputType);

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
                var requiresUnbox = clrType.IsValueType || (clrType.IsGenericParameter && !isReferencePattern);

                ILGenerator.Emit(OpCodes.Isinst, clrType);
                ILGenerator.Emit(OpCodes.Dup);
                ILGenerator.Emit(OpCodes.Brtrue, labelSuccess);
                ILGenerator.Emit(OpCodes.Pop);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Br, labelDone);

                ILGenerator.MarkLabel(labelSuccess);
                ILGenerator.Emit(requiresUnbox ? OpCodes.Unbox_Any : OpCodes.Castclass, clrType);

                if (patternLocal is not null)
                    ILGenerator.Emit(OpCodes.Stloc, patternLocal);
                else
                    ILGenerator.Emit(OpCodes.Pop);

                ILGenerator.Emit(OpCodes.Ldc_I4_1);
                ILGenerator.MarkLabel(labelDone);
            }
            else
            {
                ILGenerator.Emit(OpCodes.Isinst, clrType); // cast or null
                if (patternLocal is not null)
                {
                    ILGenerator.Emit(OpCodes.Dup);
                    ILGenerator.Emit(OpCodes.Stloc, patternLocal);
                }
                ILGenerator.Emit(OpCodes.Ldnull);
                ILGenerator.Emit(OpCodes.Cgt_Un); // not-null
            }

            return;
        }

        if (pattern is BoundCasePattern casePattern)
        {
            var unionClrType = Generator.InstantiateType(ResolveClrType(casePattern.CaseSymbol.Union));
            var caseClrType = Generator.InstantiateType(ResolveClrType(casePattern.CaseSymbol));

            // Fast path: if the scrutinee is already the DU value type, avoid boxing/isinst/unbox.any.
            // This is the common case for matching directly over a DU-typed value.
            if (inputType.TypeKind != TypeKind.Error)
            {
                var inputClr = Generator.InstantiateType(ResolveClrType(inputType));
                if (unionClrType.IsValueType && inputClr == unionClrType)
                {
                    var unionLocal2 = ILGenerator.DeclareLocal(unionClrType);
                    var caseLocal2 = ILGenerator.DeclareLocal(caseClrType);

                    var labelFail2 = ILGenerator.DefineLabel();
                    var labelDone2 = ILGenerator.DefineLabel();

                    // stack: <union>
                    ILGenerator.Emit(OpCodes.Stloc, unionLocal2);

                    ILGenerator.Emit(OpCodes.Ldloca, caseLocal2);
                    ILGenerator.Emit(OpCodes.Initobj, caseClrType);

                    ILGenerator.Emit(OpCodes.Ldloca, unionLocal2);
                    ILGenerator.Emit(OpCodes.Ldloca, caseLocal2);
                    ILGenerator.Emit(OpCodes.Call, GetMethodInfo(casePattern.TryGetMethod));
                    ILGenerator.Emit(OpCodes.Brfalse, labelFail2);

                    var parameterCount2 = Math.Min(
                        casePattern.CaseSymbol.ConstructorParameters.Length,
                        casePattern.Arguments.Length);

                    for (var i = 0; i < parameterCount2; i++)
                    {
                        var parameter = casePattern.CaseSymbol.ConstructorParameters[i];
                        var propertyName = GetCasePropertyName(parameter.Name);

                        var propertySymbol = casePattern.CaseSymbol
                            .GetMembers(propertyName)
                            .OfType<IPropertySymbol>()
                            .FirstOrDefault();

                        if (propertySymbol?.GetMethod is null)
                        {
                            ILGenerator.Emit(OpCodes.Br, labelFail2);
                            break;
                        }

                        ILGenerator.Emit(OpCodes.Ldloca, caseLocal2);
                        ILGenerator.Emit(OpCodes.Call, GetMethodInfo(propertySymbol.GetMethod));

                        // IMPORTANT: do not pre-box; nested patterns decide.
                        EmitPattern(casePattern.Arguments[i], propertySymbol.Type, scope);
                        ILGenerator.Emit(OpCodes.Brfalse, labelFail2);
                    }

                    ILGenerator.Emit(OpCodes.Ldc_I4_1);
                    ILGenerator.Emit(OpCodes.Br, labelDone2);

                    ILGenerator.MarkLabel(labelFail2);
                    ILGenerator.Emit(OpCodes.Ldc_I4_0);

                    ILGenerator.MarkLabel(labelDone2);
                    return;
                }
            }

            // Boxed pipeline for case patterns uses Isinst => object input
            EnsureObjectOnStack(ref inputType);

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

                // IMPORTANT: do not pre-box; nested patterns decide.
                EmitPattern(casePattern.Arguments[i], propertySymbol.Type, scope);
                ILGenerator.Emit(OpCodes.Brfalse, labelFail);
            }

            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            ILGenerator.Emit(OpCodes.Br, labelDone);

            ILGenerator.MarkLabel(labelFail);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);

            ILGenerator.MarkLabel(labelDone);
            return;
        }

        if (pattern is BoundUnaryPattern unaryPattern)
        {
            // Preserve scrutinee without forcing object boxing
            var scrutineeLocal = SpillScrutineeToLocal(inputType);

            ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
            EmitPattern(unaryPattern.Pattern, inputType, scope);

            if (unaryPattern.Kind == BoundUnaryPatternKind.Not)
            {
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq);
                return;
            }

            throw new NotSupportedException("Unsupported unary pattern kind");
        }

        if (pattern is BoundBinaryPattern binaryPattern)
        {
            var scrutineeLocal = SpillScrutineeToLocal(inputType);

            var labelFail = ILGenerator.DefineLabel();
            var labelDone = ILGenerator.DefineLabel();

            if (binaryPattern.Kind == BoundPatternKind.And)
            {
                ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
                EmitPattern(binaryPattern.Left, inputType, scope);
                ILGenerator.Emit(OpCodes.Brfalse, labelFail);

                ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
                EmitPattern(binaryPattern.Right, inputType, scope);
                ILGenerator.Emit(OpCodes.Brfalse, labelFail);

                ILGenerator.Emit(OpCodes.Ldc_I4_1);
                ILGenerator.Emit(OpCodes.Br, labelDone);

                ILGenerator.MarkLabel(labelFail);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);

                ILGenerator.MarkLabel(labelDone);
                return;
            }

            if (binaryPattern.Kind == BoundPatternKind.Or)
            {
                var labelTrue = ILGenerator.DefineLabel();

                ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
                EmitPattern(binaryPattern.Left, inputType, scope);
                ILGenerator.Emit(OpCodes.Brtrue, labelTrue);

                ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);
                EmitPattern(binaryPattern.Right, inputType, scope);
                ILGenerator.Emit(OpCodes.Brtrue, labelTrue);

                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Br, labelDone);

                ILGenerator.MarkLabel(labelTrue);
                ILGenerator.Emit(OpCodes.Ldc_I4_1);

                ILGenerator.MarkLabel(labelDone);
                return;
            }

            throw new NotSupportedException("Unsupported binary pattern kind");
        }

        if (pattern is BoundTuplePattern tuplePattern)
        {
            EnsureObjectOnStack(ref inputType);

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
                ILGenerator.Emit(OpCodes.Callvirt, itemGetter); // object

                EmitPattern(tuplePattern.Elements[i], Compilation.GetSpecialType(SpecialType.System_Object), scope);
                ILGenerator.Emit(OpCodes.Brfalse, labelFail);
            }

            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            ILGenerator.Emit(OpCodes.Br, labelDone);

            ILGenerator.MarkLabel(labelFail);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);

            ILGenerator.MarkLabel(labelDone);
            return;
        }

        if (pattern is BoundPropertyPattern propertyPattern)
        {
            EmitPropertyPattern(propertyPattern, inputType, scope);
            return;
        }

        throw new NotSupportedException($"Unsupported pattern");
    }

    private void EmitRelationalPattern(BoundRelationalPattern pattern, ITypeSymbol inputType, Generator scope)
    {
        // Stack on entry: <scrutinee>  (typed if PatternInput.Typed worked correctly)

        // If binder already marked it as error-ish, just evaluate to false safely.
        if (inputType.TypeKind == TypeKind.Error || pattern.Value.Type.TypeKind == TypeKind.Error)
        {
            ILGenerator.Emit(OpCodes.Pop);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            return;
        }

        // Spill scrutinee so we can reuse it (and handle nullable without duplicating stack games)
        var scrutineeClr = ResolveClrType(inputType);
        var scrutineeLocal = ILGenerator.DeclareLocal(scrutineeClr);
        ILGenerator.Emit(OpCodes.Stloc, scrutineeLocal);

        // Nullable<T> path
        if (inputType.IsNullable)
        {
            var nullableClr = scrutineeClr;
            var underlyingType = inputType.GetNullableUnderlyingType();
            var underlyingClr = ResolveClrType(underlyingType);

            var hasValueGetter = nullableClr.GetProperty("HasValue")!.GetGetMethod()!;
            var getValueOrDefault = nullableClr.GetMethod("GetValueOrDefault", Type.EmptyTypes)!;

            var labelFalse = ILGenerator.DefineLabel();
            var labelDone = ILGenerator.DefineLabel();

            // if (!loc.HasValue) -> false
            ILGenerator.Emit(OpCodes.Ldloca_S, scrutineeLocal);
            ILGenerator.Emit(OpCodes.Call, hasValueGetter);
            ILGenerator.Emit(OpCodes.Brfalse, labelFalse);

            // left = loc.GetValueOrDefault()
            ILGenerator.Emit(OpCodes.Ldloca_S, scrutineeLocal);
            ILGenerator.Emit(OpCodes.Call, getValueOrDefault); // underlying T on stack

            // right = constant (emit in underlying type)
            EmitConstantForRelational((BoundConstantPattern)pattern.Value, underlyingType);

            // compare => int
            EmitCompare(underlyingType);

            // apply operator vs 0
            EmitRelationalOperator(pattern.Operator);

            ILGenerator.Emit(OpCodes.Br, labelDone);

            ILGenerator.MarkLabel(labelFalse);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);

            ILGenerator.MarkLabel(labelDone);
            return;
        }

        // Non-nullable path
        ILGenerator.Emit(OpCodes.Ldloc, scrutineeLocal);

        var t = (BoundLiteralExpression)pattern.Value;

        EmitConstantForRelational(new BoundConstantPattern((LiteralTypeSymbol)t.Type), inputType);

        EmitCompare(inputType);

        EmitRelationalOperator(pattern.Operator);
    }

    // ============================================
    // Property patterns
    // ============================================

    private void EmitPropertyPattern(BoundPropertyPattern propertyPattern, ITypeSymbol inputType, Generator scope)
    {
        // Property patterns are object-pipeline in this implementation.
        if (RequiresValueTypeHandling(inputType) && inputType.TypeKind != TypeKind.Error)
            ILGenerator.Emit(OpCodes.Box, ResolveClrType(inputType));

        var objLocal = ILGenerator.DeclareLocal(typeof(object));
        ILGenerator.Emit(OpCodes.Stloc, objLocal);

        var labelFail = ILGenerator.DefineLabel();
        var labelDone = ILGenerator.DefineLabel();

        // Null fails for property patterns
        ILGenerator.Emit(OpCodes.Ldloc, objLocal);
        ILGenerator.Emit(OpCodes.Brfalse, labelFail);

        // Optional narrowed type-test
        if (propertyPattern.NarrowedType is not null)
        {
            var narrowedClrType = ResolveClrType(propertyPattern.NarrowedType);

            ILGenerator.Emit(OpCodes.Ldloc, objLocal);
            ILGenerator.Emit(OpCodes.Isinst, narrowedClrType);
            ILGenerator.Emit(OpCodes.Brfalse, labelFail);
        }

        if (propertyPattern.Properties.Length == 0)
        {
            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            ILGenerator.Emit(OpCodes.Br, labelDone);

            ILGenerator.MarkLabel(labelFail);
            ILGenerator.Emit(OpCodes.Ldc_I4_0);

            ILGenerator.MarkLabel(labelDone);
            return;
        }

        var lookupType = propertyPattern.ReceiverType;
        var lookupClrType = ResolveClrType(lookupType);

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

        foreach (var sub in propertyPattern.Properties)
        {
            ILGenerator.Emit(OpCodes.Ldloc, typedLocal);

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
                    ILGenerator.Emit(OpCodes.Br, labelFail);
                    break;
            }

            // IMPORTANT: do not pre-box; nested pattern decides.
            EmitPattern(sub.Pattern, sub.Type, scope);
            ILGenerator.Emit(OpCodes.Brfalse, labelFail);
        }

        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Br, labelDone);

        ILGenerator.MarkLabel(labelFail);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);

        ILGenerator.MarkLabel(labelDone);
    }

    // ============================================
    // Constant patterns (typed, avoid boxing)
    // ============================================

    private void EmitConstantPattern(BoundConstantPattern constantPattern, ITypeSymbol inputType)
    {
        var literal = constantPattern.LiteralType;
        var value = literal.ConstantValue;

        var scrutineeType = inputType;
        var scrutineeClr = ResolveClrType(scrutineeType);

        // NULL literal
        if (value is null)
        {
            EmitNullConstantPattern(scrutineeType, scrutineeClr);
            return;
        }

        // Nullable<T> value types
        if (scrutineeType.IsNullable)
        {
            EmitNullableValueConstantCompare(scrutineeType, value, literal);
            return;
        }

        // Non-nullable value types: fast typed compare, no boxing
        if (scrutineeType.IsValueType)
        {
            var loc = ILGenerator.DeclareLocal(scrutineeClr);
            ILGenerator.Emit(OpCodes.Stloc, loc);
            ILGenerator.Emit(OpCodes.Ldloc, loc);

            EmitLiteralInTargetType(value, scrutineeType, literal);
            ILGenerator.Emit(OpCodes.Ceq);
            return;
        }

        // Reference types
        EmitReferenceConstantCompare(value, literal);
    }

    private void EmitNullConstantPattern(ITypeSymbol scrutineeType, Type scrutineeClr)
    {
        if (scrutineeType.IsValueType && !scrutineeType.IsNullable)
        {
            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            return;
        }

        if (scrutineeType.IsReferenceType)
        {
            ILGenerator.Emit(OpCodes.Ldnull);
            ILGenerator.Emit(OpCodes.Ceq);
            return;
        }

        var loc = ILGenerator.DeclareLocal(scrutineeClr);
        ILGenerator.Emit(OpCodes.Stloc, loc);

        ILGenerator.Emit(OpCodes.Ldloca_S, loc);
        ILGenerator.Emit(OpCodes.Call, scrutineeClr.GetProperty("HasValue")!.GetGetMethod()!);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Ceq);
    }

    private void EmitReferenceConstantCompare(object value, LiteralTypeSymbol literal)
    {
        var obj = ILGenerator.DeclareLocal(typeof(object));
        ILGenerator.Emit(OpCodes.Stloc, obj);

        var notNull = ILGenerator.DefineLabel();
        var done = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldloc, obj);
        ILGenerator.Emit(OpCodes.Brtrue, notNull);

        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Br, done);

        ILGenerator.MarkLabel(notNull);

        if (value is string s)
        {
            ILGenerator.Emit(OpCodes.Ldloc, obj);
            ILGenerator.Emit(OpCodes.Isinst, typeof(string));
            ILGenerator.Emit(OpCodes.Ldstr, s);

            var opEq = typeof(string).GetMethod("op_Equality", new[] { typeof(string), typeof(string) })!;
            ILGenerator.Emit(OpCodes.Call, opEq);

            ILGenerator.Emit(OpCodes.Br, done);
            ILGenerator.MarkLabel(done);
            return;
        }

        ILGenerator.Emit(OpCodes.Ldloc, obj);
        EmitConstantAsObject(literal, value);

        var equals = typeof(object).GetMethod(nameof(object.Equals), new[] { typeof(object) })!;
        ILGenerator.Emit(OpCodes.Callvirt, equals);

        ILGenerator.MarkLabel(done);
    }

    private void EmitNullableValueConstantCompare(ITypeSymbol nullableType, object value, LiteralTypeSymbol literal)
    {
        var nullableClr = ResolveClrType(nullableType);
        var underlyingType = nullableType.GetNullableUnderlyingType();

        var loc = ILGenerator.DeclareLocal(nullableClr);
        ILGenerator.Emit(OpCodes.Stloc, loc);

        var hasValue = nullableClr.GetProperty("HasValue")!.GetGetMethod()!;
        var getValueOrDefault = nullableClr.GetMethod("GetValueOrDefault", Type.EmptyTypes)!;

        var has = ILGenerator.DefineLabel();
        var done = ILGenerator.DefineLabel();

        ILGenerator.Emit(OpCodes.Ldloca_S, loc);
        ILGenerator.Emit(OpCodes.Call, hasValue);
        ILGenerator.Emit(OpCodes.Brtrue, has);

        ILGenerator.Emit(OpCodes.Ldc_I4_0);
        ILGenerator.Emit(OpCodes.Br, done);

        ILGenerator.MarkLabel(has);

        ILGenerator.Emit(OpCodes.Ldloca_S, loc);
        ILGenerator.Emit(OpCodes.Call, getValueOrDefault); // underlying T

        EmitLiteralInTargetType(value, underlyingType, literal);
        ILGenerator.Emit(OpCodes.Ceq);

        ILGenerator.MarkLabel(done);
    }

    private void EmitLiteralInTargetType(object value, ITypeSymbol targetType, LiteralTypeSymbol literal)
    {
        // Unwrap literal wrapper types
        if (targetType is LiteralTypeSymbol lt)
            targetType = lt.UnderlyingType;

        var litUnderlying = literal.UnderlyingType;

        // Enums compare via underlying integral type
        if (targetType.TypeKind == TypeKind.Enum)
            targetType = ((INamedTypeSymbol)targetType); //.EnumUnderlyingType!;

        if (litUnderlying.TypeKind == TypeKind.Enum)
            litUnderlying = ((INamedTypeSymbol)litUnderlying); //.EnumUnderlyingType!;

        switch (targetType.SpecialType)
        {
            case SpecialType.System_Boolean:
                ILGenerator.Emit((bool)value ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                return;

            case SpecialType.System_Char:
                ILGenerator.Emit(OpCodes.Ldc_I4, (int)Convert.ToChar(value));
                return;

            case SpecialType.System_SByte:
                ILGenerator.Emit(OpCodes.Ldc_I4, (int)Convert.ToSByte(value));
                return;

            case SpecialType.System_Byte:
                ILGenerator.Emit(OpCodes.Ldc_I4, (int)Convert.ToByte(value));
                return;

            case SpecialType.System_Int16:
                ILGenerator.Emit(OpCodes.Ldc_I4, (int)Convert.ToInt16(value));
                return;

            case SpecialType.System_UInt16:
                ILGenerator.Emit(OpCodes.Ldc_I4, (int)Convert.ToUInt16(value));
                return;

            case SpecialType.System_Int32:
                ILGenerator.Emit(OpCodes.Ldc_I4, Convert.ToInt32(value));
                return;

            case SpecialType.System_UInt32:
                unchecked { ILGenerator.Emit(OpCodes.Ldc_I4, (int)Convert.ToUInt32(value)); }
                return;

            case SpecialType.System_Int64:
                ILGenerator.Emit(OpCodes.Ldc_I8, Convert.ToInt64(value));
                return;

            case SpecialType.System_UInt64:
                unchecked { ILGenerator.Emit(OpCodes.Ldc_I8, (long)Convert.ToUInt64(value)); }
                return;

            case SpecialType.System_Single:
                ILGenerator.Emit(OpCodes.Ldc_R4, Convert.ToSingle(value, CultureInfo.InvariantCulture));
                return;

            case SpecialType.System_Double:
                ILGenerator.Emit(OpCodes.Ldc_R8, Convert.ToDouble(value, CultureInfo.InvariantCulture));
                return;

            case SpecialType.System_String:
                ILGenerator.Emit(OpCodes.Ldstr, (string)value);
                return;
        }

        // Fallback: boxed constant (caller must compare via Equals)
        EmitConstantAsObject(literal, value);
    }

    private void EmitConstantAsObject(LiteralTypeSymbol literal, object value)
    {
        switch (value)
        {
            case string s:
                ILGenerator.Emit(OpCodes.Ldstr, s);
                return;

            case char ch:
                ILGenerator.Emit(OpCodes.Ldc_I4, (int)ch);
                ILGenerator.Emit(OpCodes.Conv_U2);
                ILGenerator.Emit(OpCodes.Box, ResolveClrType(literal.UnderlyingType));
                return;

            case bool b:
                ILGenerator.Emit(b ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Box, typeof(bool));
                return;

            default:
                EmitLiteral(value);
                if (literal.UnderlyingType.IsValueType)
                    ILGenerator.Emit(OpCodes.Box, ResolveClrType(literal.UnderlyingType));
                return;
        }
    }

    // ============================================
    // Unboxed DU fast path helpers
    // ============================================

    private bool TryEmitPatternTest_UnboxedValueTypeDU(
        BoundPattern pattern,
        Generator scope,
        IILocal unionLocal,
        Type unionClrType)
    {
        if (pattern is BoundCasePattern cp)
        {
            EmitCasePatternUnboxed(cp, scope, unionLocal, unionClrType);
            return true;
        }

        // Binder may insert a redundant declaration/type pattern when the scrutinee is already
        // statically known to be the DU type (especially for generics). Treat it as always-true
        // and perform the binding without boxing/isinst.
        if (pattern is BoundDeclarationPattern dp)
        {
            var dpClr = ResolveClrType(dp.Type);
            dpClr = Generator.InstantiateType(dpClr);

            // Only handle exact DU type matches; anything else must fall back.
            if (dpClr != unionClrType)
                return false;

            var boundLocal = EmitDesignation(dp.Designator, scope);
            if (boundLocal is not null)
            {
                ILGenerator.Emit(OpCodes.Ldloc, unionLocal);
                ILGenerator.Emit(OpCodes.Stloc, boundLocal);
            }

            ILGenerator.Emit(OpCodes.Ldc_I4_1);
            return true;
        }

        if (pattern is BoundUnaryPattern up && up.Kind == BoundUnaryPatternKind.Not)
        {
            if (!TryEmitPatternTest_UnboxedValueTypeDU(up.Pattern, scope, unionLocal, unionClrType))
                return false;

            ILGenerator.Emit(OpCodes.Ldc_I4_0);
            ILGenerator.Emit(OpCodes.Ceq);
            return true;
        }

        if (pattern is BoundBinaryPattern bp)
        {
            if (bp.Kind == BoundPatternKind.And)
                return TryEmitAnd_UnboxedDU(bp.Left, bp.Right, scope, unionLocal, unionClrType);

            if (bp.Kind == BoundPatternKind.Or)
                return TryEmitOr_UnboxedDU(bp.Left, bp.Right, scope, unionLocal, unionClrType);

            return false;
        }

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
        var caseClrType = Generator.InstantiateType(ResolveClrType(casePattern.CaseSymbol));
        var caseLocal = ILGenerator.DeclareLocal(caseClrType);

        var labelFail = ILGenerator.DefineLabel();
        var labelDone = ILGenerator.DefineLabel();

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

            EmitPattern(casePattern.Arguments[i], propertySymbol.Type, scope);
            ILGenerator.Emit(OpCodes.Brfalse, labelFail);
        }

        ILGenerator.Emit(OpCodes.Ldc_I4_1);
        ILGenerator.Emit(OpCodes.Br, labelDone);

        ILGenerator.MarkLabel(labelFail);
        ILGenerator.Emit(OpCodes.Ldc_I4_0);

        ILGenerator.MarkLabel(labelDone);
    }

    // ============================================
    // Misc helpers (existing in your project)
    // ============================================

    private static bool IsDiscriminatedUnionValueType(ITypeSymbol type)
    {
        if (type is not INamedTypeSymbol named)
            return false;

        if (!named.IsValueType)
            return false;

        // Constructed generic DU instances may not carry DU metadata directly.
        if (named.TryGetDiscriminatedUnion() is not null)
            return true;

        var def = named.OriginalDefinition;
        return def is not null && def.TryGetDiscriminatedUnion() is not null;
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

    private IILocal? EmitDesignation(BoundDesignator designation, Generator scope)
    {
        if (designation is BoundSingleVariableDesignator single)
        {
            var symbol = single.Local;

            var local = ILGenerator.DeclareLocal(ResolveClrType(symbol.Type));
            local.SetLocalSymInfo(single.Local.Name);

            scope.AddLocal(symbol, local);
            return local;
        }

        if (designation is BoundDiscardDesignator)
            return null;

        throw new NotSupportedException("Unsupported designation");
    }

    // Helpers

    private void EmitConstantForRelational(BoundConstantPattern constant, ITypeSymbol targetType)
    {
        var value = constant.ConstantValue; // from LiteralTypeSymbol
        if (value is null)
        {
            // binder should prevent this; be defensive
            ILGenerator.Emit(OpCodes.Ldnull);
            return;
        }

        // Use your existing literal emitter, but ensure it's emitted as the target type.
        // For relational comparisons, the "targetType" should be the scrutinee/member type.
        EmitLiteralInTargetType(value, targetType, constant.LiteralType);
    }

    private void EmitCompare(ITypeSymbol type)
    {
        var clr = Generator.InstantiateType(ResolveClrType(type));

        var rightLocal = ILGenerator.DeclareLocal(clr);
        ILGenerator.Emit(OpCodes.Stloc, rightLocal); // pop right

        var leftLocal = ILGenerator.DeclareLocal(clr);
        ILGenerator.Emit(OpCodes.Stloc, leftLocal); // pop left

        var comparerType = typeof(System.Collections.Generic.Comparer<>).MakeGenericType(clr);
        var defaultGetter = comparerType.GetProperty("Default")!.GetGetMethod()!;
        var compareMethod = comparerType.GetMethod("Compare", new[] { clr, clr })!;

        ILGenerator.Emit(OpCodes.Call, defaultGetter);
        ILGenerator.Emit(OpCodes.Ldloc, leftLocal);
        ILGenerator.Emit(OpCodes.Ldloc, rightLocal);
        ILGenerator.Emit(OpCodes.Callvirt, compareMethod); // int
    }

    private void EmitRelationalOperator(BoundRelationalPatternOperator op)
    {
        // Stack: <compareResult:int>
        ILGenerator.Emit(OpCodes.Ldc_I4_0);

        switch (op)
        {
            case BoundRelationalPatternOperator.LessThan:
                ILGenerator.Emit(OpCodes.Clt);
                break;

            case BoundRelationalPatternOperator.LessThanOrEqual:
                // !(result > 0)
                ILGenerator.Emit(OpCodes.Cgt);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq);
                break;

            case BoundRelationalPatternOperator.GreaterThan:
                ILGenerator.Emit(OpCodes.Cgt);
                break;

            case BoundRelationalPatternOperator.GreaterThanOrEqual:
                // !(result < 0)
                ILGenerator.Emit(OpCodes.Clt);
                ILGenerator.Emit(OpCodes.Ldc_I4_0);
                ILGenerator.Emit(OpCodes.Ceq);
                break;

            default:
                throw new NotSupportedException();
        }
    }
}
