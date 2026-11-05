using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private ImmutableArray<MethodSymbol> _extensionConversionOperators;
    private bool _extensionConversionOperatorsInitialized;
    private readonly ConcurrentDictionary<ConversionCacheKey, Conversion> _conversionCache = new(new ConversionCacheKeyComparer());

    public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined = true)
    {
        if (source is null || destination is null)
            return Conversion.None;

        // Declaration binding can query conversions before source base/interface
        // relationships are complete. Do not retain those provisional answers.
        if (!SourceDeclarationsComplete)
            return ClassifyConversionCore(source, destination, includeUserDefined);

        var key = new ConversionCacheKey(source, destination, includeUserDefined);
        if (_conversionCache.TryGetValue(key, out var cached))
            return cached;

        var conversion = ClassifyConversionCore(source, destination, includeUserDefined);
        return _conversionCache.GetOrAdd(key, conversion);
    }

    internal Conversion ClassifyCheckedConversion(TypeSymbol source, TypeSymbol destination)
        => ClassifyConversionCore(source, destination, isChecked: true);

    private static bool IsSystemNullableDefinition(NamedTypeSymbol named)
        => named.SpecialType == SpecialType.System_Nullable_T
           || (named.Arity == 1
               && named.Name == "Nullable"
               && named.ContainingNamespace?.ToDisplayString() == "System");

    internal static TypeSymbol NormalizeExplicitNullableGeneric(TypeSymbol type)
    {
        if (type is NamedTypeSymbol named &&
            IsSystemNullableDefinition(named) &&
            named.TypeArguments.Length == 1)
        {
            var underlying = named.TypeArguments[0];
            return underlying.IsNullable ? underlying : underlying.GetNullableType();
        }

        return type;
    }

    private Conversion ClassifyConversionCore(TypeSymbol source, TypeSymbol destination, bool includeUserDefined = true, bool isChecked = false)
    {
        if (source is null || destination is null)
            return Conversion.None;

        static TypeSymbol Unalias(TypeSymbol type, ref bool wasAlias)
        {
            while (true)
            {
                if (!type.IsAlias)
                    return type;

                wasAlias = true;

                if (type.UnderlyingSymbol is TypeSymbol aliasTarget)
                {
                    type = aliasTarget;
                    continue;
                }

                return type;
            }
        }

        bool sourceUsedAlias = false;
        source = Unalias(source, ref sourceUsedAlias);

        bool destinationUsedAlias = false;
        destination = Unalias(destination, ref destinationUsedAlias);

        static bool IsNumericType(SpecialType type) => type is SpecialType.System_SByte or SpecialType.System_Byte
            or SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_Int32 or SpecialType.System_UInt32
            or SpecialType.System_Int64 or SpecialType.System_UInt64 or SpecialType.System_Single or SpecialType.System_Double
            or SpecialType.System_Decimal or SpecialType.System_IntPtr or SpecialType.System_UIntPtr;

        if (Options.UseGraphemeChar)
        {
            var from = source.UnwrapLiteralType().SpecialType;
            var to = destination.UnwrapLiteralType().SpecialType;
            if (from != to && ((from == SpecialType.System_Char && IsNumericType(to))
                || (to == SpecialType.System_Char && IsNumericType(from))))
                return Conversion.None;
        }

        source = NormalizeExplicitNullableGeneric(source);
        destination = NormalizeExplicitNullableGeneric(destination);

        // Unconstrained T? retains its annotation after substitution, but T = a
        // value type does not introduce Nullable<T>. Classify that representation
        // as the underlying value in both directions.
        if (source is NullableTypeSymbol { UnderlyingType.IsValueType: true } annotatedSource &&
            annotatedSource.GetNullableAbiProjection() == NullableAbiProjection.AnnotatedUnderlyingType)
            source = annotatedSource.UnderlyingType;
        if (destination is NullableTypeSymbol { UnderlyingType.IsValueType: true } annotatedDestination &&
            annotatedDestination.GetNullableAbiProjection() == NullableAbiProjection.AnnotatedUnderlyingType)
            destination = annotatedDestination.UnderlyingType;

        if (destination is null)
            return Conversion.None;

        static bool TryGetExpressionTreeDelegateType(TypeSymbol type, out NamedTypeSymbol delegateType)
        {
            delegateType = null!;

            static TypeSymbol Unalias(TypeSymbol symbol)
            {
                while (symbol.IsAlias && symbol.UnderlyingSymbol is TypeSymbol underlying)
                    symbol = underlying;

                return symbol;
            }

            type = Unalias(type);
            if (type is NullableTypeSymbol nullable)
                type = nullable.UnderlyingType;

            if (type is not NamedTypeSymbol named)
                return false;

            var definition = (named.OriginalDefinition as NamedTypeSymbol) ?? named;
            if (definition.Arity != 1)
                return false;

            var isExpressionType =
                string.Equals(definition.Name, "Expression", StringComparison.Ordinal) ||
                string.Equals(definition.MetadataName, "Expression`1", StringComparison.Ordinal);
            if (!isExpressionType)
                return false;

            if (named.TypeArguments.Length != 1)
                return false;

            var candidate = Unalias(named.TypeArguments[0]);
            if (candidate is not NamedTypeSymbol candidateDelegate)
                return false;

            if (candidateDelegate.TypeKind != TypeKind.Delegate &&
                candidateDelegate.GetDelegateInvokeMethod() is null)
            {
                return false;
            }

            delegateType = candidateDelegate;
            return true;
        }

        var aliasInvolved = sourceUsedAlias || destinationUsedAlias;

        if (source.SpecialType is SpecialType.System_Unit &&
            destination.SpecialType is SpecialType.System_Void ||
            source.SpecialType is SpecialType.System_Void &&
            destination.SpecialType is SpecialType.System_Unit)
        {
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));
        }

        Conversion Finalize(Conversion conversion)
        {
            if (!conversion.Exists)
                return conversion;

            return conversion.WithAlias(aliasInvolved);
        }

        if (SymbolEqualityComparer.Default.Equals(source, destination))
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));

        if (source is IntersectionTypeSymbol or StructuralUnionTypeSymbol || destination is IntersectionTypeSymbol or StructuralUnionTypeSymbol)
            return Finalize(ClassifyStructuralCompositionConversion(source, destination));

        if (TryGetExpressionTreeDelegateType(destination, out var expressionTreeDelegate) &&
            source is NamedTypeSymbol sourceDelegate &&
            (sourceDelegate.TypeKind == TypeKind.Delegate || sourceDelegate.GetDelegateInvokeMethod() is not null) &&
            SymbolEqualityComparer.Default.Equals(sourceDelegate, expressionTreeDelegate))
        {
            return Finalize(new Conversion(isImplicit: true, isReference: true));
        }

        if (source.ContainsErrorType() || destination.ContainsErrorType())
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));

        if (source is LiteralTypeSymbol litSrc && destination is LiteralTypeSymbol litDest)
            return Equals(litSrc.ConstantValue, litDest.ConstantValue)
                ? Finalize(new Conversion(isImplicit: true, isIdentity: true))
                : Conversion.None;

        if (destination is LiteralTypeSymbol)
            return Conversion.None;

        // Implicit constant numeric conversions (C#-style):
        // An integral constant expression of type int can be implicitly converted to certain smaller integral types
        // (e.g. byte/char) if the value is within the target type's range.
        if (source is LiteralTypeSymbol literalSource)
        {
            // Unwrap literal sources to their underlying numeric type.
            var underlying = literalSource.UnderlyingType;
            if (underlying.SpecialType == SpecialType.System_Int32)
            {
                if (TryGetInt32ConstantValue(literalSource.ConstantValue, out var intValue) &&
                    IsImplicitInt32ConstantConversion(intValue, destination.SpecialType))
                {
                    return Finalize(new Conversion(isImplicit: true, isNumeric: true));
                }
            }
        }

        if (source is NamedTypeSymbol { OriginalDefinition: { } taskSourceDefinition } taskSourceNamed &&
            taskSourceNamed.TypeKind != TypeKind.Error &&
            taskSourceDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            taskSourceNamed.TypeArguments.Length == 1 &&
            taskSourceNamed.TypeArguments[0] is { } taskSourceArgument &&
            taskSourceArgument.SpecialType == SpecialType.System_Unit &&
            destination.SpecialType == SpecialType.System_Threading_Tasks_Task)
        {
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));
        }

        if (destination is NamedTypeSymbol { OriginalDefinition: { } taskDestinationDefinition } taskDestinationNamed &&
            taskDestinationNamed.TypeKind != TypeKind.Error &&
            taskDestinationDefinition.SpecialType == SpecialType.System_Threading_Tasks_Task_T &&
            taskDestinationNamed.TypeArguments.Length == 1 &&
            taskDestinationNamed.TypeArguments[0] is { } taskDestinationArgument &&
            taskDestinationArgument.SpecialType == SpecialType.System_Unit &&
            source.SpecialType == SpecialType.System_Threading_Tasks_Task)
        {
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));
        }

        if (UnionFacts.IsStandardCarrierWidening(source, destination))
            return Finalize(new Conversion(isImplicit: true, isDiscriminatedUnion: true));

        var sourceUnionCase = source.TryGetUnionCase();
        var destinationUnion = destination.TryGetUnion();
        var sourceUnionForCase = ResolveSourceUnionForCase(source, sourceUnionCase);
        if (sourceUnionCase is not null &&
            sourceUnionForCase is not null &&
            destinationUnion is not null &&
            (SymbolEqualityComparer.Default.Equals(sourceUnionForCase, destinationUnion) ||
             (SymbolEqualityComparer.Default.Equals(sourceUnionForCase.OriginalDefinition, destinationUnion.OriginalDefinition) &&
              // For same-family unions, only allow when concrete type args at each position are
              // compatible. A type parameter at either position is freely compatible; two concrete
              // args must be equal. This prevents Ok<int> → Result<(), E> while still allowing
              // Error<E> → Result<T[], E> (type params) and Ok<int> → Result<int, string> (E open).
              ConcreteTypeArgumentsAreCompatible(sourceUnionForCase, (TypeSymbol)destinationUnion))))
        {
            var unionConstructor = FindUnionCarrierConstructor(source, destination);
            if (unionConstructor is not null)
            {
                return Finalize(new Conversion(
                    isImplicit: true,
                    isDiscriminatedUnion: true,
                    isUserDefined: false,
                    methodSymbol: null,
                    constructorSymbol: unionConstructor));
            }
        }

        if (sourceUnionCase is not null &&
            sourceUnionForCase is not null &&
            destinationUnion is not null &&
            SymbolEqualityComparer.Default.Equals(sourceUnionForCase.OriginalDefinition, destinationUnion.OriginalDefinition) &&
            !ConcreteTypeArgumentsAreCompatible(sourceUnionForCase, (TypeSymbol)destinationUnion))
        {
            return Conversion.None;
        }

        if (destinationUnion is NamedTypeSymbol providerUnion &&
            UnionFacts.GetMemberProvider(providerUnion) is not null &&
            (source.TypeKind != TypeKind.Null || providerUnion.IsValueType))
        {
            var factories = UnionFacts.GetProviderFactories(providerUnion);
            var exact = factories.Where(factory => SymbolEqualityComparer.Default.Equals(
                factory.Parameters[0].GetByRefElementType(), source)).ToArray();
            var applicable = exact.Length > 0 ? exact : factories.Where(factory =>
                ClassifyConversion(source, factory.Parameters[0].GetByRefElementType(), includeUserDefined: false)
                    is { Exists: true, IsImplicit: true }).ToArray();
            if (applicable.Length == 1)
                return Finalize(new Conversion(isImplicit: true, isDiscriminatedUnion: true, methodSymbol: applicable[0]));
        }

        if (source.TypeKind != TypeKind.Null &&
            destinationUnion is not null &&
            (sourceUnionCase is null || sourceUnionForCase is null))
        {
            var fallbackUnionConstructor = FindUnionCarrierConstructor(source, destination);
            if (fallbackUnionConstructor is not null)
            {
                return Finalize(new Conversion(
                    isImplicit: true,
                    isDiscriminatedUnion: true,
                    isUserDefined: false,
                    methodSymbol: null,
                    constructorSymbol: fallbackUnionConstructor));
            }
        }

        if (source is NamedTypeSymbol sourceNamedUnion &&
            sourceNamedUnion.TryGetUnion() is not null &&
            destination is TypeSymbol destinationMemberType &&
            FindUnionTryGetValueMethod(sourceNamedUnion, destinationMemberType) is not null)
        {
            return Finalize(new Conversion(
                isImplicit: false,
                isDiscriminatedUnion: true,
                isUserDefined: false,
                methodSymbol: null,
                constructorSymbol: null));
        }

        bool ElementTypesAreCompatible(TypeSymbol sourceElement, TypeSymbol destinationElement)
        {
            bool sourceElementUsedAlias = false;
            var unaliasedSource = Unalias(sourceElement, ref sourceElementUsedAlias);

            bool destinationElementUsedAlias = false;
            var unaliasedDestination = Unalias(destinationElement, ref destinationElementUsedAlias);

            if (!unaliasedSource.MetadataIdentityEquals(unaliasedDestination))
                return false;

            if (sourceElementUsedAlias || destinationElementUsedAlias)
                aliasInvolved = true;

            return true;
        }

        // A native address may be reinterpreted explicitly without changing its bits.
        // Erasing the pointee to void is the one implicit non-identity pointer conversion.
        if (source is PointerTypeSymbol && destination is PointerTypeSymbol pointerTarget)
            return Finalize(new Conversion(
                isImplicit: pointerTarget.PointedAtType.SpecialType == SpecialType.System_Void,
                isPointer: true));

        if (source is AddressTypeSymbol addressSource)
        {
            if (destination is RefTypeSymbol refTypeDestination &&
                ElementTypesAreCompatible(addressSource.ReferencedType, refTypeDestination.ElementType))
            {
                return Finalize(new Conversion(isImplicit: true));
            }

            if (destination is PointerTypeSymbol addressPointerDestination &&
                ElementTypesAreCompatible(addressSource.ReferencedType, addressPointerDestination.PointedAtType))
            {
                return Finalize(new Conversion(isImplicit: true, isPointer: true));
            }
        }

        if (source is RefTypeSymbol refTypeSource &&
            destination is PointerTypeSymbol pointerDestination &&
            ElementTypesAreCompatible(refTypeSource.ElementType, pointerDestination.PointedAtType))
        {
            return Finalize(new Conversion(isImplicit: true, isPointer: true));
        }

        if (source is ArrayTypeSymbol sourceArray && destination is ArrayTypeSymbol destinationArray)
        {
            if (sourceArray.Rank != destinationArray.Rank)
            {
                return Conversion.None;
            }

            var elementConversionIsIdentity = false;
            if (sourceArray.ElementType.IsValueType || destinationArray.ElementType.IsValueType)
            {
                if (!ElementTypesAreCompatible(sourceArray.ElementType, destinationArray.ElementType))
                    return Conversion.None;

                elementConversionIsIdentity = true;
            }
            else
            {
                var elementConversion = ClassifyConversion(
                    sourceArray.ElementType,
                    destinationArray.ElementType,
                    includeUserDefined: false);
                if (!elementConversion.IsImplicit ||
                    !(elementConversion.IsIdentity || elementConversion.IsReference))
                {
                    return Conversion.None;
                }

                elementConversionIsIdentity = elementConversion.IsIdentity;
            }

            if (!Options.AllowArrayCovariance && !elementConversionIsIdentity)
                return Conversion.None;

            if (sourceArray.FixedLength == destinationArray.FixedLength)
                return Finalize(new Conversion(
                    isImplicit: true,
                    isIdentity: elementConversionIsIdentity,
                    isReference: !elementConversionIsIdentity));

            if (sourceArray.FixedLength is not null && destinationArray.FixedLength is null)
                return Finalize(new Conversion(isImplicit: true, isReference: true));

            return Conversion.None;
        }

        if (source.MetadataIdentityEquals(destination) &&
            !Conversion.IsNullable(source) &&
            !Conversion.IsNullable(destination))
        {
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));
        }

        if (source.TypeKind == TypeKind.Null)
        {
            if (destination.TypeKind == TypeKind.Nullable)
                return Finalize(new Conversion(isImplicit: true, isReference: true));

            return Conversion.None;
        }

        if (source is NullableTypeSymbol nullableSource)
        {
            if (!nullableSource.UnderlyingType.IsValueType)
            {
                if (destination is NullableTypeSymbol nullableReferenceDestination &&
                    !nullableReferenceDestination.UnderlyingType.IsValueType)
                {
                    var conv = ClassifyConversion(nullableSource.UnderlyingType, nullableReferenceDestination.UnderlyingType, includeUserDefined);
                    if (conv.Exists)
                    {
                        return Finalize(new Conversion(
                            isImplicit: conv.IsImplicit,
                            isIdentity: conv.IsIdentity,
                            isNumeric: conv.IsNumeric,
                            isReference: true,
                            isBoxing: conv.IsBoxing,
                            isUnboxing: conv.IsUnboxing,
                            isPointer: conv.IsPointer,
                            isDiscriminatedUnion: conv.IsUnion,
                            isLifted: true,
                            isUserDefined: conv.IsUserDefined,
                            isAlias: conv.IsAlias,
                            methodSymbol: conv.MethodSymbol,
                            constructorSymbol: conv.ConstructorSymbol));
                    }
                }
                else if (nullableSource.UnderlyingType.IsReferenceType && destination.IsReferenceType)
                {
                    // An explicit reference cast may assert a nonnullable target,
                    // while retaining the CLR cast's null-preserving behavior.
                    // Do not let annotation narrowing become an implicit conversion.
                    var referenceConversion = ClassifyConversion(nullableSource.UnderlyingType, destination, includeUserDefined: false);
                    if (referenceConversion.IsIdentity || referenceConversion.IsReference)
                        return Finalize(new Conversion(isImplicit: false, isReference: true, isAlias: referenceConversion.IsAlias));
                }
            }
            else
            {
                if (destination is NullableTypeSymbol nullableDestination && nullableDestination.UnderlyingType.IsValueType)
                {
                    var conv2 = ClassifyConversion(nullableSource.UnderlyingType, nullableDestination.UnderlyingType, includeUserDefined);
                    if (conv2.Exists)
                    {
                        return Finalize(new Conversion(
                            isImplicit: conv2.IsImplicit,
                            isIdentity: conv2.IsIdentity,
                            isNumeric: conv2.IsNumeric,
                            isReference: conv2.IsReference,
                            isBoxing: conv2.IsBoxing,
                            isUnboxing: conv2.IsUnboxing,
                            isUserDefined: conv2.IsUserDefined,
                            isAlias: conv2.IsAlias,
                            isLifted: true,
                            methodSymbol: conv2.MethodSymbol));
                    }
                }

                var conv3 = ClassifyConversion(nullableSource.UnderlyingType, destination, includeUserDefined);
                if (conv3.Exists)
                {
                    var isImplicit = !destination.IsValueType && conv3.IsImplicit;
                    return Finalize(new Conversion(
                        isImplicit: isImplicit,
                        isIdentity: conv3.IsIdentity,
                        isNumeric: conv3.IsNumeric,
                        isReference: conv3.IsReference,
                        isBoxing: conv3.IsBoxing,
                        isUnboxing: conv3.IsUnboxing,
                        isUserDefined: conv3.IsUserDefined,
                        isAlias: conv3.IsAlias,
                        isLifted: true,
                        methodSymbol: conv3.MethodSymbol));
                }
            }
        }

        if (destination is NullableTypeSymbol nullableDest)
        {
            if (!nullableDest.UnderlyingType.IsValueType)
            {
                var conv = ClassifyConversion(source, nullableDest.UnderlyingType, includeUserDefined);
                if (conv.Exists)
                    return Finalize(conv);
            }
            else
            {
                var conv = ClassifyConversion(source, nullableDest.UnderlyingType, includeUserDefined);
                if (conv.Exists)
                {
                    return Finalize(new Conversion(
                        isImplicit: true,
                        isIdentity: false,
                        isNumeric: conv.IsNumeric,
                        isReference: conv.IsReference || !source.IsValueType,
                        isBoxing: conv.IsBoxing,
                        isUnboxing: conv.IsUnboxing,
                        isUserDefined: conv.IsUserDefined,
                        isAlias: conv.IsAlias,
                        isLifted: true,
                        methodSymbol: conv.MethodSymbol));
                }
            }
        }

        if (source.SpecialType == SpecialType.System_Void)
            return Conversion.None;

        if (SemanticFacts.MayBeRefLike(source) && destination.IsReferenceType)
            return Conversion.None;

        var objType = GetSpecialType(SpecialType.System_Object);

        if (destination.MetadataIdentityEquals(objType))
        {
            if (source.MetadataIdentityEquals(objType))
            {
                return Conversion.None;
            }

            return Finalize(new Conversion(
                isImplicit: true,
                isReference: !source.IsValueType,
                isBoxing: source.IsValueType));
        }

        if (source is LiteralTypeSymbol litSrc2)
            return Finalize(ClassifyConversion(litSrc2.UnderlyingType, destination, includeUserDefined));

        if (IsReferenceConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: true, isReference: true));
        }

        if (IsExplicitReferenceConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: false, isReference: true));
        }

        if (IsBoxingConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: true, isBoxing: true));
        }

        if (IsUnboxingConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: false, isUnboxing: true));
        }

        // C#-style explicit enum conversions:
        // - enum -> any integral type
        // - any integral type -> enum
        // - enum -> enum
        // Treat them as explicit numeric conversions for ranking and diagnostics.
        if (source is NamedTypeSymbol sourceNamedEnum &&
            sourceNamedEnum.TypeKind == TypeKind.Enum &&
            (IsIntegralType(destination) ||
             destination is NamedTypeSymbol { TypeKind: TypeKind.Enum }))
        {
            return Finalize(new Conversion(isImplicit: false, isNumeric: true));
        }

        if (destination is NamedTypeSymbol destNamedEnum &&
            destNamedEnum.TypeKind == TypeKind.Enum &&
            IsIntegralType(source))
        {
            return Finalize(new Conversion(isImplicit: false, isNumeric: true));
        }

        if (IsImplicitNumericConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: true, isNumeric: true));
        }

        if (IsExplicitNumericConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: false, isNumeric: true));
        }

        if (includeUserDefined &&
            TryClassifyCovariantReadOnlySpanConversion(source, destination, out var covariantReadOnlySpanConversion))
        {
            return Finalize(covariantReadOnlySpanConversion);
        }

        var sourceNamed = source as NamedTypeSymbol;
        var destinationNamed = destination as NamedTypeSymbol;

        if (includeUserDefined && (sourceNamed != null || destinationNamed != null))
        {
            IEnumerable<MethodSymbol> candidateConversions =
                Enumerable.Empty<MethodSymbol>();

            if (sourceNamed != null)
                candidateConversions = candidateConversions.Concat(sourceNamed.GetMembers().OfType<MethodSymbol>());
            if (destinationNamed != null && !source.MetadataIdentityEquals(destination))
                candidateConversions = candidateConversions.Concat(destinationNamed.GetMembers().OfType<MethodSymbol>());

            candidateConversions = candidateConversions.Concat(GetExtensionConversionCandidates(source, destination));

            var applicableConversions = ImmutableArray.CreateBuilder<MethodSymbol>();
            var seenConversions = new HashSet<MethodSymbol>(ReferenceEqualityComparer.Instance);

            foreach (var candidate in candidateConversions)
            {
                // Checked explicit conversions replace only their paired ordinary declaration.
                // Other ordinary and implicit conversions still participate in best-candidate selection.
                var method = isChecked && candidate.Name == "op_Explicit"
                    ? candidate.ContainingType.GetMembers("op_CheckedExplicit").OfType<MethodSymbol>().FirstOrDefault(m =>
                        m.Parameters.Length == 1 && candidate.Parameters.Length == 1 &&
                        SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type, candidate.Parameters[0].Type) &&
                        SymbolEqualityComparer.Default.Equals(m.ReturnType, candidate.ReturnType)) ?? candidate
                    : candidate;
                if (method.MethodKind is not MethodKind.Conversion ||
                    (method.Name is not ("op_Implicit" or "op_Explicit") && !(isChecked && method.Name == "op_CheckedExplicit")) ||
                    method.Parameters.Length != 1)
                {
                    continue;
                }

                if (ContainsUnboundTypeParameter(method.Parameters[0].Type) ||
                    ContainsUnboundTypeParameter(method.ReturnType))
                {
                    continue;
                }

                if (!SatisfiesMethodAndContainingTypeConstraints(method))
                    continue;

                if (source.TypeKind == TypeKind.Null)
                    continue;

                var sourceConversion = ClassifyConversion(source, method.Parameters[0].Type, includeUserDefined: false);
                var sourceConvertible = sourceConversion.Exists && sourceConversion.IsImplicit;

                if (!sourceConvertible && SourceMatchesDiscriminatedUnionCase(source, method.Parameters[0].Type))
                    sourceConvertible = true;

                if (sourceConvertible &&
                    ClassifyConversion(method.ReturnType, destination, includeUserDefined: false) is { Exists: true, IsImplicit: true })
                {
                    if (method.Name == "op_Implicit" &&
                        SymbolEqualityComparer.Default.Equals(source, method.Parameters[0].Type) &&
                        SymbolEqualityComparer.Default.Equals(method.ReturnType, destination))
                    {
                        return Finalize(new Conversion(
                            isImplicit: true,
                            isUserDefined: true,
                            methodSymbol: method));
                    }

                    if (seenConversions.Add(method))
                        applicableConversions.Add(method);
                }
            }

            if (SelectBestUserDefinedConversion(source, destination, applicableConversions) is { } bestConversion)
            {
                var isImplicit = bestConversion.Name == "op_Implicit";
                return Finalize(new Conversion(isImplicit: isImplicit, isUserDefined: true, methodSymbol: bestConversion));
            }
        }

        if (source.TryGetUnionCase() is { } unionCase &&
            !SymbolEqualityComparer.Default.Equals(source, unionCase.Union))
        {
            var unionConversion = ClassifyConversion(unionCase.Union, destination, includeUserDefined);
            if (unionConversion.Exists)
                return unionConversion;
        }

        return Conversion.None;

        static bool TryGetInt32ConstantValue(object? constantValue, out int value)
        {
            switch (constantValue)
            {
                case int i:
                    value = i;
                    return true;

                case byte b:
                    value = b;
                    return true;

                case char c:
                    value = c;
                    return true;

                case sbyte sb:
                    value = sb;
                    return true;

                case short s:
                    value = s;
                    return true;

                case ushort us:
                    value = us;
                    return true;

                default:
                    value = default;
                    return false;
            }
        }

        static bool IsImplicitInt32ConstantConversion(int value, SpecialType destination)
        {
            // Match C#'s implicit constant expression conversions.
            // An int constant can be implicitly converted to smaller integral types when the value fits.
            return destination switch
            {
                SpecialType.System_SByte => value >= sbyte.MinValue && value <= sbyte.MaxValue,
                SpecialType.System_Byte => (uint)value <= byte.MaxValue,
                SpecialType.System_Int16 => value >= short.MinValue && value <= short.MaxValue,
                SpecialType.System_UInt16 => (uint)value <= ushort.MaxValue,
                SpecialType.System_UInt32 => value >= 0,
                SpecialType.System_UInt64 => value >= 0,
                SpecialType.System_Char => (uint)value <= char.MaxValue,
                _ => false
            };
        }

        bool SourceMatchesDiscriminatedUnionCase(TypeSymbol sourceType, TypeSymbol parameterType)
        {
            var sourceCase = sourceType.TryGetUnionCase();
            var parameterUnion = parameterType.TryGetUnion();
            var sourceUnion = ResolveSourceUnionForCase(sourceType, sourceCase);

            if (sourceCase is null || sourceUnion is null || parameterUnion is null)
                return false;

            if (SymbolEqualityComparer.Default.Equals(sourceUnion, parameterUnion))
                return true;

            if (!SymbolEqualityComparer.Default.Equals(sourceUnion.OriginalDefinition, parameterUnion.OriginalDefinition))
                return false;

            return ConcreteTypeArgumentsAreCompatible(sourceUnion, (TypeSymbol)parameterUnion);
        }

        // Returns true when, for each type argument position, at least one side is a type parameter
        // (freely compatible) or both concrete args are equal. This allows Error<E> → Result<T,E>
        // (type params) and Ok<int> → Result<int,string> (E open on source side) while blocking
        // Ok<int> → Result<(),string> (int ≠ () in the same position).
        bool ConcreteTypeArgumentsAreCompatible(TypeSymbol sourceUnion, TypeSymbol destUnion)
        {
            return TypeArgumentsAreCompatible(sourceUnion, destUnion);
        }

        static TypeSymbol? ResolveSourceUnionForCase(TypeSymbol sourceType, UnionCaseTypeSymbol? sourceCase)
        {
            if (sourceCase is null || sourceCase.Union is not NamedTypeSymbol sourceUnion)
                return null;

            if (sourceType is NamedTypeSymbol sourceNamed &&
                TryProjectUnionFromCaseArguments(sourceNamed, sourceCase, out var projectedUnion))
            {
                return projectedUnion;
            }

            return sourceUnion;
        }

        static bool TryProjectUnionFromCaseArguments(
            NamedTypeSymbol caseType,
            UnionCaseTypeSymbol caseSymbol,
            out NamedTypeSymbol? projectedUnion)
        {
            projectedUnion = null;

            if (caseType.TypeArguments.IsDefaultOrEmpty)
                return false;

            var caseDefinition = caseSymbol.OriginalDefinition as UnionCaseTypeSymbol ?? caseSymbol;
            if (caseDefinition is not NamedTypeSymbol caseDefinitionNamed ||
                caseDefinitionNamed.TypeParameters.IsDefaultOrEmpty ||
                caseDefinitionNamed.TypeParameters.Length != caseType.TypeArguments.Length)
            {
                return false;
            }

            var unionDefinition = caseDefinition.Union.OriginalDefinition as NamedTypeSymbol ?? caseDefinition.Union as NamedTypeSymbol;
            if (unionDefinition is null || unionDefinition.TypeParameters.IsDefaultOrEmpty)
                return false;

            var unionTypeArguments = unionDefinition.TypeParameters
                .Select(typeParameter => (TypeSymbol)typeParameter)
                .ToArray();

            var changed = false;

            for (var i = 0; i < caseDefinitionNamed.TypeParameters.Length; i++)
            {
                var caseTypeParameter = caseDefinitionNamed.TypeParameters[i];
                TypeParameterSymbol? unionTypeParameter = null;

                if (caseDefinition is SourceUnionCaseTypeSymbol sourceCaseDefinition &&
                    sourceCaseDefinition.TryGetProjectedUnionTypeParameter(caseTypeParameter, out var mapped))
                {
                    unionTypeParameter = mapped;
                }
                else
                {
                    unionTypeParameter = unionDefinition.TypeParameters
                        .FirstOrDefault(tp => string.Equals(tp.Name, caseTypeParameter.Name, StringComparison.Ordinal));
                }

                if (unionTypeParameter is null)
                    continue;

                var unionIndex = -1;
                for (var unionParameterIndex = 0; unionParameterIndex < unionDefinition.TypeParameters.Length; unionParameterIndex++)
                {
                    if (SymbolEqualityComparer.Default.Equals(unionDefinition.TypeParameters[unionParameterIndex], unionTypeParameter))
                    {
                        unionIndex = unionParameterIndex;
                        break;
                    }
                }

                if (unionIndex < 0 || unionIndex >= unionTypeArguments.Length)
                    continue;

                unionTypeArguments[unionIndex] = caseType.TypeArguments[i];
                changed = true;
            }

            if (!changed)
                return false;

            projectedUnion = (NamedTypeSymbol)unionDefinition.Construct(unionTypeArguments);
            return true;
        }

        static bool ContainsUnboundTypeParameter(TypeSymbol type)
        {
            type = type.GetNonNullableType();

            return type switch
            {
                TypeParameterSymbol => true,
                ArrayTypeSymbol array => ContainsUnboundTypeParameter(array.ElementType),
                NamedTypeSymbol named when !named.TypeArguments.IsDefaultOrEmpty =>
                    named.TypeArguments.Any(ContainsUnboundTypeParameter),
                NamedTypeSymbol => false,
                _ => false
            };
        }

        static bool SatisfiesMethodAndContainingTypeConstraints(MethodSymbol method)
        {
            var methodDefinition = method.OriginalDefinition ?? method;
            var methodTypeParameters = methodDefinition.TypeParameters;

            if (!methodTypeParameters.IsDefaultOrEmpty && methodTypeParameters.Length > 0)
            {
                var methodTypeArguments = method.TypeArguments;
                if (methodTypeArguments.IsDefaultOrEmpty || methodTypeArguments.Length != methodTypeParameters.Length)
                    return false;

                for (int i = 0; i < methodTypeParameters.Length; i++)
                {
                    var argument = methodTypeArguments[i];
                    if (argument is TypeParameterSymbol || !argument.SatisfiesConstraints(methodTypeParameters[i]))
                        return false;
                }
            }

            if (method.ContainingType is not NamedTypeSymbol containingType)
                return true;

            var containerDefinition = containingType.ConstructedFrom as NamedTypeSymbol ?? containingType;
            var containerTypeParameters = containerDefinition.TypeParameters;
            if (containerTypeParameters.IsDefaultOrEmpty || containerTypeParameters.Length == 0)
                return true;

            var containerTypeArguments = containingType.TypeArguments;
            if (containerTypeArguments.IsDefaultOrEmpty || containerTypeArguments.Length != containerTypeParameters.Length)
                return false;

            for (int i = 0; i < containerTypeParameters.Length; i++)
            {
                var argument = containerTypeArguments[i];
                if (argument is TypeParameterSymbol || !argument.SatisfiesConstraints(containerTypeParameters[i]))
                    return false;
            }

            return true;
        }
    }

    private MethodSymbol? SelectBestUserDefinedConversion(
        TypeSymbol source,
        TypeSymbol destination,
        ImmutableArray<MethodSymbol>.Builder applicableConversions)
    {
        if (applicableConversions.Count == 0)
            return null;

        var implicitConversions = applicableConversions
            .Where(static method => method.Name == "op_Implicit")
            .ToImmutableArray();
        var candidates = implicitConversions.IsEmpty
            ? applicableConversions.ToImmutable()
            : implicitConversions;

        if (candidates.Length == 1)
            return candidates[0];

        MethodSymbol? best = null;
        foreach (var candidate in candidates)
        {
            var dominatesEveryOtherCandidate = true;
            foreach (var other in candidates)
            {
                if (ReferenceEquals(candidate, other) ||
                    SymbolEqualityComparer.Default.Equals(candidate, other))
                {
                    continue;
                }

                if (!IsBetterUserDefinedConversion(candidate, other, source, destination))
                {
                    dominatesEveryOtherCandidate = false;
                    break;
                }
            }

            if (!dominatesEveryOtherCandidate)
                continue;

            if (best is not null && !SymbolEqualityComparer.Default.Equals(best, candidate))
                return null;

            best = candidate;
        }

        return best;
    }

    private bool IsBetterUserDefinedConversion(
        MethodSymbol candidate,
        MethodSymbol other,
        TypeSymbol source,
        TypeSymbol destination)
    {
        var sourceComparison = CompareUserDefinedSourceTypes(
            candidate.Parameters[0].Type,
            other.Parameters[0].Type,
            source);
        var targetComparison = CompareUserDefinedTargetTypes(
            candidate.ReturnType,
            other.ReturnType,
            destination);

        return sourceComparison <= 0 &&
            targetComparison <= 0 &&
            (sourceComparison < 0 || targetComparison < 0);
    }

    private int CompareUserDefinedSourceTypes(
        TypeSymbol candidate,
        TypeSymbol other,
        TypeSymbol source)
    {
        if (SymbolEqualityComparer.Default.Equals(candidate, other))
            return 0;
        if (SymbolEqualityComparer.Default.Equals(candidate, source))
            return -1;
        if (SymbolEqualityComparer.Default.Equals(other, source))
            return 1;

        var candidateToOther = ClassifyConversion(candidate, other, includeUserDefined: false).IsImplicit;
        var otherToCandidate = ClassifyConversion(other, candidate, includeUserDefined: false).IsImplicit;
        if (candidateToOther != otherToCandidate)
            return candidateToOther ? -1 : 1;

        return 0;
    }

    private int CompareUserDefinedTargetTypes(
        TypeSymbol candidate,
        TypeSymbol other,
        TypeSymbol destination)
    {
        if (SymbolEqualityComparer.Default.Equals(candidate, other))
            return 0;
        if (SymbolEqualityComparer.Default.Equals(candidate, destination))
            return -1;
        if (SymbolEqualityComparer.Default.Equals(other, destination))
            return 1;

        var candidateToOther = ClassifyConversion(candidate, other, includeUserDefined: false).IsImplicit;
        var otherToCandidate = ClassifyConversion(other, candidate, includeUserDefined: false).IsImplicit;
        if (candidateToOther != otherToCandidate)
            return otherToCandidate ? -1 : 1;

        return 0;
    }

    private bool TryClassifyCovariantReadOnlySpanConversion(
        TypeSymbol source,
        TypeSymbol destination,
        out Conversion conversion)
    {
        conversion = Conversion.None;

        if (!TryGetReadOnlySpanElementType(destination, out var destinationElementType) ||
            !TryGetSpanConversionSourceElementType(source, out var sourceElementType) ||
            SymbolEqualityComparer.Default.Equals(sourceElementType, destinationElementType))
        {
            return false;
        }

        var elementConversion = ClassifyConversion(
            sourceElementType,
            destinationElementType,
            includeUserDefined: false);

        if (!elementConversion.IsImplicit || !elementConversion.IsReference)
            return false;

        var destinationNamed = (NamedTypeSymbol)destination;
        var castUp = destinationNamed
            .GetMembers("CastUp")
            .OfType<MethodSymbol>()
            .FirstOrDefault(static method =>
                method.IsStatic &&
                method.Arity == 1 &&
                method.Parameters.Length == 1);

        if (castUp is null)
            return false;

        var constructedCastUp = castUp.Construct(sourceElementType);
        conversion = new Conversion(
            isImplicit: true,
            isUserDefined: true,
            methodSymbol: constructedCastUp);
        return true;
    }

    private static bool TryGetReadOnlySpanElementType(
        TypeSymbol type,
        out TypeSymbol elementType)
    {
        elementType = null!;

        if (type is not NamedTypeSymbol named ||
            named.TypeArguments.Length != 1 ||
            named.Name != "ReadOnlySpan" ||
            named.ContainingNamespace?.ToDisplayString() != "System")
        {
            return false;
        }

        elementType = named.TypeArguments[0];
        return true;
    }

    private static bool TryGetSpanConversionSourceElementType(
        TypeSymbol type,
        out TypeSymbol elementType)
    {
        if (type is ArrayTypeSymbol { Rank: 1 } array)
        {
            elementType = array.ElementType;
            return true;
        }

        elementType = null!;
        if (type is not NamedTypeSymbol named ||
            named.TypeArguments.Length != 1 ||
            named.ContainingNamespace?.ToDisplayString() != "System" ||
            named.Name is not ("Span" or "ReadOnlySpan"))
        {
            return false;
        }

        elementType = named.TypeArguments[0];
        return true;
    }

    private readonly record struct ConversionCacheKey(
        TypeSymbol Source,
        TypeSymbol Destination,
        bool IncludeUserDefined);

    private sealed class ConversionCacheKeyComparer : IEqualityComparer<ConversionCacheKey>
    {
        public bool Equals(ConversionCacheKey x, ConversionCacheKey y)
        {
            return x.IncludeUserDefined == y.IncludeUserDefined &&
                x.Source.GetNullableAbiProjection() == y.Source.GetNullableAbiProjection() &&
                x.Destination.GetNullableAbiProjection() == y.Destination.GetNullableAbiProjection() &&
                SymbolEqualityComparer.Default.Equals(x.Source, y.Source) &&
                SymbolEqualityComparer.Default.Equals(x.Destination, y.Destination);
        }

        public int GetHashCode(ConversionCacheKey obj)
        {
            return HashCode.Combine(
                SymbolEqualityComparer.Default.GetHashCode(obj.Source),
                SymbolEqualityComparer.Default.GetHashCode(obj.Destination),
                obj.IncludeUserDefined,
                obj.Source.GetNullableAbiProjection(),
                obj.Destination.GetNullableAbiProjection());
        }
    }

    private IEnumerable<MethodSymbol> GetExtensionConversionCandidates(TypeSymbol source, TypeSymbol destination)
    {
        var extensionOperators = GetExtensionConversionOperators();
        if (extensionOperators.IsDefaultOrEmpty)
            yield break;

        var seen = new HashSet<MethodSymbol>(SymbolEqualityComparer.Default);
        var receiverTypes = ImmutableArray.CreateBuilder<TypeSymbol>(4);
        receiverTypes.Add(source);
        receiverTypes.Add(destination);

        if (source.TryGetUnionCase() is { } sourceUnionCase)
            receiverTypes.Add(sourceUnionCase.Union);
        if (destination.TryGetUnionCase() is { } destinationUnionCase)
            receiverTypes.Add(destinationUnionCase.Union);

        foreach (var method in extensionOperators)
        {
            MethodSymbol? constructed = null;

            foreach (var receiverType in receiverTypes)
            {
                constructed = TryConstructExtensionConversion(method, receiverType);
                if (constructed is not null)
                    break;
            }
            if (constructed is null && !IsOpenGenericConversionMethod(method))
                constructed = method;

            if (constructed is not null && seen.Add(constructed))
                yield return constructed;
        }

        static bool IsOpenGenericConversionMethod(MethodSymbol method)
        {
            if (!method.TypeParameters.IsDefaultOrEmpty && method.TypeParameters.Length > 0)
                return true;

            if (method.ContainingType is not NamedTypeSymbol containingType || !containingType.IsGenericType)
                return false;

            var typeArguments = containingType.TypeArguments;
            if (!typeArguments.IsDefaultOrEmpty && typeArguments.Length > 0)
                return typeArguments.Any(static argument => argument is TypeParameterSymbol);

            return !containingType.TypeParameters.IsDefaultOrEmpty && containingType.TypeParameters.Length > 0;
        }
    }

    private ImmutableArray<MethodSymbol> GetExtensionConversionOperators()
    {
        if (_extensionConversionOperatorsInitialized)
            return _extensionConversionOperators;

        _extensionConversionOperatorsInitialized = true;

        var builder = ImmutableArray.CreateBuilder<MethodSymbol>();
        var members = SymbolLookup.GetExtensionConversionContainers();

        foreach (var type in members)
        {
            foreach (var member in type.GetMembers("op_Implicit").Concat(type.GetMembers("op_Explicit")))
            {
                if (member is not MethodSymbol method)
                    continue;

                if (method.MethodKind is not MethodKind.Conversion)
                    continue;

                if (!TryGetExtensionConversionReceiverType(method, out _))
                    continue;

                builder.Add(method);
            }
        }

        _extensionConversionOperators = builder.ToImmutable();
        return _extensionConversionOperators;

        static bool TryGetExtensionConversionReceiverType(MethodSymbol method, out TypeSymbol? receiverType)
        {
            try
            {
                receiverType = method.GetExtensionReceiverType();
                return receiverType is not null;
            }
            catch (InvalidOperationException)
            {
                receiverType = null;
                return false;
            }
        }
    }

    private MethodSymbol? TryConstructExtensionConversion(MethodSymbol method, TypeSymbol receiverType)
    {
        var extensionReceiverType = method.GetExtensionReceiverType();
        if (extensionReceiverType is null)
            return null;

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return null;

        var methodDefinition = method.OriginalDefinition ?? method;
        var substitutions = new Dictionary<TypeParameterSymbol, TypeSymbol>(SymbolEqualityComparer.Default);

        if (!TryUnifyExtensionReceiver(extensionReceiverType, receiverType, substitutions))
            return null;

        if (!methodDefinition.TypeParameters.IsDefaultOrEmpty && methodDefinition.TypeParameters.Length > 0)
        {
            var methodTypeArguments = new TypeSymbol[methodDefinition.TypeParameters.Length];

            for (int i = 0; i < methodDefinition.TypeParameters.Length; i++)
            {
                var typeParameter = methodDefinition.TypeParameters[i];
                if (!TryGetMethodSubstitution(substitutions, typeParameter, out var typeArgument))
                    return null;

                methodTypeArguments[i] = typeArgument;
            }

            if (!SatisfiesTypeParameterConstraints(methodDefinition.TypeParameters, methodTypeArguments))
                return null;

            if (!SatisfiesContainingTypeConstraints(methodDefinition, substitutions))
                return null;

            return methodDefinition.Construct(methodTypeArguments);
        }

        if (methodDefinition.ContainingType is not NamedTypeSymbol container)
            return null;

        var containerDefinition = container.ConstructedFrom as NamedTypeSymbol ?? container;
        if (!containerDefinition.IsGenericType ||
            containerDefinition.TypeParameters.IsDefaultOrEmpty ||
            containerDefinition.TypeParameters.Length == 0)
        {
            return method;
        }

        var typeArguments = new TypeSymbol[containerDefinition.TypeParameters.Length];

        for (int i = 0; i < containerDefinition.TypeParameters.Length; i++)
        {
            var typeParameter = containerDefinition.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                return null;

            typeArguments[i] = typeArgument;
        }

        if (!SatisfiesTypeParameterConstraints(containerDefinition.TypeParameters, typeArguments))
            return null;

        if (containerDefinition.Construct(typeArguments) is not NamedTypeSymbol constructedContainer)
            return null;

        var originalDefinition = methodDefinition;

        foreach (var candidate in constructedContainer.GetMembers(method.Name).OfType<MethodSymbol>())
        {
            var candidateOriginal = candidate.OriginalDefinition ?? candidate;
            if (SymbolEqualityComparer.Default.Equals(candidateOriginal, originalDefinition))
                return candidate;
        }

        return null;

        static bool TryGetMethodSubstitution(
            Dictionary<TypeParameterSymbol, TypeSymbol> substitutions,
            TypeParameterSymbol methodParameter,
            out TypeSymbol typeArgument)
        {
            if (substitutions.TryGetValue(methodParameter, out typeArgument))
                return true;

            foreach (var (parameter, argument) in substitutions)
            {
                if (string.Equals(parameter.Name, methodParameter.Name, StringComparison.Ordinal) &&
                    parameter.Ordinal == methodParameter.Ordinal)
                {
                    typeArgument = argument;
                    return true;
                }
            }

            typeArgument = null!;
            return false;
        }
    }

    private static bool SatisfiesTypeParameterConstraints(
        ImmutableArray<TypeParameterSymbol> typeParameters,
        TypeSymbol[] typeArguments)
    {
        if (typeParameters.Length != typeArguments.Length)
            return false;

        for (int i = 0; i < typeParameters.Length; i++)
        {
            if (!typeArguments[i].SatisfiesConstraints(typeParameters[i]))
                return false;
        }

        return true;
    }

    private static bool SatisfiesContainingTypeConstraints(
        MethodSymbol methodDefinition,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
    {
        if (methodDefinition.ContainingType is not NamedTypeSymbol containingType)
            return true;

        var containerDefinition = containingType.ConstructedFrom as NamedTypeSymbol ?? containingType;
        if (!containerDefinition.IsGenericType || containerDefinition.TypeParameters.IsDefaultOrEmpty)
            return true;

        var typeArguments = new TypeSymbol[containerDefinition.TypeParameters.Length];

        for (int i = 0; i < containerDefinition.TypeParameters.Length; i++)
        {
            var typeParameter = containerDefinition.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var typeArgument))
                return false;

            typeArguments[i] = typeArgument;
        }

        return SatisfiesTypeParameterConstraints(containerDefinition.TypeParameters, typeArguments);
    }

    private bool TryUnifyExtensionReceiver(
        TypeSymbol parameterType,
        TypeSymbol argumentType,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
    {
        parameterType = NormalizeTypeForExtensionInference(parameterType);
        argumentType = NormalizeTypeForExtensionInference(argumentType);

        if (parameterType is TypeParameterSymbol parameter)
            return TryRecordExtensionSubstitution(parameter, argumentType, substitutions);

        if (parameterType is NamedTypeSymbol paramNamed && argumentType is NamedTypeSymbol argNamed)
        {
            if (TryUnifyNamedType(paramNamed, argNamed, substitutions))
                return true;

            foreach (var iface in argNamed.AllInterfaces)
            {
                if (TryUnifyNamedType(paramNamed, iface, substitutions))
                    return true;
            }

            for (var baseType = argNamed.BaseType; baseType is not null; baseType = baseType.BaseType)
            {
                if (TryUnifyNamedType(paramNamed, baseType, substitutions))
                    return true;
            }

            return false;
        }

        if (parameterType is ArrayTypeSymbol paramArray && argumentType is ArrayTypeSymbol argArray)
            return TryUnifyExtensionReceiver(paramArray.ElementType, argArray.ElementType, substitutions);

        if (parameterType is NullableTypeSymbol paramNullable)
        {
            if (argumentType is NullableTypeSymbol argNullable)
                return TryUnifyExtensionReceiver(paramNullable.UnderlyingType, argNullable.UnderlyingType, substitutions);

            if (!argumentType.IsValueType)
                return TryUnifyExtensionReceiver(paramNullable.UnderlyingType, argumentType, substitutions);

            return false;
        }

        return SymbolEqualityComparer.Default.Equals(parameterType, argumentType);

        bool TryUnifyNamedType(
            NamedTypeSymbol parameterNamed,
            NamedTypeSymbol? argumentNamed,
            Dictionary<TypeParameterSymbol, TypeSymbol> map)
        {
            if (argumentNamed is null)
                return false;

            var parameterDefinition = parameterNamed.OriginalDefinition ?? parameterNamed;
            var argumentDefinition = argumentNamed.OriginalDefinition ?? argumentNamed;

            if (!SymbolEqualityComparer.Default.Equals(parameterDefinition, argumentDefinition))
                return false;

            var parameterArguments = parameterNamed.TypeArguments;
            var argumentArguments = argumentNamed.TypeArguments;

            if (parameterArguments.Length != argumentArguments.Length)
                return false;

            for (int i = 0; i < parameterArguments.Length; i++)
            {
                if (!TryUnifyExtensionReceiver(parameterArguments[i], argumentArguments[i], map))
                    return false;
            }

            return true;
        }
    }

    private bool TryRecordExtensionSubstitution(
        TypeParameterSymbol typeParameter,
        TypeSymbol argumentType,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutions)
    {
        argumentType = NormalizeTypeForExtensionInference(argumentType);

        if (substitutions.TryGetValue(typeParameter, out var existing))
        {
            existing = NormalizeTypeForExtensionInference(existing);

            if (SymbolEqualityComparer.Default.Equals(existing, argumentType))
                return true;

            if (ClassifyConversion(argumentType, existing, includeUserDefined: false).IsImplicit)
                return true;

            if (ClassifyConversion(existing, argumentType, includeUserDefined: false).IsImplicit)
            {
                substitutions[typeParameter] = argumentType;
                return true;
            }

            return false;
        }

        substitutions[typeParameter] = argumentType;
        return true;
    }

    private static TypeSymbol NormalizeTypeForExtensionInference(TypeSymbol type)
    {
        return type switch
        {
            LiteralTypeSymbol literal => literal.UnderlyingType,
            _ => type
        };
    }

    private static MethodSymbol? FindDiscriminatedUnionConversionMethod(TypeSymbol source, TypeSymbol destination)
    {
        if (destination is NamedTypeSymbol destinationNamed)
        {
            var method = FindMatchingConversion(destinationNamed, source, destination);
            if (method is not null)
                return method;
        }

        if (source is NamedTypeSymbol sourceNamed)
        {
            var method = FindMatchingConversion(sourceNamed, source, destination);
            if (method is not null)
                return method;
        }

        return null;

        static MethodSymbol? FindMatchingConversion(
            NamedTypeSymbol owner,
            TypeSymbol sourceType,
            TypeSymbol destinationType)
        {
            foreach (var member in owner.GetMembers("op_Implicit"))
            {
                if (member is not MethodSymbol method)
                    continue;

                if (method.MethodKind is not MethodKind.Conversion)
                    continue;

                if (method.Parameters.Length != 1)
                    continue;

                var parameterType = method.Parameters[0].Type;
                if (parameterType is null || method.ReturnType is null)
                    continue;

                if (!SymbolEqualityComparer.Default.Equals(parameterType, sourceType) &&
                    !SymbolEqualityComparer.Default.Equals(
                        parameterType.OriginalDefinition ?? parameterType,
                        sourceType.OriginalDefinition ?? sourceType))
                    continue;

                if (!SymbolEqualityComparer.Default.Equals(method.ReturnType, destinationType) &&
                    !SymbolEqualityComparer.Default.Equals(
                        method.ReturnType.OriginalDefinition ?? method.ReturnType,
                        destinationType.OriginalDefinition ?? destinationType))
                    continue;

                return method;
            }

            return null;
        }
    }

    private MethodSymbol? FindUnionCarrierConstructor(TypeSymbol source, TypeSymbol destination)
    {
        if (destination is not NamedTypeSymbol destinationNamed)
            return null;

        if (UnionFacts.GetMemberProvider(destinationNamed) is not null)
            return null;

        var sourceCase = source.TryGetUnionCase();
        var sourceDefinition = source.OriginalDefinition ?? source;
        var sourceCaseDefinition = (sourceCase as TypeSymbol)?.OriginalDefinition ?? sourceCase as TypeSymbol;

        foreach (var candidate in destinationNamed.Constructors)
        {
            if (candidate.IsStatic || candidate.Parameters.Length != 1)
                continue;

            var parameterType = candidate.Parameters[0].Type;
            if (SymbolEqualityComparer.Default.Equals(parameterType, source))
                return EnsureConstructedConstructor(candidate, destinationNamed);

            var parameterDefinition = parameterType.OriginalDefinition ?? parameterType;
            if (SymbolEqualityComparer.Default.Equals(parameterDefinition, sourceDefinition) &&
                TypeArgumentsAreCompatible(source, parameterType))
            {
                return EnsureConstructedConstructor(candidate, destinationNamed);
            }

            if (sourceCaseDefinition is not null &&
                SymbolEqualityComparer.Default.Equals(parameterDefinition, sourceCaseDefinition) &&
                TypeArgumentsAreCompatible(source, parameterType))
            {
                return EnsureConstructedConstructor(candidate, destinationNamed);
            }

            var parameterConversion = ClassifyConversion(source, parameterType, includeUserDefined: false);
            if (parameterConversion is { Exists: true, IsImplicit: true })
                return EnsureConstructedConstructor(candidate, destinationNamed);
        }

        return null;

        static MethodSymbol EnsureConstructedConstructor(MethodSymbol constructorSymbol, NamedTypeSymbol targetUnionType)
        {
            if (constructorSymbol is SubstitutedMethodSymbol or ConstructedMethodSymbol)
                return constructorSymbol;

            if (targetUnionType is not ConstructedNamedTypeSymbol constructedType)
                return constructorSymbol;

            if (constructedType.ConstructedFrom is not NamedTypeSymbol constructedDefinition)
                return constructorSymbol;

            if (constructorSymbol.ContainingType is not NamedTypeSymbol constructorContainingType)
                return constructorSymbol;

            var constructorContainingDefinition = constructorContainingType.OriginalDefinition as NamedTypeSymbol ?? constructorContainingType;
            if (!SymbolEqualityComparer.Default.Equals(constructorContainingDefinition, constructedDefinition))
                return constructorSymbol;

            return new SubstitutedMethodSymbol(constructorSymbol, constructedType);
        }
    }

    private static bool TypeArgumentsAreCompatible(TypeSymbol sourceType, TypeSymbol destinationType)
    {
        if (sourceType is not NamedTypeSymbol sourceNamed ||
            destinationType is not NamedTypeSymbol destinationNamed)
        {
            return true;
        }

        var sourceArguments = sourceNamed.TypeArguments;
        var destinationArguments = destinationNamed.TypeArguments;

        if (sourceArguments.Length != destinationArguments.Length)
            return false;

        for (var i = 0; i < sourceArguments.Length; i++)
        {
            if (sourceArguments[i] is TypeParameterSymbol ||
                destinationArguments[i] is TypeParameterSymbol)
            {
                continue;
            }

            if (!SymbolEqualityComparer.Default.Equals(sourceArguments[i], destinationArguments[i]))
                return false;
        }

        return true;
    }

    private static MethodSymbol? FindUnionTryGetValueMethod(NamedTypeSymbol unionType, TypeSymbol memberType)
    {
        foreach (var method in (UnionFacts.GetMemberProvider(unionType) ?? unionType).GetMembers("TryGetValue").OfType<MethodSymbol>())
        {
            if (method.IsStatic || method.DeclaredAccessibility != Accessibility.Public ||
                method.ReturnType.SpecialType != SpecialType.System_Boolean || method.Parameters.Length != 1)
                continue;

            var parameter = method.Parameters[0];
            if (parameter.RefKind != RefKind.Out)
                continue;

            if (SymbolEqualityComparer.Default.Equals(parameter.GetByRefElementType(), memberType))
                return method;

            var parameterDefinition = parameter.GetByRefElementType().OriginalDefinition ?? parameter.GetByRefElementType();
            var memberDefinition = memberType.OriginalDefinition ?? memberType;
            if (SymbolEqualityComparer.Default.Equals(parameterDefinition, memberDefinition))
                return method;
        }

        return null;
    }

    private bool IsReferenceConversion(TypeSymbol source, TypeSymbol destination)
    {
        if (source.IsValueType || destination.IsValueType)
            return false;

        if (source is NullableTypeSymbol { UnderlyingType.IsValueType: false } &&
            destination is not NullableTypeSymbol)
        {
            return false;
        }

        if (source.MetadataIdentityEquals(destination))
            return false;

        if (source is NamedTypeSymbol { TypeKind: TypeKind.Delegate } sourceDelegate &&
            destination is NamedTypeSymbol { TypeKind: TypeKind.Delegate } destinationDelegate &&
            sourceDelegate.OriginalDefinition is NamedTypeSymbol delegateDefinition &&
            delegateDefinition.MetadataIdentityEquals(destinationDelegate.OriginalDefinition))
        {
            var parameters = delegateDefinition.TypeParameters;
            for (var i = 0; i < parameters.Length; i++)
            {
                var from = sourceDelegate.TypeArguments[i];
                var to = destinationDelegate.TypeArguments[i];
                if (SymbolEqualityComparer.Default.Equals(from, to))
                    continue;

                if (parameters[i].Variance == VarianceKind.None || from.IsValueType || to.IsValueType)
                    return false;

                var conversion = parameters[i].Variance == VarianceKind.Out
                    ? ClassifyConversion(from, to, includeUserDefined: false)
                    : ClassifyConversion(to, from, includeUserDefined: false);
                if (!conversion.IsImplicit || !(conversion.IsIdentity || conversion.IsReference))
                    return false;
            }

            return true;
        }

        if (source is TypeParameterSymbol sourceParameter)
        {
            var pending = new Stack<TypeParameterSymbol>();
            var visited = new HashSet<TypeParameterSymbol>(SymbolEqualityComparer.Default);
            pending.Push(sourceParameter);
            while (pending.TryPop(out var parameter))
            {
                if (!visited.Add(parameter))
                    continue;
                foreach (var constraint in parameter.ConstraintTypes)
                {
                    if (constraint is TypeParameterSymbol nextParameter)
                    {
                        pending.Push(nextParameter);
                    }
                    else if (constraint is NamedTypeSymbol { TypeKind: TypeKind.Class } classConstraint &&
                        (SymbolEqualityComparer.Default.Equals(classConstraint, destination) ||
                         IsReferenceConversion(classConstraint, destination)))
                    {
                        return true;
                    }
                }
            }
        }

        var current = source.BaseType;
        while (current is not null)
        {
            if (current.MetadataIdentityEquals(destination))
                return true;

            current = current.BaseType;
        }

        if (destination is NamedTypeSymbol destinationNamed &&
            destinationNamed.TypeKind == TypeKind.Interface)
        {
            if (SemanticFacts.ImplementsInterface(source, destinationNamed, SymbolEqualityComparer.Default))
                return true;
        }

        if (source.TypeKind == TypeKind.Interface && destination.SpecialType is SpecialType.System_Object)
            return true;

        return false;
    }

    private bool IsExplicitReferenceConversion(TypeSymbol source, TypeSymbol destination)
    {
        if (source.IsValueType || destination.IsValueType)
            return false;

        if (source.MetadataIdentityEquals(destination))
            return false;

        var comparer = SymbolEqualityComparer.Default;

        if (SemanticFacts.IsDerivedFrom(destination, source, comparer))
            return true;

        if (source.SpecialType is SpecialType.System_Object && !destination.IsValueType)
            return true;

        if (source is NamedTypeSymbol sourceInterface && sourceInterface.TypeKind == TypeKind.Interface)
        {
            if (destination.SpecialType is SpecialType.System_Object)
                return true;

            if (destination is NamedTypeSymbol destinationNamed &&
                destinationNamed.TypeKind != TypeKind.Interface &&
                (destinationNamed.TypeKind == TypeKind.Class && !destinationNamed.IsClosed ||
                 SemanticFacts.ImplementsInterface(destinationNamed, sourceInterface, comparer)))
            {
                return true;
            }

            // One runtime object may implement unrelated interfaces. Their conversion
            // is explicit unless an implicit interface relationship was found earlier.
            if (destination.TypeKind == TypeKind.Interface)
            {
                return true;
            }
        }

        if (destination is NamedTypeSymbol targetInterface && targetInterface.TypeKind == TypeKind.Interface &&
            (source is NamedTypeSymbol { TypeKind: TypeKind.Class, IsClosed: false } ||
             SemanticFacts.ImplementsInterface(source, targetInterface, comparer)))
        {
            return true;
        }

        return false;
    }

    private bool IsBoxingConversion(TypeSymbol source, TypeSymbol destination)
    {
        if (Options.TargetPlatform == TargetPlatform.NeoCLR && source is FunctionReturnTypeSymbol &&
            destination.SpecialType == SpecialType.System_Object) return true;
        if (!source.IsValueType)
            return false;

        if (destination.SpecialType is SpecialType.System_Object or SpecialType.System_ValueType)
            return true;

        if (source.TypeKind == TypeKind.Enum && destination.SpecialType == SpecialType.System_Enum)
            return true;

        return destination is NamedTypeSymbol { TypeKind: TypeKind.Interface } targetInterface &&
               SemanticFacts.ImplementsInterface(source, targetInterface, SymbolEqualityComparer.Default);
    }

    private bool IsUnboxingConversion(TypeSymbol source, TypeSymbol destination)
    {
        if (Options.TargetPlatform == TargetPlatform.NeoCLR && destination is FunctionReturnTypeSymbol &&
            source.GetNonNullableType().SpecialType == SpecialType.System_Object) return true;
        if (!destination.IsValueType)
            return false;

        if (source.SpecialType is SpecialType.System_Object or SpecialType.System_ValueType)
            return true;

        if (destination.TypeKind == TypeKind.Enum && source.SpecialType == SpecialType.System_Enum)
            return true;

        return source is NamedTypeSymbol { TypeKind: TypeKind.Interface } sourceInterface &&
               SemanticFacts.ImplementsInterface(destination, sourceInterface, SymbolEqualityComparer.Default);
    }

    private bool IsImplicitNumericConversion(TypeSymbol source, TypeSymbol destination)
    {
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        // Native integer conversions use platform-independent ranges: native storage
        // is at least 32 bits, but is not assumed to be 64 bits.
        if (destType == SpecialType.System_IntPtr)
            return sourceType is SpecialType.System_SByte or SpecialType.System_Byte
                or SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_Int32 or SpecialType.System_Char;
        if (destType == SpecialType.System_UIntPtr)
            return sourceType is SpecialType.System_Byte or SpecialType.System_UInt16 or SpecialType.System_UInt32 or SpecialType.System_Char;
        if (sourceType == SpecialType.System_IntPtr && destType == SpecialType.System_Int64 ||
            sourceType == SpecialType.System_UIntPtr && destType == SpecialType.System_UInt64)
            return true;

        // Fixed-width numeric conversions follow C# 10.2.3.
        return sourceType switch
        {
            SpecialType.System_SByte => destType is
                SpecialType.System_Int16 or
                SpecialType.System_Int32 or
                SpecialType.System_Int64 or
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal,
            SpecialType.System_Byte => destType is
                SpecialType.System_Int16 or
                SpecialType.System_UInt16 or
                SpecialType.System_Int32 or
                SpecialType.System_UInt32 or
                SpecialType.System_Int64 or
                SpecialType.System_UInt64 or
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal,
            SpecialType.System_Int16 => destType is
                SpecialType.System_Int32 or
                SpecialType.System_Int64 or
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal,
            SpecialType.System_UInt16 => destType is
                SpecialType.System_Int32 or
                SpecialType.System_UInt32 or
                SpecialType.System_Int64 or
                SpecialType.System_UInt64 or
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal,
            SpecialType.System_Int32 => destType is
                SpecialType.System_Int64 or
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal,
            SpecialType.System_UInt32 => destType is
                SpecialType.System_Int64 or
                SpecialType.System_UInt64 or
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal,
            SpecialType.System_Int64 => destType is
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal,
            SpecialType.System_UInt64 => destType is
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal,
            SpecialType.System_Char => destType is
                SpecialType.System_UInt16 or
                SpecialType.System_Int32 or
                SpecialType.System_UInt32 or
                SpecialType.System_Int64 or
                SpecialType.System_UInt64 or
                SpecialType.System_Single or
                SpecialType.System_Double or
                SpecialType.System_Decimal,
            SpecialType.System_Single => destType is
                SpecialType.System_Double,
            _ => false
        };
    }

    private static bool IsIntegralType(TypeSymbol type)
        => type.SpecialType is
            SpecialType.System_SByte or
            SpecialType.System_Byte or
            SpecialType.System_Int16 or
            SpecialType.System_UInt16 or
            SpecialType.System_Int32 or
            SpecialType.System_UInt32 or
            SpecialType.System_Int64 or
            SpecialType.System_UInt64 or
            SpecialType.System_Char;

    private bool IsExplicitNumericConversion(TypeSymbol source, TypeSymbol destination)
    {
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        // Native integer casts are built-in for integral operands; this also works
        // with target libraries that do not declare nominal conversion operators.
        if ((sourceType is SpecialType.System_IntPtr or SpecialType.System_UIntPtr &&
                (IsIntegralType(destination) || destType is SpecialType.System_IntPtr or SpecialType.System_UIntPtr)) ||
            (destType is SpecialType.System_IntPtr or SpecialType.System_UIntPtr && IsIntegralType(source)))
            return true;

        // Every fixed-width integral pair has an explicit conversion when no
        // implicit conversion was selected above.
        if (IsIntegralType(source) && IsIntegralType(destination))
            return true;

        if (sourceType is SpecialType.System_Single or SpecialType.System_Double or SpecialType.System_Decimal &&
            IsIntegralType(destination))
            return true;

        return (sourceType, destType) switch
        {
            // -------- to byte --------
            (SpecialType.System_Int32, SpecialType.System_Byte) => true,
            (SpecialType.System_Int64, SpecialType.System_Byte) => true,
            (SpecialType.System_Char, SpecialType.System_Byte) => true,
            (SpecialType.System_Single, SpecialType.System_Byte) => true,
            (SpecialType.System_Double, SpecialType.System_Byte) => true,
            (SpecialType.System_Decimal, SpecialType.System_Byte) => true,

            // -------- to char --------
            (SpecialType.System_Byte, SpecialType.System_Char) => true,
            (SpecialType.System_Int32, SpecialType.System_Char) => true,
            (SpecialType.System_Int64, SpecialType.System_Char) => true,
            (SpecialType.System_Single, SpecialType.System_Char) => true,
            (SpecialType.System_Double, SpecialType.System_Char) => true,
            (SpecialType.System_Decimal, SpecialType.System_Char) => true,

            // -------- floating to integral --------
            (SpecialType.System_Double, SpecialType.System_Int32) => true,
            (SpecialType.System_Double, SpecialType.System_Int64) => true,
            (SpecialType.System_Single, SpecialType.System_Int32) => true,
            (SpecialType.System_Single, SpecialType.System_Int64) => true,

            // -------- integral narrowing --------
            (SpecialType.System_Int64, SpecialType.System_Int32) => true,

            // -------- decimal to integral --------
            (SpecialType.System_Decimal, SpecialType.System_Int32) => true,
            (SpecialType.System_Decimal, SpecialType.System_Int64) => true,

            // -------- decimal <-> floating are explicit only in C# --------
            (SpecialType.System_Decimal, SpecialType.System_Double) => true,
            (SpecialType.System_Double, SpecialType.System_Decimal) => true,
            (SpecialType.System_Decimal, SpecialType.System_Single) => true,
            (SpecialType.System_Single, SpecialType.System_Decimal) => true,

            // explicit from double to float
            (SpecialType.System_Double, SpecialType.System_Single) => true,

            _ => false
        };
    }
}
