using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    public Conversion ClassifyConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        static ITypeSymbol Unalias(ITypeSymbol type, ref bool wasAlias)
        {
            while (true)
            {
                if (!type.IsAlias)
                    return type;

                wasAlias = true;

                if (type.UnderlyingSymbol is ITypeSymbol aliasTarget)
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

        if (destination is null)
            return Conversion.None;

        var aliasInvolved = sourceUsedAlias || destinationUsedAlias;

        Conversion Finalize(Conversion conversion)
        {
            if (!conversion.Exists)
                return conversion;

            return conversion.WithAlias(aliasInvolved);
        }

        if (source.ContainsErrorType() || destination.ContainsErrorType())
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));

        if (source is LiteralTypeSymbol litSrc && destination is LiteralTypeSymbol litDest)
            return Equals(litSrc.ConstantValue, litDest.ConstantValue)
                ? Finalize(new Conversion(isImplicit: true, isIdentity: true))
                : Conversion.None;

        if (destination is LiteralTypeSymbol)
            return Conversion.None;

        var sourceUnionCase = source.TryGetDiscriminatedUnionCase();
        var destinationUnion = destination.TryGetDiscriminatedUnion();
        if (sourceUnionCase is not null &&
            destinationUnion is not null &&
            SymbolEqualityComparer.Default.Equals(sourceUnionCase.Union, destinationUnion))
        {
            var conversionMethod = FindDiscriminatedUnionConversionMethod(source, destination);
            return Finalize(new Conversion(
                isImplicit: true,
                isDiscriminatedUnion: true,
                isUserDefined: conversionMethod is not null,
                methodSymbol: conversionMethod));
        }

        bool ElementTypesAreCompatible(ITypeSymbol sourceElement, ITypeSymbol destinationElement)
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

        if (source is IAddressTypeSymbol addressSource)
        {
            if (destination is ByRefTypeSymbol byRefDestination &&
                ElementTypesAreCompatible(addressSource.ReferencedType, byRefDestination.ElementType))
            {
                return Finalize(new Conversion(isImplicit: true));
            }

            if (destination is IPointerTypeSymbol addressPointerDestination &&
                ElementTypesAreCompatible(addressSource.ReferencedType, addressPointerDestination.PointedAtType))
            {
                return Finalize(new Conversion(isImplicit: true, isPointer: true));
            }
        }

        if (source is ByRefTypeSymbol byRefSource &&
            destination is IPointerTypeSymbol pointerDestination &&
            ElementTypesAreCompatible(byRefSource.ElementType, pointerDestination.PointedAtType))
        {
            return Finalize(new Conversion(isImplicit: true, isPointer: true));
        }

        if (source.MetadataIdentityEquals(destination) &&
            source is not NullableTypeSymbol &&
            destination is not NullableTypeSymbol)
        {
            return Finalize(new Conversion(isImplicit: true, isIdentity: true));
        }

        static bool UnionContainsNull(IUnionTypeSymbol union)
        {
            foreach (var member in union.Types)
            {
                if (member.TypeKind == TypeKind.Null)
                    return true;

                if (member is IUnionTypeSymbol nested && UnionContainsNull(nested))
                    return true;
            }

            return false;
        }

        if (source.TypeKind == TypeKind.Null)
        {
            if (destination.TypeKind == TypeKind.Nullable)
                return Finalize(new Conversion(isImplicit: true, isReference: true));

            if (destination is IUnionTypeSymbol unionDest && UnionContainsNull(unionDest))
                return Finalize(new Conversion(isImplicit: true, isReference: true));

            return Conversion.None;
        }

        if (source is NullableTypeSymbol nullableSource)
        {
            var conv = ClassifyConversion(nullableSource.UnderlyingType, destination);
            if (conv.Exists)
            {
                var isImplicit = !nullableSource.UnderlyingType.MetadataIdentityEquals(destination) && conv.IsImplicit;
                return Finalize(new Conversion(
                    isImplicit: isImplicit,
                    isIdentity: conv.IsIdentity,
                    isNumeric: conv.IsNumeric,
                    isReference: conv.IsReference,
                    isBoxing: conv.IsBoxing,
                    isUnboxing: conv.IsUnboxing,
                    isUserDefined: conv.IsUserDefined,
                    isAlias: conv.IsAlias,
                    methodSymbol: conv.MethodSymbol));
            }
        }

        if (destination is NullableTypeSymbol nullableDest)
        {
            if (source is IUnionTypeSymbol unionSource &&
                unionSource.Types.Count() == 2 &&
                unionSource.Types.Any(t => t.TypeKind == TypeKind.Null) &&
                unionSource.Types.Any(t => t.MetadataIdentityEquals(nullableDest.UnderlyingType)))
            {
                return Finalize(new Conversion(isImplicit: true, isReference: true));
            }

            var conv = ClassifyConversion(source, nullableDest.UnderlyingType);
            if (conv.Exists)
                return Finalize(new Conversion(
                    isImplicit: true,
                    isIdentity: false,
                    isNumeric: conv.IsNumeric,
                    isReference: conv.IsReference || !source.IsValueType,
                    isBoxing: conv.IsBoxing,
                    isUnboxing: conv.IsUnboxing,
                    isUserDefined: conv.IsUserDefined,
                    isAlias: conv.IsAlias,
                    methodSymbol: conv.MethodSymbol));
        }

        if (source is IUnionTypeSymbol unionSource2)
        {
            var conversions = unionSource2.Types.Select(t => ClassifyConversion(t, destination)).ToArray();
            if (conversions.All(c => c.Exists))
            {
                var isImplicit = conversions.All(c => c.IsImplicit);
                var isAlias = conversions.Any(c => c.IsAlias);
                var destinationIsValueType = destination.IsValueType;

                return Finalize(new Conversion(
                    isImplicit: isImplicit,
                    isReference: !destinationIsValueType,
                    isUnboxing: destinationIsValueType,
                    isAlias: isAlias));
            }

            return Conversion.None;
        }

        if (source.SpecialType == SpecialType.System_Void)
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

        if (destination is IUnionTypeSymbol unionType)
        {
            Conversion matchConversion = default;
            var foundMatch = false;

            foreach (var branch in unionType.Types)
            {
                var branchConversion = ClassifyConversion(source, branch);
                if (branchConversion.Exists)
                {
                    matchConversion = branchConversion;
                    foundMatch = true;
                    break;
                }
            }

            if (!foundMatch)
                return Conversion.None;

            return Finalize(new Conversion(
                isImplicit: true,
                isBoxing: source.IsValueType,
                isAlias: matchConversion.IsAlias));
        }

        if (source is LiteralTypeSymbol litSrc2)
            return Finalize(ClassifyConversion(litSrc2.UnderlyingType, destination));

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

        if (IsImplicitNumericConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: true, isNumeric: true));
        }

        if (IsExplicitNumericConversion(source, destination))
        {
            return Finalize(new Conversion(isImplicit: false, isNumeric: true));
        }

        var sourceNamed = source as INamedTypeSymbol;
        var destinationNamed = destination as INamedTypeSymbol;

        if (sourceNamed != null || destinationNamed != null)
        {
            IEnumerable<IMethodSymbol> candidateConversions =
                Enumerable.Empty<IMethodSymbol>();

            if (sourceNamed != null)
                candidateConversions = candidateConversions.Concat(sourceNamed.GetMembers().OfType<IMethodSymbol>());
        if (destinationNamed != null && !source.MetadataIdentityEquals(destination))
                candidateConversions = candidateConversions.Concat(destinationNamed.GetMembers().OfType<IMethodSymbol>());

            foreach (var method in candidateConversions)
            {
                if (method.MethodKind is MethodKind.Conversion &&
                    method.Parameters.Length == 1 &&
                    method.Parameters[0].Type.MetadataIdentityEquals(source) &&
                    method.ReturnType.MetadataIdentityEquals(destination))
                {
                    var isImplicit = method.Name == "op_Implicit";
                    return Finalize(new Conversion(isImplicit: isImplicit, isUserDefined: true, methodSymbol: method));
                }
            }
        }

        return Conversion.None;
    }

    private static IMethodSymbol? FindDiscriminatedUnionConversionMethod(ITypeSymbol source, ITypeSymbol destination)
    {
        if (destination is INamedTypeSymbol destinationNamed)
        {
            var method = FindMatchingConversion(destinationNamed, source, destination);
            if (method is not null)
                return method;
        }

        if (source is INamedTypeSymbol sourceNamed)
        {
            var method = FindMatchingConversion(sourceNamed, source, destination);
            if (method is not null)
                return method;
        }

        return null;

        static IMethodSymbol? FindMatchingConversion(
            INamedTypeSymbol owner,
            ITypeSymbol sourceType,
            ITypeSymbol destinationType)
        {
            foreach (var member in owner.GetMembers("op_Implicit"))
            {
                if (member is not IMethodSymbol method)
                    continue;

                if (method.MethodKind is not MethodKind.Conversion)
                    continue;

                if (method.Parameters.Length != 1)
                    continue;

                if (!SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, sourceType))
                    continue;

                if (!SymbolEqualityComparer.Default.Equals(method.ReturnType, destinationType))
                    continue;

                return method;
            }

            return null;
        }
    }

    private bool IsReferenceConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        if (source.IsValueType || destination.IsValueType)
            return false;

        if (source.MetadataIdentityEquals(destination))
            return false;

        var current = source.BaseType;
        while (current is not null)
        {
            if (current.MetadataIdentityEquals(destination))
                return true;

            current = current.BaseType;
        }

        if (destination is INamedTypeSymbol destinationNamed &&
            destinationNamed.TypeKind == TypeKind.Interface)
        {
            if (SemanticFacts.ImplementsInterface(source, destinationNamed, SymbolEqualityComparer.Default))
                return true;
        }

        if (source.TypeKind == TypeKind.Interface && destination.SpecialType is SpecialType.System_Object)
            return true;

        return false;
    }

    private bool IsExplicitReferenceConversion(ITypeSymbol source, ITypeSymbol destination)
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

        if (source is INamedTypeSymbol sourceInterface && sourceInterface.TypeKind == TypeKind.Interface)
        {
            if (destination.SpecialType is SpecialType.System_Object)
                return true;

            if (destination is INamedTypeSymbol destinationNamed &&
                destinationNamed.TypeKind != TypeKind.Interface &&
                SemanticFacts.ImplementsInterface(destinationNamed, sourceInterface, comparer))
            {
                return true;
            }

            if (destination is INamedTypeSymbol sourceToTargetInterface && sourceToTargetInterface.TypeKind == TypeKind.Interface &&
                (SemanticFacts.ImplementsInterface(sourceInterface, sourceToTargetInterface, comparer) ||
                 SemanticFacts.ImplementsInterface(sourceToTargetInterface, sourceInterface, comparer)))
            {
                return true;
            }
        }

        if (destination is INamedTypeSymbol targetInterface && targetInterface.TypeKind == TypeKind.Interface &&
            SemanticFacts.ImplementsInterface(source, targetInterface, comparer))
        {
            return true;
        }

        return false;
    }

    private bool IsBoxingConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        return source.IsValueType && destination.SpecialType is SpecialType.System_Object;
    }

    private bool IsUnboxingConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        return source.SpecialType is SpecialType.System_Object && destination.IsValueType;
    }

    private bool IsImplicitNumericConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        return (sourceType is SpecialType.System_Int32 && destType is SpecialType.System_Int64) ||
               (sourceType is SpecialType.System_Int32 && destType is SpecialType.System_Double) ||
               (sourceType is SpecialType.System_Single && destType is SpecialType.System_Double);
    }

    private bool IsExplicitNumericConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        return (sourceType is SpecialType.System_Double && destType is SpecialType.System_Int32) ||
               (sourceType is SpecialType.System_Int64 && destType is SpecialType.System_Int32);
    }
}
