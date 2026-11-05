using System;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis;

public sealed class SymbolEqualityComparer : IEqualityComparer<Symbol>
{
    public static SymbolEqualityComparer Default { get; } = new(includeNullability: true);

    public static SymbolEqualityComparer IgnoringNullability { get; } = new(includeNullability: false);

    private readonly bool _includeNullability;
    private readonly bool _ignoreContainingNamespaceOrType;

    public SymbolEqualityComparer(
        bool includeNullability = true,
        bool ignoreContainingNamespaceOrType = false)
    {
        _includeNullability = includeNullability;
        _ignoreContainingNamespaceOrType = ignoreContainingNamespaceOrType;
    }

    public SymbolEqualityComparer IgnoreContainingNamespaceOrType()
    {
        if (_ignoreContainingNamespaceOrType)
            return this;

        return new SymbolEqualityComparer(
            includeNullability: _includeNullability,
            ignoreContainingNamespaceOrType: true);
    }

    public bool Equals(Symbol? x, Symbol? y)
    {
        return EqualsCore(x, y, visited: null);
    }

    private bool EqualsCore(Symbol? x, Symbol? y, HashSet<SymbolPair>? visited)
    {
        if (ReferenceEquals(x, y))
            return true;

        if (x is null || y is null)
            return false;

        x = Normalize(x);
        y = Normalize(y);

        if (ReferenceEquals(x, y))
            return true;

        if (x.Kind != y.Kind)
            return false;

        var importedX = (x as ImportedAssemblySymbol)?.DefinitionIdentity;
        var importedY = (y as ImportedAssemblySymbol)?.DefinitionIdentity;
        if (importedX is not null || importedY is not null)
            return importedX is not null && importedY is not null && importedX.Equals(importedY);

        if (x is TypeSymbol typeX && y is TypeSymbol typeY)
        {
            if (typeX.TypeKind != typeY.TypeKind)
                return false;

            if (typeX.SpecialType != SpecialType.None &&
                typeX.SpecialType == typeY.SpecialType &&
                IsSimpleSpecialType(typeX.SpecialType))
            {
                return true;
            }
        }

        if (x is TypeParameterSymbol xTypeParameter && y is TypeParameterSymbol yTypeParameter)
        {
            if (xTypeParameter.OwnerKind != yTypeParameter.OwnerKind)
                return false;
        }

        visited ??= new HashSet<SymbolPair>();
        var pair = new SymbolPair(x, y);
        if (!visited.Add(pair))
            return true;
        visited.Add(new SymbolPair(y, x));

        // Nullable identity follows the underlying semantic type, not its display
        // spelling or the container in which the wrapper was constructed.
        if (x is INullableTypeIdentity || y is INullableTypeIdentity)
            return x is INullableTypeIdentity nullableX && y is INullableTypeIdentity nullableY
                && EqualsCore(nullableX.UnderlyingType, nullableY.UnderlyingType, visited);

        if (x is IStructuralTupleIdentity || y is IStructuralTupleIdentity)
        {
            if (x is not IStructuralTupleIdentity tupleX || y is not IStructuralTupleIdentity tupleY ||
                tupleX.TupleElements.Length != tupleY.TupleElements.Length) return false;
            for (var i = 0; i < tupleX.TupleElements.Length; i++)
                if (!EqualsCore(tupleX.TupleElements[i].Type, tupleY.TupleElements[i].Type, visited)) return false;
            return true;
        }

        // >>> NEW BLOCK: relaxed comparison for type parameters
        if (_ignoreContainingNamespaceOrType &&
            x is TypeParameterSymbol tpx &&
            y is TypeParameterSymbol tpy)
        {
            if (tpx.OwnerKind != tpy.OwnerKind)
                return false;

            if (!string.Equals(tpx.Name, tpy.Name, StringComparison.Ordinal))
                return false;

            // Ordinal usually matters for nested/outer type params
            if (tpx.Ordinal != tpy.Ordinal)
                return false;

            // Optional but usually a good idea:
            if (tpx.Variance != tpy.Variance)
                return false;

            if (tpx.OwnerKind == TypeParameterOwnerKind.Method)
            {
                if (!EqualsCore(tpx.DeclaringMethodParameterOwner, tpy.DeclaringMethodParameterOwner, visited))
                    return false;
            }
            else
            {
                if (!EqualsCore(tpx.DeclaringTypeParameterOwner, tpy.DeclaringTypeParameterOwner, visited))
                    return false;
            }

            /*
            if (tpx.HasReferenceTypeConstraint != tpy.HasReferenceTypeConstraint ||
                tpx.HasValueTypeConstraint != tpy.HasValueTypeConstraint ||
                tpx.HasConstructorConstraint != tpy.HasConstructorConstraint)
            {
                return false;
            }
            */

            var constraintsX = tpx.ConstraintTypes;
            var constraintsY = tpy.ConstraintTypes;

            if (constraintsX.Length != constraintsY.Length)
                return false;

            for (var i = 0; i < constraintsX.Length; i++)
            {
                if (!EqualsCore(constraintsX[i], constraintsY[i], visited))
                    return false;
            }

            // NOTE: we *intentionally* do NOT compare containing symbol or namespace here.
            return true;
        }
        // <<< END NEW BLOCK

        if (x is ParameterSymbol px && y is ParameterSymbol py)
        {
            if (ReferenceEquals(px, py))
                return true;

            if (px.RefKind != py.RefKind ||
                px.MacroRole != py.MacroRole)
                return false;

            if (!EqualsCore(px.Type, py.Type, visited))
                return false;

            var hasOrdinalX = TryGetParameterOrdinal(px, out var ordinalX);
            var hasOrdinalY = TryGetParameterOrdinal(py, out var ordinalY);

            if (hasOrdinalX && hasOrdinalY)
            {
                if (ordinalX != ordinalY)
                    return false;

                return EqualsCore(px.ContainingSymbol, py.ContainingSymbol, visited);
            }

            if (!EqualsCore(px.ContainingSymbol, py.ContainingSymbol, visited))
                return false;

            return string.Equals(px.Name, py.Name, StringComparison.Ordinal);
        }

        if (x is LocalSymbol localX && y is LocalSymbol localY)
        {
            if (!string.Equals(localX.Name, localY.Name, StringComparison.Ordinal))
                return false;

            if (localX.IsMutable != localY.IsMutable)
                return false;

            if (localX.IsConst != localY.IsConst)
                return false;

            if (!EqualsCore(localX.Type, localY.Type, visited))
                return false;

            if (!EqualsCore(localX.ContainingSymbol, localY.ContainingSymbol, visited))
                return false;

            return HaveEquivalentDeclarationIdentity(localX, localY);
        }

        if (x is LabelSymbol labelX && y is LabelSymbol labelY)
        {
            if (!string.Equals(labelX.Name, labelY.Name, StringComparison.Ordinal))
                return false;

            if (!EqualsCore(labelX.ContainingSymbol, labelY.ContainingSymbol, visited))
                return false;

            return HaveEquivalentDeclarationIdentity(labelX, labelY);
        }

        if (x is FunctionReturnTypeSymbol || y is FunctionReturnTypeSymbol)
            return x is FunctionReturnTypeSymbol leftProjection && y is FunctionReturnTypeSymbol rightProjection &&
                EqualsCore(leftProjection.FunctionType, rightProjection.FunctionType, visited);

        if (x is IStructuralFunctionIdentity || y is IStructuralFunctionIdentity)
        {
            if (x is not IStructuralFunctionIdentity left || y is not IStructuralFunctionIdentity right ||
                left.ParameterTypes.Length != right.ParameterTypes.Length ||
                !left.ParameterRefKinds.SequenceEqual(right.ParameterRefKinds) ||
                !EqualsCore(left.ReturnType, right.ReturnType, visited)) return false;
            for (var i = 0; i < left.ParameterTypes.Length; i++)
                if (!EqualsCore(left.ParameterTypes[i], right.ParameterTypes[i], visited)) return false;
            return true;
        }

        if (x is StructuralUnionTypeSymbol unionX && y is StructuralUnionTypeSymbol unionY)
        {
            if (unionX.ConstituentTypes.Length != unionY.ConstituentTypes.Length)
                return false;

            var matched = new bool[unionY.ConstituentTypes.Length];
            foreach (var left in unionX.ConstituentTypes)
            {
                var index = -1;
                for (var i = 0; i < matched.Length; i++)
                {
                    if (!matched[i] && EqualsCore(left, unionY.ConstituentTypes[i], new HashSet<SymbolPair>(visited)))
                    {
                        index = i;
                        break;
                    }
                }

                if (index < 0)
                    return false;
                matched[index] = true;
            }

            return true;
        }

        if (x is IntersectionTypeSymbol intersectionX && y is IntersectionTypeSymbol intersectionY)
        {
            if (intersectionX.ConstituentTypes.Length != intersectionY.ConstituentTypes.Length)
                return false;

            var matched = new bool[intersectionY.ConstituentTypes.Length];
            foreach (var left in intersectionX.ConstituentTypes)
            {
                var index = -1;
                for (var i = 0; i < matched.Length; i++)
                {
                    if (!matched[i] && EqualsCore(left, intersectionY.ConstituentTypes[i], new HashSet<SymbolPair>(visited)))
                    {
                        index = i;
                        break;
                    }
                }

                if (index < 0)
                    return false;
                matched[index] = true;
            }

            return true;
        }

        // Arrays have structural identity. Imported and source-created array
        // symbols may have different construction containers for the same CLI type.
        if (x is ArrayTypeSymbol arrayX && y is ArrayTypeSymbol arrayY)
            return arrayX.Rank == arrayY.Rank
                && arrayX.FixedLength == arrayY.FixedLength
                && EqualsCore(arrayX.ElementType, arrayY.ElementType, visited);

        if (!string.Equals(x.Name, y.Name, StringComparison.Ordinal))
            return false;

        if (!string.Equals(GetMetadataNameForIdentity(x), GetMetadataNameForIdentity(y), StringComparison.Ordinal))
            return false;

        if (!_ignoreContainingNamespaceOrType)
        {
            if (!EqualsCore(x.ContainingNamespace, y.ContainingNamespace, visited))
                return false;

            if (!EqualsCore(x.ContainingSymbol, y.ContainingSymbol, visited))
                return false;
        }

        if (x is NamedTypeSymbol namedTypeX && y is NamedTypeSymbol namedTypeY)
        {
            if (namedTypeX.IsGenericType || namedTypeY.IsGenericType)
            {
                var definitionX = GetGenericDefinition(namedTypeX);
                var definitionY = GetGenericDefinition(namedTypeY);

                if (!ReferenceEquals(namedTypeX, definitionX) || !ReferenceEquals(namedTypeY, definitionY))
                {
                    if (!EqualsCore(definitionX, definitionY, visited))
                        return false;
                }

                var typeArgumentsX = GetTypeArgumentsOrParameters(namedTypeX);
                var typeArgumentsY = GetTypeArgumentsOrParameters(namedTypeY);

                if (typeArgumentsX.Length != typeArgumentsY.Length)
                    return false;

                for (var i = 0; i < typeArgumentsX.Length; i++)
                {
                    if (!EqualsCore(typeArgumentsX[i], typeArgumentsY[i], visited))
                        return false;
                }
            }
        }

        if (x is PointerTypeSymbol pointerX && y is PointerTypeSymbol pointerY)
        {
            if (!EqualsCore(pointerX.PointedAtType, pointerY.PointedAtType, visited))
                return false;
        }

        if (x is AddressTypeSymbol addressX && y is AddressTypeSymbol addressY)
        {
            if (!EqualsCore(addressX.ReferencedType, addressY.ReferencedType, visited))
                return false;
        }

        if (x is FieldSymbol fieldX && y is FieldSymbol fieldY)
        {
            if (!EqualsCore(fieldX.Type, fieldY.Type, visited))
                return false;

            if (fieldX.IsConst != fieldY.IsConst)
                return false;
        }

        if (x is PropertySymbol propertyX && y is PropertySymbol propertyY)
        {
            if (!EqualsCore(propertyX.Type, propertyY.Type, visited))
                return false;

            if (propertyX.IsIndexer != propertyY.IsIndexer)
                return false;

            if ((propertyX.GetMethod is null) != (propertyY.GetMethod is null))
                return false;

            if ((propertyX.SetMethod is null) != (propertyY.SetMethod is null))
                return false;
        }

        if (x is MethodSymbol methodX && y is MethodSymbol methodY)
        {
            // Local functions are scoped declarations, even when their emitted
            // method symbols share the containing type and signature.
            var localFunctionX = IsLocalFunction(methodX);
            var localFunctionY = IsLocalFunction(methodY);
            if (localFunctionX != localFunctionY ||
                (localFunctionX && !HaveEquivalentDeclarationIdentity(methodX, methodY)))
                return false;

            if (!EqualsCore(methodX.ReturnType, methodY.ReturnType, visited))
                return false;

            if (methodX.Parameters.Length != methodY.Parameters.Length)
                return false;

            for (int i = 0; i < methodX.Parameters.Length; i++)
            {
                if (!EqualsCore(methodX.Parameters[i], methodY.Parameters[i], visited))
                    return false;
            }

            var typeArgumentsX = methodX.TypeArguments;
            var typeArgumentsY = methodY.TypeArguments;

            if (typeArgumentsX.IsDefault)
                typeArgumentsX = ImmutableArray<TypeSymbol>.Empty;

            if (typeArgumentsY.IsDefault)
                typeArgumentsY = ImmutableArray<TypeSymbol>.Empty;

            if (typeArgumentsX.Length != typeArgumentsY.Length)
                return false;

            for (var i = 0; i < typeArgumentsX.Length; i++)
            {
                if (!EqualsCore(typeArgumentsX[i], typeArgumentsY[i], visited))
                    return false;
            }
        }

        if (x is MacroDeclarationSymbol macroX && y is MacroDeclarationSymbol macroY)
        {
            if (macroX.MacroKind != macroY.MacroKind ||
                macroX.Targets != macroY.Targets ||
                !string.Equals(macroX.TargetName, macroY.TargetName, StringComparison.Ordinal) ||
                !EqualsCore(macroX.ReturnType, macroY.ReturnType, visited) ||
                macroX.Parameters.Length != macroY.Parameters.Length ||
                macroX.TypeParameters.Length != macroY.TypeParameters.Length)
            {
                return false;
            }

            for (var i = 0; i < macroX.Parameters.Length; i++)
            {
                if (!EqualsCore(macroX.Parameters[i], macroY.Parameters[i], visited))
                    return false;
            }
        }

        return true;
    }

    private static bool IsSimpleSpecialType(SpecialType specialType)
    {
        return specialType is SpecialType.System_Boolean
            or SpecialType.System_Char
            or SpecialType.System_Double
            or SpecialType.System_Int32
            or SpecialType.System_Int64
            or SpecialType.System_Type
            or SpecialType.System_Object
            or SpecialType.System_Single
            or SpecialType.System_String
            or SpecialType.System_Unit
            or SpecialType.System_Threading_Tasks_Task;
    }

    private static bool TryGetParameterOrdinal(ParameterSymbol parameter, out int ordinal)
    {
        if (parameter.ContainingSymbol is MethodSymbol method)
        {
            var parameters = method.Parameters;
            for (var i = 0; i < parameters.Length; i++)
            {
                if (ReferenceEquals(parameters[i], parameter))
                {
                    ordinal = i;
                    return true;
                }
            }
        }
        else if (parameter.ContainingSymbol is MacroDeclarationSymbol macro)
        {
            var parameters = macro.Parameters;
            for (var i = 0; i < parameters.Length; i++)
            {
                if (ReferenceEquals(parameters[i], parameter))
                {
                    ordinal = i;
                    return true;
                }
            }
        }

        ordinal = -1;
        return false;
    }

    public int GetHashCode(Symbol obj)
    {
        if (obj is null)
            return 0;

        return GetHashCodeCore(obj, visited: null);
    }

    private int GetHashCodeCore(Symbol obj, HashSet<Symbol>? visited)
    {
        obj = Normalize(obj);

        visited ??= new HashSet<Symbol>(SymbolReferenceComparer.Instance);
        if (!visited.Add(obj))
            return 0;

        var hash = new HashCode();
        hash.Add(obj.Kind);
        if (obj is ImportedAssemblySymbol { DefinitionIdentity: { } identity })
        {
            hash.Add(identity);
            return hash.ToHashCode();
        }

        if (obj is TypeSymbol typeSymbol)
        {
            hash.Add(typeSymbol.TypeKind);

            if (typeSymbol.SpecialType != SpecialType.None &&
                IsSimpleSpecialType(typeSymbol.SpecialType))
            {
                hash.Add(typeSymbol.SpecialType);
                return hash.ToHashCode();
            }
        }

        if (obj is INullableTypeIdentity nullableType)
        {
            hash.Add(GetHashCodeCore(nullableType.UnderlyingType, visited));
            return hash.ToHashCode();
        }

        if (obj is FunctionReturnTypeSymbol projection)
        {
            hash.Add(GetHashCodeCore(projection.FunctionType, new HashSet<Symbol>(visited, SymbolReferenceComparer.Instance)));
            return hash.ToHashCode();
        }

        if (obj is IStructuralFunctionIdentity function)
        {
            hash.Add(GetHashCodeCore(function.ReturnType, new HashSet<Symbol>(visited, SymbolReferenceComparer.Instance)));
            for (var i = 0; i < function.ParameterTypes.Length; i++)
            {
                hash.Add(function.ParameterRefKinds[i]);
                hash.Add(GetHashCodeCore(function.ParameterTypes[i], new HashSet<Symbol>(visited, SymbolReferenceComparer.Instance)));
            }
            return hash.ToHashCode();
        }

        if (obj is IStructuralTupleIdentity tuple)
        {
            hash.Add(tuple.TupleElements.Length);
            foreach (var element in tuple.TupleElements)
                hash.Add(GetHashCodeCore(element.Type, new HashSet<Symbol>(visited, SymbolReferenceComparer.Instance)));
            return hash.ToHashCode();
        }

        if (obj is StructuralUnionTypeSymbol union)
        {
            // Each branch gets the same recursion ancestry so sibling ordering
            // cannot change the hash through shared containing symbols.
            var constituentHash = 0;
            foreach (var constituent in union.ConstituentTypes)
                constituentHash = unchecked(constituentHash + GetHashCodeCore(
                    constituent, new HashSet<Symbol>(visited, SymbolReferenceComparer.Instance)));
            hash.Add(union.ConstituentTypes.Length);
            hash.Add(constituentHash);
            return hash.ToHashCode();
        }

        if (obj is IntersectionTypeSymbol intersection)
        {
            // Each branch gets the same recursion ancestry so sibling ordering
            // cannot change the hash through shared containing symbols.
            var constituentHash = 0;
            foreach (var constituent in intersection.ConstituentTypes)
                constituentHash = unchecked(constituentHash + GetHashCodeCore(
                    constituent, new HashSet<Symbol>(visited, SymbolReferenceComparer.Instance)));
            hash.Add(intersection.ConstituentTypes.Length);
            hash.Add(constituentHash);
            return hash.ToHashCode();
        }

        if (obj is ArrayTypeSymbol arrayType)
        {
            hash.Add(arrayType.Rank);
            hash.Add(arrayType.FixedLength);
            hash.Add(GetHashCodeCore(arrayType.ElementType, visited));
            return hash.ToHashCode();
        }

        // >>> NEW BLOCK: type parameter hashing in relaxed mode
        if (_ignoreContainingNamespaceOrType &&
            obj is TypeParameterSymbol tp)
        {
            hash.Add(tp.Name, StringComparer.Ordinal);
            hash.Add(tp.Ordinal);
            hash.Add((int)tp.Variance);
            hash.Add((int)tp.OwnerKind);

            if (tp.OwnerKind == TypeParameterOwnerKind.Method)
            {
                if (tp.DeclaringMethodParameterOwner is { } methodOwner)
                    hash.Add(GetHashCodeCore(methodOwner, visited));
            }
            else
            {
                if (tp.DeclaringTypeParameterOwner is { } typeOwner)
                    hash.Add(GetHashCodeCore(typeOwner, visited));
            }
            /*hash.Add(tp.HasReferenceTypeConstraint);
            hash.Add(tp.HasValueTypeConstraint);
            hash.Add(tp.HasConstructorConstraint); */

            return hash.ToHashCode();
        }
        // <<< END NEW BLOCK

        if (obj is TypeParameterSymbol typeParameterSymbol)
        {
            hash.Add(typeParameterSymbol.Name, StringComparer.Ordinal);
            hash.Add(typeParameterSymbol.Ordinal);
            hash.Add((int)typeParameterSymbol.Variance);
            hash.Add((int)typeParameterSymbol.OwnerKind);
            return hash.ToHashCode();
        }

        if (obj is LocalSymbol localSymbol)
        {
            hash.Add(localSymbol.Name, StringComparer.Ordinal);
            hash.Add(localSymbol.IsMutable);
            hash.Add(localSymbol.IsConst);
            hash.Add(GetHashCodeCore(localSymbol.Type, visited));

            if (localSymbol.ContainingSymbol is { } localContainingSymbol)
                hash.Add(GetHashCodeCore(localContainingSymbol, visited));

            AddDeclarationIdentityHash(ref hash, localSymbol);
            return hash.ToHashCode();
        }

        if (obj is LabelSymbol labelSymbol)
        {
            hash.Add(labelSymbol.Name, StringComparer.Ordinal);

            if (labelSymbol.ContainingSymbol is { } labelContainingSymbol)
                hash.Add(GetHashCodeCore(labelContainingSymbol, visited));

            AddDeclarationIdentityHash(ref hash, labelSymbol);
            return hash.ToHashCode();
        }

        hash.Add(obj.Name, StringComparer.Ordinal);
        hash.Add(GetMetadataNameForIdentity(obj), StringComparer.Ordinal);

        if (!_ignoreContainingNamespaceOrType && obj.ContainingNamespace is { } containingNamespace)
            hash.Add(containingNamespace.Name, StringComparer.Ordinal);

        if (!_ignoreContainingNamespaceOrType && obj.ContainingSymbol is { } containingSymbol)
            hash.Add(GetHashCodeCore(containingSymbol, visited));

        if (obj is ParameterSymbol parameterSymbol)
        {
            hash.Add(parameterSymbol.RefKind);
            hash.Add(parameterSymbol.MacroRole);
            hash.Add(GetHashCodeCore(parameterSymbol.Type, visited));
            return hash.ToHashCode();
        }

        if (obj is MethodSymbol method)
        {
            if (IsLocalFunction(method))
                AddDeclarationIdentityHash(ref hash, method);

            hash.Add(method.Parameters.Length);
            hash.Add(GetHashCodeCore(method.ReturnType, visited));

            for (var i = 0; i < method.Parameters.Length; i++)
                hash.Add(GetHashCodeCore(method.Parameters[i], visited));

            var typeArguments = method.TypeArguments;
            if (typeArguments.IsDefault)
                typeArguments = ImmutableArray<TypeSymbol>.Empty;

            hash.Add(typeArguments.Length);
            for (var i = 0; i < typeArguments.Length; i++)
                hash.Add(GetHashCodeCore(typeArguments[i], visited));
        }

        if (obj is MacroDeclarationSymbol macro)
        {
            hash.Add(macro.MacroKind);
            hash.Add(macro.Targets);
            hash.Add(macro.TargetName, StringComparer.Ordinal);
            hash.Add(macro.Parameters.Length);
            hash.Add(GetHashCodeCore(macro.ReturnType, visited));

            foreach (var parameter in macro.Parameters)
                hash.Add(GetHashCodeCore(parameter, visited));

            hash.Add(macro.TypeParameters.Length);
        }

        if (obj is NamedTypeSymbol namedType)
        {
            var typeArguments = GetTypeArgumentsOrParameters(namedType);
            hash.Add(namedType.Arity);
            hash.Add(typeArguments.Length);
            hash.Add(namedType.TypeKind);

            for (var i = 0; i < typeArguments.Length; i++)
                hash.Add(GetHashCodeCore(typeArguments[i], visited));
        }

        if (obj is FieldSymbol field)
        {
            hash.Add(field.IsConst);
            hash.Add(GetHashCodeCore(field.Type, visited));
        }

        if (obj is PropertySymbol property)
        {
            hash.Add(property.IsIndexer);
            hash.Add(property.GetMethod is not null);
            hash.Add(property.SetMethod is not null);
            hash.Add(GetHashCodeCore(property.Type, visited));
        }

        return hash.ToHashCode();
    }

    private Symbol Normalize(Symbol symbol)
    {
        var current = symbol;

        while (true)
        {
            // An explicitly selected unit value has the identity of that exact
            // imported type, including inside constructed generic signatures.
            // Keep the source Unit symbol itself for language/return-position rules.
            if (current is IUnitTypeIdentity { RuntimeRepresentation: { } representation })
            {
                current = representation;
                continue;
            }

            if (current.IsAlias)
            {
                current = current.UnderlyingSymbol;
                continue;
            }

            if (!_includeNullability &&
                current is INullableTypeIdentity nullableType &&
                !nullableType.UnderlyingType.IsValueType)
            {
                current = nullableType.UnderlyingType;
                continue;
            }

            return current;
        }
    }

    private static NamedTypeSymbol GetGenericDefinition(NamedTypeSymbol symbol)
    {
        if (symbol.ConstructedFrom is NamedTypeSymbol constructedFrom && !ReferenceEquals(symbol, constructedFrom))
            return constructedFrom;

        if (symbol.OriginalDefinition is NamedTypeSymbol original && !ReferenceEquals(symbol, original))
            return original;

        return symbol;
    }

    private static string GetMetadataNameForIdentity(Symbol symbol)
    {
        var metadataName = symbol.MetadataName;
        if (symbol is not NamedTypeSymbol)
            return metadataName;

        var nestedSeparator = metadataName.LastIndexOf('+');
        var namespaceSeparator = metadataName.LastIndexOf('.');
        var separator = Math.Max(nestedSeparator, namespaceSeparator);
        return separator >= 0 ? metadataName[(separator + 1)..] : metadataName;
    }

    private static ImmutableArray<TypeSymbol> GetTypeArgumentsOrParameters(NamedTypeSymbol symbol)
    {
        if (symbol is IConstructedTypeSubstitutionInfo constructed)
            return constructed.ExplicitTypeArgumentsForSubstitution;

        var typeArguments = symbol.TypeArguments;
        if (!typeArguments.IsDefault && typeArguments.Length > 0)
            return typeArguments;

        if (!symbol.TypeParameters.IsDefault && symbol.TypeParameters.Length > 0)
            return symbol.TypeParameters.Select(static tp => (TypeSymbol)tp).ToImmutableArray();

        if (!typeArguments.IsDefault)
            return typeArguments;

        return ImmutableArray<TypeSymbol>.Empty;
    }

    private static bool IsLocalFunction(MethodSymbol method)
        => method.DeclaringSyntaxReferences.Any(reference =>
            reference.GetSyntax() is Syntax.FunctionStatementSyntax
            {
                Parent: not Syntax.GlobalStatementSyntax
            });

    private static bool HaveEquivalentDeclarationIdentity(Symbol x, Symbol y)
    {
        if (!x.Locations.IsDefaultOrEmpty && !y.Locations.IsDefaultOrEmpty)
            return HaveEquivalentLocations(x.Locations, y.Locations);

        if (!x.DeclaringSyntaxReferences.IsDefaultOrEmpty && !y.DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return HaveEquivalentSyntaxReferences(x.DeclaringSyntaxReferences, y.DeclaringSyntaxReferences);

        return x.Locations.IsDefaultOrEmpty == y.Locations.IsDefaultOrEmpty
            && x.DeclaringSyntaxReferences.IsDefaultOrEmpty == y.DeclaringSyntaxReferences.IsDefaultOrEmpty;
    }

    private static bool HaveEquivalentLocations(ImmutableArray<Location> x, ImmutableArray<Location> y)
    {
        if (x.Length != y.Length)
            return false;

        for (var i = 0; i < x.Length; i++)
        {
            var xLocation = x[i];
            var yLocation = y[i];

            if (!string.Equals(xLocation.SourceTree?.FilePath, yLocation.SourceTree?.FilePath, StringComparison.OrdinalIgnoreCase))
                return false;

            if (xLocation.SourceSpan != yLocation.SourceSpan)
                return false;
        }

        return true;
    }

    private static bool HaveEquivalentSyntaxReferences(ImmutableArray<SyntaxReference> x, ImmutableArray<SyntaxReference> y)
    {
        if (x.Length != y.Length)
            return false;

        for (var i = 0; i < x.Length; i++)
        {
            if (!string.Equals(x[i].SyntaxTree.FilePath, y[i].SyntaxTree.FilePath, StringComparison.OrdinalIgnoreCase))
                return false;

            if (x[i].Span != y[i].Span)
                return false;
        }

        return true;
    }

    private static void AddDeclarationIdentityHash(ref HashCode hash, Symbol symbol)
    {
        if (!symbol.Locations.IsDefaultOrEmpty)
        {
            for (var i = 0; i < symbol.Locations.Length; i++)
            {
                var location = symbol.Locations[i];
                if (location is null)
                    continue;

                hash.Add(location.SourceTree?.FilePath, StringComparer.OrdinalIgnoreCase);
                hash.Add(location.SourceSpan);
            }

            return;
        }

        if (!symbol.DeclaringSyntaxReferences.IsDefaultOrEmpty)
        {
            for (var i = 0; i < symbol.DeclaringSyntaxReferences.Length; i++)
            {
                var reference = symbol.DeclaringSyntaxReferences[i];
                if (reference is null)
                    continue;

                hash.Add(reference.SyntaxTree.FilePath, StringComparer.OrdinalIgnoreCase);
                hash.Add(reference.Span);
            }
        }
    }

    private readonly struct SymbolPair : IEquatable<SymbolPair>
    {
        public SymbolPair(Symbol first, Symbol second)
        {
            First = first;
            Second = second;
        }

        public Symbol First { get; }

        public Symbol Second { get; }

        public bool Equals(SymbolPair other)
        {
            return ReferenceEquals(First, other.First) && ReferenceEquals(Second, other.Second);
        }

        public override bool Equals(object? obj)
        {
            return obj is SymbolPair other && Equals(other);
        }

        public override int GetHashCode()
        {
            var hash = new HashCode();
            hash.Add(RuntimeHelpers.GetHashCode(First));
            hash.Add(RuntimeHelpers.GetHashCode(Second));
            return hash.ToHashCode();
        }
    }

    private sealed class SymbolReferenceComparer : IEqualityComparer<Symbol>
    {
        public static SymbolReferenceComparer Instance { get; } = new();

        private SymbolReferenceComparer()
        {
        }

        public bool Equals(Symbol? x, Symbol? y) => ReferenceEquals(x, y);

        public int GetHashCode(Symbol obj) => RuntimeHelpers.GetHashCode(obj);
    }
}
