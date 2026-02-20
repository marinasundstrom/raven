using System;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public sealed class SymbolEqualityComparer : IEqualityComparer<ISymbol>
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

    public bool Equals(ISymbol? x, ISymbol? y)
    {
        return EqualsCore(x, y, visited: null);
    }

    private bool EqualsCore(ISymbol? x, ISymbol? y, HashSet<SymbolPair>? visited)
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

        if (x is ITypeSymbol typeX && y is ITypeSymbol typeY)
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

        if (x is ITypeParameterSymbol xTypeParameter && y is ITypeParameterSymbol yTypeParameter)
        {
            if (xTypeParameter.OwnerKind != yTypeParameter.OwnerKind)
                return false;
        }

        visited ??= new HashSet<SymbolPair>();
        var pair = new SymbolPair(x, y);
        if (!visited.Add(pair))
            return true;
        visited.Add(new SymbolPair(y, x));

        // >>> NEW BLOCK: relaxed comparison for type parameters
        if (_ignoreContainingNamespaceOrType &&
            x is ITypeParameterSymbol tpx &&
            y is ITypeParameterSymbol tpy)
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

        if (x is IParameterSymbol px && y is IParameterSymbol py)
        {
            if (ReferenceEquals(px, py))
                return true;

            if (px.RefKind != py.RefKind)
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

        if (!string.Equals(x.Name, y.Name, StringComparison.Ordinal))
            return false;

        if (!string.Equals(x.MetadataName, y.MetadataName, StringComparison.Ordinal))
            return false;

        if (!_ignoreContainingNamespaceOrType)
        {
            if (!EqualsCore(x.ContainingNamespace, y.ContainingNamespace, visited))
                return false;

            if (!EqualsCore(x.ContainingSymbol, y.ContainingSymbol, visited))
                return false;
        }

        if (x is INamedTypeSymbol namedTypeX && y is INamedTypeSymbol namedTypeY)
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

        if (x is IArrayTypeSymbol arrayX && y is IArrayTypeSymbol arrayY)
        {
            if (arrayX.Rank != arrayY.Rank)
                return false;

            if (!EqualsCore(arrayX.ElementType, arrayY.ElementType, visited))
                return false;
        }

        if (x is IPointerTypeSymbol pointerX && y is IPointerTypeSymbol pointerY)
        {
            if (!EqualsCore(pointerX.PointedAtType, pointerY.PointedAtType, visited))
                return false;
        }

        if (x is IAddressTypeSymbol addressX && y is IAddressTypeSymbol addressY)
        {
            if (!EqualsCore(addressX.ReferencedType, addressY.ReferencedType, visited))
                return false;
        }

        if (x is IFieldSymbol fieldX && y is IFieldSymbol fieldY)
        {
            if (!EqualsCore(fieldX.Type, fieldY.Type, visited))
                return false;

            if (fieldX.IsConst != fieldY.IsConst)
                return false;
        }

        if (x is IPropertySymbol propertyX && y is IPropertySymbol propertyY)
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

        if (x is IMethodSymbol methodX && y is IMethodSymbol methodY)
        {
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
                typeArgumentsX = ImmutableArray<ITypeSymbol>.Empty;

            if (typeArgumentsY.IsDefault)
                typeArgumentsY = ImmutableArray<ITypeSymbol>.Empty;

            if (typeArgumentsX.Length != typeArgumentsY.Length)
                return false;

            for (var i = 0; i < typeArgumentsX.Length; i++)
            {
                if (!EqualsCore(typeArgumentsX[i], typeArgumentsY[i], visited))
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

    private static bool TryGetParameterOrdinal(IParameterSymbol parameter, out int ordinal)
    {
        if (parameter.ContainingSymbol is IMethodSymbol method)
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

        ordinal = -1;
        return false;
    }

    public int GetHashCode(ISymbol obj)
    {
        if (obj is null)
            return 0;

        return GetHashCodeCore(obj, visited: null);
    }

    private int GetHashCodeCore(ISymbol obj, HashSet<ISymbol>? visited)
    {
        obj = Normalize(obj);

        visited ??= new HashSet<ISymbol>(SymbolReferenceComparer.Instance);
        if (!visited.Add(obj))
            return 0;

        var hash = new HashCode();
        hash.Add(obj.Kind);

        if (obj is ITypeSymbol typeSymbol)
        {
            hash.Add(typeSymbol.TypeKind);

            if (typeSymbol.SpecialType != SpecialType.None &&
                IsSimpleSpecialType(typeSymbol.SpecialType))
            {
                hash.Add(typeSymbol.SpecialType);
                return hash.ToHashCode();
            }
        }

        // >>> NEW BLOCK: type parameter hashing in relaxed mode
        if (_ignoreContainingNamespaceOrType &&
            obj is ITypeParameterSymbol tp)
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

        if (obj is ITypeParameterSymbol typeParameterSymbol)
        {
            hash.Add(typeParameterSymbol.Name, StringComparer.Ordinal);
            hash.Add(typeParameterSymbol.Ordinal);
            hash.Add((int)typeParameterSymbol.Variance);
            hash.Add((int)typeParameterSymbol.OwnerKind);
            return hash.ToHashCode();
        }

        hash.Add(obj.Name, StringComparer.Ordinal);
        hash.Add(obj.MetadataName, StringComparer.Ordinal);

        if (!_ignoreContainingNamespaceOrType && obj.ContainingNamespace is { } containingNamespace)
            hash.Add(containingNamespace.Name, StringComparer.Ordinal);

        if (!_ignoreContainingNamespaceOrType && obj.ContainingSymbol is { } containingSymbol)
            hash.Add(GetHashCodeCore(containingSymbol, visited));

        if (obj is IParameterSymbol parameterSymbol)
        {
            hash.Add(parameterSymbol.RefKind);
            hash.Add(GetHashCodeCore(parameterSymbol.Type, visited));
            return hash.ToHashCode();
        }

        if (obj is IMethodSymbol method)
        {
            hash.Add(method.Parameters.Length);
            hash.Add(GetHashCodeCore(method.ReturnType, visited));

            for (var i = 0; i < method.Parameters.Length; i++)
                hash.Add(GetHashCodeCore(method.Parameters[i], visited));

            var typeArguments = method.TypeArguments;
            if (typeArguments.IsDefault)
                typeArguments = ImmutableArray<ITypeSymbol>.Empty;

            hash.Add(typeArguments.Length);
            for (var i = 0; i < typeArguments.Length; i++)
                hash.Add(GetHashCodeCore(typeArguments[i], visited));
        }

        if (obj is INamedTypeSymbol namedType)
        {
            var typeArguments = GetTypeArgumentsOrParameters(namedType);
            hash.Add(namedType.Arity);
            hash.Add(typeArguments.Length);
            hash.Add(namedType.TypeKind);

            for (var i = 0; i < typeArguments.Length; i++)
                hash.Add(GetHashCodeCore(typeArguments[i], visited));
        }

        if (obj is IArrayTypeSymbol arrayType)
            hash.Add(arrayType.Rank);

        if (obj is IFieldSymbol field)
        {
            hash.Add(field.IsConst);
            hash.Add(GetHashCodeCore(field.Type, visited));
        }

        if (obj is IPropertySymbol property)
        {
            hash.Add(property.IsIndexer);
            hash.Add(property.GetMethod is not null);
            hash.Add(property.SetMethod is not null);
            hash.Add(GetHashCodeCore(property.Type, visited));
        }

        return hash.ToHashCode();
    }

    private ISymbol Normalize(ISymbol symbol)
    {
        var current = symbol;

        while (true)
        {
            if (current.IsAlias)
            {
                current = current.UnderlyingSymbol;
                continue;
            }

            return current;
        }
    }

    private static INamedTypeSymbol GetGenericDefinition(INamedTypeSymbol symbol)
    {
        if (symbol.ConstructedFrom is INamedTypeSymbol constructedFrom && !ReferenceEquals(symbol, constructedFrom))
            return constructedFrom;

        if (symbol.OriginalDefinition is INamedTypeSymbol original && !ReferenceEquals(symbol, original))
            return original;

        return symbol;
    }

    private static ImmutableArray<ITypeSymbol> GetTypeArgumentsOrParameters(INamedTypeSymbol symbol)
    {
        if (symbol is ConstructedNamedTypeSymbol constructed)
            return constructed.GetExplicitTypeArgumentsForInference();

        var typeArguments = symbol.TypeArguments;
        if (!typeArguments.IsDefault && typeArguments.Length > 0)
            return typeArguments;

        if (!symbol.TypeParameters.IsDefault && symbol.TypeParameters.Length > 0)
            return symbol.TypeParameters.Select(static tp => (ITypeSymbol)tp).ToImmutableArray();

        if (!typeArguments.IsDefault)
            return typeArguments;

        return ImmutableArray<ITypeSymbol>.Empty;
    }

    private readonly struct SymbolPair : IEquatable<SymbolPair>
    {
        public SymbolPair(ISymbol first, ISymbol second)
        {
            First = first;
            Second = second;
        }

        public ISymbol First { get; }

        public ISymbol Second { get; }

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

    private sealed class SymbolReferenceComparer : IEqualityComparer<ISymbol>
    {
        public static SymbolReferenceComparer Instance { get; } = new();

        private SymbolReferenceComparer()
        {
        }

        public bool Equals(ISymbol? x, ISymbol? y) => ReferenceEquals(x, y);

        public int GetHashCode(ISymbol obj) => RuntimeHelpers.GetHashCode(obj);
    }
}
