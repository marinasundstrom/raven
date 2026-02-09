using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

[Flags]
public enum ExtensionMemberKinds
{
    None = 0,
    InstanceMethods = 1 << 0,
    InstanceProperties = 1 << 1,
    StaticMethods = 1 << 2,
    StaticProperties = 1 << 3,
    All = InstanceMethods | InstanceProperties | StaticMethods | StaticProperties,
}

public readonly struct ExtensionMemberLookupResult
{
    public static ExtensionMemberLookupResult Empty { get; } = new(
        ImmutableArray<IMethodSymbol>.Empty,
        ImmutableArray<IPropertySymbol>.Empty,
        ImmutableArray<IMethodSymbol>.Empty,
        ImmutableArray<IPropertySymbol>.Empty);

    public ExtensionMemberLookupResult(
        ImmutableArray<IMethodSymbol> instanceMethods,
        ImmutableArray<IPropertySymbol> instanceProperties,
        ImmutableArray<IMethodSymbol> staticMethods,
        ImmutableArray<IPropertySymbol> staticProperties)
    {
        InstanceMethods = instanceMethods.IsDefault ? ImmutableArray<IMethodSymbol>.Empty : instanceMethods;
        InstanceProperties = instanceProperties.IsDefault ? ImmutableArray<IPropertySymbol>.Empty : instanceProperties;
        StaticMethods = staticMethods.IsDefault ? ImmutableArray<IMethodSymbol>.Empty : staticMethods;
        StaticProperties = staticProperties.IsDefault ? ImmutableArray<IPropertySymbol>.Empty : staticProperties;
    }

    public ImmutableArray<IMethodSymbol> InstanceMethods { get; }

    public ImmutableArray<IPropertySymbol> InstanceProperties { get; }

    public ImmutableArray<IMethodSymbol> StaticMethods { get; }

    public ImmutableArray<IPropertySymbol> StaticProperties { get; }

    public bool IsEmpty =>
        InstanceMethods.IsDefaultOrEmpty &&
        InstanceProperties.IsDefaultOrEmpty &&
        StaticMethods.IsDefaultOrEmpty &&
        StaticProperties.IsDefaultOrEmpty;
}

internal static class ExtensionMemberLookup
{
    public static ExtensionMemberLookupResult Lookup(
        Binder binder,
        ITypeSymbol receiverType,
        string? name = null,
        bool includePartialMatches = false,
        ExtensionMemberKinds kinds = ExtensionMemberKinds.All)
    {
        ArgumentNullException.ThrowIfNull(binder);

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error || kinds == ExtensionMemberKinds.None)
            return ExtensionMemberLookupResult.Empty;

        var instanceMethods = kinds.HasFlag(ExtensionMemberKinds.InstanceMethods)
            ? DistinctSymbols(binder.LookupExtensionMethods(name, receiverType, includePartialMatches))
            : ImmutableArray<IMethodSymbol>.Empty;

        var instanceProperties = kinds.HasFlag(ExtensionMemberKinds.InstanceProperties)
            ? DistinctSymbols(binder.LookupExtensionProperties(name, receiverType, includePartialMatches))
            : ImmutableArray<IPropertySymbol>.Empty;

        var staticMethods = kinds.HasFlag(ExtensionMemberKinds.StaticMethods)
            ? DistinctSymbols(binder.LookupExtensionStaticMethods(name, receiverType, includePartialMatches))
            : ImmutableArray<IMethodSymbol>.Empty;

        var staticProperties = kinds.HasFlag(ExtensionMemberKinds.StaticProperties)
            ? DistinctSymbols(binder.LookupExtensionStaticProperties(name, receiverType, includePartialMatches))
            : ImmutableArray<IPropertySymbol>.Empty;

        return new ExtensionMemberLookupResult(instanceMethods, instanceProperties, staticMethods, staticProperties);
    }

    private static ImmutableArray<TSymbol> DistinctSymbols<TSymbol>(IEnumerable<TSymbol> symbols)
        where TSymbol : class, ISymbol
    {
        var seen = new HashSet<TSymbol>(SymbolEqualityComparer.Default);
        var builder = ImmutableArray.CreateBuilder<TSymbol>();

        foreach (var symbol in symbols)
        {
            if (!seen.Add(symbol))
                continue;

            builder.Add(symbol);
        }

        return builder.ToImmutable();
    }
}
