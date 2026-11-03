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
        ImmutableArray<MethodSymbol>.Empty,
        ImmutableArray<PropertySymbol>.Empty,
        ImmutableArray<MethodSymbol>.Empty,
        ImmutableArray<PropertySymbol>.Empty);

    public ExtensionMemberLookupResult(
        ImmutableArray<MethodSymbol> instanceMethods,
        ImmutableArray<PropertySymbol> instanceProperties,
        ImmutableArray<MethodSymbol> staticMethods,
        ImmutableArray<PropertySymbol> staticProperties)
    {
        InstanceMethods = instanceMethods.IsDefault ? ImmutableArray<MethodSymbol>.Empty : instanceMethods;
        InstanceProperties = instanceProperties.IsDefault ? ImmutableArray<PropertySymbol>.Empty : instanceProperties;
        StaticMethods = staticMethods.IsDefault ? ImmutableArray<MethodSymbol>.Empty : staticMethods;
        StaticProperties = staticProperties.IsDefault ? ImmutableArray<PropertySymbol>.Empty : staticProperties;
    }

    public ImmutableArray<MethodSymbol> InstanceMethods { get; }

    public ImmutableArray<PropertySymbol> InstanceProperties { get; }

    public ImmutableArray<MethodSymbol> StaticMethods { get; }

    public ImmutableArray<PropertySymbol> StaticProperties { get; }

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
        TypeSymbol receiverType,
        string? name = null,
        bool includePartialMatches = false,
        ExtensionMemberKinds kinds = ExtensionMemberKinds.All)
    {
        ArgumentNullException.ThrowIfNull(binder);

        if (receiverType is null || receiverType.TypeKind == TypeKind.Error || kinds == ExtensionMemberKinds.None)
            return ExtensionMemberLookupResult.Empty;

        var instanceMethods = kinds.HasFlag(ExtensionMemberKinds.InstanceMethods)
            ? DistinctSymbols(binder.LookupExtensionMethods(name, receiverType, includePartialMatches))
            : ImmutableArray<MethodSymbol>.Empty;

        var instanceProperties = kinds.HasFlag(ExtensionMemberKinds.InstanceProperties)
            ? DistinctSymbols(binder.LookupExtensionProperties(name, receiverType, includePartialMatches))
            : ImmutableArray<PropertySymbol>.Empty;

        var staticMethods = kinds.HasFlag(ExtensionMemberKinds.StaticMethods)
            ? DistinctSymbols(binder.LookupExtensionStaticMethods(name, receiverType, includePartialMatches))
            : ImmutableArray<MethodSymbol>.Empty;

        var staticProperties = kinds.HasFlag(ExtensionMemberKinds.StaticProperties)
            ? DistinctSymbols(binder.LookupExtensionStaticProperties(name, receiverType, includePartialMatches))
            : ImmutableArray<PropertySymbol>.Empty;

        return new ExtensionMemberLookupResult(instanceMethods, instanceProperties, staticMethods, staticProperties);
    }

    public static bool TryGetCached(
        Binder binder,
        TypeSymbol receiverType,
        out ExtensionMemberLookupResult result,
        string? name = null,
        bool includePartialMatches = false,
        ExtensionMemberKinds kinds = ExtensionMemberKinds.All)
    {
        ArgumentNullException.ThrowIfNull(binder);

        result = ExtensionMemberLookupResult.Empty;
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error || kinds == ExtensionMemberKinds.None)
            return false;

        var allRequestedLookupsCached = true;
        var instanceMethods = ImmutableArray<MethodSymbol>.Empty;
        var instanceProperties = ImmutableArray<PropertySymbol>.Empty;
        var staticMethods = ImmutableArray<MethodSymbol>.Empty;
        var staticProperties = ImmutableArray<PropertySymbol>.Empty;

        if (kinds.HasFlag(ExtensionMemberKinds.InstanceMethods))
        {
            if (binder.TryGetCachedExtensionMethods(name, receiverType, includePartialMatches, out instanceMethods))
                instanceMethods = DistinctSymbols(instanceMethods);
            else
                allRequestedLookupsCached = false;
        }

        if (kinds.HasFlag(ExtensionMemberKinds.InstanceProperties))
        {
            if (binder.TryGetCachedExtensionProperties(name, receiverType, includePartialMatches, out instanceProperties))
                instanceProperties = DistinctSymbols(instanceProperties);
            else
                allRequestedLookupsCached = false;
        }

        if (kinds.HasFlag(ExtensionMemberKinds.StaticMethods))
        {
            if (binder.TryGetCachedExtensionStaticMethods(name, receiverType, includePartialMatches, out staticMethods))
                staticMethods = DistinctSymbols(staticMethods);
            else
                allRequestedLookupsCached = false;
        }

        if (kinds.HasFlag(ExtensionMemberKinds.StaticProperties))
        {
            if (binder.TryGetCachedExtensionStaticProperties(name, receiverType, includePartialMatches, out staticProperties))
                staticProperties = DistinctSymbols(staticProperties);
            else
                allRequestedLookupsCached = false;
        }

        result = new ExtensionMemberLookupResult(instanceMethods, instanceProperties, staticMethods, staticProperties);
        return allRequestedLookupsCached;
    }

    private static ImmutableArray<TSymbol> DistinctSymbols<TSymbol>(IEnumerable<TSymbol> symbols)
        where TSymbol : class, Symbol
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<TSymbol>();

        foreach (var symbol in symbols)
        {
            if (!seen.Add(symbol.GetShallowLookupIdentityKey()))
                continue;

            builder.Add(symbol);
        }

        return builder.ToImmutable();
    }
}
