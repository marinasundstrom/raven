using System.Collections.Immutable;
using System.Reflection;
using System.Text.Json;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class FrameworkProjectionCatalog
{
    private const string ResourceName = "Raven.CodeAnalysis.FrameworkProjections.standard.json";
    private static readonly Lazy<ImmutableArray<FrameworkProjectionDescriptor>> s_standard =
        new(LoadStandard, LazyThreadSafetyMode.ExecutionAndPublication);

    public static bool TryGetStandard(
        ITypeSymbol receiverType,
        string memberName,
        out FrameworkProjectionDescriptor descriptor)
    {
        var namespaceName = receiverType.ContainingNamespace?.ToMetadataName();
        var receiverMetadataName = string.IsNullOrEmpty(namespaceName)
            ? receiverType.MetadataName
            : $"{namespaceName}.{receiverType.MetadataName}";

        foreach (var candidate in s_standard.Value)
        {
            if (string.Equals(candidate.ReceiverType, receiverMetadataName, StringComparison.Ordinal) &&
                string.Equals(candidate.MemberName, memberName, StringComparison.Ordinal))
            {
                descriptor = candidate;
                return true;
            }
        }

        descriptor = default!;
        return false;
    }

    public static ImmutableArray<IMethodSymbol> GetStandardMethods(
        Compilation compilation,
        ITypeSymbol receiverType,
        string? memberName = null)
    {
        if (receiverType is not INamedTypeSymbol namedReceiverType)
            return ImmutableArray<IMethodSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var descriptor in s_standard.Value)
        {
            if (memberName is not null && !string.Equals(descriptor.MemberName, memberName, StringComparison.Ordinal))
                continue;
            if (!MatchesReceiver(descriptor, receiverType))
                continue;
            if (compilation.GetTypeByMetadataName(descriptor.ProjectedContainer) is not { } container)
                continue;

            foreach (var adapter in container.GetMembers(descriptor.MemberName).OfType<IMethodSymbol>())
            {
                if (!IsProjectionAdapter(adapter) ||
                    !string.Equals(adapter.ContainingAssembly?.Name, "Raven.Core", StringComparison.Ordinal) ||
                    adapter.Parameters.Length != 1 ||
                    adapter.ReturnType is not INamedTypeSymbol { Name: "Option", Arity: 1 })
                    continue;
                if (!SymbolEqualityComparer.Default.Equals(adapter.GetExtensionReceiverType(), receiverType))
                    continue;

                builder.Add(new ProjectedMethodSymbol(namedReceiverType, adapter));
            }
        }

        return builder.ToImmutable();
    }

    private static bool IsProjectionAdapter(IMethodSymbol method) =>
        method.GetAttributes().Any(static attribute =>
            string.Equals(
                $"{attribute.AttributeClass.ContainingNamespace?.ToMetadataName()}.{attribute.AttributeClass.MetadataName}",
                "System.Runtime.CompilerServices.FrameworkProjectionAttribute",
                StringComparison.Ordinal));

    private static bool MatchesReceiver(FrameworkProjectionDescriptor descriptor, ITypeSymbol receiverType)
    {
        var namespaceName = receiverType.ContainingNamespace?.ToMetadataName();
        var receiverMetadataName = string.IsNullOrEmpty(namespaceName)
            ? receiverType.MetadataName
            : $"{namespaceName}.{receiverType.MetadataName}";
        return string.Equals(descriptor.ReceiverType, receiverMetadataName, StringComparison.Ordinal);
    }

    private static ImmutableArray<FrameworkProjectionDescriptor> LoadStandard()
    {
        using var stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(ResourceName);
        if (stream is null)
            return ImmutableArray<FrameworkProjectionDescriptor>.Empty;

        var catalog = JsonSerializer.Deserialize<FrameworkProjectionCatalogFile>(
            stream,
            new JsonSerializerOptions { PropertyNameCaseInsensitive = true });
        if (catalog?.Projections is null)
            return ImmutableArray<FrameworkProjectionDescriptor>.Empty;

        return catalog.Projections
            .Select(static entry => new FrameworkProjectionDescriptor(
                entry.ReceiverType,
                GetMemberName(entry.Source),
                entry.Source,
                entry.ProjectedContainer,
                entry.ProjectedSignature,
                entry.Lowering,
                entry.Exceptions?.ToImmutableArray() ?? ImmutableArray<FrameworkProjectionException>.Empty))
            .ToImmutableArray();
    }

    private static string GetMemberName(string source)
    {
        var openParen = source.IndexOf('(', StringComparison.Ordinal);
        var lastDot = openParen < 0
            ? source.LastIndexOf(".", StringComparison.Ordinal)
            : source.LastIndexOf('.', openParen);
        return lastDot < 0 || openParen <= lastDot
            ? string.Empty
            : source[(lastDot + 1)..openParen];
    }
}

internal sealed record FrameworkProjectionDescriptor(
    string ReceiverType,
    string MemberName,
    string SourceSignature,
    string ProjectedContainer,
    string ProjectedSignature,
    string Lowering,
    ImmutableArray<FrameworkProjectionException> Exceptions);

internal sealed record FrameworkProjectionException(string Type, string Case);

internal sealed class FrameworkProjectionCatalogFile
{
    public int Version { get; init; }
    public FrameworkProjectionCatalogEntry[]? Projections { get; init; }
}

internal sealed class FrameworkProjectionCatalogEntry
{
    public string ReceiverType { get; init; } = string.Empty;
    public string Source { get; init; } = string.Empty;
    public string ProjectedContainer { get; init; } = string.Empty;
    public string ProjectedSignature { get; init; } = string.Empty;
    public string Lowering { get; init; } = string.Empty;
    public FrameworkProjectionException[]? Exceptions { get; init; }
}
