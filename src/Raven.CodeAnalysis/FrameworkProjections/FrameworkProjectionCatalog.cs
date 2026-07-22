using System.Collections.Immutable;
using System.Reflection;
using System.Text.Json;

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
