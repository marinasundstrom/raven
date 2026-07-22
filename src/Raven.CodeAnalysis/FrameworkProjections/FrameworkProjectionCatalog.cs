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
        foreach (var candidate in s_standard.Value)
        {
            if (MatchesReceiver(candidate, receiverType) &&
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
        => ResolveStandardMethods(compilation, receiverType, memberName).Methods;

    public static FrameworkProjectionResolution ResolveStandardMethods(
        Compilation compilation,
        ITypeSymbol receiverType,
        string? memberName = null)
    {
        if (receiverType is not INamedTypeSymbol namedReceiverType)
            return FrameworkProjectionResolution.Empty;

        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();
        var failures = ImmutableArray.CreateBuilder<FrameworkProjectionFailure>();

        foreach (var descriptor in s_standard.Value)
        {
            if (memberName is not null && !string.Equals(descriptor.MemberName, memberName, StringComparison.Ordinal))
                continue;
            if (!MatchesReceiver(descriptor, receiverType))
                continue;
            var receiverDefinition = namedReceiverType.OriginalDefinition as INamedTypeSymbol ?? namedReceiverType;
            if (!HasExactSourceSignature(receiverDefinition, descriptor.SourceSignature))
            {
                failures.Add(new(descriptor.Id, "source framework signature does not match the catalog descriptor"));
                continue;
            }
            if (compilation.GetTypeByMetadataName(descriptor.ProjectedContainer) is not { } containerDefinition)
            {
                failures.Add(new(descriptor.Id, $"bridge container '{descriptor.ProjectedContainer}' was not found"));
                continue;
            }

            var adapterDefinitions = containerDefinition.GetMembers(descriptor.MemberName)
                .OfType<IMethodSymbol>()
                .Where(method => HasProjectionId(method, descriptor.Id))
                .ToImmutableArray();
            if (adapterDefinitions.IsDefaultOrEmpty)
            {
                failures.Add(new(descriptor.Id, "no bridge carries the matching projection ID"));
                continue;
            }
            if (adapterDefinitions.Length > 1)
            {
                failures.Add(new(descriptor.Id, "multiple bridges carry the same projection ID"));
                continue;
            }

            var adapterDefinition = adapterDefinitions[0];
            if (!string.Equals(adapterDefinition.ContainingAssembly?.Name, "Raven.Core", StringComparison.Ordinal) ||
                !HasExactProjectedSignature(adapterDefinition, descriptor.ProjectedSignature))
            {
                failures.Add(new(descriptor.Id, "bridge signature does not match the catalog descriptor"));
                continue;
            }

            var container = ConstructBridgeContainer(containerDefinition, namedReceiverType);
            if (container is null)
            {
                failures.Add(new(descriptor.Id, "bridge generic arity does not match the catalog receiver"));
                continue;
            }

            var adapter = container.GetMembers(descriptor.MemberName)
                .OfType<IMethodSymbol>()
                .SingleOrDefault(method => SymbolEqualityComparer.Default.Equals(
                    method.OriginalDefinition ?? method,
                    adapterDefinition));
            if (adapter is null)
            {
                failures.Add(new(descriptor.Id, "constructed bridge does not contain the validated adapter"));
                continue;
            }

            if (adapter.IsGenericMethod)
            {
                if (adapter.Arity != namedReceiverType.TypeArguments.Length)
                {
                    failures.Add(new(descriptor.Id, "bridge method generic arity does not match the catalog receiver"));
                    continue;
                }

                adapter = adapter.Construct(namedReceiverType.TypeArguments.ToArray());
            }
            if (!SymbolEqualityComparer.Default.Equals(adapter.GetExtensionReceiverType(), receiverType))
            {
                failures.Add(new(descriptor.Id, "bridge receiver does not match the catalog receiver"));
                continue;
            }

            builder.Add(new ProjectedMethodSymbol(namedReceiverType, adapter));
        }

        return new(builder.ToImmutable(), failures.ToImmutable());
    }

    private static bool HasExactSourceSignature(INamedTypeSymbol receiverType, string expectedSignature) =>
        receiverType.GetMembers()
            .OfType<PEMethodSymbol>()
            .Any(method => string.Equals(GetReflectedSignature(method, includeContainingType: true), expectedSignature, StringComparison.Ordinal));

    private static bool HasExactProjectedSignature(IMethodSymbol method, string expectedSignature) =>
        method is PEMethodSymbol peMethod &&
        string.Equals(GetReflectedSignature(peMethod, includeContainingType: false), expectedSignature, StringComparison.Ordinal);

    private static string GetReflectedSignature(PEMethodSymbol method, bool includeContainingType)
    {
        var methodBase = method.GetMethodBase();
        var returnType = methodBase is MethodInfo methodInfo
            ? GetReflectedTypeName(methodInfo.ReturnType)
            : "System.Void";
        var owner = includeContainingType
            ? $"{methodBase.DeclaringType?.FullName}."
            : string.Empty;
        var parameters = string.Join(", ", methodBase.GetParameters().Select(GetReflectedParameterName));
        return $"{returnType} {owner}{methodBase.Name}({parameters})";
    }

    private static string GetReflectedParameterName(ParameterInfo parameter)
    {
        var prefix = parameter.IsOut
            ? "out "
            : parameter.IsIn
                ? "in "
                : parameter.ParameterType.IsByRef
                    ? "ref "
                    : string.Empty;
        return $"{prefix}{GetReflectedTypeName(parameter.ParameterType)}";
    }

    private static string GetReflectedTypeName(Type type)
    {
        if (type.IsByRef)
            type = type.GetElementType()!;
        if (type.IsArray)
            return $"{GetReflectedTypeName(type.GetElementType()!)}[{new string(',', type.GetArrayRank() - 1)}]";
        if (!type.IsGenericType)
            return type.FullName ?? type.Name;

        var definitionName = type.GetGenericTypeDefinition().FullName ?? type.Name;
        var tick = definitionName.IndexOf('`');
        if (tick >= 0)
            definitionName = definitionName[..tick];
        return $"{definitionName}<{string.Join(", ", type.GetGenericArguments().Select(GetReflectedTypeName))}>";
    }

    internal static bool IsProjectionAdapter(IMethodSymbol method) =>
        method.GetAttributes().Any(static attribute =>
            string.Equals(
                $"{attribute.AttributeClass.ContainingNamespace?.ToMetadataName()}.{attribute.AttributeClass.MetadataName}",
                "System.Runtime.CompilerServices.FrameworkProjectionAttribute",
                StringComparison.Ordinal));

    private static bool HasProjectionId(IMethodSymbol method, string projectionId) =>
        method.GetAttributes().Any(attribute =>
            string.Equals(
                $"{attribute.AttributeClass.ContainingNamespace?.ToMetadataName()}.{attribute.AttributeClass.MetadataName}",
                "System.Runtime.CompilerServices.FrameworkProjectionAttribute",
                StringComparison.Ordinal) &&
            attribute.ConstructorArguments is [{ Value: string adapterId }] &&
            string.Equals(adapterId, projectionId, StringComparison.Ordinal));

    private static bool MatchesReceiver(FrameworkProjectionDescriptor descriptor, ITypeSymbol receiverType)
    {
        if (receiverType is INamedTypeSymbol namedType && namedType.OriginalDefinition is INamedTypeSymbol originalDefinition)
            receiverType = originalDefinition;

        var namespaceName = receiverType.ContainingNamespace?.ToMetadataName();
        var receiverMetadataName = string.IsNullOrEmpty(namespaceName)
            ? receiverType.MetadataName
            : $"{namespaceName}.{receiverType.MetadataName}";
        return string.Equals(descriptor.ReceiverType, receiverMetadataName, StringComparison.Ordinal);
    }

    private static INamedTypeSymbol? ConstructBridgeContainer(
        INamedTypeSymbol containerDefinition,
        INamedTypeSymbol receiverType)
    {
        if (containerDefinition.Arity == 0)
            return containerDefinition;

        if (containerDefinition.Arity != receiverType.TypeArguments.Length)
            return null;

        return (INamedTypeSymbol)containerDefinition.Construct(receiverType.TypeArguments.ToArray());
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
            .Where(static entry => !string.IsNullOrWhiteSpace(entry.Id))
            .GroupBy(static entry => entry.Id, StringComparer.Ordinal)
            .Where(static group => group.Count() == 1)
            .Select(static group => group.Single())
            .Select(static entry => new FrameworkProjectionDescriptor(
                entry.Id,
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

internal readonly record struct FrameworkProjectionResolution(
    ImmutableArray<IMethodSymbol> Methods,
    ImmutableArray<FrameworkProjectionFailure> Failures)
{
    public static FrameworkProjectionResolution Empty { get; } = new([], []);
}

internal readonly record struct FrameworkProjectionFailure(string ProjectionId, string Reason);

internal sealed record FrameworkProjectionDescriptor(
    string Id,
    string ReceiverType,
    string MemberName,
    string SourceSignature,
    string ProjectedContainer,
    string ProjectedSignature,
    string Lowering,
    ImmutableArray<FrameworkProjectionException> Exceptions);

internal sealed record FrameworkProjectionException(string Type, string? Case, string? Behavior);

internal sealed class FrameworkProjectionCatalogFile
{
    public int Version { get; init; }
    public FrameworkProjectionCatalogEntry[]? Projections { get; init; }
}

internal sealed class FrameworkProjectionCatalogEntry
{
    public string Id { get; init; } = string.Empty;
    public string ReceiverType { get; init; } = string.Empty;
    public string Source { get; init; } = string.Empty;
    public string ProjectedContainer { get; init; } = string.Empty;
    public string ProjectedSignature { get; init; } = string.Empty;
    public string Lowering { get; init; } = string.Empty;
    public FrameworkProjectionException[]? Exceptions { get; init; }
}
