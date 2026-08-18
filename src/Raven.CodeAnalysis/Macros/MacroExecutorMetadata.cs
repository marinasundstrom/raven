using System.Collections.Immutable;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Creates portable executor metadata without exposing generated collection
/// conversion details in Raven-authored providers.
/// </summary>
public static class MacroExecutorMetadata
{
    private static readonly ConditionalWeakTable<Type, ExecutorMetadata> s_cache = new();

    public static ImmutableArray<string> GetTypeParameters(Type executorType)
        => GetMetadata(executorType).TypeParameters;

    public static ImmutableArray<MacroExecutorParameter> GetParameters(Type executorType)
        => GetMetadata(executorType).Parameters;

    public static ImmutableArray<string> CreateTypeParameters(params string[] names)
        => ImmutableArray.Create(names);

    public static ImmutableArray<MacroExecutorParameter> CreateParameters(
        params MacroExecutorParameter[] parameters)
        => ImmutableArray.Create(parameters);

    private static ExecutorMetadata GetMetadata(Type executorType)
    {
        ArgumentNullException.ThrowIfNull(executorType);
        return s_cache.GetValue(executorType, static type => new ExecutorMetadata(
            type.GetCustomAttributes<MacroExecutorTypeParameterAttribute>(inherit: false)
                .OrderBy(static attribute => attribute.Ordinal)
                .Select(static attribute => attribute.Name)
                .ToImmutableArray(),
            type.GetCustomAttributes<MacroExecutorParameterAttribute>(inherit: false)
                .Select(static attribute => attribute.Parameter)
                .OrderBy(static parameter => parameter.DeclarationOrdinal)
                .ToImmutableArray()));
    }

    private sealed record ExecutorMetadata(
        ImmutableArray<string> TypeParameters,
        ImmutableArray<MacroExecutorParameter> Parameters);
}
