using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Creates portable executor metadata without exposing generated collection
/// conversion details in Raven-authored providers.
/// </summary>
public static class MacroExecutorMetadata
{
    public static ImmutableArray<string> CreateTypeParameters(params string[] names)
        => ImmutableArray.Create(names);

    public static ImmutableArray<MacroExecutorParameter> CreateParameters(
        params MacroExecutorParameter[] parameters)
        => ImmutableArray.Create(parameters);
}
