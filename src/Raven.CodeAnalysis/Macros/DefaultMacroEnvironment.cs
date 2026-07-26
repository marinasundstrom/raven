using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

internal static class DefaultMacroEnvironment
{
    public static ImmutableArray<IRavenMacroPlugin> Plugins { get; } =
        [IntrinsicMacroPlugin.Instance];
}
