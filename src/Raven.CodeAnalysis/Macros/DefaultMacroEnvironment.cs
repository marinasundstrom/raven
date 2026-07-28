using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

internal static class DefaultMacroEnvironment
{
    public static ImmutableArray<IMacroDefinition> Macros { get; } =
        [IntrinsicQuoteMacro.Instance, IntrinsicCompileMacro.Instance];
}
