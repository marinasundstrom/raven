using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

internal sealed class IntrinsicMacroPlugin : IRavenMacroPlugin
{
    public static IntrinsicMacroPlugin Instance { get; } = new();

    private IntrinsicMacroPlugin()
    {
    }

    public string Name => "Raven.Compiler";

    public ImmutableArray<IMacroDefinition> GetMacros()
        => [IntrinsicQuoteMacro.Instance];
}
