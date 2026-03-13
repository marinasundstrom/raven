using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

public interface IRavenMacroPlugin
{
    string Name { get; }

    ImmutableArray<IMacroDefinition> GetMacros();
}
