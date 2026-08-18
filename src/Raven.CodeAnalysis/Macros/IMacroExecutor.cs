namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Executes a macro through the compiler's erased invocation boundary.
/// </summary>
public interface IMacroExecutor : IMacroDefinition
{
    System.Collections.Immutable.ImmutableArray<string> TypeParameters =>
        MacroExecutorMetadata.GetTypeParameters(GetType());

    System.Collections.Immutable.ImmutableArray<MacroExecutorParameter> Parameters =>
        MacroExecutorMetadata.GetParameters(GetType());

    MacroApplicationKind ApplicationKind { get; }

    bool HasTokenBody => false;

    MacroTarget Targets => MacroTarget.None;

    MacroExecutionResult Expand(MacroExecutionContext context);
}
