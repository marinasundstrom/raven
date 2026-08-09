namespace Raven.CodeAnalysis;

/// <summary>
/// Describes the submission chain associated with a script compilation.
/// </summary>
public sealed class ScriptCompilationInfo
{
    internal ScriptCompilationInfo(Compilation? previousScriptCompilation)
    {
        PreviousScriptCompilation = previousScriptCompilation;
    }

    /// <summary>
    /// Gets the script compilation that immediately precedes this submission, if any.
    /// </summary>
    public Compilation? PreviousScriptCompilation { get; }
}
