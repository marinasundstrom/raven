namespace Raven.CodeAnalysis;

/// <summary>
/// Describes the submission chain associated with a script compilation.
/// </summary>
public sealed class ScriptCompilationInfo
{
    internal ScriptCompilationInfo(
        Compilation? previousScriptCompilation,
        MetadataReference? previousScriptCompilationReference)
    {
        PreviousScriptCompilation = previousScriptCompilation;
        PreviousScriptCompilationReference = previousScriptCompilationReference;
    }

    /// <summary>
    /// Gets the script compilation that immediately precedes this submission, if any.
    /// </summary>
    public Compilation? PreviousScriptCompilation { get; }

    /// <summary>
    /// Gets the emitted metadata reference for the immediately preceding submission, if available.
    /// </summary>
    public MetadataReference? PreviousScriptCompilationReference { get; }
}
