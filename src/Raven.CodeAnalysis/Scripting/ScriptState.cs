namespace Raven.CodeAnalysis.Scripting;

/// <summary>
/// Represents the executed state of a Raven script submission chain.
/// </summary>
public sealed class ScriptState : IDisposable
{
    private readonly ScriptExecutionSession _session;

    internal ScriptState(
        ScriptExecutionSession session,
        Script script,
        Compilation compilation,
        MetadataReference emittedReference)
    {
        _session = session;
        Script = script;
        Compilation = compilation;
        EmittedReference = emittedReference;
    }

    /// <summary>
    /// Gets the script that produced this state.
    /// </summary>
    public Script Script { get; }

    /// <summary>
    /// Gets the compilation for this submission.
    /// </summary>
    public Compilation Compilation { get; }

    internal MetadataReference EmittedReference { get; }

    /// <summary>
    /// Compiles and executes another submission in this state.
    /// </summary>
    public Task<ScriptState> ContinueWithAsync(
        string code,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(code);
        return _session.ExecuteAsync(new Script(code, Script.Options), this, cancellationToken);
    }

    /// <summary>
    /// Releases the execution resources for the entire submission chain.
    /// </summary>
    public void Dispose()
        => _session.Dispose();
}
