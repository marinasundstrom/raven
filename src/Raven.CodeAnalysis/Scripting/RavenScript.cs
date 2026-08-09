namespace Raven.CodeAnalysis.Scripting;

/// <summary>
/// Creates and executes Raven scripts.
/// </summary>
public static class RavenScript
{
    /// <summary>
    /// Creates a Raven script without executing it.
    /// </summary>
    public static Script Create(string code, ScriptOptions? options = null)
    {
        ArgumentNullException.ThrowIfNull(code);
        return new Script(code, options ?? ScriptOptions.Default);
    }

    /// <summary>
    /// Compiles and executes a Raven script.
    /// </summary>
    public static Task<ScriptState> RunAsync(
        string code,
        ScriptOptions? options = null,
        CancellationToken cancellationToken = default)
        => Create(code, options).RunAsync(cancellationToken);
}
