using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Scripting;

/// <summary>
/// Represents one unexecuted Raven script submission.
/// </summary>
public sealed class Script
{
    internal Script(string code, ScriptOptions options)
    {
        Code = code;
        Options = options;
    }

    /// <summary>
    /// Gets the source text for this submission.
    /// </summary>
    public string Code { get; }

    /// <summary>
    /// Gets the options used to compile this submission and its continuations.
    /// </summary>
    public ScriptOptions Options { get; }

    /// <summary>
    /// Compiles the submission and returns its diagnostics without executing it.
    /// </summary>
    public ImmutableArray<Diagnostic> Compile(CancellationToken cancellationToken = default)
        => CreateCompilation().GetDiagnostics(cancellationToken: cancellationToken);

    /// <summary>
    /// Compiles and executes this submission in a new script session.
    /// </summary>
    public async Task<ScriptState> RunAsync(CancellationToken cancellationToken = default)
    {
        var session = new ScriptExecutionSession(Options);
        try
        {
            return await session.ExecuteAsync(this, null, cancellationToken).ConfigureAwait(false);
        }
        catch
        {
            session.Dispose();
            throw;
        }
    }

    internal Compilation CreateCompilation(
        Compilation? previousCompilation = null,
        MetadataReference? previousReference = null)
    {
        var syntaxTree = SyntaxTree.ParseText(
            Code,
            new ParseOptions { Kind = SourceCodeKind.Script },
            path: $"<submission-{Guid.NewGuid():N}>");

        return Compilation.CreateScriptCompilation(
            $"Raven.Script.{Guid.NewGuid():N}",
            syntaxTree,
            Options.MetadataReferences.ToArray(),
            new CompilationOptions(OutputKind.ConsoleApplication),
            previousCompilation,
            previousReference);
    }
}
