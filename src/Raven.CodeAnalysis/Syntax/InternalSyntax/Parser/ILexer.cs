
namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal interface ILexer
{
    Action<DiagnosticInfo>? DiagnosticSink { get; set; }

    bool IsEndOfFile { get; }

    Token ReadToken();

    IEnumerable<Token> ReadTokens(int count);

    void ReadAndDiscardTokens(int count);

    Token PeekToken(int index = 0);

    void ResetToPosition(int length);

    /// <summary>
    /// Create a checkpoint
    /// </summary>
    void CreateCheckpoint();

    /// <summary>
    /// Rewind to the last checkpoint
    /// </summary>
    void RewindToCheckpoint();


    /// <summary>
    /// Clears the last checkpoint
    /// </summary>
    void ClearCheckpoint();
}

internal interface IMacroBodyScanner
{
    Token ReadMacroBody(out bool isTerminated);
}
