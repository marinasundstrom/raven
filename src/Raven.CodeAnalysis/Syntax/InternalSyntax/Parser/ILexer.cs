
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

internal struct Token
{
    public Token(SyntaxKind kind, string text)
    {
        Kind = kind;
        Text = string.Intern(text);
        Length = text.Length;
    }

    public Token(SyntaxKind kind, string text, object? value)
    {
        Kind = kind;
        Text = string.Intern(text);
        Value = value;
        Length = text.Length;
    }

    public Token(SyntaxKind kind, string text, object? value, int length)
    {
        Kind = kind;
        Text = string.Intern(text);
        Value = value;
        Length = length;
    }

    public SyntaxKind Kind { get; }
    public string Text { get; }
    public object? Value { get; }
    public int Length { get; }

}
