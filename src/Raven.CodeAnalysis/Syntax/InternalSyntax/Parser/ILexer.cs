
namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal interface ILexer
{
    bool IsEndOfFile { get; }
    
    Token ReadToken();

    IEnumerable<Token> ReadTokens(int count);

    void ReadAndDiscardTokens(int count);

    Token PeekToken(int index = 0);

    void RestorePosition(int length);
}

internal struct Token
{
    private readonly IEnumerable<DiagnosticInfo>? _diagnostics;

    public Token(SyntaxKind kind, string text, IEnumerable<DiagnosticInfo>? diagnostics = null)
    {
        Kind = kind;
        Text = string.Intern(text);
        Length = text.Length;
        _diagnostics = diagnostics ?? Enumerable.Empty<DiagnosticInfo>();
    }

    public Token(SyntaxKind kind, string text, object? value, IEnumerable<DiagnosticInfo>? diagnostics = null)
    {
        Kind = kind;
        Text = string.Intern(text);
        Value = value;
        Length = text.Length;
        _diagnostics = diagnostics ?? Enumerable.Empty<DiagnosticInfo>();
    }

    public Token(SyntaxKind kind, string text, object? value, int length, IEnumerable<DiagnosticInfo>? diagnostics = null)
    {
        Kind = kind;
        Text = string.Intern(text);
        Value = value;
        Length = length;
        _diagnostics = diagnostics ?? Enumerable.Empty<DiagnosticInfo>();
    }

    public SyntaxKind Kind { get; }
    public string Text { get; }
    public object? Value { get; }
    public int Length { get; }

    internal IEnumerable<DiagnosticInfo>? GetDiagnostics() => _diagnostics;
}