
namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal interface ILexer
{
    bool IsEndOfFile { get; }

    Token ReadToken();

    IEnumerable<Token> ReadTokens(int count);

    void ReadAndDiscardTokens(int count);

    Token PeekToken(int index = 0);
}

internal struct Token
{
    private IEnumerable<DiagnosticInfo>? _diagnostics;

    public Token(SyntaxKind kind, object? value, IEnumerable<DiagnosticInfo>? diagnostics = null)
    {
        Kind = kind;
        Value = value;
        _diagnostics = diagnostics ?? Enumerable.Empty<DiagnosticInfo>();
    }

    public Token(SyntaxKind kind, object? value, int length, IEnumerable<DiagnosticInfo>? diagnostics = null)
    {
        Kind = kind;
        Value = value;
        Length = length;
        _diagnostics = diagnostics ?? Enumerable.Empty<DiagnosticInfo>();
    }

    public SyntaxKind Kind { get; }
    public object? Value { get; }
    public int Length { get; }
    public string? Text => GetValueText();

    public string? GetValueText()
    {
        if (Value is null) return string.Empty;

        if (Value is string text) return string.Intern(text);

        return string.Intern(Value.ToString()!);
    }

    internal IEnumerable<DiagnosticInfo>? GetDiagnostics() => _diagnostics;
}