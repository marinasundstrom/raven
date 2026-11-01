namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

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
