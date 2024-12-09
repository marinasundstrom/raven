namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxTrivia
{
    private readonly InternalSyntax.SyntaxTrivia _trivia;
    private readonly SyntaxToken _parent;

    public SyntaxKind Kind => _trivia.Kind;
    public string Text => _trivia.Text;
    public IEnumerable<DiagnosticInfo> Diagnostics => _trivia.Diagnostics;

    public SyntaxTrivia(InternalSyntax.SyntaxTrivia trivia, SyntaxToken parent)
    {
        _trivia = trivia ?? throw new ArgumentNullException(nameof(trivia));
        _parent = parent;
    }

    public override string ToString() => Text;
}