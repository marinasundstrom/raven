using System.Threading;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal abstract class ParseContext
{
    private List<DiagnosticInfo>? _diagnostics;


    private int _parenDepth;
    public void EnterParens() => _parenDepth++;
    public void ExitParens() => _parenDepth--;
    public bool IsInsideParens => _parenDepth > 0;

    protected ParseContext() { }

    protected ParseContext(ParseContext? parent)
    {
        Parent = parent;
    }

    public IReadOnlyList<DiagnosticInfo> Diagnostics => _diagnostics ?? [];

    public void AddDiagnostic(DiagnosticInfo diagnostic)
    {
        (_diagnostics ??= new List<DiagnosticInfo>()).Add(diagnostic);
    }

    public ParseContext? Parent { get; protected set; }

    public virtual CancellationToken CancellationToken => Parent?.CancellationToken ?? CancellationToken.None;

    public virtual int Position => Parent?.Position ?? throw new InvalidOperationException("No base or parent set");

    public virtual SyntaxToken? LastToken => Parent?.LastToken ?? throw new InvalidOperationException("No base or parent set");

    public virtual SyntaxToken PeekToken(int index = 0) => Parent?.PeekToken(index) ?? throw new InvalidOperationException("No base or parent set");

    public virtual SyntaxToken ReadToken() => Parent?.ReadToken() ?? throw new InvalidOperationException("No base or parent set");

    public virtual TextSpan GetStartOfLastToken() => Parent?.GetStartOfLastToken() ?? throw new InvalidOperationException("No base or parent set");

    public virtual TextSpan GetEndOfLastToken() => Parent?.GetEndOfLastToken() ?? throw new InvalidOperationException("No base or parent set");

    public virtual TextSpan GetSpanOfLastToken() => Parent?.GetSpanOfLastToken() ?? throw new InvalidOperationException("No base or parent set");

    public virtual TextSpan GetFullSpanOfLastToken() => Parent?.GetFullSpanOfLastToken() ?? throw new InvalidOperationException("No base or parent set");

    public virtual ParserCheckpoint CreateCheckpoint(string debugName = "")
    {
        return Parent!.CreateCheckpoint(debugName);
    }

    public virtual void RewindToPosition(int position)
    {
        Parent?.RewindToPosition(position);
    }

    public virtual bool TreatNewlinesAsTokens => Parent?.TreatNewlinesAsTokens ?? throw new InvalidOperationException("No base or parent set");

    public virtual void SetTreatNewlinesAsTokens(bool value) => Parent?.SetTreatNewlinesAsTokens(value);

    public virtual SyntaxToken SkipUntil(params SyntaxKind[] expectedKind)
    {
        return Parent?.SkipUntil(expectedKind) ?? throw new InvalidOperationException("No base or parent set");
    }
}
