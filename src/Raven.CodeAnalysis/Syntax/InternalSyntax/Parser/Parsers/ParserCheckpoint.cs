namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal struct ParserCheckpoint : IDisposable
{
    private readonly BaseParseContext _context;
    private readonly string _debugName;
    private readonly int _position;

    private readonly SyntaxToken? _lastToken;
    private readonly List<SyntaxTrivia> _pendingTriviaSnapshot;

    internal ParserCheckpoint(BaseParseContext context, string debugName = "")
    {
        _context = context;
        _debugName = debugName;
        _position = context.Position;
        _lastToken = context._lastToken;
        _pendingTriviaSnapshot = [.. context._pendingTrivia];
    }

    public string DebugName => _debugName;

    public readonly void Rewind()
    {
        _context.RewindToPosition(_position);
        _context._lastToken = _lastToken;
        _context._pendingTrivia.Clear();
        _context._pendingTrivia.AddRange(_pendingTriviaSnapshot);
    }

    public void Dispose()
    {
        Rewind();
    }
}
