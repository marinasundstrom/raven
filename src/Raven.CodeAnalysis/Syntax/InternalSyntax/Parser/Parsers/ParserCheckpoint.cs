using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal struct ParserCheckpoint : IDisposable
{
    private readonly BaseParseContext _context;
    private readonly string _debugName;
    private readonly int _position;

    private readonly SyntaxToken? _lastToken;
    private readonly List<SyntaxTrivia> _pendingTriviaSnapshot;
    private readonly int _blockDepth;

    internal ParserCheckpoint(BaseParseContext context, string debugName = "")
    {
        _context = context;
        _debugName = debugName;
        _position = context.Position;
        _lastToken = context._lastToken;
        _pendingTriviaSnapshot = [.. context._pendingTrivia];
        _blockDepth = context._blockDepth;
    }

    public string DebugName => _debugName;

    public readonly void Rewind(
        [CallerMemberName] string? callerMemberName = null,
        [CallerFilePath] string? callerFilePath = null,
        [CallerLineNumber] int? callerLineNumber = null)
    {
        if (ParseContext.IsInDebugMode)
        {
            callerFilePath = Path.GetRelativePath(Environment.CurrentDirectory, callerFilePath ?? Environment.CurrentDirectory);
            Console.WriteLine($"Rewind to checkpoint{(string.IsNullOrEmpty(DebugName) ? string.Empty : $" \"{DebugName}\"")} (position {_position}), at {callerMemberName}, in {callerFilePath}, at line {callerLineNumber} ");
        }
        _context.RewindToPosition(_position);
        _context._lastToken = _lastToken;
        _context._blockDepth = _blockDepth;
        _context._pendingTrivia.Clear();
        _context._pendingTrivia.AddRange(_pendingTriviaSnapshot);
    }

    public void Dispose()
    {
        Rewind();
    }
}
