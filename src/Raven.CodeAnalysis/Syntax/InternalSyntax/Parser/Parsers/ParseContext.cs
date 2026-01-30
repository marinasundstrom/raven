using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal abstract class ParseContext
{
    protected static Stopwatch _stopwatch = new Stopwatch();

    [Conditional("DEBUG")]
    protected static void PrintDebug(string text)
    {
        if (CodeGenFlags.PrintDebug)
        {
            Console.WriteLine($"{PrintLeadingDebug()}{text}");
        }
    }

    protected static string PrintLeadingDebug()
    {
        if (SyntaxParserFlags.PrintTimestamp)
        {
            return $"[{_stopwatch.ElapsedMilliseconds} ms] ";
        }
        return string.Empty;
    }

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

    public virtual int BlockDepth => Parent?.BlockDepth ?? throw new InvalidOperationException("No base or parent set");

    public virtual bool IsInBlock => BlockDepth > 0;

    public ParseContext? Parent { get; protected set; }

    public virtual int Position => Parent?.Position ?? throw new InvalidOperationException("No base or parent set");

    public virtual SyntaxToken? LastToken => Parent?.LastToken ?? throw new InvalidOperationException("No base or parent set");

    public virtual SyntaxToken PeekToken(int index = 0) => Parent?.PeekToken(index) ?? throw new InvalidOperationException("No base or parent set");

    public virtual SyntaxToken ReadToken() => Parent?.ReadToken() ?? throw new InvalidOperationException("No base or parent set");

    public virtual TextSpan GetStartOfLastToken() => Parent?.GetStartOfLastToken() ?? throw new InvalidOperationException("No base or parent set");

    public virtual TextSpan GetEndOfLastToken(int width = 0) => Parent?.GetEndOfLastToken(width) ?? throw new InvalidOperationException("No base or parent set");

    public virtual TextSpan GetSpanOfLastToken() => Parent?.GetSpanOfLastToken() ?? throw new InvalidOperationException("No base or parent set");

    public virtual TextSpan GetFullSpanOfLastToken() => Parent?.GetFullSpanOfLastToken() ?? throw new InvalidOperationException("No base or parent set");

    public virtual ParserCheckpoint CreateCheckpoint(string debugName = "",
        [CallerMemberName] string? callerMemberName = null,
        [CallerFilePath] string? callerFilePath = null,
        [CallerLineNumber] int? callerLineNumber = null)
    {
        var checkpoint = Parent!.CreateCheckpoint(debugName, callerMemberName, callerFilePath, callerLineNumber);
        return checkpoint;
    }

    public virtual void RewindToPosition(int position)
    {
        Parent?.RewindToPosition(position);
        Console.WriteLine($"{PrintLeadingDebug()}Rewinded to {position}");
    }

    public virtual bool TreatNewlinesAsTokens => Parent?.TreatNewlinesAsTokens ?? throw new InvalidOperationException("No base or parent set");

    public virtual void SetTreatNewlinesAsTokens(bool value) => Parent?.SetTreatNewlinesAsTokens(value);

    public virtual SyntaxToken SkipUntil(params IEnumerable<SyntaxKind> expectedKind)
    {
        return Parent?.SkipUntil(expectedKind) ?? throw new InvalidOperationException("No base or parent set");
    }
}
