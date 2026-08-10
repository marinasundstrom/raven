using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides a cursor over a macro body together with Raven fragment parsers
/// that consume from the cursor's current position.
/// </summary>
public sealed class MacroTokenStream : IMacroTokenStream
{
    private readonly TokenTreeMacroContext _context;
    private readonly IMacroTokenStream _inner;

    internal MacroTokenStream(
        TokenTreeMacroContext context,
        IMacroTokenStream inner)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
        _inner = inner ?? throw new ArgumentNullException(nameof(inner));
    }

    public bool IsEndOfFile => _inner.IsEndOfFile;

    public SyntaxToken PeekToken(int offset = 0)
        => _inner.PeekToken(offset);

    public SyntaxToken ReadToken()
        => _inner.ReadToken();

    /// <summary>
    /// Parses one Raven expression at the current position and advances past it.
    /// </summary>
    public MacroSyntaxParseResult<ExpressionSyntax> ParseExpression()
        => _context.ParseExpression(this);

    /// <summary>
    /// Parses one Raven statement at the current position and advances past it.
    /// </summary>
    public MacroSyntaxParseResult<StatementSyntax> ParseStatement()
        => _context.ParseStatement(this);

    /// <summary>
    /// Parses one Raven type at the current position and advances past it.
    /// </summary>
    public MacroSyntaxParseResult<TypeSyntax> ParseType()
        => _context.ParseType(this);

    /// <summary>
    /// Parses one Raven pattern at the current position and advances past it.
    /// </summary>
    public MacroSyntaxParseResult<PatternSyntax> ParsePattern()
        => _context.ParsePattern(this);
}
