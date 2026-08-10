using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal readonly record struct ParseResult(
    SyntaxNode Root,
    IReadOnlyList<DiagnosticInfo> Diagnostics,
    int ConsumedPosition);

internal class LanguageParser
{
    private readonly string _filePath;

    public ParseOptions Options { get; }

    public LanguageParser(string? filePath, ParseOptions options)
    {
        _filePath = filePath ?? string.Empty;
        Options = options ?? new ParseOptions();
    }

    public ParseResult Parse(SourceText sourceText)
    {
        using var textReader = sourceText.GetTextReader();

        var lexer = new Lexer(textReader, options: Options);

        var parseContext = new BaseParseContext(lexer, Options);
        var root = new CompilationUnitSyntaxParser(parseContext).Parse();
        return new ParseResult(root, parseContext.Diagnostics, parseContext.Position);
    }

    public SyntaxNode? ParseSyntax(Type requestedSyntaxType, SourceText sourceText, int position)
        => ParseSyntaxWithDiagnostics(requestedSyntaxType, sourceText, position)?.Root;

    public ParseResult? ParseSyntaxWithDiagnostics(
        Type requestedSyntaxType,
        SourceText sourceText,
        int position,
        bool consumeFullText = false)
    {
        using var textReader = sourceText.GetTextReader(position);

        var lexer = new Lexer(textReader, position, Options);
        var parseContext = new BaseParseContext(lexer, Options, position);

        try
        {
            var root = ParseRequestedType(parseContext, requestedSyntaxType);
            if (root is not null &&
                consumeFullText &&
                parseContext.PeekToken().Kind != SyntaxKind.EndOfFileToken)
            {
                var trailingToken = parseContext.ReadToken();
                parseContext.AddDiagnostic(DiagnosticInfo.Create(
                    CompilerDiagnostics.InvalidExpressionTerm,
                    parseContext.GetSpanOfLastToken(),
                    trailingToken.Text));
            }

            return root is null
                ? null
                : new ParseResult(root, parseContext.Diagnostics, parseContext.Position);
        }
        catch (NotSupportedException)
        {
            return null;
        }
    }

    private SyntaxNode? ParseRequestedType(BaseParseContext context, Type requestedSyntaxType)
    {
        if (requestedSyntaxType == typeof(Syntax.CompilationUnitSyntax))
        {
            return new CompilationUnitSyntaxParser(context).Parse();
        }
        else if (requestedSyntaxType == typeof(Syntax.PatternSyntax))
        {
            return new PatternSyntaxParser(context).ParsePattern();
        }
        else if (requestedSyntaxType == typeof(Syntax.TypeSyntax))
        {
            return new NameSyntaxParser(context).ParseTypeName();
        }
        else if (requestedSyntaxType.IsAssignableTo(typeof(Syntax.StatementSyntax)))
        {
            return new StatementSyntaxParser(context).ParseStatement();
        }
        else if (requestedSyntaxType == typeof(Syntax.BlockSyntax))
        {
            return new ExpressionSyntaxParser(context).ParseBlockSyntax();
        }
        else if (requestedSyntaxType.IsAssignableTo(typeof(Syntax.ExpressionSyntax)))
        {
            return new ExpressionSyntaxParser(context).ParseExpression();
        }
        else if (requestedSyntaxType == typeof(Syntax.ReturnStatementSyntax))
        {
            return new StatementSyntaxParser(context).ParseStatement();
        }
        else if (requestedSyntaxType == typeof(Syntax.NameSyntax))
        {
            return new NameSyntaxParser(context).ParseName();
        }
        else if (requestedSyntaxType == typeof(Syntax.IdentifierNameSyntax))
        {
            return new NameSyntaxParser(context).ParseSimpleName();
        }

        return null;
    }

    public StatementSyntax ParseStatement(SourceText sourceText, int offset = 0, bool consumeFullText = true)
    {
        using var textReader = sourceText.GetTextReader(offset);

        var lexer = new Lexer(textReader, offset, Options);
        var parseContext = new BaseParseContext(lexer, Options, offset);

        return new StatementSyntaxParser(parseContext).ParseStatement();
    }
}
