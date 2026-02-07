using System.Diagnostics.CodeAnalysis;
using System.Text;

using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal readonly record struct ParseResult(SyntaxNode Root, IReadOnlyList<DiagnosticInfo> Diagnostics);

internal class LanguageParser
{
    private readonly string _filePath;
    private ILexer _lexer;

    private int Position { get; set; }

    public ParseOptions Options { get; }
    public Encoding Encoding { get; }

    public LanguageParser(string? filePath, ParseOptions options)
    {
        _filePath = filePath ?? string.Empty;
        Options = options ?? new ParseOptions();
    }

    public ParseResult Parse(SourceText sourceText)
    {
        using var textReader = sourceText.GetTextReader();

        _lexer = new Lexer(textReader);

        var parseContext = new BaseParseContext(_lexer, Options);
        var root = new CompilationUnitSyntaxParser(parseContext).Parse();
        return new ParseResult(root, parseContext.Diagnostics);
    }

    public SyntaxNode? ParseSyntax(Type requestedSyntaxType, SourceText sourceText, int position)
    {
        using var textReader = sourceText.GetTextReader(position);

        _lexer = new Lexer(textReader, position);
        var parseContext = new BaseParseContext(_lexer, Options, position);

        try
        {
            return ParseRequestedType(parseContext, requestedSyntaxType);
        }
        catch (NotSupportedException)
        {
            return null;
        }
    }

    private SyntaxNode? ParseRequestedType(BaseParseContext context, Type requestedSyntaxType)
    {
        if (requestedSyntaxType.IsAssignableTo(typeof(Syntax.StatementSyntax)))
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

        var lexer = new Lexer(textReader, offset);
        var parseContext = new BaseParseContext(lexer, Options, offset);

        return new StatementSyntaxParser(parseContext).ParseStatement();
    }
}
