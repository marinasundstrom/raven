using System.Diagnostics.CodeAnalysis;
using System.Text;

using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

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

    public SyntaxNode Parse(SourceText sourceText)
    {
        using var textReader = sourceText.GetTextReader();

        _lexer = new Lexer(textReader);

        var parseContext = new BaseParseContext(_lexer);
        return new CompilationUnitSyntaxParser(parseContext).Parse();
    }

    public SyntaxNode? ParseSyntax(Type requestedSyntaxType, SourceText sourceText, int position)
    {
        using var textReader = sourceText.GetTextReader(position);

        _lexer = new Lexer(textReader);
        var parseContext = new BaseParseContext(_lexer, position);

        return ParseRequestedType(parseContext, requestedSyntaxType);
    }

    private SyntaxNode? ParseRequestedType(BaseParseContext context, Type requestedSyntaxType)
    {
        if (requestedSyntaxType.IsAssignableTo(typeof(Syntax.StatementSyntax)))
        {
            return new StatementSyntaxParser(context).ParseStatement();
        }
        else if (requestedSyntaxType == typeof(Syntax.BlockSyntax))
        {
            return new ExpressionSyntaxParser(context).ParseExpression();
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

        throw new NotSupportedException("Syntax not supported");
    }

    public StatementSyntax ParseStatement(SourceText sourceText, int offset = 0, bool consumeFullText = true)
    {
        using var textReader = sourceText.GetTextReader();

        var lexer = new Lexer(textReader);
        var parseContext = new BaseParseContext(lexer, offset);

        return new StatementSyntaxParser(parseContext).ParseStatement();
    }
}