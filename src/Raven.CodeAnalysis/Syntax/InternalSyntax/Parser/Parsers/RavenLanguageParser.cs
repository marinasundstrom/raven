using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class RavenLanguageParser
{
    private readonly string _filePath;
    private Tokenizer _tokenizer;
    private ParseContext _parseContext;
    public ParseOptions Options { get; }

    public RavenLanguageParser(string? filePath, ParseOptions options)
    {
        _filePath = filePath ?? string.Empty;
        Options = options ?? new ParseOptions();
    }

    public SyntaxNode Parse(SourceText sourceText)
    {
        using var textReader = sourceText.GetTextReader();

        _tokenizer = new Tokenizer(textReader);
        _parseContext = new BaseParseContext(_tokenizer);
        return new StatementParser(_parseContext).ParseStatement();
    }
}
