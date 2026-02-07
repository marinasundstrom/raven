using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

using IS = Raven.CodeAnalysis.Syntax.InternalSyntax;
using ParserInternal = Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class IdentifierTokenTests
{
    private sealed class SingleTokenLexer : ILexer
    {
        private readonly ParserInternal.Token[] _tokens;
        private int _index;
        public Action<IS.DiagnosticInfo>? DiagnosticSink { get; set; }

        public SingleTokenLexer(ParserInternal.Token token)
        {
            _tokens = new[] { token, new ParserInternal.Token(SyntaxKind.EndOfFileToken, string.Empty) };
        }

        public bool IsEndOfFile => _index >= _tokens.Length - 1;

        public ParserInternal.Token ReadToken() => _tokens[_index++];

        public IEnumerable<ParserInternal.Token> ReadTokens(int count)
        {
            var result = new List<ParserInternal.Token>();
            for (int i = 0; i < count; i++)
                result.Add(ReadToken());
            return result;
        }

        public void ReadAndDiscardTokens(int count) => _index += count;

        public ParserInternal.Token PeekToken(int index = 0) => _tokens[_index + index];

        public void ResetToPosition(int length) { }

        public void CreateCheckpoint() { }

        public void RewindToCheckpoint() { }

        public void ClearCheckpoint() { }
    }

    private sealed class TestParser : SyntaxParser
    {
        public TestParser(ParseContext parent) : base(parent)
        {
        }

        public IS.SyntaxToken ReadIdentifier() => ReadIdentifierToken();
    }

    [Fact]
    public void ConsumeToken_DoesNotPromoteKeyword()
    {
        var lexer = new SingleTokenLexer(new ParserInternal.Token(SyntaxKind.EnumKeyword, "enum"));
        var context = new BaseParseContext(lexer);
        var parser = new SyntaxParser(context);

        Assert.False(parser.ConsumeToken(SyntaxKind.IdentifierToken, out IS.SyntaxToken token));
        Assert.Equal(SyntaxKind.EnumKeyword, token.Kind);
        Assert.Equal(SyntaxKind.EnumKeyword, context.PeekToken().Kind);
    }

    [Fact]
    public void ReadIdentifierToken_PromotesKeyword()
    {
        var lexer = new SingleTokenLexer(new ParserInternal.Token(SyntaxKind.ImportKeyword, "import"));
        var context = new BaseParseContext(lexer);
        var parser = new TestParser(context);

        var token = parser.ReadIdentifier();

        Assert.Equal(SyntaxKind.IdentifierToken, token.Kind);
        Assert.Equal("import", token.Text);
    }
}
