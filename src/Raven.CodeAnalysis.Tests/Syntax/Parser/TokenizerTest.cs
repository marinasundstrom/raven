using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;
public class TokenizerTest
{
    [Fact]
    public void ReadIdentifierToken()
    {
        var str =
                """
                foo
                """;

        Tokenizer tokenizer = GetTokenizer(str);

        var result = tokenizer.ReadToken();

        result.Kind.ShouldBe(SyntaxKind.IdentifierToken);
        result.Width.ShouldBe(3);
        result.FullWidth.ShouldBe(3);
    }

    [Fact]
    public void ReadKeyword()
    {
        var str =
                """
                void
                """;

        Tokenizer tokenizer = GetTokenizer(str);

        var result = tokenizer.ReadToken();

        result.Kind.ShouldBe(SyntaxKind.VoidKeyword);
        result.Width.ShouldBe(4);
        result.FullWidth.ShouldBe(4);
    }

    [Fact]
    public void ReadNumericalToken()
    {
        var str =
                """
                22
                """;

        Tokenizer tokenizer = GetTokenizer(str);

        var result = tokenizer.ReadToken();

        result.Kind.ShouldBe(SyntaxKind.NumericLiteralToken);
        result.Width.ShouldBe(2);
        result.FullWidth.ShouldBe(2);
    }

    [Fact]
    public void ReadTokenWithLeadingTrivia()
    {
        var str =
            """
              foo
            """;

        Tokenizer tokenizer = GetTokenizer(str);

        var result = tokenizer.ReadToken();

        result.LeadingTrivia.ShouldContain(x => x.Kind == SyntaxKind.WhitespaceTrivia);
        result.TrailingTrivia.ShouldBeEmpty();
        result.Width.ShouldBe(3);
        result.FullWidth.ShouldBe(5);

    }

    [Fact]
    public void ReadTokenWithTrailingTrivia()
    {
        var str =
            """
            foo  
            """;

        Tokenizer tokenizer = GetTokenizer(str);

        var result = tokenizer.ReadToken();

        result.LeadingTrivia.ShouldBeEmpty();
        result.TrailingTrivia.ShouldContain(x => x.Kind == SyntaxKind.WhitespaceTrivia);
        result.Width.ShouldBe(3);
        result.FullWidth.ShouldBe(5);
    }

    [Fact]
    public void Test()
    {
        var str =
            """
            if (foo)  {
                if(x) {}
            }
            """;

        Tokenizer tokenizer = GetTokenizer(str);

        var tokens = (new TokenIterator(tokenizer)).ToList();
    }

    [Fact]
    public void Test2()
    {
        var str =
            """
            if (foo)  {
                if(x) {
                
                } 
                else {}
            }
            """;

        Tokenizer tokenizer = GetTokenizer(str);

        var tokens = (new TokenIterator(tokenizer)).ToList();
    }

    [Fact]
    public void Test3()
    {
        var str =
            """
            { 
            
            }
            """;

        Tokenizer tokenizer = GetTokenizer(str);

        var tokens = (new TokenIterator(tokenizer)).ToList();
    }

    private static Tokenizer GetTokenizer(string str)
    {
        StringReader stringReader = new StringReader(str);
        return new Tokenizer(stringReader, new List<DiagnosticInfo>());
    }
}