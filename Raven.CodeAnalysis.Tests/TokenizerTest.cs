using Raven.CodeAnalysis.Parser;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.Parser;

using Shouldly;

namespace Raven.CodeAnalysis.Tests;
public class TokenizerTest(ITestOutputHelper testOutputHelper)
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
        return new Tokenizer(stringReader);
    }
}
