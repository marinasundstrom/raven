using System.Collections;
using System.Text;

using Raven.CodeAnalysis.Parser.Internal;
using Raven.CodeAnalysis.Syntax;

using Shouldly;

using Xunit;
using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

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

public class TokenIterator(Tokenizer tokenizer) : IEnumerable<SyntaxToken>
{
    public IEnumerator<SyntaxToken> GetEnumerator()
    {
        SyntaxToken token;
        do
        {
            token = tokenizer.ReadToken();
            yield return token;
        } while (token.Kind != SyntaxKind.EndOfFileToken);
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}