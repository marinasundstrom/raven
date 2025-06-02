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
            import System;
            
            let list = [1, 3, 4];
            
            let w = 2
            let x = 1 + 2
            let y = if x > 2 { 40 + w; } else { true; }
            
            test(y);
            
            // External methods receiving: int | bool
            TestDep.Foo.Test(y);
            TestDep.Foo.Test(x);
            
            // External method returning type: int | bool
            let v = TestDep.Foo.Test2(true);
            
            //Console.WriteLine(v.ToString());
            
            if v is int a {
                Console.WriteLine(a);
            }
            else if v is bool b {
                Console.WriteLine(b);
            }
            
            func test (input : int | bool) -> void {
                if input is int a {
                    Console.WriteLine(a);
                }
                else if input is bool b {
                    Console.WriteLine(b);
                }
            }
            
            func f () -> void {
            
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