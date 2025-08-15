using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class Foo(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test()
    {
        var code =
        """
        import System;
        import System.Text;

        let list = [1, 42, 3];
        var i = 0; 

        let stringBuilder = new StringBuilder();

        while i < list.Length {
            let x = list[i];
            stringBuilder.AppendLine(x.ToString());
            if x > 3 {
                Console.WriteLine("Hello, World!");   
            }
            i = i + 1;
        }

        Console.WriteLine(stringBuilder.ToString());
        """;

        var syntaxTree = SyntaxTree.ParseText(code);

        var root = syntaxTree.GetRoot();

        var f = root.DescendantNodes().OfType<LiteralExpressionSyntax>().First();
    }
}