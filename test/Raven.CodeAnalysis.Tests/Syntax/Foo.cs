using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class Foo(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Test()
    {
        var code =
        """
        import System.*;
        import System.Text.*;

        val list = [1, 42, 3];
        var i = 0; 

        val stringBuilder = new StringBuilder();

        while i < list.Length {
            val x = list[i];
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
        var node = (LiteralExpressionSyntax)f.WithAdditionalAnnotations(new SyntaxAnnotation("test"));

        var node2 = node.WithToken(SyntaxFactory.Literal(42));

        var newRoot = root.ReplaceNode(f, node2);

        var f2 = newRoot.DescendantNodes().OfType<LiteralExpressionSyntax>().First();

        var str = newRoot.ToFullString();

    }
}
