namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Tests;

public class GreenTreeTest
{

    [Fact]
    public void FullWidth_Equals_Source_Length()
    {
        var text = "int x = 0; // hello\n";
        var tree = SyntaxTree.ParseText(text);
        var root = tree.GetRoot();
        root.ToFullString().ShouldBe(text);
        root.Green.FullWidth.ShouldBe(text.Length);
    }

    [Fact]
    public void Token_FullWidth_Decomposes()
    {
        var text = "  /*c*/  x  ";
        var tree = SyntaxTree.ParseText(text);
        var token = tree.GetRoot().DescendantTokens().First(t => t.Kind == SyntaxKind.IdentifierToken).Green;

        (token.LeadingTrivia.Width + token.Width + token.TrailingTrivia.Width)
            .ShouldBe(token.FullWidth);
    }

    [Fact]
    public void Node_FullWidth_Equals_Sum_Of_Children()
    {
        var text = "class C { M () -> unit {} }";
        var green = SyntaxTree.ParseText(text).GetRoot().Green;

        static void Check(GreenNode n)
        {
            if (n is SyntaxToken tok)
            {
                // Token-specific invariant: FullWidth == leading + width + trailing
                (tok.LeadingTrivia.Width + tok.Width + tok.TrailingTrivia.Width)
                    .ShouldBe(tok.FullWidth);
                return;
            }

            int sum = 0;
            for (int i = 0; i < n.SlotCount; i++)
                sum += n.GetSlot(i)?.FullWidth ?? 0;

            sum.ShouldBe(n.FullWidth);

            // Recurse
            for (int i = 0; i < n.SlotCount; i++)
                n.GetSlot(i)?.Let(Check);
        }

        Check(green);
    }
}

public static class FunctionalExtensions
{
    public static void Let<T>(this T? obj, Action<T> action) where T : class
    {
        if (obj != null)
            action(obj);
    }
}
