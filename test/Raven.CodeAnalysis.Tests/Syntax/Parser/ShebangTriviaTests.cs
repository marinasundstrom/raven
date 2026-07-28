using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Syntax.Parser;

public sealed class ShebangTriviaTests
{
    [Fact]
    public void Shebang_OnFirstLine_IsPreservedAsTrivia()
    {
        const string source = """
            #!/usr/bin/env rvn
            System.Console.WriteLine("Hello")
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var firstToken = root.GetFirstToken(includeZeroWidth: true);

        Assert.Empty(tree.GetDiagnostics());
        Assert.Equal(source, root.ToFullString());
        Assert.Contains(
            firstToken.LeadingTrivia,
            trivia => trivia.Kind == SyntaxKind.SingleLineCommentTrivia &&
                trivia.Text == "#!/usr/bin/env rvn");
    }

    [Fact]
    public void HashBang_AfterFirstLine_IsNotTreatedAsShebang()
    {
        const string source = """
            System.Console.WriteLine("Hello")
            #!/usr/bin/env rvn
            """;

        var tree = SyntaxTree.ParseText(source);

        Assert.NotEmpty(tree.GetDiagnostics());
    }
}
